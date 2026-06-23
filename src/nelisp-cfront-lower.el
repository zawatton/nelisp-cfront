;;; nelisp-cfront-lower.el --- Lower the C AST to the nelisp-cc grammar -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M2.4 — lower the M2.2 AST to a `(seq (defun ...) ...)' grammar form
;; ready for `nelisp-aot-compile-to-object'.  Implements the Doc 02
;; scheme: mutable C locals become frame-slot `let' bindings (with a
;; NON-foldable init — a call to the per-object `nelisp_cfront__zero'
;; helper, which works even for no-param functions), control flow uses
;; grammar `if'/`while', assignment uses `setq', integers map to the
;; native i64 ops.
;;
;; MVP subset (integer C): functions, params, int/long locals, arithmetic
;; (+ - * / %), bitwise (& | ^ << >> ~), comparisons (< <= > >= == !=),
;; logical (&& || !), if/else, while, for, tail `return', assignment
;; (= and compound), calls, ?:, sizeof(type), unary -, *p load, *p = e.
;;
;; Deferred (clear error): early `return' inside loops / non-tail return,
;; break/continue, struct member access (needs M2.3 layout), &addr,
;; array indexing element size != 8, string literals (need rodata),
;; function pointers.  These are M2.3/M3/M4 work.

;;; Code:

(require 'cl-lib)
(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-type)
(require 'nelisp-cfront-float)

(define-error 'nelisp-cfront-lower-error "nelisp-cfront lowering error")

(defvar nelisp-cfront-lower--uses-float nil
  "Set non-nil during lowering when any float op needs the soft-float
helpers; `lower-program' then prepends the helper defuns.")

(defvar nelisp-cfront-lower--structs nil
  "Struct table (name->layout) for the program being lowered.")
(defvar nelisp-cfront-lower--funcs nil
  "Function return-type table (name->type) for the program.")
(defvar nelisp-cfront-lower--func-params nil
  "Function parameter-type table (name->list-of-param-types) for the
program, used to coerce call arguments between int and double-bits.")
(defvar nelisp-cfront-lower--tenv nil
  "Type environment (var-name->type) for the current function.")
(defvar nelisp-cfront-lower--synth nil
  "Synthetic frame-slot locals created during lowering (e.g. break flags).")
(defvar nelisp-cfront-lower--brk-stack nil
  "Stack of break-flag symbols for the enclosing loops (innermost first).")
(defvar nelisp-cfront-lower--brk-counter 0
  "Per-function counter for generating unique break-flag names.")
(defvar nelisp-cfront-lower--ret-mode nil
  "When non-nil, (RET-SET-SYM . RET-VAL-SYM) for single-exit return mode
\(used for functions that `return' from inside a loop).")
(defvar nelisp-cfront-lower--goto-flag nil
  "Goto flag symbol when the function uses `goto' (single forward label).")
(defvar nelisp-cfront-lower--ret-float nil
  "Non-nil when the current function returns a scalar float/double (so
`return' coerces an integer operand to double-bits).")
(defvar nelisp-cfront-lower--mem-vars nil
  "Alist NAME->type of memory-backed locals in the current function — C
arrays, struct-by-value, and address-taken scalars.  Such a variable's
grammar slot holds the *address* of a `frame-alloc' block; an aggregate
decays to that address, a scalar is read/written through it.")

(defvar nelisp-cfront-lower--globals nil
  "Alist NAME->(:type TY :bytes UNIBYTE) of read-only integer global
scalars/arrays with a constant initializer (Doc 06 Step B).  Each is
emitted as a `data-blob' in `.rodata' and referenced via `(data-addr
NAME)'.  Globals not in this table (no/non-const/non-integer init, or
struct/pointer globals) are left for Step C and keep their old behaviour.")

(defvar nelisp-cfront-lower--local-names nil
  "List of the current function's param + local variable NAMEs (strings).
A `var' whose name is here is a local/param (lowers to its grammar slot);
otherwise a name present in `--globals' is a global (lowers via
`data-addr').  Locals therefore shadow same-named globals.")

(defvar nelisp-cfront-lower--string-pool nil
  "Alist S->SYM interning the program's C string literals (Doc 06 Step D).
Each distinct (`equal') string literal becomes one NUL-terminated
`data-blob' rodata symbol; `(str S)' lowers to `(data-addr SYM)'.  Filled
as a side effect of lowering function bodies, then emitted by
`lower-program'.")

(defvar nelisp-cfront-lower--string-counter 0
  "Counter for generating unique string-pool symbol names.")

(defconst nelisp-cfront-lower--zero-fn 'nelisp_cfront__zero
  "Per-object helper returning a non-foldable 0 (forces frame-slot let).")

(defconst nelisp-cfront-lower--binop-map
  '(("+" . +) ("-" . -) ("*" . *) ("/" . /) ("%" . mod)
    ("&" . logand) ("|" . logior) ("^" . logxor) ("<<" . shl) (">>" . sar)
    ("<" . <) ("<=" . <=) (">" . >) (">=" . >=) ("==" . =))
  "C binary operators that map directly to a grammar op.")

(defun nelisp-cfront-lower--err (what node)
  (signal 'nelisp-cfront-lower-error (list what node)))

(defun nelisp-cfront-lower--sym (name)
  "Function-name symbol (kept literal for C linkage / .o symbol name)."
  (if (stringp name) (intern name)
    (nelisp-cfront-lower--err :bad-name name)))

(defun nelisp-cfront-lower--lvar (name)
  "Mangle a C local/param NAME to a collision-safe grammar symbol.
C identifiers like `t'/`nil' collide with elisp constants and grammar
special forms; locals/params are internal so we namespace them.  (Function
names stay literal — the linker needs the original C symbol.)"
  (if (stringp name) (intern (concat "nlcf_v_" name))
    (nelisp-cfront-lower--err :bad-name name)))

;;; --- typed memory access (M2.3) -------------------------------------

(defun nelisp-cfront-lower--type-of (e)
  (nelisp-cfront-type-of e nelisp-cfront-lower--tenv
                         nelisp-cfront-lower--structs
                         nelisp-cfront-lower--funcs))

(defun nelisp-cfront-lower--var-is-function-p (name)
  "Non-nil when NAME denotes a function (not shadowed by a local/param)."
  (and (not (assoc name nelisp-cfront-lower--tenv))
       (assoc name nelisp-cfront-lower--funcs)))

;;; --- memory-backed locals (frame-alloc: arrays / structs / &x) -------

(defun nelisp-cfront-lower--aggregate-type-p (ty)
  "Non-nil when TY is an aggregate stored by value: a C array, or a
struct/union value (base struct, no pointer)."
  (and ty
       (or (plist-get ty :array)
           (and (eq (plist-get ty :base) 'struct)
                (= 0 (or (plist-get ty :ptr) 0))))))

(defun nelisp-cfront-lower--narrow-int-width (ty)
  "Byte width (1/2/4) when TY is a narrow integer carried in a 64-bit
slot/register, else nil (full-width long/pointer, aggregate, or float).
cfront keeps every value as i64, so a narrow int needs explicit sign/zero
extension to behave like its C width at ABI / load boundaries."
  (and (= 0 (or (plist-get ty :ptr) 0))
       (null (plist-get ty :array))
       (memq (plist-get ty :base) '(char short int))
       (let ((w (nelisp-cfront-type-size ty nelisp-cfront-lower--structs)))
         (and (< w 8) w))))

(defun nelisp-cfront-lower--normalize-narrow (g ty)
  "Wrap grammar expr G to hold the correct narrow-int value of TY:
sign-extend (signed) or mask (unsigned) to the type width.  Identity for
full-width / non-integer types."
  (let ((w (nelisp-cfront-lower--narrow-int-width ty)))
    (cond
     ((not w) g)
     ((plist-get ty :unsigned) `(logand ,g ,(1- (ash 1 (* 8 w)))))
     (t (let ((k (- 64 (* 8 w)))) `(sar (shl ,g ,k) ,k))))))

(defun nelisp-cfront-lower--mem-var (name)
  "Return (NAME . TYPE) when NAME is a memory-backed local, else nil."
  (assoc name nelisp-cfront-lower--mem-vars))

(defun nelisp-cfront-lower--mem-var-scalar-p (name)
  "Non-nil when memory-backed NAME is a scalar (read/written through the
pointer) rather than an aggregate (which decays to the address)."
  (let ((mv (nelisp-cfront-lower--mem-var name)))
    (and mv (not (nelisp-cfront-lower--aggregate-type-p (cdr mv))))))

(defun nelisp-cfront-lower--collect-addr-taken (node acc)
  "Collect names appearing as `&NAME' (scalar address-of) anywhere in the
NODE cons tree into ACC (a full car/cdr walk, so arg lists are covered)."
  (cond
   ((and (consp node) (eq (car node) 'unop) (equal (nth 1 node) "&")
         (consp (nth 2 node)) (eq (car (nth 2 node)) 'var))
    (push (nth 1 (nth 2 node)) acc)
    (nelisp-cfront-lower--collect-addr-taken (nth 2 node) acc))
   ((consp node)
    (setq acc (nelisp-cfront-lower--collect-addr-taken (car node) acc))
    (nelisp-cfront-lower--collect-addr-taken (cdr node) acc))
   (t acc)))

;;; --- read-only integer global data (Doc 06 Step B) ------------------

(defun nelisp-cfront-lower--global-var-p (name)
  "Non-nil when NAME refers to a const integer global (in `--globals') and
is not shadowed by a param/local of the current function."
  (and (not (member name nelisp-cfront-lower--local-names))
       (assoc name nelisp-cfront-lower--globals)))

(defun nelisp-cfront-lower--pack-int (v width)
  "Pack integer V into a little-endian unibyte string of WIDTH bytes,
masking to the low WIDTH*8 bits (so negatives wrap two's-complement)."
  (let ((u (logand v (1- (ash 1 (* 8 width)))))
        (bytes nil))
    (dotimes (i width) (push (logand (ash u (* -8 i)) 255) bytes))
    (apply #'unibyte-string (nreverse bytes))))

(defun nelisp-cfront-lower--global-bytes (ty init)
  "Return the `.rodata' unibyte image of a read-only integer global of type
TY with initializer INIT, or nil when it is not a const integer
scalar/array this step handles (signals on a non-constant element so the
caller skips the whole global).  Element widths 1/2/4/8 are supported;
arrays zero-pad to their declared dimension."
  (let ((arr (plist-get ty :array)))
    (cond
     ;; --- integer array: {e, ...} or "string" for char[] ---
     (arr
      (let* ((elem-ty (nelisp-cfront-type--strip-array ty)))
        ;; Step B handles flat (single-dimension) integer arrays only.
        (when (and (null (plist-get elem-ty :array))
                   (= 0 (or (plist-get elem-ty :ptr) 0))
                   (memq (plist-get elem-ty :base) '(char short int long)))
          (let* ((w (nelisp-cfront-type-size elem-ty nelisp-cfront-lower--structs))
                 (declared (and (integerp arr) arr)))
            (cond
             ((and (consp init) (eq (car init) 'init-list))
              (let* ((elts (cdr init))
                     (packed (mapconcat
                              (lambda (e)
                                (nelisp-cfront-lower--pack-int
                                 (nelisp-cfront-parse--const-eval e) w))
                              elts ""))
                     (total (* (or declared (length elts)) w)))
                (cond ((= (length packed) total) packed)
                      ((< (length packed) total)
                       (concat packed (make-string (- total (length packed)) 0)))
                      (t (substring packed 0 total)))))
             ;; char x[] = "..."  (NUL-terminated; padded to declared size)
             ((and (consp init) (eq (car init) 'str) (= w 1))
              (let* ((s (encode-coding-string (nth 1 init) 'utf-8 t))
                     (total (or declared (1+ (length s))))
                     (buf (make-string total 0)))
                (dotimes (j (min (length s) total)) (aset buf j (aref s j)))
                buf))
             (t nil))))))
     ;; --- integer scalar with a constant initializer ---
     ((and init
           (= 0 (or (plist-get ty :ptr) 0))
           (memq (plist-get ty :base) '(char short int long)))
      (nelisp-cfront-lower--pack-int
       (nelisp-cfront-parse--const-eval init)
       (nelisp-cfront-type-size ty nelisp-cfront-lower--structs)))
     (t nil))))

(defun nelisp-cfront-lower--collect-globals (ast)
  "Return an alist NAME->(:type TY :bytes UNIBYTE) for every const integer
global scalar/array in AST `(program TOP...)'.  Non-constant or
non-integer globals are skipped (left for Step C)."
  (let ((out nil))
    (dolist (top (cdr ast))
      (when (eq (car top) 'global)
        (let ((ty (nth 1 top)) (name (nth 2 top)) (init (nth 3 top)))
          (when init
            (condition-case nil
                (let ((bytes (nelisp-cfront-lower--global-bytes ty init)))
                  (when (and bytes (not (assoc name out)))
                    (push (cons name (list :type ty :bytes bytes)) out)))
              (error nil))))))           ; non-foldable -> skip this global
    (nreverse out)))

;;; --- C string literal pool (Doc 06 Step D) --------------------------

(defun nelisp-cfront-lower--intern-string (s)
  "Return the rodata symbol for C string literal S, interning it into
`--string-pool' (deduped by `equal') on first use."
  (or (cdr (assoc s nelisp-cfront-lower--string-pool))
      (let ((sym (intern (format "nlcf_str_%d"
                                 nelisp-cfront-lower--string-counter))))
        (setq nelisp-cfront-lower--string-counter
              (1+ nelisp-cfront-lower--string-counter))
        (push (cons s sym) nelisp-cfront-lower--string-pool)
        sym)))

(defun nelisp-cfront-lower--string-bytes (s)
  "Return the NUL-terminated unibyte `.rodata' image of C string S."
  (concat (encode-coding-string s 'utf-8 t) (unibyte-string 0)))

;;; --- soft-float conversions (double carried as i64 bits) ------------

(defun nelisp-cfront-lower--expr-float-p (e)
  "Non-nil when C expression E has scalar float/double type."
  (nelisp-cfront-float-type-p (nelisp-cfront-lower--type-of e)))

(defun nelisp-cfront-lower--mark-float ()
  (setq nelisp-cfront-lower--uses-float t))

(defun nelisp-cfront-lower--as-double-bits (e)
  "Lower C expression E to grammar double-bits (i64), converting from int
with the `i2d' helper when E is integer-typed."
  (let ((g (nelisp-cfront-lower--expr e)))
    (if (nelisp-cfront-lower--expr-float-p e)
        g
      (nelisp-cfront-lower--mark-float)
      (list 'nelisp_cfront__i2d g))))

(defun nelisp-cfront-lower--as-int (e)
  "Lower C expression E to a grammar integer, converting from double-bits
with the `d2i' helper (truncation) when E is float-typed."
  (let ((g (nelisp-cfront-lower--expr e)))
    (if (nelisp-cfront-lower--expr-float-p e)
        (progn (nelisp-cfront-lower--mark-float)
               (list 'nelisp_cfront__d2i g))
      g)))

(defun nelisp-cfront-lower--coerce (g from-float-p to-float-p)
  "Coerce already-lowered grammar G between int and double-bits per the
FROM-FLOAT-P / TO-FLOAT-P flags."
  (cond
   ((eq (not from-float-p) (not to-float-p)) g)   ; same class
   (to-float-p (nelisp-cfront-lower--mark-float)
               (list 'nelisp_cfront__i2d g))      ; int -> double bits
   (t (nelisp-cfront-lower--mark-float)
      (list 'nelisp_cfront__d2i g))))             ; double bits -> int

(defun nelisp-cfront-lower--return-value (expr)
  "Lower a `return' operand EXPR, coercing to the function's return class."
  (if (null expr) 0
    (nelisp-cfront-lower--coerce
     (nelisp-cfront-lower--expr expr)
     (nelisp-cfront-lower--expr-float-p expr)
     nelisp-cfront-lower--ret-float)))

(defun nelisp-cfront-lower--load-w (addr width)
  "Load WIDTH bytes from grammar address ADDR (offset folded into ADDR)."
  (pcase width
    (1 `(ptr-read-u8 ,addr 0))
    (8 `(ptr-read-u64 ,addr 0))
    (2 `(logior (ptr-read-u8 ,addr 0) (shl (ptr-read-u8 ,addr 1) 8)))
    (4 `(logior (ptr-read-u8 ,addr 0)
         (logior (shl (ptr-read-u8 ,addr 1) 8)
          (logior (shl (ptr-read-u8 ,addr 2) 16)
                  (shl (ptr-read-u8 ,addr 3) 24)))))
    (_ (nelisp-cfront-lower--err :unsupported-load-width width))))

(defun nelisp-cfront-lower--store-w (addr width val)
  "Store low WIDTH bytes of VAL to grammar address ADDR."
  (pcase width
    (1 `(ptr-write-u8 ,addr 0 ,val))
    (8 `(ptr-write-u64 ,addr 0 ,val))
    (2 `(seq (ptr-write-u8 ,addr 0 (logand ,val 255))
             (ptr-write-u8 ,addr 1 (logand (sar ,val 8) 255))))
    (4 `(seq (ptr-write-u8 ,addr 0 (logand ,val 255))
             (ptr-write-u8 ,addr 1 (logand (sar ,val 8) 255))
             (ptr-write-u8 ,addr 2 (logand (sar ,val 16) 255))
             (ptr-write-u8 ,addr 3 (logand (sar ,val 24) 255))))
    (_ (nelisp-cfront-lower--err :unsupported-store-width width))))

(defun nelisp-cfront-lower--copy-bytes (dst src n)
  "Lower a struct/array by-value copy of N bytes from address SRC to DST.
DST and SRC are evaluated once (into fresh frame temps) then copied in
descending word widths (u64/u32/u16/u8) at constant offsets."
  (let ((d (nelisp-cfront-lower--gensym "cpd"))
        (s (nelisp-cfront-lower--gensym "cps"))
        (forms nil) (off 0))
    (while (<= (+ off 8) n)
      (push `(ptr-write-u64 ,d ,off (ptr-read-u64 ,s ,off)) forms) (setq off (+ off 8)))
    (while (<= (+ off 4) n)
      (push `(ptr-write-u32 ,d ,off (ptr-read-u32 ,s ,off)) forms) (setq off (+ off 4)))
    (while (<= (+ off 2) n)
      (push `(ptr-write-u16 ,d ,off (ptr-read-u16 ,s ,off)) forms) (setq off (+ off 2)))
    (while (<= (+ off 1) n)
      (push `(ptr-write-u8 ,d ,off (ptr-read-u8 ,s ,off)) forms) (setq off (+ off 1)))
    `(let ((,d ,dst) (,s ,src)) ,(nelisp-cfront-lower--seq (nreverse forms)))))

(defun nelisp-cfront-lower--elem-size (ptr-expr)
  "Element size of the pointee/array element of PTR-EXPR's type."
  (nelisp-cfront-type-size
   (nelisp-cfront-type-elem (nelisp-cfront-lower--type-of ptr-expr))
   nelisp-cfront-lower--structs))

(defun nelisp-cfront-lower--addr (e)
  "Grammar address of lvalue E (deref / index / arrow / member / var)."
  (pcase (car e)
    ('var (cond
           ((nelisp-cfront-lower--mem-var (nth 1 e))
            (nelisp-cfront-lower--lvar (nth 1 e)))    ; frame-alloc block address
           ((nelisp-cfront-lower--global-var-p (nth 1 e))
            `(data-addr ,(intern (nth 1 e))))         ; rodata global symbol address
           (t (nelisp-cfront-lower--err :addr-of-non-memory-var e))))
    ('unop (if (string= (nth 1 e) "*")
               (nelisp-cfront-lower--expr (nth 2 e))
             (nelisp-cfront-lower--err :not-an-lvalue e)))
    ('index `(+ ,(nelisp-cfront-lower--expr (nth 1 e))
                (* ,(nelisp-cfront-lower--expr (nth 2 e))
                   ,(nelisp-cfront-lower--elem-size (nth 1 e)))))
    ('arrow
     (let* ((pty (nelisp-cfront-lower--type-of (nth 1 e)))
            (fld (nelisp-cfront-type-field-ty pty (nth 2 e)
                                              nelisp-cfront-lower--structs)))
       `(+ ,(nelisp-cfront-lower--expr (nth 1 e)) ,(plist-get fld :offset))))
    ('member
     (let* ((oty (nelisp-cfront-lower--type-of (nth 1 e)))
            (fld (nelisp-cfront-type-field-ty oty (nth 2 e)
                                              nelisp-cfront-lower--structs)))
       `(+ ,(nelisp-cfront-lower--addr (nth 1 e)) ,(plist-get fld :offset))))
    (_ (nelisp-cfront-lower--err :not-an-lvalue e))))

(defun nelisp-cfront-lower--field-of (e)
  "Return the field plist (:type :offset :size [:bits :bit-offset]) for an
arrow/member lvalue E, or nil."
  (pcase (car e)
    ((or 'arrow 'member)
     (let ((oty (nelisp-cfront-lower--type-of (nth 1 e))))
       (and (eq (plist-get oty :base) 'struct)
            (ignore-errors
              (nelisp-cfront-type-field-ty oty (nth 2 e)
                                           nelisp-cfront-lower--structs)))))
    (_ nil)))

(defun nelisp-cfront-lower--bitfield-assign (lhs op grhs fld)
  "Lower an assignment to bitfield LHS: read unit, clear bits, OR in value."
  (let* ((addr (nelisp-cfront-lower--addr lhs))
         (bo (plist-get fld :bit-offset))
         (w (plist-get fld :bits))
         (mask (1- (ash 1 w)))
         (clear (logand (lognot (ash mask bo)) #xFFFFFFFF))
         (newval (if (string= op "=")
                     grhs
                   (let* ((bop (substring op 0 (1- (length op))))
                          (g (cdr (assoc bop nelisp-cfront-lower--binop-map)))
                          (cur `(logand (sar ,(nelisp-cfront-lower--load-w addr 4) ,bo) ,mask)))
                     (unless g (nelisp-cfront-lower--err :unsupported-compound-assign op))
                     (list g cur grhs)))))
    (nelisp-cfront-lower--store-w
     addr 4
     `(logior (logand ,(nelisp-cfront-lower--load-w addr 4) ,clear)
              (shl (logand ,newval ,mask) ,bo)))))

(defun nelisp-cfront-lower--load-lvalue (e)
  "Load the value of memory lvalue E (bitfield-aware)."
  (let ((fld (nelisp-cfront-lower--field-of e)))
    (if (and fld (plist-get fld :bits))
        ;; bitfield read: (unit >> bit-offset) & ((1<<bits)-1)
        (let ((mask (1- (ash 1 (plist-get fld :bits)))))
          `(logand (sar ,(nelisp-cfront-lower--load-w
                          (nelisp-cfront-lower--addr e) 4)
                        ,(plist-get fld :bit-offset))
                   ,mask))
      ;; scalar load; `--load-w' zero-extends, so a SIGNED narrow int must
      ;; be sign-extended to behave like its C width (unsigned is already
      ;; exact from the masked byte load).
      (let* ((ty (nelisp-cfront-lower--type-of e))
             (g (nelisp-cfront-lower--load-w
                 (nelisp-cfront-lower--addr e)
                 (nelisp-cfront-type-size ty nelisp-cfront-lower--structs)))
             (w (nelisp-cfront-lower--narrow-int-width ty)))
        (if (and w (not (plist-get ty :unsigned)))
            (let ((k (- 64 (* 8 w)))) `(sar (shl ,g ,k) ,k))
          g)))))

(defun nelisp-cfront-lower--rvalue (e)
  "Lower memory lvalue E in rvalue (value) context.
Aggregate lvalues (a C array or struct/union value) *decay* to their
address — `a[i]', `s.f', `&s.f', struct copy all consume that address —
so we never try to load a whole struct/array.  Scalars are loaded."
  (if (nelisp-cfront-lower--aggregate-type-p (nelisp-cfront-lower--type-of e))
      (nelisp-cfront-lower--addr e)
    (nelisp-cfront-lower--load-lvalue e)))

;;; --- collect mutable locals (for the outer frame-slot let) -----------

(defun nelisp-cfront-lower--collect-decls (node acc)
  "Collect local declaration names from NODE into ACC (a list); return ACC."
  (when (consp node)
    (pcase (car node)
      ('decl (push (nth 2 node) acc))
      ('decls (dolist (d (cdr node)) (setq acc (nelisp-cfront-lower--collect-decls d acc))))
      ('block (dolist (s (cdr node)) (setq acc (nelisp-cfront-lower--collect-decls s acc))))
      ('if (setq acc (nelisp-cfront-lower--collect-decls (nth 2 node) acc))
           (setq acc (nelisp-cfront-lower--collect-decls (nth 3 node) acc)))
      ('while (setq acc (nelisp-cfront-lower--collect-decls (nth 2 node) acc)))
      ('for (dolist (k (list (nth 1 node) (nth 4 node)))
              (setq acc (nelisp-cfront-lower--collect-decls k acc))))
      (_ nil)))
  acc)

(defun nelisp-cfront-lower--collect-decl-types (node acc)
  "Collect (NAME . TYPE) for local declarations in NODE into ACC."
  (when (consp node)
    (pcase (car node)
      ('decl (push (cons (nth 2 node) (nth 1 node)) acc))
      ('decls (dolist (d (cdr node)) (setq acc (nelisp-cfront-lower--collect-decl-types d acc))))
      ('block (dolist (s (cdr node)) (setq acc (nelisp-cfront-lower--collect-decl-types s acc))))
      ('if (setq acc (nelisp-cfront-lower--collect-decl-types (nth 2 node) acc))
           (setq acc (nelisp-cfront-lower--collect-decl-types (nth 3 node) acc)))
      ('while (setq acc (nelisp-cfront-lower--collect-decl-types (nth 2 node) acc)))
      ('for (dolist (k (list (nth 1 node) (nth 4 node)))
              (setq acc (nelisp-cfront-lower--collect-decl-types k acc))))
      (_ nil)))
  acc)

(defun nelisp-cfront-lower--collect-func-types (program)
  "Alist NAME->ret-type for `func'/`proto' top-levels in PROGRAM."
  (let ((acc nil))
    (dolist (top (cdr program))
      (when (memq (car top) '(func proto))
        (push (cons (nth 2 top) (nth 1 top)) acc)))
    acc))

(defun nelisp-cfront-lower--collect-func-params (program)
  "Alist NAME -> list of param TYPEs (positional) for `func'/`proto'.
The lone `(void)' marker is dropped; unnamed prototype params are kept
so positions stay aligned to call arguments."
  (let ((acc nil))
    (dolist (top (cdr program))
      (when (memq (car top) '(func proto))
        (push (cons (nth 2 top)
                    (delq nil (mapcar
                               (lambda (p)
                                 (let ((ty (nth 1 p)))
                                   (unless (and (eq (plist-get ty :base) 'void)
                                                (= 0 (or (plist-get ty :ptr) 0)))
                                     ty)))
                               (nth 3 top))))
              acc)))
    acc))

;;; --- expressions -----------------------------------------------------

(defun nelisp-cfront-lower--expr (e)
  (pcase (car e)
    ('int (nth 1 e))
    ('var (let ((name (nth 1 e)))
            (cond
             ((nelisp-cfront-lower--var-is-function-p name)
              `(addr-of ,(nelisp-cfront-lower--sym name)))   ; function -> pointer
             ((nelisp-cfront-lower--mem-var-scalar-p name)
              ;; address-taken scalar: read through the frame-alloc pointer
              (nelisp-cfront-lower--load-w
               (nelisp-cfront-lower--lvar name)
               (nelisp-cfront-type-size (cdr (nelisp-cfront-lower--mem-var name))
                                        nelisp-cfront-lower--structs)))
             ((nelisp-cfront-lower--global-var-p name)
              ;; rodata global: a scalar loads through `(data-addr NAME)',
              ;; an array/struct decays to that address (handled by --rvalue).
              (nelisp-cfront-lower--rvalue e))
             ;; aggregate local decays to its block address (= the slot value);
             ;; plain scalar reads its value slot directly.
             (t (nelisp-cfront-lower--lvar name)))))
    ('str `(data-addr ,(nelisp-cfront-lower--intern-string (nth 1 e)))) ; rodata pool
    ('binop (nelisp-cfront-lower--binop (nth 1 e) (nth 2 e) (nth 3 e)))
    ('unop (if (string= (nth 1 e) "*")
               (nelisp-cfront-lower--rvalue e)   ; typed deref (aggregate decays)
             (nelisp-cfront-lower--unop (nth 1 e) (nth 2 e))))
    ('assign (nelisp-cfront-lower--assign (nth 1 e) (nth 2 e) (nth 3 e)))
    ('call (nelisp-cfront-lower--call (nth 1 e) (nth 2 e)))
    ('ternary `(if ,(nelisp-cfront-lower--cond (nth 1 e))
                   ,(nelisp-cfront-lower--expr (nth 2 e))
                 ,(nelisp-cfront-lower--expr (nth 3 e))))
    ('index (nelisp-cfront-lower--rvalue e))      ; element load (aggregate decays)
    ('sizeof (nelisp-cfront-lower--sizeof (nth 1 e)))
    ('sizeof-expr (nelisp-cfront-type-size (nelisp-cfront-lower--type-of (nth 1 e))
                                           nelisp-cfront-lower--structs))
    ('cast (nelisp-cfront-lower--coerce             ; int<->double conversion; else identity
            (nelisp-cfront-lower--expr (nth 2 e))
            (nelisp-cfront-lower--expr-float-p (nth 2 e))
            (nelisp-cfront-float-type-p (nth 1 e))))
    ('va-arg (nelisp-cfront-lower--err :varargs-unsupported e))  ; Tier 3 / upstream
    ('fnum (progn (nelisp-cfront-lower--mark-float)               ; double carried as i64 bits
                  (nelisp-cfront-float--double-to-bits (nth 1 e))))
    ('comma (nelisp-cfront-lower--seq (list (nelisp-cfront-lower--expr (nth 1 e))
                                            (nelisp-cfront-lower--expr (nth 2 e)))))
    ((or 'pre 'post) (nelisp-cfront-lower--incdec e))
    ((or 'member 'arrow) (nelisp-cfront-lower--rvalue e))   ; aggregate decays
    (_ (nelisp-cfront-lower--err :unsupported-expr e))))

(defun nelisp-cfront-lower--binop (op l r)
  ;; --- floating-point arithmetic / comparison (soft-float helpers) ---
  ;; When either operand is float-typed and OP is arithmetic or a
  ;; comparison, both operands are coerced to double-bits and the op
  ;; lowers to a per-object helper call (arithmetic -> bits, comparison
  ;; -> i64 0/1).  `%' on doubles is a C type error and never appears.
  (if (and (or (nelisp-cfront-lower--expr-float-p l)
               (nelisp-cfront-lower--expr-float-p r))
           (member op '("+" "-" "*" "/" "<" "<=" ">" ">=" "==" "!=")))
      (let ((a (nelisp-cfront-lower--as-double-bits l))
            (b (nelisp-cfront-lower--as-double-bits r)))
        (nelisp-cfront-lower--mark-float)
        (pcase op
          ("+"  (list 'nelisp_cfront__dadd a b))
          ("-"  (list 'nelisp_cfront__dsub a b))
          ("*"  (list 'nelisp_cfront__dmul a b))
          ("/"  (list 'nelisp_cfront__ddiv a b))
          ("<"  (list 'nelisp_cfront__dlt a b))
          (">"  (list 'nelisp_cfront__dgt a b))
          ("<=" (list 'nelisp_cfront__dle a b))
          (">=" (list 'nelisp_cfront__dge a b))
          ("==" (list 'nelisp_cfront__deq a b))
          ("!=" `(if ,(list 'nelisp_cfront__deq a b) 0 1))))
    ;; --- integer / pointer path ---
    (let ((gl (nelisp-cfront-lower--expr l))
          (gr (nelisp-cfront-lower--expr r))
          (g (cdr (assoc op nelisp-cfront-lower--binop-map))))
      (cond
       ;; pointer +/- integer: scale the integer by the pointee size
       ((and (member op '("+" "-"))
             (> (or (plist-get (nelisp-cfront-lower--type-of l) :ptr) 0) 0)
             (= 0 (or (plist-get (nelisp-cfront-lower--type-of r) :ptr) 0)))
        (let ((es (nelisp-cfront-lower--elem-size l)))
          (list (cdr (assoc op nelisp-cfront-lower--binop-map))
                gl (if (= es 1) gr `(* ,gr ,es)))))
       (g (list g gl gr))
       ((string= op "!=") `(if (= ,gl ,gr) 0 1))
       ((string= op "&&") `(if ,(nelisp-cfront-lower--truth gl)
                               ,(nelisp-cfront-lower--truth gr) 0))
       ((string= op "||") `(if ,(nelisp-cfront-lower--truth gl) 1
                             ,(nelisp-cfront-lower--truth gr)))
       (t (nelisp-cfront-lower--err :unsupported-binop op))))))

(defun nelisp-cfront-lower--truth (g)
  "Normalise grammar value G to 0/1 (C truthiness, non-zero -> 1)."
  `(if (= ,g 0) 0 1))

(defun nelisp-cfront-lower--cond (e)
  "Lower a C condition.  Grammar if/while treat non-zero as true, which
matches C, so the raw value is used directly."
  (nelisp-cfront-lower--expr e))

(defun nelisp-cfront-lower--unop (op e)
  (let ((g (nelisp-cfront-lower--expr e)))
    (pcase op
      ;; float negation = flip the IEEE sign bit (no helper needed)
      ("-" (if (nelisp-cfront-lower--expr-float-p e)
               `(logxor ,g ,nelisp-cfront-float--sign-bit)
             `(- 0 ,g)))
      ("+" g)
      ("!" `(if (= ,g 0) 1 0))
      ("~" `(logxor ,g -1))
      ("*" `(ptr-read-u64 ,g 0))              ; MVP: 8-byte deref
      ("&" (if (and (consp e) (eq (car e) 'var)
                    (nelisp-cfront-lower--var-is-function-p (nth 1 e)))
               `(addr-of ,(nelisp-cfront-lower--sym (nth 1 e)))  ; &function
             ;; &lvalue: address of a memory-backed var / index / member /
             ;; arrow / deref (computed without loading the value).
             (nelisp-cfront-lower--addr e)))
      (_ (nelisp-cfront-lower--err :unsupported-unop op)))))

(defun nelisp-cfront-lower--float-arith-helper (bop)
  "Soft-float helper symbol for compound-assignment arithmetic BOP."
  (pcase bop
    ("+" 'nelisp_cfront__dadd) ("-" 'nelisp_cfront__dsub)
    ("*" 'nelisp_cfront__dmul) ("/" 'nelisp_cfront__ddiv)
    (_ nil)))

(defun nelisp-cfront-lower--assign (op lhs rhs)
  (if (and (string= op "=")
           (nelisp-cfront-lower--aggregate-type-p (nelisp-cfront-lower--type-of lhs)))
      ;; struct/array by-value copy: copy sizeof(lhs) bytes from rhs's
      ;; address to lhs's address (rhs must be an lvalue — a struct-
      ;; returning call is struct-return ABI, deferred).
      (nelisp-cfront-lower--copy-bytes
       (nelisp-cfront-lower--addr lhs)
       (nelisp-cfront-lower--addr rhs)
       (nelisp-cfront-type-size (nelisp-cfront-lower--type-of lhs)
                                nelisp-cfront-lower--structs))
    (nelisp-cfront-lower--assign-scalar op lhs rhs)))

(defun nelisp-cfront-lower--assign-scalar (op lhs rhs)
  (let* ((lhs-float (nelisp-cfront-lower--expr-float-p lhs))
         ;; RHS value coerced to the LHS class (int<->double bits) for `='
         (grhs (nelisp-cfront-lower--coerce
                (nelisp-cfront-lower--expr rhs)
                (nelisp-cfront-lower--expr-float-p rhs)
                lhs-float)))
    (cond
     ;; plain value-slot scalar (NOT an address-taken / memory-backed var)
     ((and (eq (car lhs) 'var)
           (not (nelisp-cfront-lower--mem-var-scalar-p (nth 1 lhs))))
       (let ((name (nelisp-cfront-lower--lvar (nth 1 lhs))))
         (if (string= op "=")
             `(setq ,name ,grhs)
           ;; compound: a OP= b  ->  a = a OP b
           (let ((bop (substring op 0 (1- (length op)))))
             (if lhs-float
                 (let ((h (nelisp-cfront-lower--float-arith-helper bop)))
                   (unless h (nelisp-cfront-lower--err :unsupported-compound-assign op))
                   (nelisp-cfront-lower--mark-float)
                   `(setq ,name (,h ,name ,(nelisp-cfront-lower--as-double-bits rhs))))
               (let ((g (cdr (assoc bop nelisp-cfront-lower--binop-map))))
                 (unless g (nelisp-cfront-lower--err :unsupported-compound-assign op))
                 `(setq ,name (,g ,name ,(nelisp-cfront-lower--expr rhs)))))))))
      ;; memory lvalue: *p / a[i] / p->f / s.f / address-taken scalar var
      ((memq (car lhs) '(var unop index arrow member))
       (let ((fld (nelisp-cfront-lower--field-of lhs)))
         (if (and fld (plist-get fld :bits))
             (nelisp-cfront-lower--bitfield-assign lhs op grhs fld)
           (let ((addr (nelisp-cfront-lower--addr lhs))
                 (width (nelisp-cfront-type-size (nelisp-cfront-lower--type-of lhs)
                                                 nelisp-cfront-lower--structs)))
             (if (string= op "=")
                 (nelisp-cfront-lower--store-w addr width grhs)
               (let ((bop (substring op 0 (1- (length op)))))
                 (if lhs-float
                     (let ((h (nelisp-cfront-lower--float-arith-helper bop)))
                       (unless h (nelisp-cfront-lower--err :unsupported-compound-assign op))
                       (nelisp-cfront-lower--mark-float)
                       (nelisp-cfront-lower--store-w
                        addr width (list h (nelisp-cfront-lower--load-w addr width)
                                         (nelisp-cfront-lower--as-double-bits rhs))))
                   (let ((g (cdr (assoc bop nelisp-cfront-lower--binop-map))))
                     (unless g (nelisp-cfront-lower--err :unsupported-compound-assign op))
                     (nelisp-cfront-lower--store-w
                      addr width (list g (nelisp-cfront-lower--load-w addr width)
                                       (nelisp-cfront-lower--expr rhs)))))))))))
      (t (nelisp-cfront-lower--err :unsupported-assign-target lhs)))))

(defun nelisp-cfront-lower--call-args (args ptypes)
  "Lower call ARGS, coercing each to its positional param type in PTYPES
\(int<->double bits).  Args beyond PTYPES (varargs) pass through as-is."
  (cl-loop for a in args
           for i from 0
           collect (let ((g (nelisp-cfront-lower--expr a))
                         (pt (nth i ptypes)))
                     (if pt
                         (nelisp-cfront-lower--coerce
                          g (nelisp-cfront-lower--expr-float-p a)
                          (nelisp-cfront-float-type-p pt))
                       g))))

(defun nelisp-cfront-lower--call (fn args)
  (if (and (eq (car fn) 'var)
           (nelisp-cfront-lower--var-is-function-p (nth 1 fn)))
      ;; direct call to a named function: coerce args to the param types
      (let* ((name (nth 1 fn))
             (ptypes (cdr (assoc name nelisp-cfront-lower--func-params))))
        (cons (nelisp-cfront-lower--sym name)
              (nelisp-cfront-lower--call-args args ptypes)))
    ;; indirect call through a function-pointer value: fp(...) / (*fp)(...)
    ;; (param types are not tracked for fn-ptrs; pass args uncoerced)
    (let ((target (if (and (eq (car fn) 'unop) (string= (nth 1 fn) "*"))
                      (nth 2 fn)
                    fn))
          (gargs (mapcar #'nelisp-cfront-lower--expr args)))
      (cons 'call-ptr (cons (nelisp-cfront-lower--expr target) gargs)))))

(defun nelisp-cfront-lower--incdec (e)
  "Lower ++/-- (pre or post) on ANY lvalue by desugaring to
`TARGET = TARGET <op> 1' and reusing `--assign' / `--binop'.  This
covers var / p->f / a[i] / *p / bitfield / address-taken scalar, and —
because the `+'/`-' goes through `--binop' — gets pointer-arithmetic
scaling (=p++= advances by the pointee size) and float (=d++= adds 1.0)
right.  Returns the new value (the assigned value); a post-increment in
value position thus yields the new value in this MVP, exact in statement
position (the dominant case).  TARGET is re-read for the binop, so an
lvalue with a side-effecting subexpression (=a[i++]++=) is not modelled."
  (let ((target (nth 2 e))
        (op (if (string= (nth 1 e) "++") "+" "-")))
    (nelisp-cfront-lower--assign "=" target (list 'binop op target '(int 1)))))

(defun nelisp-cfront-lower--sizeof (ty)
  (let ((ptr (plist-get ty :ptr)))
    (if (and ptr (> ptr 0)) 8
      (pcase (plist-get ty :base)
        ('char 1) ('short 2) ('int 4) ('long 8) ('void 1)
        (_ 8)))))

;;; --- statements ------------------------------------------------------

(defun nelisp-cfront-lower--gensym-brk ()
  (prog1 (intern (format "nlcf_brk%d" nelisp-cfront-lower--brk-counter))
    (setq nelisp-cfront-lower--brk-counter (1+ nelisp-cfront-lower--brk-counter))))

(defun nelisp-cfront-lower--gensym (prefix)
  (prog1 (intern (format "nlcf_%s%d" prefix nelisp-cfront-lower--brk-counter))
    (setq nelisp-cfront-lower--brk-counter (1+ nelisp-cfront-lower--brk-counter))))

(defun nelisp-cfront-lower--switch-groups (body)
  "Split a switch BODY block into ((LABEL-NODE . STMTS) ...) groups.
LABEL-NODE is a `(case V)' or `(default)'.  Statements before the first
label (unreachable) are dropped."
  (let ((groups nil) (curlabel nil) (curstmts nil))
    (dolist (s (if (eq (car body) 'block) (cdr body) (list body)))
      (if (memq (car s) '(case default))
          (progn
            (when curlabel (push (cons curlabel (nreverse curstmts)) groups))
            (setq curlabel s curstmts nil))
        (when curlabel (push s curstmts))))
    (when curlabel (push (cons curlabel (nreverse curstmts)) groups))
    (nreverse groups)))

(defun nelisp-cfront-lower--switch-anymatch (sw vals)
  "Grammar test: non-zero iff SW equals any of grammar VALS."
  (if (null vals) 0
    `(if (= ,sw ,(car vals)) 1 ,(nelisp-cfront-lower--switch-anymatch sw (cdr vals)))))

(defun nelisp-cfront-lower--body-has-break (s)
  "Non-nil when S contains a `break'/`continue' for THIS loop level
\(does not descend into nested while/for, which capture their own)."
  (and (consp s)
       (pcase (car s)
         ((or 'break 'continue) t)
         ('block (cl-some #'nelisp-cfront-lower--body-has-break (cdr s)))
         ('if (or (nelisp-cfront-lower--body-has-break (nth 2 s))
                  (nelisp-cfront-lower--body-has-break (nth 3 s))))
         ((or 'while 'for) nil)         ; nested loop: not ours
         (_ nil))))

(defun nelisp-cfront-lower--guard-clear (flags inner)
  "Return INNER guarded so it only runs when all FLAGS are 0."
  (if (null flags) inner
    `(if (= ,(car flags) 0)
         ,(nelisp-cfront-lower--guard-clear (cdr flags) inner)
       0)))

(defun nelisp-cfront-lower--active-exit-flags ()
  "Exit flags in scope: break + return + goto flags (whichever are active)."
  (delq nil (list (car nelisp-cfront-lower--brk-stack)
                  (and nelisp-cfront-lower--ret-mode
                       (car nelisp-cfront-lower--ret-mode))
                  nelisp-cfront-lower--goto-flag)))

(defun nelisp-cfront-lower--has-goto-p (node)
  "Non-nil when NODE contains a `goto' or a `label'."
  (and (consp node)
       (pcase (car node)
         ((or 'goto 'label) t)
         ('block (cl-some #'nelisp-cfront-lower--has-goto-p (cdr node)))
         ('if (or (nelisp-cfront-lower--has-goto-p (nth 2 node))
                  (nelisp-cfront-lower--has-goto-p (nth 3 node))))
         ('while (nelisp-cfront-lower--has-goto-p (nth 2 node)))
         ('do-while (nelisp-cfront-lower--has-goto-p (nth 1 node)))
         ('switch (nelisp-cfront-lower--has-goto-p (nth 2 node)))
         ('for (or (nelisp-cfront-lower--has-goto-p (nth 1 node))
                   (nelisp-cfront-lower--has-goto-p (nth 4 node))))
         (_ nil))))

(defun nelisp-cfront-lower--has-return-p (node)
  "Non-nil when NODE contains a `return' anywhere."
  (and (consp node)
       (pcase (car node)
         ('return t)
         ((or 'block 'decls) (cl-some #'nelisp-cfront-lower--has-return-p (cdr node)))
         ('if (or (nelisp-cfront-lower--has-return-p (nth 2 node))
                  (nelisp-cfront-lower--has-return-p (nth 3 node))))
         ('while (nelisp-cfront-lower--has-return-p (nth 2 node)))
         ('do-while (nelisp-cfront-lower--has-return-p (nth 1 node)))
         ('switch (nelisp-cfront-lower--has-return-p (nth 2 node)))
         ('for (or (nelisp-cfront-lower--has-return-p (nth 1 node))
                   (nelisp-cfront-lower--has-return-p (nth 4 node))))
         (_ nil))))

;; Single-exit detection precisely mirrors the tail-lowering rules
;; (`--stmts-tail' / `--tail' / `--block-tail').  A function needs the
;; return flag iff some `return' would be lowered through `--effect'
;; rather than guard-lifted in tail position — this subsumes returns in
;; loops, switches, and partially-returning if/else-if chains.

(defun nelisp-cfront-lower--tail-effect-return-p (s)
  "Non-nil when lowering statement S in TAIL position routes some return
through `--effect' (i.e. needs the return flag)."
  (and (consp s)
       (pcase (car s)
         ('return nil)                  ; a tail return is emitted directly
         ('block (nelisp-cfront-lower--stmts-tail-effect-return-p (cdr s)))
         ('if (or (nelisp-cfront-lower--tail-effect-return-p (nth 2 s))
                  (and (nth 3 s) (nelisp-cfront-lower--tail-effect-return-p (nth 3 s)))))
         ;; any other statement in tail position runs via `--effect'
         (_ (nelisp-cfront-lower--has-return-p s)))))

(defun nelisp-cfront-lower--stmts-tail-effect-return-p (stmts)
  "Non-nil when lowering STMTS as a tail sequence (per `--stmts-tail')
routes some return through `--effect'."
  (cond
   ((null stmts) nil)
   ((null (cdr stmts)) (nelisp-cfront-lower--tail-effect-return-p (car stmts)))
   (t
    (let ((s (car stmts)) (rest (cdr stmts)))
      (cond
       ;; if/else where both branches return: both lowered in tail
       ((and (eq (car s) 'if) (nth 3 s)
             (nelisp-cfront-lower--always-returns-p (nth 2 s))
             (nelisp-cfront-lower--always-returns-p (nth 3 s)))
        (or (nelisp-cfront-lower--tail-effect-return-p (nth 2 s))
            (nelisp-cfront-lower--tail-effect-return-p (nth 3 s))))
       ;; guard `if (c) <returns>;': then in tail, rest in tail
       ((and (eq (car s) 'if) (null (nth 3 s))
             (nelisp-cfront-lower--always-returns-p (nth 2 s)))
        (or (nelisp-cfront-lower--tail-effect-return-p (nth 2 s))
            (nelisp-cfront-lower--stmts-tail-effect-return-p rest)))
       ;; otherwise S runs via `--effect' (any return in it is an effect
       ;; return), and the rest continues in tail position
       (t
        (or (nelisp-cfront-lower--has-return-p s)
            (nelisp-cfront-lower--stmts-tail-effect-return-p rest))))))))

(defun nelisp-cfront-lower--exit-stmt-p (s)
  "Non-nil when S always exits the current iteration (break/continue/return)."
  (and (consp s)
       (pcase (car s)
         ((or 'break 'continue 'return 'goto) t)
         ('block (let ((ss (cdr s))) (and ss (nelisp-cfront-lower--exit-stmt-p (car (last ss))))))
         ('if (and (nth 3 s)
                   (nelisp-cfront-lower--exit-stmt-p (nth 2 s))
                   (nelisp-cfront-lower--exit-stmt-p (nth 3 s))))
         (_ nil))))

(defun nelisp-cfront-lower--stmts-effect (stmts)
  "Lower STMTS in effect position, guard-lifting break/continue/return
\(if (c) <exit>; REST  ==>  if (c) <exit> else REST)."
  (cond
   ((null stmts) 0)
   ((null (cdr stmts)) (nelisp-cfront-lower--effect (car stmts)))
   (t
    (let ((s (car stmts)) (rest (cdr stmts)))
      (if (and (eq (car s) 'if) (null (nth 3 s))
               (nelisp-cfront-lower--exit-stmt-p (nth 2 s)))
          `(if ,(nelisp-cfront-lower--cond (nth 1 s))
               ,(nelisp-cfront-lower--effect (nth 2 s))
             ,(nelisp-cfront-lower--stmts-effect rest))
        (nelisp-cfront-lower--seq
         (list (nelisp-cfront-lower--effect s)
               ;; after a stmt that may break/continue/return, run the rest
               ;; only while no exit flag is set (no-op when none are active)
               (nelisp-cfront-lower--guard-clear
                (nelisp-cfront-lower--active-exit-flags)
                (nelisp-cfront-lower--stmts-effect rest)))))))))

(defun nelisp-cfront-lower--lower-loop (cnd body step)
  "Lower a loop with condition CND, BODY, optional STEP expr (for-loops).
The condition and the for-step are guarded by every active exit flag (the
loop's own break flag if the body breaks, plus the function return flag in
single-exit mode), so break/continue/return all exit correctly."
  (let ((flag (and (nelisp-cfront-lower--body-has-break body)
                   (nelisp-cfront-lower--gensym-brk))))
    (when flag (push flag nelisp-cfront-lower--synth))
    (let* ((nelisp-cfront-lower--brk-stack
            (if flag (cons flag nelisp-cfront-lower--brk-stack)
              nelisp-cfront-lower--brk-stack))
           (b (nelisp-cfront-lower--effect body))
           (flags (delq nil (list flag
                                  (and nelisp-cfront-lower--ret-mode
                                       (car nelisp-cfront-lower--ret-mode))
                                  nelisp-cfront-lower--goto-flag)))
           (acond (nelisp-cfront-lower--guard-clear
                   flags (nelisp-cfront-lower--cond cnd)))
           (st (and step (nelisp-cfront-lower--guard-clear
                          flags (nelisp-cfront-lower--expr step)))))
      `(while ,acond
         ,(nelisp-cfront-lower--seq (if st (list b st) (list b)))))))

(defun nelisp-cfront-lower--lower-body-with-goto (body)
  "Lower a function BODY that uses a forward `goto' to a top-level label.
Splits the top-level block at the label: code before is guarded by the
goto flag (so `goto' skips it); the label clears the flag; code after runs.
Only a single top-level forward label (the cleanup pattern) is supported."
  (let* ((stmts (cdr body))
         (pos (cl-position-if (lambda (s) (eq (car s) 'label)) stmts)))
    (if (null pos)
        (nelisp-cfront-lower--effect body)
      (let ((before (cl-subseq stmts 0 pos))
            (after (cl-subseq stmts (1+ pos))))   ; drop the label marker itself
        (nelisp-cfront-lower--seq
         (list (nelisp-cfront-lower--stmts-effect before)
               `(setq ,nelisp-cfront-lower--goto-flag 0)   ; the label: disarm
               (nelisp-cfront-lower--stmts-effect after)))))))

(defun nelisp-cfront-lower--effect (s)
  "Lower statement S in effect (value-discarded) position."
  (pcase (car s)
    ('decl (let ((init (nth 3 s)) (cname (nth 2 s)) (ty (nth 1 s)))
             (cond
              ;; uninitialised, or aggregate init list (local array/struct
              ;; `{...}' init deferred)
              ((or (null init) (and (consp init) (eq (car init) 'init-list))) 0)
              ;; aggregate copy-init: `struct T x = lvalue;' / `T a = b;'
              ;; copies sizeof(ty) bytes from the initializer's address.
              ((nelisp-cfront-lower--aggregate-type-p ty)
               (nelisp-cfront-lower--copy-bytes
                (nelisp-cfront-lower--lvar cname)
                (nelisp-cfront-lower--addr init)
                (nelisp-cfront-type-size ty nelisp-cfront-lower--structs)))
              ;; address-taken scalar: store the initializer through the
              ;; frame-alloc block pointer (the slot holds the address).
              ((nelisp-cfront-lower--mem-var-scalar-p cname)
               (nelisp-cfront-lower--store-w
                (nelisp-cfront-lower--lvar cname)
                (nelisp-cfront-type-size ty nelisp-cfront-lower--structs)
                (nelisp-cfront-lower--coerce
                 (nelisp-cfront-lower--expr init)
                 (nelisp-cfront-lower--expr-float-p init)
                 (nelisp-cfront-float-type-p ty))))
              ;; plain value-slot scalar
              (t `(setq ,(nelisp-cfront-lower--lvar cname)
                        ,(nelisp-cfront-lower--coerce  ; int<->double bits to match decl
                          (nelisp-cfront-lower--expr init)
                          (nelisp-cfront-lower--expr-float-p init)
                          (nelisp-cfront-float-type-p ty)))))))
    ('decls (nelisp-cfront-lower--seq
             (mapcar #'nelisp-cfront-lower--effect (cdr s))))
    ('expr-stmt (nelisp-cfront-lower--expr (nth 1 s)))
    ('block (nelisp-cfront-lower--stmts-effect (cdr s)))
    ('if `(if ,(nelisp-cfront-lower--cond (nth 1 s))
              ,(nelisp-cfront-lower--effect (nth 2 s))
            ,(if (nth 3 s) (nelisp-cfront-lower--effect (nth 3 s)) 0)))
    ('while (nelisp-cfront-lower--lower-loop (nth 1 s) (nth 2 s) nil))
    ('for (nelisp-cfront-lower--for s))
    ('do-while                            ; do BODY while(C)  ==  while(1){BODY; if(!C)break;}
     (nelisp-cfront-lower--lower-loop
      '(int 1)
      `(block ,(nth 1 s) (if (unop "!" ,(nth 2 s)) (break) nil))
      nil))
    ('switch (nelisp-cfront-lower--lower-switch s))
    ('break (if nelisp-cfront-lower--brk-stack
                `(setq ,(car nelisp-cfront-lower--brk-stack) 1)
              (nelisp-cfront-lower--err :break-outside-loop s)))
    ('continue (if nelisp-cfront-lower--brk-stack 0   ; guard-lift skips the rest
                 (nelisp-cfront-lower--err :continue-outside-loop s)))
    ('return (if nelisp-cfront-lower--ret-mode
                 (let ((rs (car nelisp-cfront-lower--ret-mode))
                       (rv (cdr nelisp-cfront-lower--ret-mode)))
                   `(seq (setq ,rv ,(nelisp-cfront-lower--return-value (nth 1 s)))
                         (setq ,rs 1)))
               (nelisp-cfront-lower--err :early-return-in-loop-unsupported s)))
    ('goto (if nelisp-cfront-lower--goto-flag
               `(setq ,nelisp-cfront-lower--goto-flag 1) ; forward jump: arm the flag
             (nelisp-cfront-lower--err :goto-unsupported s)))
    ('label (if nelisp-cfront-lower--goto-flag
                `(setq ,nelisp-cfront-lower--goto-flag 0) ; arrived: disarm the flag
              0))
    (_ (nelisp-cfront-lower--err :unsupported-stmt s))))

(defun nelisp-cfront-lower--for (s)
  (let ((init (nth 1 s)) (cnd (nth 2 s)) (step (nth 3 s)) (body (nth 4 s)))
    (nelisp-cfront-lower--seq
     (append (when init (list (nelisp-cfront-lower--effect init)))
             (list (nelisp-cfront-lower--lower-loop (or cnd '(int 1)) body step))))))

(defun nelisp-cfront-lower--lower-switch (s)
  "Lower a `switch' via a matched-flag linear scan (fall-through + break).
Each case runs when (matched OR value==label); break (the innermost flag)
guards later cases; default runs when matched OR no case value matched."
  (let* ((e (nth 1 s)) (body (nth 2 s))
         (sw (nelisp-cfront-lower--gensym "sw"))
         (m  (nelisp-cfront-lower--gensym "swm"))
         (bf (nelisp-cfront-lower--gensym "swb"))
         (groups (nelisp-cfront-lower--switch-groups body))
         (case-vals (delq nil (mapcar
                               (lambda (g) (and (eq (car (car g)) 'case)
                                                (nelisp-cfront-lower--expr (nth 1 (car g)))))
                               groups))))
    (push sw nelisp-cfront-lower--synth)
    (push m nelisp-cfront-lower--synth)
    (push bf nelisp-cfront-lower--synth)
    (let* ((anymatch (nelisp-cfront-lower--switch-anymatch sw case-vals))
           (nelisp-cfront-lower--brk-stack (cons bf nelisp-cfront-lower--brk-stack))
           (group-forms
            (mapcar
             (lambda (g)
               (let* ((label (car g)) (stmts (cdr g))
                      (match (if (eq (car label) 'case)
                                 `(if (= ,m 0)
                                      (if (= ,sw ,(nelisp-cfront-lower--expr (nth 1 label))) 1 0)
                                    1)
                               `(if (= ,m 0) (if ,anymatch 0 1) 1))))
                 ;; Skip this case group once ANY active exit flag is set —
                 ;; the switch break flag (bf), plus the function return /
                 ;; goto flags in single-exit mode — so `return' inside a
                 ;; case correctly bypasses the remaining cases.
                 (nelisp-cfront-lower--guard-clear
                  (nelisp-cfront-lower--active-exit-flags)
                  `(if ,match
                       ,(nelisp-cfront-lower--seq
                         (list `(setq ,m 1)
                               (nelisp-cfront-lower--stmts-effect stmts)))
                     0))))
             groups)))
      (nelisp-cfront-lower--seq
       (append (list `(setq ,sw ,(nelisp-cfront-lower--expr e)) `(setq ,m 0))
               group-forms)))))

(defun nelisp-cfront-lower--tail (s void-p)
  "Lower statement S in tail (return-value) position."
  (pcase (car s)
    ('return (nelisp-cfront-lower--return-value (nth 1 s)))
    ('block (nelisp-cfront-lower--block-tail s void-p))
    ('if `(if ,(nelisp-cfront-lower--cond (nth 1 s))
              ,(nelisp-cfront-lower--tail (nth 2 s) void-p)
            ,(if (nth 3 s) (nelisp-cfront-lower--tail (nth 3 s) void-p) 0)))
    ;; Any other statement in tail position: run it for effect, return 0
    ;; (valid for void functions / fall-off-end).
    (_ (nelisp-cfront-lower--seq (list (nelisp-cfront-lower--effect s) 0)))))

(defun nelisp-cfront-lower--always-returns-p (s)
  "Non-nil when statement S returns on every path (no fall-through)."
  (and (consp s)
       (pcase (car s)
         ('return t)
         ('block (let ((ss (cdr s)))
                   (and ss (nelisp-cfront-lower--always-returns-p (car (last ss))))))
         ('if (and (nth 3 s)
                   (nelisp-cfront-lower--always-returns-p (nth 2 s))
                   (nelisp-cfront-lower--always-returns-p (nth 3 s))))
         (_ nil))))

(defun nelisp-cfront-lower--block-tail (block void-p)
  (nelisp-cfront-lower--stmts-tail (cdr block) void-p))

(defun nelisp-cfront-lower--stmts-tail (stmts void-p)
  "Lower STMTS as a tail (return-valued) sequence.
Lifts guard clauses — `if (c) <returns>; REST...' — into structured
`(if c <then> <REST>)' so early returns need no goto (Doc 02 / M2.4)."
  (cond
   ((null stmts) 0)
   ((null (cdr stmts)) (nelisp-cfront-lower--tail (car stmts) void-p))
   (t
    (let ((s (car stmts)) (rest (cdr stmts)))
      (cond
       ;; if/else where BOTH branches return -> rest is unreachable
       ((and (eq (car s) 'if)
             (nth 3 s)
             (nelisp-cfront-lower--always-returns-p (nth 2 s))
             (nelisp-cfront-lower--always-returns-p (nth 3 s)))
        `(if ,(nelisp-cfront-lower--cond (nth 1 s))
             ,(nelisp-cfront-lower--tail (nth 2 s) void-p)
           ,(nelisp-cfront-lower--tail (nth 3 s) void-p)))
       ;; guard: if (c) <returns>;  REST  ==  if (c) <then> else <REST>
       ((and (eq (car s) 'if)
             (null (nth 3 s))
             (nelisp-cfront-lower--always-returns-p (nth 2 s)))
        `(if ,(nelisp-cfront-lower--cond (nth 1 s))
             ,(nelisp-cfront-lower--tail (nth 2 s) void-p)
           ,(nelisp-cfront-lower--stmts-tail rest void-p)))
       ;; non-terminating statement: run for effect, continue
       (t
        (nelisp-cfront-lower--seq
         (list (nelisp-cfront-lower--effect s)
               (nelisp-cfront-lower--stmts-tail rest void-p)))))))))

(defun nelisp-cfront-lower--seq (forms)
  "Wrap FORMS in `(seq ...)'; collapse a single form."
  (cond ((null forms) 0)
        ((null (cdr forms)) (car forms))
        (t (cons 'seq forms))))

;;; --- functions / program --------------------------------------------

(defun nelisp-cfront-lower--func (node)
  (let* ((rty (nth 1 node))
         (name (nelisp-cfront-lower--sym (nth 2 node)))
         (params (nth 3 node))
         (body (nth 4 node))
         (void-p (and (eq (plist-get rty :base) 'void) (= 0 (plist-get rty :ptr))))
         (nelisp-cfront-lower--ret-float (nelisp-cfront-float-type-p rty))
         (pnames (delq nil (mapcar (lambda (p) (and (nth 2 p)
                                                    (nelisp-cfront-lower--lvar (nth 2 p))))
                                   params)))
         (locals (nreverse (delete-dups (nelisp-cfront-lower--collect-decls body nil))))
         (nelisp-cfront-lower--local-names
          (append (delq nil (mapcar (lambda (p) (nth 2 p)) params)) locals))
         (nelisp-cfront-lower--tenv
          (append (delq nil (mapcar (lambda (p) (and (nth 2 p) (cons (nth 2 p) (nth 1 p))))
                                    params))
                  (nelisp-cfront-lower--collect-decl-types body nil)
                  ;; globals are visible for type inference but shadowed by any
                  ;; same-named param/local appended before them (Doc 06 Step B).
                  (mapcar (lambda (g) (cons (car g) (plist-get (cdr g) :type)))
                          nelisp-cfront-lower--globals)))
         ;; Locals needing a frame-alloc block: arrays / struct-by-value
         ;; (always) and address-taken scalars.  (Address-taken *params*
         ;; would need an entry spill; for now `&param' signals via --addr.)
         (addr-taken (nelisp-cfront-lower--collect-addr-taken body nil))
         (nelisp-cfront-lower--mem-vars
          (delq nil (mapcar
                     (lambda (v)
                       (let ((ty (cdr (assoc v nelisp-cfront-lower--tenv))))
                         (when (and ty (or (nelisp-cfront-lower--aggregate-type-p ty)
                                           (member v addr-taken)))
                           (cons v ty))))
                     locals)))
         (nelisp-cfront-lower--synth nil)
         (nelisp-cfront-lower--brk-stack nil)
         (nelisp-cfront-lower--brk-counter 0)
         (has-goto (nelisp-cfront-lower--has-goto-p body))
         (needs-exit (or (nelisp-cfront-lower--stmts-tail-effect-return-p (cdr body))
                         has-goto))
         (nelisp-cfront-lower--ret-mode
          (and needs-exit (cons 'nlcf_retset 'nlcf_retval)))
         (nelisp-cfront-lower--goto-flag (and has-goto 'nlcf_goto))
         (body-g (if needs-exit
                     ;; single-exit mode: run the whole body for effect
                     ;; (returns/gotos set flags), then yield the return value.
                     (progn
                       (push 'nlcf_retset nelisp-cfront-lower--synth)
                       (push 'nlcf_retval nelisp-cfront-lower--synth)
                       (when has-goto (push 'nlcf_goto nelisp-cfront-lower--synth))
                       (nelisp-cfront-lower--seq
                        (list (if has-goto
                                  (nelisp-cfront-lower--lower-body-with-goto body)
                                (nelisp-cfront-lower--effect body))
                              'nlcf_retval)))
                   (nelisp-cfront-lower--block-tail body void-p)))
         (local-binds (mapcar
                       (lambda (v)
                         (let ((mv (assoc v nelisp-cfront-lower--mem-vars)))
                           (list (nelisp-cfront-lower--lvar v)
                                 (if mv
                                     ;; memory-backed: slot holds a frame block address
                                     (list 'frame-alloc
                                           (nelisp-cfront-type-size
                                            (cdr mv) nelisp-cfront-lower--structs))
                                   (list nelisp-cfront-lower--zero-fn)))))
                       locals))
         (synth-binds (mapcar (lambda (v) (list v (list nelisp-cfront-lower--zero-fn)))
                              (reverse nelisp-cfront-lower--synth)))
         (binds (append local-binds synth-binds))
         ;; SysV passes a narrow int arg in the low bits of a 64-bit
         ;; register with the high bits unspecified (gcc zero-extends), so
         ;; re-normalize each narrow-int param to its C width at entry —
         ;; otherwise a negative `int' arg reads as a large positive i64.
         (param-norms
          (delq nil (mapcar
                     (lambda (p)
                       (and (nth 2 p)
                            (nelisp-cfront-lower--narrow-int-width (nth 1 p))
                            (let ((g (nelisp-cfront-lower--lvar (nth 2 p))))
                              `(setq ,g ,(nelisp-cfront-lower--normalize-narrow
                                          g (nth 1 p))))))
                     params)))
         (full-body (if param-norms
                        (nelisp-cfront-lower--seq (append param-norms (list body-g)))
                      body-g))
         (wrapped (if binds `(let ,binds ,full-body) full-body)))
    `(defun ,name ,pnames ,wrapped)))

(defun nelisp-cfront-lower-program (ast)
  "Lower AST `(program TOP...)' to a grammar `(seq (defun ...) ...)'.
Includes the `nelisp_cfront__zero' helper.  Globals/prototypes are
skipped in the MVP (functions only)."
  (unless (eq (car ast) 'program)
    (nelisp-cfront-lower--err :not-a-program ast))
  (let* ((nelisp-cfront-lower--structs (nelisp-cfront-type-build-structs ast))
         (nelisp-cfront-lower--funcs (nelisp-cfront-lower--collect-func-types ast))
         (nelisp-cfront-lower--func-params (nelisp-cfront-lower--collect-func-params ast))
         ;; Read-only integer globals (Doc 06 Step B): collected once (needs
         ;; `--structs' for element sizing), exposed to every function's type
         ;; env, and emitted as `data-blob' rodata symbols below.
         (nelisp-cfront-lower--globals (nelisp-cfront-lower--collect-globals ast))
         ;; String literal pool (Doc 06 Step D): filled while lowering the
         ;; function bodies below, emitted as rodata `data-blob's afterward.
         (nelisp-cfront-lower--string-pool nil)
         (nelisp-cfront-lower--string-counter 0)
         (nelisp-cfront-lower--uses-float nil)
         (funcs nil))
    (dolist (top (cdr ast))
      (pcase (car top)
        ('func (push (nelisp-cfront-lower--func top) funcs))
        ('proto nil)                          ; ignore prototypes
        ('global nil)                         ; data emitted from --globals below
        ('struct-def nil)                     ; layout already in the table
        ('typedef nil)                        ; aliases resolved during parse
        (_ (nelisp-cfront-lower--err :unsupported-toplevel top))))
    (cons 'seq
          (append
           ;; rodata data symbols first (globals + interned string literals),
           ;; then the zero helper, float helpers, and the lowered functions.
           (mapcar (lambda (g)
                     `(data-blob ,(intern (car g))
                                 ,(plist-get (cdr g) :bytes) rodata))
                   nelisp-cfront-lower--globals)
           (mapcar (lambda (e)
                     `(data-blob ,(cdr e)
                                 ,(nelisp-cfront-lower--string-bytes (car e)) rodata))
                   (reverse nelisp-cfront-lower--string-pool))
           (cons `(defun ,nelisp-cfront-lower--zero-fn () 0)
                 (append (when nelisp-cfront-lower--uses-float
                           (nelisp-cfront-float-helper-defuns))
                         (nreverse funcs)))))))

(provide 'nelisp-cfront-lower)

;;; nelisp-cfront-lower.el ends here
