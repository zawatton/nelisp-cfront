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

(define-error 'nelisp-cfront-lower-error "nelisp-cfront lowering error")

(defvar nelisp-cfront-lower--structs nil
  "Struct table (name->layout) for the program being lowered.")
(defvar nelisp-cfront-lower--funcs nil
  "Function return-type table (name->type) for the program.")
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

(defun nelisp-cfront-lower--elem-size (ptr-expr)
  "Element size of the pointee/array element of PTR-EXPR's type."
  (nelisp-cfront-type-size
   (nelisp-cfront-type-pointee (nelisp-cfront-lower--type-of ptr-expr))
   nelisp-cfront-lower--structs))

(defun nelisp-cfront-lower--addr (e)
  "Grammar address of lvalue E (deref / index / arrow / member)."
  (pcase (car e)
    ('unop (if (string= (nth 1 e) "*")
               (nelisp-cfront-lower--expr (nth 2 e))
             (nelisp-cfront-lower--err :not-an-lvalue e)))
    ('index `(+ ,(nelisp-cfront-lower--expr (nth 1 e))
                (* ,(nelisp-cfront-lower--expr (nth 2 e))
                   ,(nelisp-cfront-lower--elem-size (nth 1 e)))))
    ('arrow
     (let* ((pty (nelisp-cfront-lower--type-of (nth 1 e)))
            (fld (nelisp-cfront-type-field (plist-get pty :struct) (nth 2 e)
                                           nelisp-cfront-lower--structs)))
       `(+ ,(nelisp-cfront-lower--expr (nth 1 e)) ,(plist-get fld :offset))))
    ('member
     (let* ((oty (nelisp-cfront-lower--type-of (nth 1 e)))
            (fld (nelisp-cfront-type-field (plist-get oty :struct) (nth 2 e)
                                           nelisp-cfront-lower--structs)))
       `(+ ,(nelisp-cfront-lower--addr (nth 1 e)) ,(plist-get fld :offset))))
    (_ (nelisp-cfront-lower--err :not-an-lvalue e))))

(defun nelisp-cfront-lower--field-of (e)
  "Return the field plist (:type :offset :size [:bits :bit-offset]) for an
arrow/member lvalue E, or nil."
  (pcase (car e)
    ((or 'arrow 'member)
     (let ((sname (plist-get (nelisp-cfront-lower--type-of (nth 1 e)) :struct)))
       (and sname (ignore-errors
                    (nelisp-cfront-type-field sname (nth 2 e)
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
      (nelisp-cfront-lower--load-w
       (nelisp-cfront-lower--addr e)
       (nelisp-cfront-type-size (nelisp-cfront-lower--type-of e)
                                nelisp-cfront-lower--structs)))))

;;; --- collect mutable locals (for the outer frame-slot let) -----------

(defun nelisp-cfront-lower--collect-decls (node acc)
  "Collect local declaration names from NODE into ACC (a list); return ACC."
  (when (consp node)
    (pcase (car node)
      ('decl (push (nth 2 node) acc))
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

;;; --- expressions -----------------------------------------------------

(defun nelisp-cfront-lower--expr (e)
  (pcase (car e)
    ('int (nth 1 e))
    ('var (let ((name (nth 1 e)))
            (if (nelisp-cfront-lower--var-is-function-p name)
                `(addr-of ,(nelisp-cfront-lower--sym name)) ; function -> pointer
              (nelisp-cfront-lower--lvar name))))
    ('str (nelisp-cfront-lower--err :string-literal-unsupported e)) ; needs rodata
    ('binop (nelisp-cfront-lower--binop (nth 1 e) (nth 2 e) (nth 3 e)))
    ('unop (if (string= (nth 1 e) "*")
               (nelisp-cfront-lower--load-lvalue e)   ; typed deref
             (nelisp-cfront-lower--unop (nth 1 e) (nth 2 e))))
    ('assign (nelisp-cfront-lower--assign (nth 1 e) (nth 2 e) (nth 3 e)))
    ('call (nelisp-cfront-lower--call (nth 1 e) (nth 2 e)))
    ('ternary `(if ,(nelisp-cfront-lower--cond (nth 1 e))
                   ,(nelisp-cfront-lower--expr (nth 2 e))
                 ,(nelisp-cfront-lower--expr (nth 3 e))))
    ('index (nelisp-cfront-lower--load-lvalue e))      ; typed element load
    ('sizeof (nelisp-cfront-lower--sizeof (nth 1 e)))
    ((or 'pre 'post) (nelisp-cfront-lower--incdec e))
    ((or 'member 'arrow) (nelisp-cfront-lower--load-lvalue e))
    (_ (nelisp-cfront-lower--err :unsupported-expr e))))

(defun nelisp-cfront-lower--binop (op l r)
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
     (t (nelisp-cfront-lower--err :unsupported-binop op)))))

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
      ("-" `(- 0 ,g))
      ("+" g)
      ("!" `(if (= ,g 0) 1 0))
      ("~" `(logxor ,g -1))
      ("*" `(ptr-read-u64 ,g 0))              ; MVP: 8-byte deref
      ("&" (if (and (consp e) (eq (car e) 'var)
                    (nelisp-cfront-lower--var-is-function-p (nth 1 e)))
               `(addr-of ,(nelisp-cfront-lower--sym (nth 1 e)))
             (nelisp-cfront-lower--err :addr-of-unsupported e))) ; &local: M4 follow-on
      (_ (nelisp-cfront-lower--err :unsupported-unop op)))))

(defun nelisp-cfront-lower--assign (op lhs rhs)
  (let ((grhs (nelisp-cfront-lower--expr rhs)))
    (pcase (car lhs)
      ('var
       (let ((name (nelisp-cfront-lower--lvar (nth 1 lhs))))
         (if (string= op "=")
             `(setq ,name ,grhs)
           ;; compound: a OP= b  ->  a = a OP b
           (let* ((bop (substring op 0 (1- (length op))))
                  (g (cdr (assoc bop nelisp-cfront-lower--binop-map))))
             (unless g (nelisp-cfront-lower--err :unsupported-compound-assign op))
             `(setq ,name (,g ,name ,grhs))))))
      ((or 'unop 'index 'arrow 'member)        ; memory lvalue: *p / a[i] / p->f / s.f
       (let ((fld (nelisp-cfront-lower--field-of lhs)))
         (if (and fld (plist-get fld :bits))
             (nelisp-cfront-lower--bitfield-assign lhs op grhs fld)
           (let ((addr (nelisp-cfront-lower--addr lhs))
                 (width (nelisp-cfront-type-size (nelisp-cfront-lower--type-of lhs)
                                                 nelisp-cfront-lower--structs)))
             (if (string= op "=")
                 (nelisp-cfront-lower--store-w addr width grhs)
               (let* ((bop (substring op 0 (1- (length op))))
                      (g (cdr (assoc bop nelisp-cfront-lower--binop-map))))
                 (unless g (nelisp-cfront-lower--err :unsupported-compound-assign op))
                 (nelisp-cfront-lower--store-w
                  addr width (list g (nelisp-cfront-lower--load-w addr width) grhs))))))))
      (_ (nelisp-cfront-lower--err :unsupported-assign-target lhs)))))

(defun nelisp-cfront-lower--call (fn args)
  (let ((gargs (mapcar #'nelisp-cfront-lower--expr args)))
    (if (and (eq (car fn) 'var)
             (nelisp-cfront-lower--var-is-function-p (nth 1 fn)))
        ;; direct call to a named function
        (cons (nelisp-cfront-lower--sym (nth 1 fn)) gargs)
      ;; indirect call through a function-pointer value: fp(...) / (*fp)(...)
      (let ((target (if (and (eq (car fn) 'unop) (string= (nth 1 fn) "*"))
                        (nth 2 fn)
                      fn)))
        (cons 'call-ptr (cons (nelisp-cfront-lower--expr target) gargs))))))

(defun nelisp-cfront-lower--incdec (e)
  "Lower ++/-- (pre or post).  Returns the assigned value (exact for pre
and for statement position; post in value position returns the new value
in this MVP)."
  (let ((target (nth 2 e)) (op (nth 1 e)))
    (unless (eq (car target) 'var)
      (nelisp-cfront-lower--err :incdec-needs-var target))
    (let ((name (nelisp-cfront-lower--lvar (nth 1 target)))
          (delta (if (string= op "++") 1 -1)))
      `(setq ,name (+ ,name ,delta)))))

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
         ('for (or (nelisp-cfront-lower--has-goto-p (nth 1 node))
                   (nelisp-cfront-lower--has-goto-p (nth 4 node))))
         (_ nil))))

(defun nelisp-cfront-lower--return-in-loop-p (node in-loop)
  "Non-nil when NODE contains a `return' lexically inside a loop."
  (and (consp node)
       (pcase (car node)
         ('return in-loop)
         ('block (cl-some (lambda (s) (nelisp-cfront-lower--return-in-loop-p s in-loop))
                          (cdr node)))
         ('if (or (nelisp-cfront-lower--return-in-loop-p (nth 2 node) in-loop)
                  (nelisp-cfront-lower--return-in-loop-p (nth 3 node) in-loop)))
         ('while (nelisp-cfront-lower--return-in-loop-p (nth 2 node) t))
         ('for (or (nelisp-cfront-lower--return-in-loop-p (nth 1 node) in-loop)
                   (nelisp-cfront-lower--return-in-loop-p (nth 4 node) t)))
         (_ nil))))

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
    ('decl (if (nth 3 s)
               `(setq ,(nelisp-cfront-lower--lvar (nth 2 s))
                      ,(nelisp-cfront-lower--expr (nth 3 s)))
             0))                              ; uninitialised: slot already 0
    ('expr-stmt (nelisp-cfront-lower--expr (nth 1 s)))
    ('block (nelisp-cfront-lower--stmts-effect (cdr s)))
    ('if `(if ,(nelisp-cfront-lower--cond (nth 1 s))
              ,(nelisp-cfront-lower--effect (nth 2 s))
            ,(if (nth 3 s) (nelisp-cfront-lower--effect (nth 3 s)) 0)))
    ('while (nelisp-cfront-lower--lower-loop (nth 1 s) (nth 2 s) nil))
    ('for (nelisp-cfront-lower--for s))
    ('break (if nelisp-cfront-lower--brk-stack
                `(setq ,(car nelisp-cfront-lower--brk-stack) 1)
              (nelisp-cfront-lower--err :break-outside-loop s)))
    ('continue (if nelisp-cfront-lower--brk-stack 0   ; guard-lift skips the rest
                 (nelisp-cfront-lower--err :continue-outside-loop s)))
    ('return (if nelisp-cfront-lower--ret-mode
                 (let ((rs (car nelisp-cfront-lower--ret-mode))
                       (rv (cdr nelisp-cfront-lower--ret-mode)))
                   `(seq (setq ,rv ,(if (nth 1 s) (nelisp-cfront-lower--expr (nth 1 s)) 0))
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

(defun nelisp-cfront-lower--tail (s void-p)
  "Lower statement S in tail (return-value) position."
  (pcase (car s)
    ('return (if (nth 1 s) (nelisp-cfront-lower--expr (nth 1 s)) 0))
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
         (pnames (delq nil (mapcar (lambda (p) (and (nth 2 p)
                                                    (nelisp-cfront-lower--lvar (nth 2 p))))
                                   params)))
         (locals (nreverse (delete-dups (nelisp-cfront-lower--collect-decls body nil))))
         (nelisp-cfront-lower--tenv
          (append (delq nil (mapcar (lambda (p) (and (nth 2 p) (cons (nth 2 p) (nth 1 p))))
                                    params))
                  (nelisp-cfront-lower--collect-decl-types body nil)))
         (nelisp-cfront-lower--synth nil)
         (nelisp-cfront-lower--brk-stack nil)
         (nelisp-cfront-lower--brk-counter 0)
         (has-goto (nelisp-cfront-lower--has-goto-p body))
         (needs-exit (or (nelisp-cfront-lower--return-in-loop-p body nil) has-goto))
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
         (local-binds (mapcar (lambda (v)
                                (list (nelisp-cfront-lower--lvar v)
                                      (list nelisp-cfront-lower--zero-fn)))
                              locals))
         (synth-binds (mapcar (lambda (v) (list v (list nelisp-cfront-lower--zero-fn)))
                              (reverse nelisp-cfront-lower--synth)))
         (binds (append local-binds synth-binds))
         (wrapped (if binds `(let ,binds ,body-g) body-g)))
    `(defun ,name ,pnames ,wrapped)))

(defun nelisp-cfront-lower-program (ast)
  "Lower AST `(program TOP...)' to a grammar `(seq (defun ...) ...)'.
Includes the `nelisp_cfront__zero' helper.  Globals/prototypes are
skipped in the MVP (functions only)."
  (unless (eq (car ast) 'program)
    (nelisp-cfront-lower--err :not-a-program ast))
  (let ((nelisp-cfront-lower--structs (nelisp-cfront-type-build-structs ast))
        (nelisp-cfront-lower--funcs (nelisp-cfront-lower--collect-func-types ast))
        (funcs nil))
    (dolist (top (cdr ast))
      (pcase (car top)
        ('func (push (nelisp-cfront-lower--func top) funcs))
        ('proto nil)                          ; ignore prototypes
        ('global nil)                         ; MVP: globals deferred
        ('struct-def nil)                     ; layout already in the table
        ('typedef nil)                        ; aliases resolved during parse
        (_ (nelisp-cfront-lower--err :unsupported-toplevel top))))
    (cons 'seq
          (cons `(defun ,nelisp-cfront-lower--zero-fn () 0)
                (nreverse funcs)))))

(provide 'nelisp-cfront-lower)

;;; nelisp-cfront-lower.el ends here
