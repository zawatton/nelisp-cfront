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

(defun nelisp-cfront-lower--load-lvalue (e)
  "Load the value of memory lvalue E using its type width."
  (nelisp-cfront-lower--load-w
   (nelisp-cfront-lower--addr e)
   (nelisp-cfront-type-size (nelisp-cfront-lower--type-of e)
                            nelisp-cfront-lower--structs)))

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
    ('var (nelisp-cfront-lower--lvar (nth 1 e)))
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
      ("&" (nelisp-cfront-lower--err :addr-of-unsupported e))
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
       (let ((addr (nelisp-cfront-lower--addr lhs))
             (width (nelisp-cfront-type-size (nelisp-cfront-lower--type-of lhs)
                                             nelisp-cfront-lower--structs)))
         (if (string= op "=")
             (nelisp-cfront-lower--store-w addr width grhs)
           (let* ((bop (substring op 0 (1- (length op))))
                  (g (cdr (assoc bop nelisp-cfront-lower--binop-map))))
             (unless g (nelisp-cfront-lower--err :unsupported-compound-assign op))
             (nelisp-cfront-lower--store-w
              addr width (list g (nelisp-cfront-lower--load-w addr width) grhs))))))
      (_ (nelisp-cfront-lower--err :unsupported-assign-target lhs)))))

(defun nelisp-cfront-lower--call (fn args)
  (unless (eq (car fn) 'var)
    (nelisp-cfront-lower--err :function-pointer-call-unsupported fn))
  (cons (nelisp-cfront-lower--sym (nth 1 fn))
        (mapcar #'nelisp-cfront-lower--expr args)))

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

(defun nelisp-cfront-lower--effect (s)
  "Lower statement S in effect (value-discarded) position."
  (pcase (car s)
    ('decl (if (nth 3 s)
               `(setq ,(nelisp-cfront-lower--lvar (nth 2 s))
                      ,(nelisp-cfront-lower--expr (nth 3 s)))
             0))                              ; uninitialised: slot already 0
    ('expr-stmt (nelisp-cfront-lower--expr (nth 1 s)))
    ('block (nelisp-cfront-lower--seq (mapcar #'nelisp-cfront-lower--effect (cdr s))))
    ('if `(if ,(nelisp-cfront-lower--cond (nth 1 s))
              ,(nelisp-cfront-lower--effect (nth 2 s))
            ,(if (nth 3 s) (nelisp-cfront-lower--effect (nth 3 s)) 0)))
    ('while `(while ,(nelisp-cfront-lower--cond (nth 1 s))
               ,(nelisp-cfront-lower--effect (nth 2 s))))
    ('for (nelisp-cfront-lower--for s))
    ('return (nelisp-cfront-lower--err :early-return-unsupported s))
    ('break (nelisp-cfront-lower--err :break-unsupported s))
    ('continue (nelisp-cfront-lower--err :continue-unsupported s))
    (_ (nelisp-cfront-lower--err :unsupported-stmt s))))

(defun nelisp-cfront-lower--for (s)
  (let ((init (nth 1 s)) (cnd (nth 2 s)) (step (nth 3 s)) (body (nth 4 s)))
    (let ((wbody (nelisp-cfront-lower--seq
                  (append (list (nelisp-cfront-lower--effect body))
                          (when step (list (nelisp-cfront-lower--expr step))))))
          (wcond (if cnd (nelisp-cfront-lower--cond cnd) 1)))
      (nelisp-cfront-lower--seq
       (append (when init (list (nelisp-cfront-lower--effect init)))
               (list `(while ,wcond ,wbody)))))))

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
         (body-g (nelisp-cfront-lower--block-tail body void-p))
         (wrapped (if locals
                      `(let ,(mapcar (lambda (v)
                                       (list (nelisp-cfront-lower--lvar v)
                                             (list nelisp-cfront-lower--zero-fn)))
                                     locals)
                         ,body-g)
                    body-g)))
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
        (_ (nelisp-cfront-lower--err :unsupported-toplevel top))))
    (cons 'seq
          (cons `(defun ,nelisp-cfront-lower--zero-fn () 0)
                (nreverse funcs)))))

(provide 'nelisp-cfront-lower)

;;; nelisp-cfront-lower.el ends here
