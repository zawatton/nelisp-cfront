;;; nelisp-cfront-parse.el --- C parser (recursive descent) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M2.2 — recursive-descent parser for the C subset.  Consumes the M2.1
;; lexer's token list and produces an AST.
;;
;; AST node shapes (tagged lists):
;;   program    (program TOPLEVEL...)
;;   func       (func RET-TYPE NAME (PARAM...) BODY)   ; BODY = block
;;   global     (global TYPE NAME INIT|nil)
;;   param      (param TYPE NAME)
;;   type       plist (:base SYM :ptr N [:unsigned t] [:struct NAME])
;;              BASE in (void char short int long)
;;   stmts      (block STMT...) (if C THEN ELSE) (while C BODY)
;;              (for INIT COND STEP BODY) (return EXPR|nil)
;;              (decl TYPE NAME INIT|nil) (expr-stmt E) (break) (continue)
;;   exprs      (int N) (str S) (var NAME) (call FN (ARG...))
;;              (binop OP L R) (unop OP E) (assign OP LHS RHS)
;;              (index A I) (member O F) (arrow O F)
;;              (post OP E) (pre OP E) (ternary C A B)
;;
;; Scope: typedef/enum/switch/do-while and full declarator grammar are
;; deferred (Doc 03 backlog).  This covers what the MVP subset needs.

;;; Code:

(require 'cl-lib)
(require 'nelisp-cfront-lex)

(define-error 'nelisp-cfront-parse-error "nelisp-cfront parser error")

(defvar nelisp-cfront-parse--toks nil
  "Remaining token list during a parse (dynamically bound).")

(defvar nelisp-cfront-parse--typedefs nil
  "Alist of typedef NAME -> resolved type plist, accumulated during a parse.")

(defun nelisp-cfront-parse--typedef-lookup (name)
  "Return the type a typedef NAME resolves to, or nil."
  (cdr (assoc name nelisp-cfront-parse--typedefs)))

(defvar nelisp-cfront-parse--enum-consts nil
  "Alist of enum-constant NAME -> integer value, accumulated during a parse.
`parse-primary' folds a reference to such a NAME into its `(int VALUE)'.")

(defvar nelisp-cfront-parse--last-base-ptr 0
  "Intrinsic (declarator-independent) pointer level of the type just parsed
by `parse-type' — i.e. the pointer level carried by the type SPECIFIER
itself (a pointer typedef like `typedef T *TP'), excluding the syntactic
`*' stars that bind to one declarator.  In `T a, b;' the syntactic `*'
binds only to its declarator (`int *a, b' => a is `int*', b is `int'),
but a pointer typedef's level is shared by every declarator (`TP a, b'
=> both are `T*').  Comma-declarator loops read this to seed each later
declarator's pointer count instead of resetting it to 0.")

(defun nelisp-cfront-parse--enum-lookup (name)
  "Return (NAME . VALUE) when NAME is a known enum constant, else nil."
  (assoc name nelisp-cfront-parse--enum-consts))

(defun nelisp-cfront-parse--const-eval (e)
  "Fold a constant-expression AST E to an integer (enum values).
Enum-constant references are already folded to `(int N)' by
`parse-primary', so only literals / arithmetic appear here.  Signals
`nelisp-cfront-parse-error' when E is not a compile-time integer."
  (pcase (car e)
    ('int (nth 1 e))
    ('cast (nelisp-cfront-parse--const-eval (nth 2 e)))   ; integer cast (value-preserving)
    ('sizeof                                              ; sizeof(TYPE): scalar/pointer only
     (let* ((ty (nth 1 e)) (ptr (or (plist-get ty :ptr) 0)))
       (if (> ptr 0) 8
         (pcase (plist-get ty :base)
           ('char 1) ('short 2) ('int 4) ('long 8) ('void 1)
           ('float 4) ('double 8)
           ;; struct/array sizeof needs the layout table (unavailable here);
           ;; let the caller's tolerant fallback handle it.
           (_ (signal 'nelisp-cfront-parse-error (list :non-const-sizeof ty)))))))
    ('unop (let ((v (nelisp-cfront-parse--const-eval (nth 2 e))))
             (pcase (nth 1 e)
               ("-" (- v)) ("+" v) ("~" (lognot v)) ("!" (if (= v 0) 1 0))
               (_ (signal 'nelisp-cfront-parse-error (list :non-const-unop e))))))
    ('ternary (if (/= 0 (nelisp-cfront-parse--const-eval (nth 1 e)))
                  (nelisp-cfront-parse--const-eval (nth 2 e))
                (nelisp-cfront-parse--const-eval (nth 3 e))))
    ('binop
     (let ((a (nelisp-cfront-parse--const-eval (nth 2 e)))
           (b (nelisp-cfront-parse--const-eval (nth 3 e))))
       (pcase (nth 1 e)
         ("+" (+ a b)) ("-" (- a b)) ("*" (* a b))
         ("/" (if (= b 0) 0 (/ a b))) ("%" (if (= b 0) 0 (% a b)))
         ("<<" (ash a b)) (">>" (ash a (- b)))
         ("&" (logand a b)) ("|" (logior a b)) ("^" (logxor a b))
         ("<" (if (< a b) 1 0)) (">" (if (> a b) 1 0))
         ("<=" (if (<= a b) 1 0)) (">=" (if (>= a b) 1 0))
         ("==" (if (= a b) 1 0)) ("!=" (if (/= a b) 1 0))
         ("&&" (if (and (/= a 0) (/= b 0)) 1 0))
         ("||" (if (or (/= a 0) (/= b 0)) 1 0))
         (_ (signal 'nelisp-cfront-parse-error (list :non-const-binop e))))))
    (_ (signal 'nelisp-cfront-parse-error (list :non-const-expr e)))))

(defun nelisp-cfront-parse--fold-dim (ast)
  "Fold an already-parsed array-dimension AST to an integer size.
When the dimension is not foldable at parse time the *expression* is
returned (not `t'), so a layout-time evaluator can resolve it once the
struct table exists — notably `sizeof(struct T)' and arithmetic over it
\(e.g. `(1024-8)/sizeof(struct RowSetEntry)').  A genuine VLA stays a
non-evaluable expression and fails later as `:non-constant-array-size'."
  (condition-case nil
      (nelisp-cfront-parse--const-eval ast)
    (nelisp-cfront-parse-error ast)))

(defun nelisp-cfront-parse--parse-enum-body ()
  "Parse an enum `{ NAME [= CONST] , ... }' (point at `{'), registering
each constant into `--enum-consts' with C auto-increment semantics."
  (nelisp-cfront-parse--eat-punct "{")
  (let ((next 0))
    (while (not (nelisp-cfront-parse--at-punct "}"))
      (let ((name (nth 1 (nelisp-cfront-parse--advance))))   ; constant name (ident)
        (when (nelisp-cfront-parse--at-punct "=")
          (nelisp-cfront-parse--advance)
          (setq next (nelisp-cfront-parse--const-eval
                      (nelisp-cfront-parse--parse-ternary)))) ; constant-expression
        (push (cons name next) nelisp-cfront-parse--enum-consts)
        (setq next (1+ next))
        (when (nelisp-cfront-parse--at-punct ",")
          (nelisp-cfront-parse--advance))))
    (nelisp-cfront-parse--eat-punct "}")))

(defconst nelisp-cfront-parse--gcc-ignore
  '("__extension__" "__restrict" "__restrict__" "__inline" "__inline__"
    "__signed__" "__const" "__volatile__" "__nonnull" "__nothrow__")
  "GCC-ism identifier tokens dropped wholesale (no args).")

(defconst nelisp-cfront-parse--builtin-typedefs
  '(("__builtin_va_list" . (:base long :ptr 1))
    ("__gnuc_va_list"    . (:base long :ptr 1))
    ("_Float16"  . (:base float :ptr 0))
    ("_Float32"  . (:base float :ptr 0))
    ("_Float32x" . (:base double :ptr 0))
    ("_Float64"  . (:base double :ptr 0))
    ("_Float64x" . (:base double :ptr 0))
    ("_Float128" . (:base double :ptr 0))
    ("_Float128x" . (:base double :ptr 0)))
  "Pre-registered opaque/extended builtin types so real preprocessed C parses.")

(defun nelisp-cfront-parse--strip-gcc (toks)
  "Drop GCC-ism tokens from TOKS: `__attribute__((...))', `__extension__',
`__restrict', `__inline', `__asm__(...)' etc.  Returns a filtered list."
  (let ((out nil) (rest toks))
    (while rest
      (let* ((tk (car rest)) (ty (nth 0 tk)) (v (nth 1 tk)))
        (cond
         ;; __attribute__((...)) / __asm__(...) : drop name + balanced parens
         ((and (eq ty 'ident)
               (member v '("__attribute__" "__attribute" "__asm__" "__asm")))
          (setq rest (cdr rest))
          (when (and rest (eq (nth 0 (car rest)) 'punct)
                     (string= (nth 1 (car rest)) "("))
            (let ((depth 0) (done nil))
              (while (and rest (not done))
                (let ((x (car rest)))
                  (when (eq (nth 0 x) 'punct)
                    (cond ((string= (nth 1 x) "(") (setq depth (1+ depth)))
                          ((string= (nth 1 x) ")")
                           (setq depth (1- depth))
                           (when (= depth 0) (setq done t)))))
                  (setq rest (cdr rest)))))))
         ;; bare GCC-ism keywords-as-idents: just drop
         ((and (eq ty 'ident) (member v nelisp-cfront-parse--gcc-ignore))
          (setq rest (cdr rest)))
         ;; __int128 family: rewrite to `long' so `unsigned __int128' combines
         ((and (eq ty 'ident) (member v '("__int128" "__int128_t" "__uint128_t")))
          (push (list 'keyword "long" (nth 2 tk)) out) (setq rest (cdr rest)))
         (t (push tk out) (setq rest (cdr rest))))))
    (nreverse out)))

(defconst nelisp-cfront-parse--type-keywords
  '("void" "char" "short" "int" "long" "float" "double" "unsigned" "signed"
    "const" "struct" "union" "enum" "static" "extern" "_Bool"
    "register" "auto" "volatile" "inline" "restrict")
  "Keywords that may begin / appear in a type-specifier sequence.")

;;; --- cursor ----------------------------------------------------------

(defsubst nelisp-cfront-parse--peek () (car nelisp-cfront-parse--toks))
(defsubst nelisp-cfront-parse--ptype () (nth 0 (car nelisp-cfront-parse--toks)))
(defsubst nelisp-cfront-parse--pval () (nth 1 (car nelisp-cfront-parse--toks)))

(defun nelisp-cfront-parse--advance ()
  (prog1 (car nelisp-cfront-parse--toks)
    (setq nelisp-cfront-parse--toks (cdr nelisp-cfront-parse--toks))))

(defun nelisp-cfront-parse--at (type &optional val)
  (let ((tk (nelisp-cfront-parse--peek)))
    (and tk (eq (nth 0 tk) type)
         (or (null val) (equal (nth 1 tk) val)))))

(defun nelisp-cfront-parse--at-punct (p) (nelisp-cfront-parse--at 'punct p))
(defun nelisp-cfront-parse--at-kw (k) (nelisp-cfront-parse--at 'keyword k))

(defun nelisp-cfront-parse--eat-punct (p)
  (if (nelisp-cfront-parse--at-punct p)
      (nelisp-cfront-parse--advance)
    (signal 'nelisp-cfront-parse-error
            (list :expected-punct p :got (nelisp-cfront-parse--peek)))))

(defun nelisp-cfront-parse--eat-ident ()
  (if (nelisp-cfront-parse--at 'ident)
      (nth 1 (nelisp-cfront-parse--advance))
    (signal 'nelisp-cfront-parse-error
            (list :expected-ident :got (nelisp-cfront-parse--peek)))))

;;; --- types -----------------------------------------------------------

(defun nelisp-cfront-parse--type-start-p ()
  (or (and (nelisp-cfront-parse--at 'keyword)
           (member (nelisp-cfront-parse--pval) nelisp-cfront-parse--type-keywords))
      (and (nelisp-cfront-parse--at 'ident)
           (nelisp-cfront-parse--typedef-lookup (nelisp-cfront-parse--pval)))))

(defun nelisp-cfront-parse--token-starts-type (tk)
  "Non-nil when token TK could begin a type-specifier (for cast detection)."
  (and tk
       (or (and (eq (nth 0 tk) 'keyword)
                (member (nth 1 tk) nelisp-cfront-parse--type-keywords))
           (and (eq (nth 0 tk) 'ident)
                (nelisp-cfront-parse--typedef-lookup (nth 1 tk))))))

(defun nelisp-cfront-parse--parse-type ()
  "Parse a type-specifier sequence + pointer stars into a type plist.
A `static' storage class is recorded as `:storage static' on the result so
the lowerer can lift a function-static local to a module global."
  (let ((static-p nil))
  ;; leading storage-class / qualifier keywords
  (while (and (nelisp-cfront-parse--at 'keyword)
              (member (nelisp-cfront-parse--pval)
                      '("const" "static" "extern" "register" "volatile" "inline")))
    (when (equal (nelisp-cfront-parse--pval) "static") (setq static-p t))
    (nelisp-cfront-parse--advance))
  (if (and (nelisp-cfront-parse--at 'ident)
           (nelisp-cfront-parse--typedef-lookup (nelisp-cfront-parse--pval)))
      ;; typedef name -> its resolved type, plus any extra pointer stars
      (let* ((bt (nelisp-cfront-parse--typedef-lookup
                  (nth 1 (nelisp-cfront-parse--advance))))
             (ptr (or (plist-get bt :ptr) 0)))
        ;; the typedef's own pointer level is shared by all declarators
        (setq nelisp-cfront-parse--last-base-ptr ptr)
        (while (or (nelisp-cfront-parse--at-punct "*")
                   (and (nelisp-cfront-parse--at 'keyword)
                        (member (nelisp-cfront-parse--pval) '("const" "volatile" "restrict"))))
          (if (nelisp-cfront-parse--at-punct "*")
              (progn (nelisp-cfront-parse--advance) (setq ptr (1+ ptr)))
            (nelisp-cfront-parse--advance)))   ; skip pointer qualifier
        (let ((r (plist-put (copy-sequence bt) :ptr ptr)))
          (if static-p (append r '(:storage static)) r)))
    (let ((specs nil) (struct-name nil) (struct-fields nil) (unsigned nil)
          (is-union nil))
      (while (and (nelisp-cfront-parse--at 'keyword)
                  (member (nelisp-cfront-parse--pval) nelisp-cfront-parse--type-keywords))
      (let ((w (nth 1 (nelisp-cfront-parse--advance))))
        (cond
         ((string= w "static") (setq static-p t)) ; storage: lift to global
         ((member w '("const" "extern")) nil) ; ignore qualifiers/storage
         ((string= w "unsigned") (setq unsigned t))
         ((string= w "signed") (push "int" specs))   ; `signed' alone => int
         ((or (string= w "struct") (string= w "union"))
          (when (string= w "union") (setq is-union t))
          (when (nelisp-cfront-parse--at 'ident)         ; tag is optional (anonymous)
            (setq struct-name (nth 1 (nelisp-cfront-parse--advance))))
          (when (nelisp-cfront-parse--at-punct "{")
            (setq struct-fields (nelisp-cfront-parse--parse-struct-body))))
         ((string= w "enum")
          (when (nelisp-cfront-parse--at 'ident) (nelisp-cfront-parse--advance)) ; tag
          (when (nelisp-cfront-parse--at-punct "{")      ; parse + register constants
            (nelisp-cfront-parse--parse-enum-body))
          (push "int" specs))            ; enum is an int
         (t (push w specs)))))
    (let* ((specs (nreverse specs))
           (base (cond
                  ((or struct-name struct-fields) 'struct)  ; tagged or anonymous
                  ((member "double" specs) 'double)
                  ((member "float" specs) 'float)
                  ((member "long" specs) 'long)
                  ((member "short" specs) 'short)
                  ((member "char" specs) 'char)
                  ((member "_Bool" specs) 'char)
                  ((member "int" specs) 'int)
                  ((member "void" specs) 'void)
                  (unsigned 'int)              ; "unsigned" alone => unsigned int
                  (t (signal 'nelisp-cfront-parse-error
                             (list :not-a-type (nelisp-cfront-parse--peek))))))
           (ptr 0))
      ;; a plain type specifier carries no declarator-independent pointer
      ;; level; every `*' below binds to the (first) declarator only.
      (setq nelisp-cfront-parse--last-base-ptr 0)
      (while (or (nelisp-cfront-parse--at-punct "*")
                 (and (nelisp-cfront-parse--at 'keyword)
                      (member (nelisp-cfront-parse--pval) '("const" "volatile" "restrict"))))
        (if (nelisp-cfront-parse--at-punct "*")
            (progn (nelisp-cfront-parse--advance) (setq ptr (1+ ptr)))
          (nelisp-cfront-parse--advance)))     ; skip pointer qualifier
      (append (list :base base :ptr ptr)
              (when unsigned '(:unsigned t))
              (when struct-name (list :struct struct-name))
              (when struct-fields (list :fields struct-fields))
              (when is-union '(:union t))
              (when static-p '(:storage static))))))))

(defun nelisp-cfront-parse--skip-paren-group ()
  "Skip a balanced `( ... )' group at point (point must be at `(')."
  (nelisp-cfront-parse--eat-punct "(")
  (let ((d 1))
    (while (> d 0)
      (let ((x (nelisp-cfront-parse--advance)))
        (cond
         ((eq (nth 0 x) 'eof) (signal 'nelisp-cfront-parse-error (list :unbalanced-parens)))
         ((and (eq (nth 0 x) 'punct) (string= (nth 1 x) "(")) (setq d (1+ d)))
         ((and (eq (nth 0 x) 'punct) (string= (nth 1 x) ")")) (setq d (1- d))))))))

(defun nelisp-cfront-parse--base-type (ty)
  "Return TY's shared base (drop pointer/array; keep struct/union/unsigned)."
  (append (list :base (plist-get ty :base) :ptr 0)
          (when (plist-get ty :unsigned) '(:unsigned t))
          (when (plist-get ty :struct) (list :struct (plist-get ty :struct)))
          (when (plist-get ty :union) '(:union t))))

(defun nelisp-cfront-parse--parse-struct-body ()
  "Parse `{ TYPE NAME; ... }' into a list of (field TYPE NAME BITS)."
  (nelisp-cfront-parse--eat-punct "{")
  (let ((fields nil))
    (while (not (nelisp-cfront-parse--at-punct "}"))
      (let* ((fty (nelisp-cfront-parse--parse-type))
             ;; pointer level shared by all declarators (a pointer typedef).
             (field-base-ptr nelisp-cfront-parse--last-base-ptr)
             (fname nil) (bits nil))
        (cond
         ((nelisp-cfront-parse--at-fnptr-declarator)   ; fn-ptr field: ret (*f)(...)
          (let ((fp (nelisp-cfront-parse--parse-fnptr-name fty)))
            (setq fty (cdr fp) fname (car fp))))
         ((nelisp-cfront-parse--at-punct ":") (setq fname nil))  ; anonymous bitfield
         ((nelisp-cfront-parse--at-punct ";") (setq fname nil))  ; anonymous struct/union member
         (t (setq fname (nelisp-cfront-parse--eat-ident))))
        (cond
         ;; bitfield: TYPE NAME : WIDTH
         ((nelisp-cfront-parse--at-punct ":")
          (nelisp-cfront-parse--advance)
          (let ((w (nelisp-cfront-parse--parse-expr)))
            (unless (eq (car w) 'int)
              (signal 'nelisp-cfront-parse-error (list :non-constant-bitfield-width w)))
            (setq bits (nth 1 w))))
         ;; array field: TYPE NAME[N] / NAME[] (C99 flexible array member) /
         ;; NAME[N][M] (multi-dimensional).
         ((nelisp-cfront-parse--at-punct "[")
          (while (nelisp-cfront-parse--at-punct "[")
            (nelisp-cfront-parse--advance)
            (let ((sz (if (nelisp-cfront-parse--at-punct "]")
                          t                 ; `[]' = flexible array member
                        (nelisp-cfront-parse--fold-dim
                         (nelisp-cfront-parse--parse-expr)))))
              (nelisp-cfront-parse--eat-punct "]")
              (setq fty (append fty (list :array sz)))))))
        (push (list 'field fty fname bits) fields)
        ;; additional comma-separated declarators sharing the base type
        (let ((base (nelisp-cfront-parse--base-type fty)))
          (while (nelisp-cfront-parse--at-punct ",")
            (nelisp-cfront-parse--advance)
            (let ((ptr field-base-ptr)) ; seed from the typedef's shared level
              (while (nelisp-cfront-parse--at-punct "*")
                (nelisp-cfront-parse--advance) (setq ptr (1+ ptr)))
              (if (nelisp-cfront-parse--at-fnptr-declarator)
                  (let ((fp (nelisp-cfront-parse--parse-fnptr-name base)))
                    (push (list 'field (cdr fp) (car fp) nil) fields))
                (let ((nm (nelisp-cfront-parse--eat-ident)) (b2 nil)
                      (dty (plist-put (copy-sequence base) :ptr ptr)))
                  (cond
                   ((nelisp-cfront-parse--at-punct ":")
                    (nelisp-cfront-parse--advance)
                    (setq b2 (nth 1 (nelisp-cfront-parse--parse-expr))))
                   ((nelisp-cfront-parse--at-punct "[")
                    (nelisp-cfront-parse--advance)
                    (let ((sz (nelisp-cfront-parse--fold-dim
                               (nelisp-cfront-parse--parse-expr))))
                      (nelisp-cfront-parse--eat-punct "]")
                      (setq dty (append dty (list :array sz))))))
                  (push (list 'field dty nm b2) fields))))))
        (nelisp-cfront-parse--eat-punct ";")))
    (nelisp-cfront-parse--eat-punct "}")
    (nreverse fields)))

(defun nelisp-cfront-parse--at-fnptr-declarator ()
  "Non-nil when positioned at a function-pointer declarator `(* ...'."
  (and (nelisp-cfront-parse--at-punct "(")
       (let ((n (cadr nelisp-cfront-parse--toks)))
         (and n (eq (nth 0 n) 'punct) (string= (nth 1 n) "*")))))

(defun nelisp-cfront-parse--parse-fnptr-name (ret-ty)
  "Consume a (possibly nested) function-pointer declarator at point, e.g.
`(* NAME)(params)' or `(*(*NAME)(p))(void)'.  Balanced-paren scan: grab the
first identifier as NAME (the declared name is the leftmost ident), consume
the whole declarator (trailing `(...)' / `[...]' groups).  Return
\(cons NAME pointer-type); the type is a plain pointer marked :fnptr."
  (let ((name nil) (depth 0) (done nil))
    (while (and (not done) nelisp-cfront-parse--toks)
      (let* ((tk (nelisp-cfront-parse--advance)) (ty (nth 0 tk)) (v (nth 1 tk)))
        (cond
         ((eq ty 'eof) (signal 'nelisp-cfront-parse-error (list :unterminated-declarator)))
         ((and (eq ty 'punct) (string= v "(")) (setq depth (1+ depth)))
         ((and (eq ty 'punct) (string= v "["))  ; array group inside declarator
          (let ((d 1)) (while (> d 0)
                         (let ((x (nelisp-cfront-parse--advance)))
                           (when (eq (nth 0 x) 'punct)
                             (cond ((string= (nth 1 x) "[") (setq d (1+ d)))
                                   ((string= (nth 1 x) "]") (setq d (1- d)))))))))
         ((and (eq ty 'punct) (string= v ")"))
          (setq depth (1- depth))
          (when (<= depth 0)
            ;; declarator core/group closed: continue only if another
            ;; (params) or [dims] group follows, else the declarator is done
            (unless (or (nelisp-cfront-parse--at-punct "(")
                        (nelisp-cfront-parse--at-punct "["))
              (setq done t))))
         ((eq ty 'ident) (unless name (setq name v))))))
    ;; NAME may be nil for an unnamed fn-ptr param, e.g. `int (*)(void)'
    (cons name (list :base (or (plist-get ret-ty :base) 'long) :ptr 1 :fnptr t))))

(defun nelisp-cfront-parse--parse-initializer ()
  "Parse an initializer: a brace aggregate `{ ... }' (incl. designated
`.f =' / `[i] =') or a scalar assignment-expression.  Aggregates return
`(init-list ELT...)'; lowering of aggregates is deferred (globals/locals
arrays), so this is parse-only for now."
  (if (nelisp-cfront-parse--at-punct "{")
      (progn
        (nelisp-cfront-parse--advance)
        (let ((elts nil) (designated nil))
          (while (not (nelisp-cfront-parse--at-punct "}"))
            ;; optional designators: .field = ...  or  [index] = ...
            ;; The designator itself is discarded (positions are not tracked),
            ;; so a designated aggregate is tagged `init-list-designated' and
            ;; the lowerer refuses to byte-lay it (avoids a positional
            ;; miscompile); positional aggregates stay `init-list'.
            (while (or (nelisp-cfront-parse--at-punct ".")
                       (nelisp-cfront-parse--at-punct "["))
              (setq designated t)
              (if (nelisp-cfront-parse--at-punct ".")
                  (progn (nelisp-cfront-parse--advance) (nelisp-cfront-parse--eat-ident))
                (nelisp-cfront-parse--advance)
                (nelisp-cfront-parse--parse-expr)
                (nelisp-cfront-parse--eat-punct "]"))
              (when (nelisp-cfront-parse--at-punct "=") (nelisp-cfront-parse--advance)))
            (push (nelisp-cfront-parse--parse-initializer) elts)
            (when (nelisp-cfront-parse--at-punct ",") (nelisp-cfront-parse--advance)))
          (nelisp-cfront-parse--eat-punct "}")
          (cons (if designated 'init-list-designated 'init-list)
                (nreverse elts))))
    (nelisp-cfront-parse--parse-assign)))

;;; --- expressions (precedence climbing) -------------------------------

(defconst nelisp-cfront-parse--binops
  '(("*" . 10) ("/" . 10) ("%" . 10)
    ("+" . 9) ("-" . 9)
    ("<<" . 8) (">>" . 8)
    ("<" . 7) ("<=" . 7) (">" . 7) (">=" . 7)
    ("==" . 6) ("!=" . 6)
    ("&" . 5) ("^" . 4) ("|" . 3)
    ("&&" . 2) ("||" . 1))
  "Binary operator precedence (higher binds tighter).")

(defconst nelisp-cfront-parse--assign-ops
  '("=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=")
  "Assignment operators (right-associative, lowest precedence).")

(defun nelisp-cfront-parse--parse-expr ()
  ;; full expression: assignment-expr, then the comma operator (args and
  ;; initializers use parse-assign directly, so commas there don't apply)
  (let ((e (nelisp-cfront-parse--parse-assign)))
    (while (nelisp-cfront-parse--at-punct ",")
      (nelisp-cfront-parse--advance)
      (setq e (list 'comma e (nelisp-cfront-parse--parse-assign))))
    e))

(defun nelisp-cfront-parse--parse-assign ()
  (let ((lhs (nelisp-cfront-parse--parse-ternary)))
    (if (and (nelisp-cfront-parse--at 'punct)
             (member (nelisp-cfront-parse--pval) nelisp-cfront-parse--assign-ops))
        (let ((op (nth 1 (nelisp-cfront-parse--advance))))
          (list 'assign op lhs (nelisp-cfront-parse--parse-assign)))
      lhs)))

(defun nelisp-cfront-parse--parse-ternary ()
  (let ((c (nelisp-cfront-parse--parse-binary 1)))
    (if (nelisp-cfront-parse--at-punct "?")
        (progn
          (nelisp-cfront-parse--advance)
          ;; C grammar: `cond ? EXPRESSION : conditional-expression'.  The
          ;; THEN arm is a full expression — it may contain the comma
          ;; operator (e.g. `c ? (x=v),1 : f()'), so parse it with
          ;; `--parse-expr', not `--parse-assign'.  The ELSE arm is a
          ;; conditional-expression (no top-level comma).
          (let ((a (nelisp-cfront-parse--parse-expr)))
            (nelisp-cfront-parse--eat-punct ":")
            (list 'ternary c a (nelisp-cfront-parse--parse-assign))))
      c)))

(defun nelisp-cfront-parse--parse-binary (min-prec)
  (let ((left (nelisp-cfront-parse--parse-unary)))
    (catch 'done
      (while t
        (let* ((tk (nelisp-cfront-parse--peek))
               (op (and tk (eq (nth 0 tk) 'punct) (nth 1 tk)))
               (prec (and op (cdr (assoc op nelisp-cfront-parse--binops)))))
          (if (and prec (>= prec min-prec))
              (progn
                (nelisp-cfront-parse--advance)
                (setq left (list 'binop op left
                                 (nelisp-cfront-parse--parse-binary (1+ prec)))))
            (throw 'done left)))))))

(defconst nelisp-cfront-parse--unary-ops '("-" "!" "~" "*" "&" "+"))

(defun nelisp-cfront-parse--parse-unary ()
  (let ((tk (nelisp-cfront-parse--peek)))
    (cond
     ;; cast: ( TYPE ) unary   (disambiguated from a parenthesised expr by
     ;; the token after `(' starting a type)
     ((and (eq (nth 0 tk) 'punct) (string= (nth 1 tk) "(")
           (nelisp-cfront-parse--token-starts-type (cadr nelisp-cfront-parse--toks)))
      (nelisp-cfront-parse--advance)
      (let ((ty (nelisp-cfront-parse--parse-type)))
        ;; abstract fn-ptr declarator in a cast: (RET (*)(params))
        (when (nelisp-cfront-parse--at-fnptr-declarator)
          (setq ty (cdr (nelisp-cfront-parse--parse-fnptr-name ty))))
        ;; abstract array declarator in a cast: (T[N])
        (while (nelisp-cfront-parse--at-punct "[")
          (nelisp-cfront-parse--advance)
          (unless (nelisp-cfront-parse--at-punct "]") (nelisp-cfront-parse--parse-expr))
          (nelisp-cfront-parse--eat-punct "]"))
        (nelisp-cfront-parse--eat-punct ")")
        (list 'cast ty (nelisp-cfront-parse--parse-unary))))
     ((and (eq (nth 0 tk) 'punct) (member (nth 1 tk) '("++" "--")))
      (nelisp-cfront-parse--advance)
      (list 'pre (nth 1 tk) (nelisp-cfront-parse--parse-unary)))
     ((and (eq (nth 0 tk) 'punct) (member (nth 1 tk) nelisp-cfront-parse--unary-ops))
      (nelisp-cfront-parse--advance)
      (list 'unop (nth 1 tk) (nelisp-cfront-parse--parse-unary)))
     ((and (eq (nth 0 tk) 'keyword) (string= (nth 1 tk) "sizeof"))
      (nelisp-cfront-parse--advance)
      (if (and (nelisp-cfront-parse--at-punct "(")
               (nelisp-cfront-parse--token-starts-type (cadr nelisp-cfront-parse--toks)))
          ;; sizeof ( TYPE )
          (progn (nelisp-cfront-parse--advance)
                 (let ((ty (nelisp-cfront-parse--parse-type)))
                   (nelisp-cfront-parse--eat-punct ")")
                   (list 'sizeof ty)))
        ;; sizeof EXPR  /  sizeof ( EXPR )
        (list 'sizeof-expr (nelisp-cfront-parse--parse-unary))))
     (t (nelisp-cfront-parse--parse-postfix)))))

(defun nelisp-cfront-parse--parse-postfix ()
  (let ((e (nelisp-cfront-parse--parse-primary)))
    (catch 'done
      (while t
        (cond
         ((nelisp-cfront-parse--at-punct "(")
          (nelisp-cfront-parse--advance)
          (if (and (eq (car e) 'var)
                   (member (nth 1 e) '("__builtin_va_arg" "va_arg")))
              ;; va_arg(ap, TYPE) — the 2nd argument is a type, not an expr
              (let ((ap (nelisp-cfront-parse--parse-assign)))
                (nelisp-cfront-parse--eat-punct ",")
                (let ((ty (nelisp-cfront-parse--parse-type)))
                  (nelisp-cfront-parse--eat-punct ")")
                  (setq e (list 'va-arg ap ty))))
            (let ((args nil))
              (unless (nelisp-cfront-parse--at-punct ")")
                (push (nelisp-cfront-parse--parse-assign) args)
                (while (nelisp-cfront-parse--at-punct ",")
                  (nelisp-cfront-parse--advance)
                  (push (nelisp-cfront-parse--parse-assign) args)))
              (nelisp-cfront-parse--eat-punct ")")
              (setq e (list 'call e (nreverse args))))))
         ((nelisp-cfront-parse--at-punct "[")
          (nelisp-cfront-parse--advance)
          (let ((idx (nelisp-cfront-parse--parse-expr)))
            (nelisp-cfront-parse--eat-punct "]")
            (setq e (list 'index e idx))))
         ((nelisp-cfront-parse--at-punct ".")
          (nelisp-cfront-parse--advance)
          (setq e (list 'member e (nelisp-cfront-parse--eat-ident))))
         ((nelisp-cfront-parse--at-punct "->")
          (nelisp-cfront-parse--advance)
          (setq e (list 'arrow e (nelisp-cfront-parse--eat-ident))))
         ((or (nelisp-cfront-parse--at-punct "++") (nelisp-cfront-parse--at-punct "--"))
          (let ((op (nth 1 (nelisp-cfront-parse--advance))))
            (setq e (list 'post op e))))
         (t (throw 'done e)))))))

(defun nelisp-cfront-parse--parse-primary ()
  (let ((tk (nelisp-cfront-parse--peek)))
    (pcase (nth 0 tk)
      ('int  (nelisp-cfront-parse--advance) (list 'int (nth 1 tk)))
      ('float (nelisp-cfront-parse--advance) (list 'fnum (nth 1 tk)))
      ('char (nelisp-cfront-parse--advance) (list 'int (nth 1 tk)))
      ('string (nelisp-cfront-parse--advance)
               (let ((str (nth 1 tk)))    ; adjacent string-literal concatenation
                 (while (nelisp-cfront-parse--at 'string)
                   (setq str (concat str (nth 1 (nelisp-cfront-parse--advance)))))
                 (list 'str str)))
      ('ident (nelisp-cfront-parse--advance)
              (let ((ec (nelisp-cfront-parse--enum-lookup (nth 1 tk))))
                (if ec (list 'int (cdr ec))      ; enum constant -> its integer value
                  (list 'var (nth 1 tk)))))
      ('punct
       (if (string= (nth 1 tk) "(")
           (progn (nelisp-cfront-parse--advance)
                  (let ((e (nelisp-cfront-parse--parse-expr)))
                    (nelisp-cfront-parse--eat-punct ")")
                    e))
         (signal 'nelisp-cfront-parse-error (list :unexpected-token tk))))
      (_ (signal 'nelisp-cfront-parse-error (list :unexpected-token tk))))))

;;; --- statements ------------------------------------------------------

(defun nelisp-cfront-parse--parse-block ()
  (nelisp-cfront-parse--eat-punct "{")
  (let ((stmts nil))
    (while (not (nelisp-cfront-parse--at-punct "}"))
      (push (nelisp-cfront-parse--parse-stmt) stmts))
    (nelisp-cfront-parse--eat-punct "}")
    (cons 'block (nreverse stmts))))

(defun nelisp-cfront-parse--parse-stmt ()
  (cond
   ((nelisp-cfront-parse--at-punct ";")        ; empty statement
    (nelisp-cfront-parse--advance) (list 'block))
   ((nelisp-cfront-parse--at-punct "{") (nelisp-cfront-parse--parse-block))
   ((nelisp-cfront-parse--at-kw "typedef")      ; block-scope typedef
    (nelisp-cfront-parse--parse-typedef)        ; register the alias
    (list 'block))                              ; no runtime effect
   ((nelisp-cfront-parse--at-kw "if")
    (nelisp-cfront-parse--advance)
    (nelisp-cfront-parse--eat-punct "(")
    (let ((c (nelisp-cfront-parse--parse-expr)))
      (nelisp-cfront-parse--eat-punct ")")
      (let ((then (nelisp-cfront-parse--parse-stmt))
            (else nil))
        (when (nelisp-cfront-parse--at-kw "else")
          (nelisp-cfront-parse--advance)
          (setq else (nelisp-cfront-parse--parse-stmt)))
        (list 'if c then else))))
   ((nelisp-cfront-parse--at-kw "while")
    (nelisp-cfront-parse--advance)
    (nelisp-cfront-parse--eat-punct "(")
    (let ((c (nelisp-cfront-parse--parse-expr)))
      (nelisp-cfront-parse--eat-punct ")")
      (list 'while c (nelisp-cfront-parse--parse-stmt))))
   ((nelisp-cfront-parse--at-kw "for")
    (nelisp-cfront-parse--advance)
    (nelisp-cfront-parse--eat-punct "(")
    (let ((init (nelisp-cfront-parse--parse-for-clause))   ; ends with ;
          (cond- (if (nelisp-cfront-parse--at-punct ";") nil
                   (nelisp-cfront-parse--parse-expr))))
      (nelisp-cfront-parse--eat-punct ";")
      (let ((step (if (nelisp-cfront-parse--at-punct ")") nil
                    (nelisp-cfront-parse--parse-expr))))
        (nelisp-cfront-parse--eat-punct ")")
        (list 'for init cond- step (nelisp-cfront-parse--parse-stmt)))))
   ((nelisp-cfront-parse--at-kw "return")
    (nelisp-cfront-parse--advance)
    (let ((e (if (nelisp-cfront-parse--at-punct ";") nil
               (nelisp-cfront-parse--parse-expr))))
      (nelisp-cfront-parse--eat-punct ";")
      (list 'return e)))
   ((nelisp-cfront-parse--at-kw "switch")
    (nelisp-cfront-parse--advance)
    (nelisp-cfront-parse--eat-punct "(")
    (let ((e (nelisp-cfront-parse--parse-expr)))
      (nelisp-cfront-parse--eat-punct ")")
      (list 'switch e (nelisp-cfront-parse--parse-stmt))))
   ((nelisp-cfront-parse--at-kw "case")
    (nelisp-cfront-parse--advance)
    (let ((v (nelisp-cfront-parse--parse-expr)))
      (nelisp-cfront-parse--eat-punct ":")
      (list 'case v)))
   ((nelisp-cfront-parse--at-kw "default")
    (nelisp-cfront-parse--advance) (nelisp-cfront-parse--eat-punct ":") (list 'default))
   ((nelisp-cfront-parse--at-kw "do")
    (nelisp-cfront-parse--advance)
    (let ((body (nelisp-cfront-parse--parse-stmt)))
      (unless (nelisp-cfront-parse--at-kw "while")
        (signal 'nelisp-cfront-parse-error (list :do-without-while)))
      (nelisp-cfront-parse--advance)
      (nelisp-cfront-parse--eat-punct "(")
      (let ((cnd (nelisp-cfront-parse--parse-expr)))
        (nelisp-cfront-parse--eat-punct ")")
        (nelisp-cfront-parse--eat-punct ";")
        (list 'do-while body cnd))))
   ((nelisp-cfront-parse--at-kw "break")
    (nelisp-cfront-parse--advance) (nelisp-cfront-parse--eat-punct ";") (list 'break))
   ((nelisp-cfront-parse--at-kw "continue")
    (nelisp-cfront-parse--advance) (nelisp-cfront-parse--eat-punct ";") (list 'continue))
   ((nelisp-cfront-parse--at-kw "goto")
    (nelisp-cfront-parse--advance)
    (let ((name (nelisp-cfront-parse--eat-ident)))
      (nelisp-cfront-parse--eat-punct ";")
      (list 'goto name)))
   ;; label:  IDENT ':'  (an identifier immediately followed by a colon)
   ((and (nelisp-cfront-parse--at 'ident)
         (let ((n (cadr nelisp-cfront-parse--toks)))
           (and n (eq (nth 0 n) 'punct) (string= (nth 1 n) ":"))))
    (let ((name (nth 1 (nelisp-cfront-parse--advance))))
      (nelisp-cfront-parse--eat-punct ":")
      (list 'label name)))
   ((nelisp-cfront-parse--type-start-p)
    (prog1 (nelisp-cfront-parse--parse-decl)
      (nelisp-cfront-parse--eat-punct ";")))
   (t
    (let ((e (nelisp-cfront-parse--parse-expr)))
      (nelisp-cfront-parse--eat-punct ";")
      (list 'expr-stmt e)))))

(defun nelisp-cfront-parse--parse-for-clause ()
  "Parse the for-init clause up to (but not consuming) the first `;'."
  (cond
   ((nelisp-cfront-parse--at-punct ";") (nelisp-cfront-parse--advance) nil)
   ((nelisp-cfront-parse--type-start-p)
    (prog1 (nelisp-cfront-parse--parse-decl) (nelisp-cfront-parse--eat-punct ";")))
   (t (prog1 (list 'expr-stmt (nelisp-cfront-parse--parse-expr))
        (nelisp-cfront-parse--eat-punct ";")))))

(defun nelisp-cfront-parse--parse-decl ()
  "Parse a (possibly multi-declarator) local declaration, no trailing `;'.
Returns a `(decl TY NAME INIT)' or `(decls DECL...)' for `T a, b;'."
  (let* ((ty0 (nelisp-cfront-parse--parse-type))
         ;; pointer level intrinsic to the type specifier (a pointer typedef),
         ;; shared by every declarator; captured before any `*' is consumed.
         (base-ptr nelisp-cfront-parse--last-base-ptr)
         (base (nelisp-cfront-parse--base-type ty0))
         (decls nil) (first t)
         ;; bare type declaration (local struct/union/enum def, no declarator)
         (done (nelisp-cfront-parse--at-punct ";")))
    (while (not done)
      (let ((dty (if first ty0 base)) (name nil) (init nil))
        (unless first
          (let ((ptr base-ptr))   ; seed from the typedef's shared pointer level
            (while (nelisp-cfront-parse--at-punct "*")
              (nelisp-cfront-parse--advance) (setq ptr (1+ ptr)))
            (setq dty (plist-put (copy-sequence base) :ptr ptr))))
        (if (nelisp-cfront-parse--at-fnptr-declarator)
            (let ((fp (nelisp-cfront-parse--parse-fnptr-name dty)))
              (setq dty (cdr fp) name (car fp)))
          (setq name (nelisp-cfront-parse--eat-ident))
          (while (nelisp-cfront-parse--at-punct "[")   ; array dims
            (nelisp-cfront-parse--advance)
            (let ((sz (if (nelisp-cfront-parse--at-punct "]")
                          t            ; incomplete dimension `[]' (pointer-like)
                        (nelisp-cfront-parse--fold-dim
                         (nelisp-cfront-parse--parse-expr)))))
              (nelisp-cfront-parse--eat-punct "]")
              (setq dty (append dty (list :array sz))))))
        (when (nelisp-cfront-parse--at-punct "=")
          (nelisp-cfront-parse--advance)
          (setq init (nelisp-cfront-parse--parse-initializer)))
        (push (list 'decl dty name init) decls)
        (setq first nil)
        (if (nelisp-cfront-parse--at-punct ",")
            (nelisp-cfront-parse--advance)
          (setq done t))))
    (cond ((null decls) '(block))       ; bare type declaration -> no-op statement
          ((cdr decls) (cons 'decls (nreverse decls)))
          (t (car decls)))))

;;; --- top level -------------------------------------------------------

(defun nelisp-cfront-parse--parse-typedef ()
  "Parse `typedef <type> <name> ;' and register NAME as a type alias."
  (nelisp-cfront-parse--advance)                ; consume `typedef'
  (let ((ty (nelisp-cfront-parse--parse-type)))
    (if (nelisp-cfront-parse--at-fnptr-declarator)
        ;; typedef of a function pointer: typedef RET (*Name)(params);
        (let ((fp (nelisp-cfront-parse--parse-fnptr-name ty)))
          (nelisp-cfront-parse--eat-punct ";")
          (push (cons (car fp) (cdr fp)) nelisp-cfront-parse--typedefs)
          (list 'typedef (car fp) (cdr fp)))
      (let ((name (nelisp-cfront-parse--eat-ident)))
        (when (nelisp-cfront-parse--at-punct "(")  ; function-type typedef: RET NAME(params)
          (nelisp-cfront-parse--skip-paren-group)
          (setq ty (list :base (or (plist-get ty :base) 'long) :ptr 1 :fnptr t)))
        (when (nelisp-cfront-parse--at-punct "[")  ; array typedef: T NAME[N]
          (nelisp-cfront-parse--advance)
          (unless (nelisp-cfront-parse--at-punct "]") (nelisp-cfront-parse--parse-expr))
          (nelisp-cfront-parse--eat-punct "]")
          (setq ty (plist-put (copy-sequence ty) :ptr
                              (1+ (or (plist-get ty :ptr) 0)))))
        (nelisp-cfront-parse--eat-punct ";")
        ;; anonymous struct typedef: key the layout under the typedef name
        (when (and (eq (plist-get ty :base) 'struct)
                   (null (plist-get ty :struct))
                   (plist-get ty :fields))
          (setq ty (plist-put (copy-sequence ty) :struct name)))
        (push (cons name ty) nelisp-cfront-parse--typedefs)
        (list 'typedef name ty)))))

(defun nelisp-cfront-parse--parse-toplevel ()
  "Parse a typedef, struct definition, function definition, or global decl."
  (if (nelisp-cfront-parse--at-kw "typedef")
      (nelisp-cfront-parse--parse-typedef)
  (let ((ty (nelisp-cfront-parse--parse-type)))
    (cond
     ((nelisp-cfront-parse--at-punct ";")
      ;; bare type declaration with no declarator: `struct P { ... };'
      (nelisp-cfront-parse--advance)
      (list 'struct-def (plist-get ty :struct) (plist-get ty :fields)
            (plist-get ty :union)))
     ((nelisp-cfront-parse--at-fnptr-declarator)
      ;; function returning a fn-ptr (or a fn-ptr global): RET (*name(...))(...)
      (let ((fp (nelisp-cfront-parse--parse-fnptr-name ty)))
        (if (nelisp-cfront-parse--at-punct "{")
            (list 'func (cdr fp) (car fp) nil (nelisp-cfront-parse--parse-block))
          (when (nelisp-cfront-parse--at-punct "=")
            (nelisp-cfront-parse--advance) (nelisp-cfront-parse--parse-initializer))
          (nelisp-cfront-parse--eat-punct ";")
          (list 'proto (cdr fp) (car fp) nil))))
     (t
      (let ((name (nelisp-cfront-parse--eat-ident)))
        (if (nelisp-cfront-parse--at-punct "(")
            ;; function: params then body (or `;' prototype)
            (progn
              (nelisp-cfront-parse--advance)
              (let ((params (nelisp-cfront-parse--parse-params)))
                (nelisp-cfront-parse--eat-punct ")")
                (if (nelisp-cfront-parse--at-punct ";")
                    (progn (nelisp-cfront-parse--advance)
                           (list 'proto ty name params))
                  (list 'func ty name params (nelisp-cfront-parse--parse-block)))))
          ;; global variable (array dims + initializer + comma declarators)
          (let ((init nil) (gty ty))
            ;; Retain array dimensions on the type (Doc 06 Step B) so a global
            ;; array indexes/derefs correctly instead of defaulting to a scalar
            ;; `long' (which produced `:deref-non-pointer').  `gty' is a fresh
            ;; copy so the comma-declarator discard loop below still sees the
            ;; base type.
            (while (nelisp-cfront-parse--at-punct "[")
              (nelisp-cfront-parse--advance)
              (let ((sz (if (nelisp-cfront-parse--at-punct "]")
                            t
                          (nelisp-cfront-parse--fold-dim
                           (nelisp-cfront-parse--parse-expr)))))
                (nelisp-cfront-parse--eat-punct "]")
                (setq gty (append gty (list :array sz)))))
            (when (nelisp-cfront-parse--at-punct "=")
              (nelisp-cfront-parse--advance)
              (setq init (nelisp-cfront-parse--parse-initializer)))
            ;; additional comma-separated declarators (globals are deferred -> discard)
            (while (nelisp-cfront-parse--at-punct ",")
              (nelisp-cfront-parse--advance)
              (while (nelisp-cfront-parse--at-punct "*") (nelisp-cfront-parse--advance))
              (if (nelisp-cfront-parse--at-fnptr-declarator)
                  (nelisp-cfront-parse--parse-fnptr-name ty)
                (when (nelisp-cfront-parse--at 'ident) (nelisp-cfront-parse--advance)))
              (while (nelisp-cfront-parse--at-punct "[")
                (nelisp-cfront-parse--advance)
                (unless (nelisp-cfront-parse--at-punct "]") (nelisp-cfront-parse--parse-expr))
                (nelisp-cfront-parse--eat-punct "]"))
              (when (nelisp-cfront-parse--at-punct "=")
                (nelisp-cfront-parse--advance)
                (nelisp-cfront-parse--parse-initializer)))
            (nelisp-cfront-parse--eat-punct ";")
            (list 'global gty name init)))))))))

(defun nelisp-cfront-parse--parse-params ()
  (let ((params nil))
    (cond
     ;; () or (void)
     ((nelisp-cfront-parse--at-punct ")") nil)
     ((and (nelisp-cfront-parse--at-kw "void")
           (let ((next (cadr nelisp-cfront-parse--toks)))
             (and next (eq (nth 0 next) 'punct) (string= (nth 1 next) ")"))))
      (nelisp-cfront-parse--advance) nil)
     (t
      (push (nelisp-cfront-parse--parse-param) params)
      (while (nelisp-cfront-parse--at-punct ",")
        (nelisp-cfront-parse--advance)
        (push (nelisp-cfront-parse--parse-param) params))))
    (nreverse params)))

(defun nelisp-cfront-parse--parse-param ()
  (if (nelisp-cfront-parse--at-punct "...")       ; varargs ellipsis
      (progn (nelisp-cfront-parse--advance) (list 'vararg))
    (let ((ty (nelisp-cfront-parse--parse-type)))
      (if (nelisp-cfront-parse--at-fnptr-declarator)
          (let ((fp (nelisp-cfront-parse--parse-fnptr-name ty)))
            (list 'param (cdr fp) (car fp)))
        (let ((name (if (nelisp-cfront-parse--at 'ident)
                        (nth 1 (nelisp-cfront-parse--advance))
                      nil)))             ; unnamed param allowed
          ;; tolerate array params `T name[...]'
          (while (nelisp-cfront-parse--at-punct "[")
            (nelisp-cfront-parse--advance)
            (unless (nelisp-cfront-parse--at-punct "]") (nelisp-cfront-parse--parse-expr))
            (nelisp-cfront-parse--eat-punct "]"))
          (list 'param ty name))))))

(defun nelisp-cfront-parse (tokens-or-source)
  "Parse TOKENS-OR-SOURCE into an AST `(program TOPLEVEL...)'.
Accepts either a token list (from `nelisp-cfront-lex') or a C source
string (which is lexed first)."
  (let ((nelisp-cfront-parse--toks
         (nelisp-cfront-parse--strip-gcc
          (if (stringp tokens-or-source)
              (nelisp-cfront-lex tokens-or-source)
            tokens-or-source)))
        (nelisp-cfront-parse--typedefs
         (copy-sequence nelisp-cfront-parse--builtin-typedefs))
        (nelisp-cfront-parse--enum-consts nil)
        (tops nil))
    (while (not (nelisp-cfront-parse--at 'eof))
      (if (nelisp-cfront-parse--at-punct ";")   ; stray top-level semicolon
          (nelisp-cfront-parse--advance)
        (push (nelisp-cfront-parse--parse-toplevel) tops)))
    (cons 'program (nreverse tops))))

(provide 'nelisp-cfront-parse)

;;; nelisp-cfront-parse.el ends here
