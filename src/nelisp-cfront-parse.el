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

(defconst nelisp-cfront-parse--type-keywords
  '("void" "char" "short" "int" "long" "unsigned" "signed" "const"
    "struct" "union" "static" "extern")
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

(defun nelisp-cfront-parse--parse-type ()
  "Parse a type-specifier sequence + pointer stars into a type plist."
  ;; leading storage-class / qualifier keywords
  (while (and (nelisp-cfront-parse--at 'keyword)
              (member (nelisp-cfront-parse--pval)
                      '("const" "static" "extern" "register" "volatile" "inline")))
    (nelisp-cfront-parse--advance))
  (if (and (nelisp-cfront-parse--at 'ident)
           (nelisp-cfront-parse--typedef-lookup (nelisp-cfront-parse--pval)))
      ;; typedef name -> its resolved type, plus any extra pointer stars
      (let* ((bt (nelisp-cfront-parse--typedef-lookup
                  (nth 1 (nelisp-cfront-parse--advance))))
             (ptr (or (plist-get bt :ptr) 0)))
        (while (nelisp-cfront-parse--at-punct "*")
          (nelisp-cfront-parse--advance) (setq ptr (1+ ptr)))
        (plist-put (copy-sequence bt) :ptr ptr))
    (let ((specs nil) (struct-name nil) (struct-fields nil) (unsigned nil)
          (is-union nil))
      (while (and (nelisp-cfront-parse--at 'keyword)
                  (member (nelisp-cfront-parse--pval) nelisp-cfront-parse--type-keywords))
      (let ((w (nth 1 (nelisp-cfront-parse--advance))))
        (cond
         ((member w '("const" "static" "extern")) nil) ; ignore qualifiers/storage
         ((string= w "unsigned") (setq unsigned t))
         ((string= w "signed") nil)
         ((or (string= w "struct") (string= w "union"))
          (when (string= w "union") (setq is-union t))
          (when (nelisp-cfront-parse--at 'ident)         ; tag is optional (anonymous)
            (setq struct-name (nth 1 (nelisp-cfront-parse--advance))))
          (when (nelisp-cfront-parse--at-punct "{")
            (setq struct-fields (nelisp-cfront-parse--parse-struct-body))))
         (t (push w specs)))))
    (let* ((specs (nreverse specs))
           (base (cond
                  ((or struct-name struct-fields) 'struct)  ; tagged or anonymous
                  ((member "long" specs) 'long)
                  ((member "short" specs) 'short)
                  ((member "char" specs) 'char)
                  ((member "int" specs) 'int)
                  ((member "void" specs) 'void)
                  (unsigned 'int)              ; "unsigned" alone => unsigned int
                  (t (signal 'nelisp-cfront-parse-error
                             (list :not-a-type (nelisp-cfront-parse--peek))))))
           (ptr 0))
      (while (nelisp-cfront-parse--at-punct "*")
        (nelisp-cfront-parse--advance)
        (setq ptr (1+ ptr)))
      (append (list :base base :ptr ptr)
              (when unsigned '(:unsigned t))
              (when struct-name (list :struct struct-name))
              (when struct-fields (list :fields struct-fields))
              (when is-union '(:union t)))))))

(defun nelisp-cfront-parse--parse-struct-body ()
  "Parse `{ TYPE NAME; ... }' into a list of (field TYPE NAME)."
  (nelisp-cfront-parse--eat-punct "{")
  (let ((fields nil))
    (while (not (nelisp-cfront-parse--at-punct "}"))
      (let* ((fty (nelisp-cfront-parse--parse-type))
             (fname (nelisp-cfront-parse--eat-ident)))
        ;; optional array field: TYPE NAME[N]
        (when (nelisp-cfront-parse--at-punct "[")
          (nelisp-cfront-parse--advance)
          (let ((sz (nelisp-cfront-parse--parse-expr)))
            (nelisp-cfront-parse--eat-punct "]")
            (setq fty (append fty (list :array sz)))))
        (nelisp-cfront-parse--eat-punct ";")
        (push (list 'field fty fname) fields)))
    (nelisp-cfront-parse--eat-punct "}")
    (nreverse fields)))

(defun nelisp-cfront-parse--at-fnptr-declarator ()
  "Non-nil when positioned at a function-pointer declarator `(* ...'."
  (and (nelisp-cfront-parse--at-punct "(")
       (let ((n (cadr nelisp-cfront-parse--toks)))
         (and n (eq (nth 0 n) 'punct) (string= (nth 1 n) "*")))))

(defun nelisp-cfront-parse--parse-fnptr-name (ret-ty)
  "Consume a fn-ptr declarator `(* NAME)(params)' at point.
Return (cons NAME pointer-type).  The parameter list is skipped (the call
lowering is type-agnostic); the type is a plain pointer marked :fnptr."
  (nelisp-cfront-parse--eat-punct "(")
  (nelisp-cfront-parse--eat-punct "*")
  (let ((name (nelisp-cfront-parse--eat-ident)))
    (nelisp-cfront-parse--eat-punct ")")
    (nelisp-cfront-parse--eat-punct "(")           ; skip the (param-type-list)
    (let ((depth 1))
      (while (> depth 0)
        (let ((tk (nelisp-cfront-parse--advance)))
          (cond
           ((and (eq (nth 0 tk) 'punct) (string= (nth 1 tk) "(")) (setq depth (1+ depth)))
           ((and (eq (nth 0 tk) 'punct) (string= (nth 1 tk) ")")) (setq depth (1- depth)))
           ((eq (nth 0 tk) 'eof)
            (signal 'nelisp-cfront-parse-error (list :unterminated-fnptr-params)))))))
    (cons name (list :base (or (plist-get ret-ty :base) 'long) :ptr 1 :fnptr t))))

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
  (nelisp-cfront-parse--parse-assign))

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
          (let ((a (nelisp-cfront-parse--parse-assign)))
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
     ((and (eq (nth 0 tk) 'punct) (member (nth 1 tk) '("++" "--")))
      (nelisp-cfront-parse--advance)
      (list 'pre (nth 1 tk) (nelisp-cfront-parse--parse-unary)))
     ((and (eq (nth 0 tk) 'punct) (member (nth 1 tk) nelisp-cfront-parse--unary-ops))
      (nelisp-cfront-parse--advance)
      (list 'unop (nth 1 tk) (nelisp-cfront-parse--parse-unary)))
     ((and (eq (nth 0 tk) 'keyword) (string= (nth 1 tk) "sizeof"))
      (nelisp-cfront-parse--advance)
      ;; sizeof(type) or sizeof expr — MVP: sizeof(type) only
      (nelisp-cfront-parse--eat-punct "(")
      (let ((ty (nelisp-cfront-parse--parse-type)))
        (nelisp-cfront-parse--eat-punct ")")
        (list 'sizeof ty)))
     (t (nelisp-cfront-parse--parse-postfix)))))

(defun nelisp-cfront-parse--parse-postfix ()
  (let ((e (nelisp-cfront-parse--parse-primary)))
    (catch 'done
      (while t
        (cond
         ((nelisp-cfront-parse--at-punct "(")
          (nelisp-cfront-parse--advance)
          (let ((args nil))
            (unless (nelisp-cfront-parse--at-punct ")")
              (push (nelisp-cfront-parse--parse-assign) args)
              (while (nelisp-cfront-parse--at-punct ",")
                (nelisp-cfront-parse--advance)
                (push (nelisp-cfront-parse--parse-assign) args)))
            (nelisp-cfront-parse--eat-punct ")")
            (setq e (list 'call e (nreverse args)))))
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
      ('char (nelisp-cfront-parse--advance) (list 'int (nth 1 tk)))
      ('string (nelisp-cfront-parse--advance) (list 'str (nth 1 tk)))
      ('ident (nelisp-cfront-parse--advance) (list 'var (nth 1 tk)))
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
   ((nelisp-cfront-parse--at-punct "{") (nelisp-cfront-parse--parse-block))
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
   ((nelisp-cfront-parse--at-kw "break")
    (nelisp-cfront-parse--advance) (nelisp-cfront-parse--eat-punct ";") (list 'break))
   ((nelisp-cfront-parse--at-kw "continue")
    (nelisp-cfront-parse--advance) (nelisp-cfront-parse--eat-punct ";") (list 'continue))
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
  "Parse a local declaration `TYPE NAME [= INIT]' (no trailing `;')."
  (let ((ty (nelisp-cfront-parse--parse-type)) (name nil) (init nil))
    (if (nelisp-cfront-parse--at-fnptr-declarator)
        (let ((fp (nelisp-cfront-parse--parse-fnptr-name ty)))
          (setq ty (cdr fp) name (car fp)))
      (setq name (nelisp-cfront-parse--eat-ident))
      (when (nelisp-cfront-parse--at-punct "[")    ; array decl: TYPE NAME[SIZE]
        (nelisp-cfront-parse--advance)
        (let ((sz (nelisp-cfront-parse--parse-expr)))
          (nelisp-cfront-parse--eat-punct "]")
          (setq ty (append (list :base (plist-get ty :base)
                                 :ptr (plist-get ty :ptr)
                                 :array sz)
                           (when (plist-get ty :unsigned) '(:unsigned t))
                           (when (plist-get ty :struct) (list :struct (plist-get ty :struct))))))))
    (when (nelisp-cfront-parse--at-punct "=")
      (nelisp-cfront-parse--advance)
      (setq init (nelisp-cfront-parse--parse-assign)))
    (list 'decl ty name init)))

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
    (if (nelisp-cfront-parse--at-punct ";")
        ;; bare type declaration with no declarator: `struct P { ... };'
        (progn
          (nelisp-cfront-parse--advance)
          (list 'struct-def (plist-get ty :struct) (plist-get ty :fields)
                (plist-get ty :union)))
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
          ;; global variable
          (let ((init nil))
            (when (nelisp-cfront-parse--at-punct "=")
              (nelisp-cfront-parse--advance)
              (setq init (nelisp-cfront-parse--parse-assign)))
            (nelisp-cfront-parse--eat-punct ";")
            (list 'global ty name init))))))))

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
  (let ((ty (nelisp-cfront-parse--parse-type)))
    (if (nelisp-cfront-parse--at-fnptr-declarator)
        (let ((fp (nelisp-cfront-parse--parse-fnptr-name ty)))
          (list 'param (cdr fp) (car fp)))
      (let ((name (if (nelisp-cfront-parse--at 'ident)
                      (nth 1 (nelisp-cfront-parse--advance))
                    nil)))               ; unnamed param allowed
        (list 'param ty name)))))

(defun nelisp-cfront-parse (tokens-or-source)
  "Parse TOKENS-OR-SOURCE into an AST `(program TOPLEVEL...)'.
Accepts either a token list (from `nelisp-cfront-lex') or a C source
string (which is lexed first)."
  (let ((nelisp-cfront-parse--toks
         (if (stringp tokens-or-source)
             (nelisp-cfront-lex tokens-or-source)
           tokens-or-source))
        (nelisp-cfront-parse--typedefs nil)
        (tops nil))
    (while (not (nelisp-cfront-parse--at 'eof))
      (push (nelisp-cfront-parse--parse-toplevel) tops))
    (cons 'program (nreverse tops))))

(provide 'nelisp-cfront-parse)

;;; nelisp-cfront-parse.el ends here
