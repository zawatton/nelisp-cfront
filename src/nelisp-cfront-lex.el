;;; nelisp-cfront-lex.el --- C lexer/tokenizer for nelisp-cfront -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M2.1 — the C front-end's tokenizer.  Turns C source text into a list
;; of tokens for the M2.2 parser.
;;
;; A token is a list (TYPE VALUE POS):
;;   ident   VALUE = symbol-name string         (identifier)
;;   keyword VALUE = the keyword string         (reserved word)
;;   int     VALUE = integer                    (numeric literal value)
;;   char    VALUE = integer                    (char literal code point)
;;   string  VALUE = string (decoded)           (string literal)
;;   punct   VALUE = the operator/punctuator    (e.g. "->", "==", "{")
;;   eof     VALUE = nil
;; POS is the 0-based source offset where the token starts (for errors).
;;
;; Scope (the M2 subset): the lexer covers the lexical grammar a useful C
;; subset needs.  Preprocessor directives (#...) are NOT handled here —
;; the M2 driver runs the source through `cpp' first (or rejects #), so
;; this lexer sees already-preprocessed C.  Wide/u8 string prefixes,
;; floating-point literals, and digraphs are deferred (Doc 03 backlog).

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-cfront-lex-error "nelisp-cfront lexer error")

(defconst nelisp-cfront-lex--keywords
  '("auto" "break" "case" "char" "const" "continue" "default" "do"
    "double" "else" "enum" "extern" "float" "for" "goto" "if" "inline"
    "int" "long" "register" "restrict" "return" "short" "signed"
    "sizeof" "static" "struct" "switch" "typedef" "union" "unsigned"
    "void" "volatile" "while" "_Bool")
  "C keywords recognised by the lexer (C99 subset).")

(defconst nelisp-cfront-lex--puncts-3 '("<<=" ">>=" "...")
  "Three-character punctuators (checked before 2- and 1-char).")

(defconst nelisp-cfront-lex--puncts-2
  '("->" "++" "--" "<<" ">>" "<=" ">=" "==" "!=" "&&" "||"
    "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=")
  "Two-character punctuators.")

(defconst nelisp-cfront-lex--puncts-1
  (mapcar #'char-to-string (string-to-list "+-*/%=<>!&|^~(){}[];,.?:"))
  "Single-character punctuators.")

(defsubst nelisp-cfront-lex--id-start-p (c)
  (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z)) (= c ?_)))

(defsubst nelisp-cfront-lex--id-cont-p (c)
  (or (nelisp-cfront-lex--id-start-p c) (and (>= c ?0) (<= c ?9))))

(defsubst nelisp-cfront-lex--digit-p (c) (and (>= c ?0) (<= c ?9)))

(defsubst nelisp-cfront-lex--hex-digit-p (c)
  (or (nelisp-cfront-lex--digit-p c)
      (and (>= c ?a) (<= c ?f)) (and (>= c ?A) (<= c ?F))))

(defun nelisp-cfront-lex--hex-val (c)
  (cond ((nelisp-cfront-lex--digit-p c) (- c ?0))
        ((and (>= c ?a) (<= c ?f)) (+ 10 (- c ?a)))
        (t (+ 10 (- c ?A)))))

(defun nelisp-cfront-lex--skip-ws-comments (s i n)
  "Return the index after whitespace + comments starting at I in S (len N)."
  (let ((done nil))
    (while (and (not done) (< i n))
      (let ((c (aref s i)))
        (cond
         ((memq c '(?\s ?\t ?\n ?\r ?\f ?\v)) (setq i (1+ i)))
         ;; // line comment
         ((and (= c ?/) (< (1+ i) n) (= (aref s (1+ i)) ?/))
          (setq i (+ i 2))
          (while (and (< i n) (/= (aref s i) ?\n)) (setq i (1+ i))))
         ;; /* block comment */
         ((and (= c ?/) (< (1+ i) n) (= (aref s (1+ i)) ?*))
          (setq i (+ i 2))
          (let ((closed nil))
            (while (and (not closed) (< i n))
              (if (and (= (aref s i) ?*) (< (1+ i) n) (= (aref s (1+ i)) ?/))
                  (progn (setq i (+ i 2) closed t))
                (setq i (1+ i))))
            (unless closed
              (signal 'nelisp-cfront-lex-error (list :unterminated-block-comment i)))))
         (t (setq done t)))))
    i))

(defun nelisp-cfront-lex--escape (s i n)
  "Decode one escape after a backslash at I (S[I-1] = backslash); len N.
Return (cons CODEPOINT NEXT-INDEX)."
  (when (>= i n)
    (signal 'nelisp-cfront-lex-error (list :unterminated-escape i)))
  (let ((c (aref s i)))
    (pcase c
      (?n (cons ?\n (1+ i)))
      (?t (cons ?\t (1+ i)))
      (?r (cons ?\r (1+ i)))
      (?0 (cons 0 (1+ i)))
      (?\\ (cons ?\\ (1+ i)))
      (?' (cons ?' (1+ i)))
      (?\" (cons ?\" (1+ i)))
      (?a (cons 7 (1+ i)))
      (?b (cons 8 (1+ i)))
      (?f (cons 12 (1+ i)))
      (?v (cons 11 (1+ i)))
      (?x ;; \xHH...
       (let ((j (1+ i)) (val 0) (any nil))
         (while (and (< j n) (nelisp-cfront-lex--hex-digit-p (aref s j)))
           (setq val (+ (* val 16) (nelisp-cfront-lex--hex-val (aref s j))) j (1+ j) any t))
         (unless any (signal 'nelisp-cfront-lex-error (list :bad-hex-escape i)))
         (cons (logand val #xff) j)))
      (_ (cons c (1+ i))))))            ; unknown escape: take the char as-is

(defun nelisp-cfront-lex--number (s i n)
  "Lex an integer literal starting at I (S[I] is a digit); len N.
Return (cons VALUE NEXT-INDEX).  Consumes and ignores u/l/U/L suffixes."
  (let ((val 0) (start i))
    (cond
     ;; hex 0x...
     ((and (= (aref s i) ?0) (< (1+ i) n) (memq (aref s (1+ i)) '(?x ?X)))
      (setq i (+ i 2))
      (let ((any nil))
        (while (and (< i n) (nelisp-cfront-lex--hex-digit-p (aref s i)))
          (setq val (+ (* val 16) (nelisp-cfront-lex--hex-val (aref s i))) i (1+ i) any t))
        (unless any (signal 'nelisp-cfront-lex-error (list :bad-hex-literal start)))))
     ;; octal 0...
     ((and (= (aref s i) ?0) (< (1+ i) n) (nelisp-cfront-lex--digit-p (aref s (1+ i))))
      (setq i (1+ i))
      (while (and (< i n) (>= (aref s i) ?0) (<= (aref s i) ?7))
        (setq val (+ (* val 8) (- (aref s i) ?0)) i (1+ i))))
     ;; decimal (covers lone 0)
     (t
      (while (and (< i n) (nelisp-cfront-lex--digit-p (aref s i)))
        (setq val (+ (* val 10) (- (aref s i) ?0)) i (1+ i)))))
    ;; integer suffixes u/U/l/L (any order/combo)
    (while (and (< i n) (memq (aref s i) '(?u ?U ?l ?L)))
      (setq i (1+ i)))
    (cons val i)))

(defun nelisp-cfront-lex--punct (s i n)
  "Lex a punctuator at I; len N.  Return (cons OP-STRING NEXT-INDEX) or nil."
  (let* ((c3 (and (<= (+ i 3) n) (substring s i (+ i 3))))
         (c2 (and (<= (+ i 2) n) (substring s i (+ i 2))))
         (c1 (substring s i (1+ i))))
    (cond
     ((and c3 (member c3 nelisp-cfront-lex--puncts-3)) (cons c3 (+ i 3)))
     ((and c2 (member c2 nelisp-cfront-lex--puncts-2)) (cons c2 (+ i 2)))
     ((member c1 nelisp-cfront-lex--puncts-1) (cons c1 (1+ i)))
     (t nil))))

(defun nelisp-cfront-lex (source)
  "Tokenize C SOURCE (a string) into a list of (TYPE VALUE POS) tokens.
The final token is (eof nil POS).  Signals `nelisp-cfront-lex-error'."
  (let* ((s source) (n (length s)) (i 0) (toks nil))
    (while (progn (setq i (nelisp-cfront-lex--skip-ws-comments s i n))
                  (< i n))
      (let ((c (aref s i)) (pos i))
        (cond
         ;; identifier / keyword
         ((nelisp-cfront-lex--id-start-p c)
          (let ((j (1+ i)))
            (while (and (< j n) (nelisp-cfront-lex--id-cont-p (aref s j))) (setq j (1+ j)))
            (let ((word (substring s i j)))
              (push (list (if (member word nelisp-cfront-lex--keywords) 'keyword 'ident)
                          word pos)
                    toks)
              (setq i j))))
         ;; number (integer or floating-point)
         ((nelisp-cfront-lex--digit-p c)
          (if (and (= c ?0) (< (1+ i) n) (memq (aref s (1+ i)) '(?x ?X)))
              (let ((r (nelisp-cfront-lex--number s i n)))  ; hex integer
                (push (list 'int (car r) pos) toks) (setq i (cdr r)))
            ;; decimal: scan leading digits, then decide int vs float
            (let ((j i))
              (while (and (< j n) (nelisp-cfront-lex--digit-p (aref s j))) (setq j (1+ j)))
              (if (or (and (< j n) (= (aref s j) ?.))
                      (and (< j n) (memq (aref s j) '(?e ?E))))
                  ;; floating-point literal
                  (let ((start i))
                    (setq i j)
                    (when (and (< i n) (= (aref s i) ?.))
                      (setq i (1+ i))
                      (while (and (< i n) (nelisp-cfront-lex--digit-p (aref s i))) (setq i (1+ i))))
                    (when (and (< i n) (memq (aref s i) '(?e ?E)))
                      (setq i (1+ i))
                      (when (and (< i n) (memq (aref s i) '(?+ ?-))) (setq i (1+ i)))
                      (while (and (< i n) (nelisp-cfront-lex--digit-p (aref s i))) (setq i (1+ i))))
                    (let ((lex (substring s start i)))
                      (while (and (< i n) (memq (aref s i) '(?f ?F ?l ?L))) (setq i (1+ i)))
                      (push (list 'float (string-to-number lex) pos) toks)))
                ;; integer (decimal / octal) via the integer scanner
                (let ((r (nelisp-cfront-lex--number s i n)))
                  (push (list 'int (car r) pos) toks) (setq i (cdr r)))))))
         ;; char literal
         ((= c ?')
          (setq i (1+ i))
          (when (>= i n) (signal 'nelisp-cfront-lex-error (list :unterminated-char pos)))
          (let (code)
            (if (= (aref s i) ?\\)
                (let ((r (nelisp-cfront-lex--escape s (1+ i) n)))
                  (setq code (car r) i (cdr r)))
              (setq code (aref s i) i (1+ i)))
            (unless (and (< i n) (= (aref s i) ?'))
              (signal 'nelisp-cfront-lex-error (list :unterminated-char pos)))
            (setq i (1+ i))
            (push (list 'char code pos) toks)))
         ;; string literal
         ((= c ?\")
          (setq i (1+ i))
          (let ((chars nil) (closed nil))
            (while (and (not closed) (< i n))
              (let ((d (aref s i)))
                (cond
                 ((= d ?\") (setq closed t i (1+ i)))
                 ((= d ?\\)
                  (let ((r (nelisp-cfront-lex--escape s (1+ i) n)))
                    (push (car r) chars) (setq i (cdr r))))
                 (t (push d chars) (setq i (1+ i))))))
            (unless closed (signal 'nelisp-cfront-lex-error (list :unterminated-string pos)))
            (push (list 'string (concat (nreverse chars)) pos) toks)))
         ;; punctuator
         (t
          (let ((r (nelisp-cfront-lex--punct s i n)))
            (unless r (signal 'nelisp-cfront-lex-error (list :unexpected-char c pos)))
            (push (list 'punct (car r) pos) toks)
            (setq i (cdr r)))))))
    (push (list 'eof nil i) toks)
    (nreverse toks)))

(provide 'nelisp-cfront-lex)

;;; nelisp-cfront-lex.el ends here
