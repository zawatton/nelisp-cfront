;;; nelisp-cfront-lex-test.el --- ERT for the C lexer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT for M2.1 nelisp-cfront-lex.  Asserts token (TYPE VALUE) pairs
;; (position dropped for brevity via the helper).

;;; Code:

(require 'ert)
(require 'nelisp-cfront-lex)

(defun nelisp-cfront-lex-test--tv (src)
  "Lex SRC and return tokens as (TYPE VALUE) pairs (drop position + eof)."
  (let ((toks (nelisp-cfront-lex src)))
    (mapcar (lambda (tk) (list (nth 0 tk) (nth 1 tk)))
            (butlast toks))))           ; drop trailing eof

(ert-deftest nelisp-cfront-lex-idents-keywords ()
  (should (equal '((keyword "int") (ident "main") (punct "(") (keyword "void") (punct ")"))
                 (nelisp-cfront-lex-test--tv "int main(void)"))))

(ert-deftest nelisp-cfront-lex-int-decimal ()
  (should (equal '((int 0) (int 42) (int 1000000))
                 (nelisp-cfront-lex-test--tv "0 42 1000000"))))

(ert-deftest nelisp-cfront-lex-int-hex-octal ()
  (should (equal '((int 255) (int 305419896) (int 8))
                 (nelisp-cfront-lex-test--tv "0xff 0x12345678 010"))))

(ert-deftest nelisp-cfront-lex-int-suffixes ()
  (should (equal '((int 5) (int 7) (int 9))
                 (nelisp-cfront-lex-test--tv "5u 7L 9ul"))))

(ert-deftest nelisp-cfront-lex-char-literal ()
  (should (equal '((char 65) (char 10) (char 0) (char 92))
                 (nelisp-cfront-lex-test--tv "'A' '\\n' '\\0' '\\\\'"))))

(ert-deftest nelisp-cfront-lex-string-literal ()
  (should (equal '((string "hi\n"))
                 (nelisp-cfront-lex-test--tv "\"hi\\n\""))))

(ert-deftest nelisp-cfront-lex-puncts-longest-match ()
  (should (equal '((punct "->") (punct "==") (punct "<<=") (punct "++") (punct "<<"))
                 (nelisp-cfront-lex-test--tv "-> == <<= ++ <<"))))

(ert-deftest nelisp-cfront-lex-comments-skipped ()
  (should (equal '((keyword "int") (ident "x"))
                 (nelisp-cfront-lex-test--tv "int /* c */ x // trailing\n"))))

(ert-deftest nelisp-cfront-lex-full-function ()
  (should (equal '((keyword "long") (ident "sum") (punct "(") (keyword "long")
                   (ident "n") (punct ")") (punct "{") (keyword "return")
                   (ident "n") (punct "+") (int 1) (punct ";") (punct "}"))
                 (nelisp-cfront-lex-test--tv "long sum(long n){ return n + 1; }"))))

(ert-deftest nelisp-cfront-lex-eof-terminator ()
  (let ((toks (nelisp-cfront-lex "")))
    (should (equal '(eof nil) (list (nth 0 (car toks)) (nth 1 (car toks)))))))

(ert-deftest nelisp-cfront-lex-unterminated-string-signals ()
  (should-error (nelisp-cfront-lex "\"abc") :type 'nelisp-cfront-lex-error))

(ert-deftest nelisp-cfront-lex-unterminated-comment-signals ()
  (should-error (nelisp-cfront-lex "/* abc") :type 'nelisp-cfront-lex-error))

(provide 'nelisp-cfront-lex-test)

;;; nelisp-cfront-lex-test.el ends here
