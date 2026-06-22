;;; nelisp-cfront-parse-test.el --- ERT for the C parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT for M2.2 nelisp-cfront-parse.  Asserts AST shapes for the subset.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-parse)

(defun nelisp-cfront-parse-test--one (src)
  "Parse SRC and return its single top-level node."
  (cadr (nelisp-cfront-parse src)))

(defun nelisp-cfront-parse-test--expr (src)
  "Parse expression SRC (wrapped as a return) and return the expr AST."
  (let* ((fn (nelisp-cfront-parse-test--one (format "int f(void){ return %s; }" src)))
         (body (nth 4 fn))              ; (block (return EXPR))
         (ret (cadr body)))
    (nth 1 ret)))

(ert-deftest nelisp-cfront-parse-simple-func ()
  (should (equal '(func (:base int :ptr 0) "main" nil (block (return (int 0))))
                 (nelisp-cfront-parse-test--one "int main(void){ return 0; }"))))

(ert-deftest nelisp-cfront-parse-params-and-ptr ()
  (let ((fn (nelisp-cfront-parse-test--one "long add(long a, char *p){ return a; }")))
    (should (equal '(param (:base long :ptr 0) "a") (nth 0 (nth 3 fn))))
    (should (equal '(param (:base char :ptr 1) "p") (nth 1 (nth 3 fn))))))

(ert-deftest nelisp-cfront-parse-precedence ()
  ;; a + b * c  ==  a + (b*c)
  (should (equal '(binop "+" (var "a") (binop "*" (var "b") (var "c")))
                 (nelisp-cfront-parse-test--expr "a + b * c"))))

(ert-deftest nelisp-cfront-parse-precedence-cmp-add ()
  ;; a < b + c  ==  a < (b+c)
  (should (equal '(binop "<" (var "a") (binop "+" (var "b") (var "c")))
                 (nelisp-cfront-parse-test--expr "a < b + c"))))

(ert-deftest nelisp-cfront-parse-assign-right-assoc ()
  (should (equal '(assign "=" (var "a") (assign "=" (var "b") (int 1)))
                 (nelisp-cfront-parse-test--expr "a = b = 1"))))

(ert-deftest nelisp-cfront-parse-call-args ()
  (should (equal '(call (var "f") ((int 1) (binop "+" (var "x") (int 2))))
                 (nelisp-cfront-parse-test--expr "f(1, x + 2)"))))

(ert-deftest nelisp-cfront-parse-member-arrow-index ()
  (should (equal '(arrow (index (var "a") (int 3)) "x")
                 (nelisp-cfront-parse-test--expr "a[3]->x"))))

(ert-deftest nelisp-cfront-parse-unary-deref ()
  (should (equal '(unop "*" (var "p"))
                 (nelisp-cfront-parse-test--expr "*p"))))

(ert-deftest nelisp-cfront-parse-if-else ()
  (let* ((fn (nelisp-cfront-parse-test--one
              "int f(int n){ if (n) return 1; else return 0; }"))
         (body (nth 4 fn))
         (ifst (cadr body)))
    (should (eq 'if (nth 0 ifst)))
    (should (equal '(var "n") (nth 1 ifst)))
    (should (equal '(return (int 1)) (nth 2 ifst)))
    (should (equal '(return (int 0)) (nth 3 ifst)))))

(ert-deftest nelisp-cfront-parse-for-loop ()
  (let* ((fn (nelisp-cfront-parse-test--one
              "int f(int n){ int s = 0; for (int i = 0; i < n; i++) s += i; return s; }"))
         (body (nth 4 fn))
         (forst (nth 2 body)))           ; (decl ...) then (for ...)
    (should (eq 'for (nth 0 forst)))
    (should (equal '(decl (:base int :ptr 0) "i" (int 0)) (nth 1 forst)))
    (should (equal '(binop "<" (var "i") (var "n")) (nth 2 forst)))
    (should (equal '(post "++" (var "i")) (nth 3 forst)))))

(ert-deftest nelisp-cfront-parse-local-decl-init ()
  (let* ((fn (nelisp-cfront-parse-test--one "int f(void){ long x = 5; return x; }"))
         (body (nth 4 fn)))
    (should (equal '(decl (:base long :ptr 0) "x" (int 5)) (cadr body)))))

(ert-deftest nelisp-cfront-parse-while-block ()
  (let* ((fn (nelisp-cfront-parse-test--one
              "void f(int n){ while (n) { n = n - 1; } }"))
         (body (nth 4 fn))
         (w (cadr body)))
    (should (eq 'while (nth 0 w)))
    (should (equal '(var "n") (nth 1 w)))
    (should (eq 'block (nth 0 (nth 2 w))))))

(ert-deftest nelisp-cfront-parse-struct-and-global ()
  (let ((g (nelisp-cfront-parse-test--one "struct P *gp = 0;")))
    (should (equal '(global (:base struct :ptr 1 :struct "P") "gp" (int 0)) g))))

(ert-deftest nelisp-cfront-parse-multiple-toplevel ()
  (let ((prog (nelisp-cfront-parse "int a; int f(void){ return a; }")))
    (should (= 3 (length prog)))         ; (program GLOBAL FUNC)
    (should (eq 'global (nth 0 (nth 1 prog))))
    (should (eq 'func (nth 0 (nth 2 prog))))))

(ert-deftest nelisp-cfront-parse-error-on-garbage ()
  (should-error (nelisp-cfront-parse "int f(void){ return @; }")
                :type 'nelisp-cfront-lex-error))

(provide 'nelisp-cfront-parse-test)

;;; nelisp-cfront-parse-test.el ends here
