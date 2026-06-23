;;; nelisp-cfront-enum-test.el --- enum constant registration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 lowering — enum constants.  The parser parses an enum body (rather
;; than skipping it), registers each NAME -> value with C auto-increment
;; and constant-expression `= EXPR' (incl. <<, |, prior-constant refs),
;; and `parse-primary' folds a reference to a constant into `(int N)' so
;; lowering needs no special case.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-cc)

;; --- parse-level: constants fold to their integer values --------------

(defun nelisp-cfront-enum-test--fn-body (csrc fname)
  "Parse CSRC and return the `func' node named FNAME (a string)."
  (let ((prog (nelisp-cfront-parse csrc)))
    (cl-find-if (lambda (top) (and (eq (car top) 'func)
                                   (equal (nth 2 top) fname)))
                (cdr prog))))

(ert-deftest nelisp-cfront-enum-folds-auto-increment ()
  "Bare enum auto-increments from 0; references fold to `(int N)'."
  (let* ((f (nelisp-cfront-enum-test--fn-body
             "enum { RED, GREEN, BLUE };
              int g(void){ return GREEN; }"
             "g"))
         ;; body = (block (return (int 1)))
         (ret (car (last (cdr (nth 4 f))))))
    (should (equal ret '(return (int 1))))))

(ert-deftest nelisp-cfront-enum-folds-explicit-and-exprs ()
  "Explicit `= EXPR' (incl. <<, |, prior refs) and auto-increment fold."
  (let* ((f (nelisp-cfront-enum-test--fn-body
             "enum F { F1 = 1, F2 = 1<<1, F3 = 1<<2, FALL = F1|F2|F3 };
              int g(void){ return FALL; }"
             "g"))
         (ret (car (last (cdr (nth 4 f))))))
    (should (equal ret '(return (int 7))))))      ; 1|2|4

;; --- end-to-end: enum constants compile + run -------------------------

(defun nelisp-cfront-enum-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-enum-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-enum" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "enum e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-enum-e2e ()
  "Enum constants (auto-increment, explicit, <<, |, cross-ref) native."
  (unless (nelisp-cfront-enum-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-enum-test--run "
enum { RED, GREEN, BLUE };
enum Color { C_A = 10, C_B, C_C = 100, C_D };
enum Flags { F1 = 1, F2 = 1<<1, F3 = 1<<2, FALL = F1|F2|F3 };
int pick(int x){ if (x == GREEN) return BLUE; return C_D; }
int vals(void){ return RED + GREEN*10 + BLUE*100; }
int colors(void){ return C_A + C_B + C_C + C_D; }
int flags(void){ return FALL; }
" "
#include <stdio.h>
extern int pick(int); extern int vals(void);
extern int colors(void); extern int flags(void);
int main(void){
  int p1=pick(1), p2=pick(5), v=vals(), c=colors(), f=flags();
  printf(\"%d %d %d %d %d\\n\", p1, p2, v, c, f);
  return (p1==2 && p2==101 && v==210 && c==222 && f==7)?0:1;
}
")))
    (should (equal "2 101 210 222 7" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-enum-test)

;;; nelisp-cfront-enum-test.el ends here
