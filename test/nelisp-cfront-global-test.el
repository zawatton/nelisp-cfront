;;; nelisp-cfront-global-test.el --- read-only integer globals -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 / Doc 06 Step B — read-only integer global scalars and arrays.
;; cfront now retains a global array's dimensions on its type (so `G[i]'
;; types/indexes instead of signalling `:deref-non-pointer'), collects
;; const-initialized integer globals into `--globals', emits each as a
;; `data-blob' rodata symbol, and lowers a global reference through
;; `(data-addr NAME)' (scalar load / array address decay).

;;; Code:

(require 'ert)
(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-type)
(require 'nelisp-cfront-lower)
(require 'nelisp-cfront-cc)

;;; --- unit: collection + byte packing + lowering shape ---------------

(ert-deftest nelisp-cfront-global-collect-bytes ()
  "Const integer globals collect with correct little-endian rodata bytes
and array dimensions are retained on the type."
  (let* ((ast (nelisp-cfront-parse "
const unsigned char tbl[4] = {10, 20, 250, 99};
const int words[3] = {1, -1, 256};
const int answer = 42;
struct P { int x; } gp;            /* non-const struct global: skipped */
"))
         (nelisp-cfront-lower--structs (nelisp-cfront-type-build-structs ast))
         (g (nelisp-cfront-lower--collect-globals ast)))
    ;; struct global `gp' is not a const integer -> not collected.
    (should (equal '("tbl" "words" "answer") (mapcar #'car g)))
    ;; array dimension retained on the type.
    (should (equal 4 (plist-get (plist-get (cdr (assoc "tbl" g)) :type) :array)))
    (should (equal (unibyte-string 10 20 250 99)
                   (plist-get (cdr (assoc "tbl" g)) :bytes)))
    ;; int words: 1, -1 (=0xffffffff), 256 -> 12 LE bytes.
    (should (equal (unibyte-string 1 0 0 0  255 255 255 255  0 1 0 0)
                   (plist-get (cdr (assoc "words" g)) :bytes)))
    (should (equal (unibyte-string 42 0 0 0)
                   (plist-get (cdr (assoc "answer" g)) :bytes)))))

(ert-deftest nelisp-cfront-global-lowers-via-data-addr ()
  "A global array index lowers to a `data-addr'-based load and the program
emits a `data-blob' for the global; no `:deref-non-pointer'."
  (let* ((ast (nelisp-cfront-parse "
const unsigned char tbl[3] = {7, 8, 9};
int getb(int i){ return tbl[i]; }
"))
         (prog (nelisp-cfront-lower-program ast))
         (flat (format "%S" prog)))
    (should (string-match-p "(data-blob tbl " flat))
    (should (string-match-p "(data-addr tbl)" flat))))

;;; --- end-to-end: compile -> link -> run -----------------------------

(defun nelisp-cfront-global-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-global-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-global" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "global e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-global-readonly-arrays-e2e ()
  "Read-only integer global scalars/arrays read back correctly: unsigned
char (zero-extend), signed char (sign-extend), int array, int scalar, and
a loop summing a global array."
  (unless (nelisp-cfront-global-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-global-test--run "
const unsigned char tbl[8] = {10, 20, 30, 40, 250, 99, 7, 200};
signed char sgn[3] = {-1, -2, -128};
const int squares[6] = {0, 1, 4, 9, 16, 25};
const int answer = 42;
int getb(int i){ return tbl[i]; }
int getsgn(int i){ return sgn[i]; }
int getsq(int i){ return squares[i]; }
int getans(void){ return answer; }
int sumrow(void){ int s = 0; for (int i = 0; i < 6; i++) s += squares[i]; return s; }
" "
#include <stdio.h>
extern long getb(long), getsgn(long), getsq(long), getans(void), sumrow(void);
int main(void){
  printf(\"%ld %ld %ld %ld %ld %ld %ld %ld\\n\",
         getb(0), getb(4), getb(7), getsgn(0), getsgn(2),
         getsq(5), getans(), sumrow());
  return (getb(0)==10 && getb(4)==250 && getb(7)==200 &&
          getsgn(0)==-1 && getsgn(2)==-128 &&
          getsq(5)==25 && getans()==42 && sumrow()==55) ? 0 : 1;
}
")))
    (should (equal "10 250 200 -1 -128 25 42 55" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-global-test)

;;; nelisp-cfront-global-test.el ends here
