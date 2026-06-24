;;; nelisp-cfront-structtab-test.el --- struct-table completeness -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 / struct-table completeness.  `build-structs' now registers inline
;; struct/union definitions wherever they appear — nested inside another
;; struct's members, or at a global declaration — so a tag is resolvable
;; by name even when its only definition is inline at a sibling decl.
;; This was the dominant cause of the `:unknown-struct' bucket (47 -> 5).

;;; Code:

(require 'ert)
(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-type)
(require 'nelisp-cfront-cc)

(ert-deftest nelisp-cfront-structtab-nested-and-sibling-tags ()
  "A struct tag defined inline inside another struct's members, and one
defined inline at a global decl, are both in the struct table."
  (let* ((ast (nelisp-cfront-parse "
struct Outer { int n; struct Inner { int x; int y; } item; };
static struct PT { unsigned char i; int v; } prng;
"))
         (tbl (nelisp-cfront-type-build-structs ast)))
    (should (assoc "Outer" tbl))
    (should (assoc "Inner" tbl))   ; nested inline tag
    (should (assoc "PT" tbl))      ; inline tag at a global decl
    ;; Inner lays out as two ints = 8 bytes.
    (should (= 8 (plist-get (cdr (assoc "Inner" tbl)) :size)))))

(defun nelisp-cfront-structtab-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-structtab-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-structtab" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "structtab e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-structtab-resolve-e2e ()
  "Globals whose struct tag is only defined inline at a sibling decl (or
nested in another struct) compile and their members read back correctly."
  (unless (nelisp-cfront-structtab-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-structtab-test--run "
struct Outer { int n; struct Inner { int x; int y; } item; };
static struct Inner z = { 30, 40 };
static struct PT { unsigned char i; int v; } prng = { 5, 100 };
static struct PT saved;
int zx(void){ return z.x; }
int zy(void){ return z.y; }
void savep(void){ saved = prng; }
int savedv(void){ return saved.v; }
int prngi(void){ return prng.i; }
" "
#include <stdio.h>
extern int zx(void),zy(void),savedv(void),prngi(void); extern void savep(void);
int main(void){
  savep();
  printf(\"%d %d %d %d\\n\", zx(), zy(), savedv(), prngi());
  return (zx()==30 && zy()==40 && savedv()==100 && prngi()==5) ? 0 : 1;
}
")))
    (should (equal "30 40 100 5" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-structtab-test)

;;; nelisp-cfront-structtab-test.el ends here
