;;; nelisp-cfront-narrowint-test.el --- narrow-int width handling -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 lowering — narrow integer (char/short/int) width correctness.
;; cfront carries every value as i64, so narrow ints need explicit
;; sign/zero extension at two boundaries:
;;   - function entry: SysV passes a narrow int arg in the low bits of a
;;     64-bit register (gcc zero-extends), so a negative `int' arg would
;;     read as a large positive i64 — each narrow param is re-normalized.
;;   - memory load: `--load-w' zero-extends, so a SIGNED narrow field is
;;     sign-extended after the load (unsigned is already exact).

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)

(defun nelisp-cfront-narrowint-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-narrowint-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-narrow" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "narrowint e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-narrowint-param-and-load-e2e ()
  "Negative narrow-int params sign-extend at entry (so signed `<' works),
and signed narrow struct fields sign-extend on load; unsigned stays
zero-extended."
  (unless (nelisp-cfront-narrowint-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-narrowint-test--run "
struct S { signed char c; short h; int w; unsigned char uc; };
int rdc(struct S *s){ return s->c; }
int rdh(struct S *s){ return s->h; }
int rdw(struct S *s){ return s->w; }
int rduc(struct S *s){ return s->uc; }
int classify(int x){
  if (x < 0) return -1;
  else if (x == 0) return 0;
  else if (x < 10) return 1;
  return 2;
}
" "
#include <stdio.h>
struct S { signed char c; short h; int w; unsigned char uc; };
extern int rdc(struct S*); extern int rdh(struct S*);
extern int rdw(struct S*); extern int rduc(struct S*); extern int classify(int);
int main(void){
  struct S s; s.c=-3; s.h=-300; s.w=-100000; s.uc=200;
  int c=rdc(&s), h=rdh(&s), w=rdw(&s), uc=rduc(&s);
  int cl = classify(-5)*1000 + (classify(0)+1)*100 + classify(5)*10 + classify(50);
  printf(\"%d %d %d %d %d\\n\", c,h,w,uc,cl);
  return (c==-3 && h==-300 && w==-100000 && uc==200 && cl==-888)?0:1;
}
")))
    (should (equal "-3 -300 -100000 200 -888" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-narrowint-test)

;;; nelisp-cfront-narrowint-test.el ends here
