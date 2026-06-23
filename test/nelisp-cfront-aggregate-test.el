;;; nelisp-cfront-aggregate-test.el --- aggregate locals & &local -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 lowering — memory-backed locals via the upstream `frame-alloc' op:
;; C local arrays, struct-by-value locals, and address-taken scalars all
;; get a fixed frame block; the variable's slot holds the block address.
;; An aggregate decays to that address; an address-taken scalar is
;; read/written/`&'-ed through it.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)

(defun nelisp-cfront-aggregate-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-aggregate-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-agg" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "aggregate e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-aggregate-locals-e2e ()
  "Local array, struct-by-value, &local-into-call, and &scalar deref."
  (unless (nelisp-cfront-aggregate-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-aggregate-test--run "
int sumarr(int n){
  int a[8];
  int i;
  int s = 0;
  for (i = 0; i < n; i = i + 1) a[i] = i*i;
  for (i = 0; i < n; i = i + 1) s += a[i];
  return s;
}
void inc(int *p){ *p = *p + 1; }
int useaddr(void){ int x = 41; inc(&x); return x; }
struct P { int a; int b; };
int structlocal(void){ struct P p; p.a = 10; p.b = 32; return p.a + p.b; }
int addrscalar(void){ int y = 5; int *q = &y; *q = *q + 37; return y; }
" "
#include <stdio.h>
extern int sumarr(int); extern int useaddr(void);
extern int structlocal(void); extern int addrscalar(void);
int main(void){
  int a=sumarr(4), b=useaddr(), c=structlocal(), d=addrscalar();
  printf(\"%d %d %d %d\\n\", a, b, c, d);
  return (a==14 && b==42 && c==42 && d==42)?0:1;
}
")))
    (should (equal "14 42 42 42" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-aggregate-test)

;;; nelisp-cfront-aggregate-test.el ends here
