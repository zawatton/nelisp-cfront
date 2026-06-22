;;; nelisp-cfront-malloc-test.el --- M3 arena allocator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M3 — compile a C program that allocates via the nelisp-cfront arena
;; runtime (mmap-backed), link program + runtime + C driver, run, verify.
;; Skips when the backend or cc are unavailable.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-rt)
(require 'nelisp-cfront-e2e-test)        ; available-p helper

(defun nelisp-cfront-malloc-test--run (csource driver-c)
  "Compile CSOURCE + the arena runtime, link with DRIVER-C, run.
Return (cons EXIT STDOUT)."
  (let* ((dir (make-temp-file "nlcf-mal" t))
         (prog (expand-file-name "prog.o" dir))
         (rt (expand-file-name "rt.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource prog)
          (nelisp-cfront-rt-compile rt)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv prog rt "-o" bin))
              (error "malloc-test: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-malloc-arena ()
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
extern char *nlcf_arena_new(long);
extern void *nlcf_arena_alloc(char*, long);
extern long  nlcf_arena_free(char*);
long arena_demo(void){
  char *a = nlcf_arena_new(4096);
  long *p = nlcf_arena_alloc(a, 8); *p = 42;
  long *q = nlcf_arena_alloc(a, 8); *q = 100;
  long r = *p + *q;
  nlcf_arena_free(a);
  return r;
}
long arena_array(long n){
  char *a = nlcf_arena_new(65536);
  long *xs = nlcf_arena_alloc(a, n * 8);
  for (long i = 0; i < n; i = i + 1) xs[i] = i * i;
  long s = 0;
  for (long i = 0; i < n; i = i + 1) s = s + xs[i];
  nlcf_arena_free(a);
  return s;
}
")
         (drv "
#include <stdio.h>
extern long arena_demo(void); extern long arena_array(long);
int main(void){
  long d = arena_demo(), s = arena_array(10);
  printf(\"%ld %ld\\n\", d, s);
  return (d==142 && s==285) ? 0 : 1;   /* 142 ; sum i^2 0..9 = 285 */
}
")
         (res (nelisp-cfront-malloc-test--run csrc drv)))
    (should (equal "142 285" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-malloc-test)

;;; nelisp-cfront-malloc-test.el ends here
