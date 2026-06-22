;;; nelisp-cfront-e2e-test.el --- end-to-end: real C -> native -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M2.5 — end-to-end ERT: compile a REAL C program through the full
;; nelisp-cfront pipeline (lex -> parse -> lower -> AOT -> .o), link it
;; with a C driver via cc, run the native binary, assert the output.
;;
;; Skips (not fails) when the nelisp AOT backend or cc are unavailable,
;; so `make test' stays green on a host without them.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)

(defun nelisp-cfront-e2e--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-e2e--run (csource driver-c)
  "Compile CSOURCE, link with DRIVER-C, run; return (cons EXIT STDOUT)."
  (let* ((dir (make-temp-file "nlcf-e2e" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-e2e-fib-loop-gcd ()
  "Recursion, for-loop with a local, and gcd (locals/while/%/!=)."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
long fib(long n){ if (n < 2) return n; return fib(n-1) + fib(n-2); }
long sumto(long n){ long s = 0; for (long i = 1; i <= n; i = i + 1) s = s + i; return s; }
long gcd(long a0, long b0){ long a = a0; long b = b0;
  while (b != 0){ long t = b; b = a % b; a = t; } return a; }
")
         (drv "
#include <stdio.h>
extern long fib(long); extern long sumto(long); extern long gcd(long,long);
int main(void){
  long a=fib(10), b=sumto(100), c=gcd(48,36);
  printf(\"%ld %ld %ld\\n\", a, b, c);
  return (a==55 && b==5050 && c==12) ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "55 5050 12" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-e2e-bitwise-cond ()
  "Bitwise ops, ternary, nested if, compound assignment."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
long f(long x){
  long r = 0;
  r = x & 0xff;
  r = r | 0x100;
  r ^= 1;
  return (r > 100) ? r : 0;
}
long g(long n){ if (n > 0) { if (n > 10) return 2; return 1; } return 0; }
")
         (drv "
#include <stdio.h>
extern long f(long); extern long g(long);
int main(void){
  long a=f(0x37), b=g(5), c=g(20), d=g(-1);
  printf(\"%ld %ld %ld %ld\\n\", a, b, c, d);
  /* f: (0x37&0xff)=0x37 |0x100 =0x137 ^1 =0x136=310 ; >100 -> 310 */
  return (a==310 && b==1 && c==2 && d==0) ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "310 1 2 0" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-e2e-test)

;;; nelisp-cfront-e2e-test.el ends here
