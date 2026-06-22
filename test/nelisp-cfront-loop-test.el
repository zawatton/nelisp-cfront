;;; nelisp-cfront-loop-test.el --- M4 break/continue -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M4 (first piece) — `break' / `continue' in while/for loops, lowered via
;; a break-flag + guarded condition/step and guard-lifting.  Compiled C
;; runs natively with correct semantics.  Skips if backend/cc unavailable.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-e2e-test)        ; available-p + run helper

(ert-deftest nelisp-cfront-loop-break-continue ()
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
long find(long *a, long n, long target){
  long idx = 0 - 1;
  for (long i = 0; i < n; i = i + 1) { if (a[i] == target) { idx = i; break; } }
  return idx;
}
long count_pos(long *a, long n){
  long c = 0;
  for (long i = 0; i < n; i = i + 1){ if (a[i] <= 0) continue; c = c + 1; }
  return c;
}
long sum_until_neg(long *a, long n){
  long s = 0; long i = 0;
  while (i < n){ if (a[i] < 0) break; s = s + a[i]; i = i + 1; }
  return s;
}
")
         (drv "
#include <stdio.h>
extern long find(long*,long,long); extern long count_pos(long*,long); extern long sum_until_neg(long*,long);
int main(void){
  long a[7] = {3, -2, 5, 7, -1, 9, 4};
  long b[5] = {1, -3, 2, 4, 5};
  long f=find(a,7,7), c=count_pos(a,7), s=sum_until_neg(b,5);
  printf(\"%ld %ld %ld\\n\", f, c, s);
  return (f==3 && c==5 && s==1) ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "3 5 1" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-loop-return-in-loop ()
  "M4: early `return' inside loops via the single-exit (return-flag) transform."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
long find_idx(long *a, long n, long t){
  for (long i = 0; i < n; i = i + 1) { if (a[i] == t) return i; }
  return 0 - 1;
}
long all_pos(long *a, long n){
  for (long i = 0; i < n; i = i + 1) { if (a[i] <= 0) return 0; }
  return 1;
}
long first_neg_or(long *a, long n, long dflt){
  long i = 0;
  while (i < n){ if (a[i] < 0) return a[i]; i = i + 1; }
  return dflt;
}
")
         (drv "
#include <stdio.h>
extern long find_idx(long*,long,long); extern long all_pos(long*,long); extern long first_neg_or(long*,long,long);
int main(void){
  long a[5] = {2,4,6,8,10}; long b[5] = {2,4,-6,8,10};
  long f1=find_idx(a,5,6), f2=find_idx(a,5,99), p1=all_pos(a,5), p2=all_pos(b,5), n=first_neg_or(b,5,-100);
  printf(\"%ld %ld %ld %ld %ld\\n\", f1, f2, p1, p2, n);
  return (f1==2 && f2==-1 && p1==1 && p2==0 && n==-6) ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "2 -1 1 0 -6" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-loop-test)

;;; nelisp-cfront-loop-test.el ends here
