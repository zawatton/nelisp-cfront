;;; nelisp-cfront-goto-test.el --- M4 forward goto -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M4 — forward `goto' to a single top-level label (the SQLite-style cleanup
;; pattern), lowered via a goto flag in single-exit mode: code before the
;; label is guarded by the flag (so `goto' skips it, including out of loops),
;; the label clears the flag, and code after it runs.  General/backward goto
;; (relooper) is out of scope.  Skips if backend/cc unavailable.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-e2e-test)

(ert-deftest nelisp-cfront-goto-cleanup ()
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
long process(long *a, long n){
  long sum = 0; long rc = 0;
  for (long i = 0; i < n; i = i + 1) {
    if (a[i] < 0) { rc = 0 - 1; goto done; }
    sum = sum + a[i];
  }
  rc = sum;
done:
  return rc;
}
")
         (drv "
#include <stdio.h>
extern long process(long*,long);
int main(void){
  long a[3] = {1,2,3};       /* no negative -> falls through to label, rc=6 */
  long b[3] = {1,-2,3};      /* negative    -> goto done out of loop, rc=-1 */
  printf(\"%ld %ld\\n\", process(a,3), process(b,3));
  return (process(a,3)==6 && process(b,3)==-1) ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "6 -1" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-goto-test)

;;; nelisp-cfront-goto-test.el ends here
