;;; nelisp-cfront-union-test.el --- M4 union -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M4 — union types: every field at offset 0, size = max field size.
;; Accessed via pointers (like structs).  Covers overlapping members and
;; a mixed-width (long/int) union.  Skips if backend/cc unavailable.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-e2e-test)

(ert-deftest nelisp-cfront-union-overlap ()
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
union Box { long a; long b; };
void seta(union Box *u, long x){ u->a = x; }
long getb(union Box *u){ return u->b; }
typedef union { long l; int i; } Mix;
void setl(Mix *u, long x){ u->l = x; }
long geti(Mix *u){ return u->i; }
")
         (drv "
#include <stdio.h>
union Box { long a; long b; };
typedef union { long l; int i; } Mix;
extern void seta(union Box*,long); extern long getb(union Box*);
extern void setl(Mix*,long); extern long geti(Mix*);
int main(void){
  union Box b; seta(&b, 42);
  Mix m; setl(&m, 0x100000007L);
  printf(\"%ld %ld\\n\", getb(&b), geti(&m));
  return (getb(&b)==42 && geti(&m)==7) ? 0 : 1;  /* overlap; low 32 bits */
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "42 7" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-union-test)

;;; nelisp-cfront-union-test.el ends here
