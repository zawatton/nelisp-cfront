;;; nelisp-cfront-typedef-test.el --- M4 typedef -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M4 — typedef: the parser tracks typedef names so a typedef'd identifier
;; is recognised as a type-specifier (the classic type-name disambiguation
;; real headers rely on).  Covers scalar aliases, tagged-struct typedefs,
;; and anonymous-struct typedefs.  fn-ptr-typedef declarator syntax is a
;; follow-on.  Skips if backend/cc unavailable.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-e2e-test)

(ert-deftest nelisp-cfront-typedef-scalar-and-structs ()
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
typedef long i64;
typedef struct Point { i64 x; i64 y; } Point;
typedef struct { i64 a; i64 b; } Pair;
i64 dbl(i64 n){ return n + n; }
i64 padd(Point *p){ return p->x + p->y; }
i64 prsum(Pair *r){ return r->a + r->b; }
")
         (drv "
#include <stdio.h>
struct Point { long x; long y; };
struct Pair  { long a; long b; };
extern long dbl(long);
extern long padd(struct Point*);
extern long prsum(struct Pair*);
int main(void){
  struct Point pt = {3, 4};
  struct Pair  pr = {10, 20};
  long d=dbl(21), p=padd(&pt), s=prsum(&pr);
  printf(\"%ld %ld %ld\\n\", d, p, s);
  return (d==42 && p==7 && s==30) ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "42 7 30" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-typedef-test)

;;; nelisp-cfront-typedef-test.el ends here
