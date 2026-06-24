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
(require 'cl-lib)
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

(ert-deftest nelisp-cfront-typedef-pointer-shared-by-declarators ()
  "A pointer typedef's level is shared by EVERY comma declarator
(`TP a, b' => both `T*'), while a syntactic `*' binds only to its own
declarator (`int *a, b' => a is `int*', b is `int').  Parse-level guard
for the multi-declarator pointer-level fix (libxml2's `_xmlDefAttrs'-style
`xmlEnumerationPtr ret=NULL,last=NULL,cur,tmp;')."
  (cl-flet ((ptrs (src)
              (let* ((ast (nelisp-cfront-parse src))
                     (fn (car (last (cdr ast))))
                     (body (nth 4 fn))
                     (stmts (if (eq (car body) 'block) (nth 1 body) body))
                     (out nil))
                (dolist (st stmts)
                  (when (memq (car-safe st) '(decl decls))
                    (dolist (d (if (eq (car st) 'decls) (cdr st) (list st)))
                      (push (cons (nth 2 d) (or (plist-get (nth 1 d) :ptr) 0))
                            out))))
                (nreverse out))))
    ;; pointer typedef: all three share level 1, `*c' adds one => 2
    (should (equal '(("a" . 1) ("b" . 1) ("c" . 2))
                   (ptrs "typedef int *IP; int f(void){ IP a, b, *c; return 0; }")))
    ;; syntactic star binds per-declarator: a/c are pointers, b is not
    (should (equal '(("a" . 1) ("b" . 0) ("c" . 1))
                   (ptrs "int f(void){ int *a, b, *c; return 0; }")))))

(ert-deftest nelisp-cfront-typedef-pointer-multidecl-e2e ()
  "The exact libxml2 idiom that used to mis-lower: a pointer typedef with
several declarators initialized to NULL in one declaration, then a chained
assignment `ret = last = cur' building a linked list.  Before the fix the
2nd+ declarators lost their pointer level, were typed as struct values, and
their `= NULL' init lowered to a struct copy from a non-lvalue."
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
typedef struct _E *EP;
struct _E { struct _E *next; int v; };
static struct _E pool[8];
static int pooln;
EP mk(int v){ EP e = &pool[pooln]; pooln = pooln + 1; e->next = ((void*)0); e->v = v; return e; }
int build(void){
  EP ret = ((void*)0), last = ((void*)0), cur, tmp;
  int i, sum = 0;
  for (i = 1; i <= 3; i++) {
    cur = mk(i * 10);
    if (last == ((void*)0)) ret = last = cur;
    else { last->next = cur; last = cur; }
  }
  for (tmp = ret; tmp != ((void*)0); tmp = tmp->next) sum = sum + tmp->v;
  return sum;
}
")
         (drv "
#include <stdio.h>
extern int build(void);
int main(void){ int s = build(); printf(\"%d\\n\", s); return (s==60)?0:1; }
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "60" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-typedef-test)

;;; nelisp-cfront-typedef-test.el ends here
