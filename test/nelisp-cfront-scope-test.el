;;; nelisp-cfront-scope-test.el --- nested-scope local type tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 / Doc 06 follow-on — local-variable type inference must reach
;; declarations nested inside `switch'/`do-while'/labeled statements, not
;; just the top-level block and if/while/for bodies.  A typed pointer
;; declared in a `switch' case used to fall back to `long', so `p->field'
;; signalled `:unknown-struct'/`:deref-non-pointer'.  `--collect-decls' /
;; `--collect-decl-types' now recurse into those forms.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)

(defun nelisp-cfront-scope-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-scope-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-scope" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "scope e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-scope-nested-pointer-decl-e2e ()
  "A typed pointer declared inside a `switch' case (and a `while' body) is
tracked as its declared type, so `p->field' traverses a linked list."
  (unless (nelisp-cfront-scope-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-scope-test--run "
struct Node { int val; struct Node *next; };
int sum_from(struct Node *head, int mode){
  int s = 0;
  switch(mode){
    case 1: {
      struct Node *p = head;
      while(p){ s += p->val; p = p->next; }
      break;
    }
    default:
      s = -1;
  }
  return s;
}
" "
#include <stdio.h>
struct Node { int val; struct Node *next; };
extern int sum_from(struct Node*, int);
int main(void){
  struct Node c = {30, 0}, b = {20, &c}, a = {10, &b};
  printf(\"%d %d\\n\", sum_from(&a, 1), sum_from(&a, 9));
  return (sum_from(&a,1)==60 && sum_from(&a,9)==-1) ? 0 : 1;
}
")))
    (should (equal "60 -1" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-scope-deref-postincr-e2e ()
  "`*p++' over a pointer keeps the pointer type (type-of pre/post), so the
deref loads through it instead of signalling `:deref-non-pointer'."
  (unless (nelisp-cfront-scope-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-scope-test--run "
int sumbytes(const unsigned char *p, int n){
  int s = 0;
  while(n-- > 0){ s += *p++; }
  return s;
}
" "
#include <stdio.h>
extern int sumbytes(const unsigned char*, int);
int main(void){
  unsigned char a[5] = {10,20,30,40,200};
  printf(\"%d\\n\", sumbytes(a,5));
  return sumbytes(a,5)==300 ? 0 : 1;
}
")))
    (should (equal "300" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-scope-function-pointer-table-e2e ()
  "A static struct array with string + function-pointer fields lays out via
.data relocations (string pool + function symbols), read back by value."
  (unless (nelisp-cfront-scope-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-scope-test--run "
typedef int (*fp)(int);
int dbl(int x){ return x*2; }
int neg(int x){ return 0-x; }
static struct { const char *name; fp f; int tag; } tbl[2] = {
  { \"dbl\", (fp)dbl, 7 },
  { \"neg\", (fp)neg, 9 },
};
long getf(int i){ return (long)tbl[i].f; }
const char *nameof(int i){ return tbl[i].name; }
int tagof(int i){ return tbl[i].tag; }
" "
#include <stdio.h>
#include <string.h>
typedef int (*fp)(int);
extern long getf(int); extern const char *nameof(int); extern int tagof(int);
int main(void){
  fp f0=(fp)getf(0), f1=(fp)getf(1);
  printf(\"%d %d %s %s %d %d\\n\", f0(5), f1(5), nameof(0), nameof(1), tagof(0), tagof(1));
  return (f0(5)==10 && f1(5)==-5 && strcmp(nameof(0),\"dbl\")==0 &&
          strcmp(nameof(1),\"neg\")==0 && tagof(0)==7 && tagof(1)==9) ? 0 : 1;
}
")))
    (should (equal "10 -5 dbl neg 7 9" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-scope-function-static-locals-e2e ()
  "A function-`static' local is lifted to a module global: a read-only
const array (implicit `[]' size) is indexed, and a mutable scalar persists
its value across calls."
  (unless (nelisp-cfront-scope-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-scope-test--run "
int logest(unsigned long x){
  static int a[] = { 0, 2, 3, 5, 6, 7, 8, 9 };
  int y = 40;
  if( x<8 ){ if( x<2 ) return 0; while( x<8 ){ y -= 10; x <<= 1; } }
  return a[x&7] + y - 10;
}
int counter(void){ static int n = 0; n += 1; return n; }
" "
#include <stdio.h>
extern int logest(unsigned long); extern int counter(void);
int main(void){
  int c1=counter(), c2=counter(), c3=counter();
  printf(\"%d %d %d %d %d %d\\n\", logest(1), logest(15), logest(13), c1, c2, c3);
  return (logest(1)==0 && logest(15)==39 && logest(13)==37 &&
          c1==1 && c2==2 && c3==3) ? 0 : 1;
}
")))
    (should (equal "0 39 37 1 2 3" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-scope-test)

;;; nelisp-cfront-scope-test.el ends here
