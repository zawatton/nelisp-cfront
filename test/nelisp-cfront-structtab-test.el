;;; nelisp-cfront-structtab-test.el --- struct-table completeness -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 / struct-table completeness.  `build-structs' now registers inline
;; struct/union definitions wherever they appear — nested inside another
;; struct's members, or at a global declaration — so a tag is resolvable
;; by name even when its only definition is inline at a sibling decl.
;; This was the dominant cause of the `:unknown-struct' bucket (47 -> 5).

;;; Code:

(require 'ert)
(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-type)
(require 'nelisp-cfront-cc)

(ert-deftest nelisp-cfront-structtab-nested-and-sibling-tags ()
  "A struct tag defined inline inside another struct's members, and one
defined inline at a global decl, are both in the struct table."
  (let* ((ast (nelisp-cfront-parse "
struct Outer { int n; struct Inner { int x; int y; } item; };
static struct PT { unsigned char i; int v; } prng;
"))
         (tbl (nelisp-cfront-type-build-structs ast)))
    (should (assoc "Outer" tbl))
    (should (assoc "Inner" tbl))   ; nested inline tag
    (should (assoc "PT" tbl))      ; inline tag at a global decl
    ;; Inner lays out as two ints = 8 bytes.
    (should (= 8 (plist-get (cdr (assoc "Inner" tbl)) :size)))))

(defun nelisp-cfront-structtab-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-structtab-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-structtab" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "structtab e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-structtab-resolve-e2e ()
  "Globals whose struct tag is only defined inline at a sibling decl (or
nested in another struct) compile and their members read back correctly."
  (unless (nelisp-cfront-structtab-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-structtab-test--run "
struct Outer { int n; struct Inner { int x; int y; } item; };
static struct Inner z = { 30, 40 };
static struct PT { unsigned char i; int v; } prng = { 5, 100 };
static struct PT saved;
int zx(void){ return z.x; }
int zy(void){ return z.y; }
void savep(void){ saved = prng; }
int savedv(void){ return saved.v; }
int prngi(void){ return prng.i; }
" "
#include <stdio.h>
extern int zx(void),zy(void),savedv(void),prngi(void); extern void savep(void);
int main(void){
  savep();
  printf(\"%d %d %d %d\\n\", zx(), zy(), savedv(), prngi());
  return (zx()==30 && zy()==40 && savedv()==100 && prngi()==5) ? 0 : 1;
}
")))
    (should (equal "30 40 100 5" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-structtab-sizeof-dim-and-sizeof ()
  "An array field dimensioned by `sizeof(struct T)' is laid out correctly
(resolved at layout time via the struct table), and `sizeof(TYPE)' returns
the true size for struct/array/float (not the old flat 8)."
  (unless (nelisp-cfront-structtab-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-structtab-test--run "
struct E { int x; int y; };
struct Box { int tag; struct E es[32 / sizeof(struct E)]; int last; };
static struct Box b;
int setget(void){ b.tag=7; b.es[3].x=99; b.last=5; return b.tag + b.es[3].x + b.last; }
int boxsz(void){ return (int)sizeof(struct Box); }
int fsz(void){ return (int)sizeof(float); }
" "
#include <stdio.h>
struct E { int x; int y; };
struct Box { int tag; struct E es[32 / sizeof(struct E)]; int last; };
extern int setget(void), boxsz(void), fsz(void);
int main(void){
  printf(\"%d %d %d\\n\", setget(), boxsz(), fsz());
  return (setget()==111 && boxsz()==(int)sizeof(struct Box) && fsz()==4) ? 0 : 1;
}
")))
    (should (equal "111 40 4" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-structtab-sizeof-expr-array-dim-e2e ()
  "A local array dimensioned by `sizeof(EXPR)' is sized at lower time using
the type env: `sizeof(global)+N', `sizeof(param->field)', and the count
idiom `sizeof(g)/sizeof(g[0])'."
  (unless (nelisp-cfront-structtab-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-structtab-test--run "
static const unsigned char magic[] = { 217, 213, 5, 249 };
struct P { unsigned char ver[16]; int n; };
int hdrsize(void){ unsigned char h[sizeof(magic) + 4]; h[0]=1; h[7]=2; return (int)sizeof(h); }
int verbuf(struct P *p){ unsigned char b[sizeof(p->ver)]; b[0]=9; return (int)sizeof(b) + b[0]; }
int tblcount(void){ int a[sizeof(magic)/sizeof(magic[0])]; a[0]=10; a[3]=20;
  return (int)(sizeof(a)/sizeof(a[0])); }
" "
#include <stdio.h>
struct P { unsigned char ver[16]; int n; };
extern int hdrsize(void); extern int verbuf(struct P*); extern int tblcount(void);
int main(void){ struct P p;
  printf(\"%d %d %d\\n\", hdrsize(), verbuf(&p), tblcount());
  return (hdrsize()==8 && verbuf(&p)==25 && tblcount()==4) ? 0 : 1; }
")))
    (should (equal "8 25 4" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-structtab-addr-of-global-init-e2e ()
  "A pointer field initialized with `&global' / `&global[const]' in an
aggregate (here a function-static struct array) resolves via a `.data'
reloc to the global's symbol (with addend for the indexed form)."
  (unless (nelisp-cfront-structtab-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-structtab-test--run "
static int gv = 42;
static const unsigned char arr[4] = { 10, 20, 30, 40 };
struct E { int id; const int *p; const unsigned char *q; };
int getgv(void){
  static struct E tbl[] = { {1, &gv, &arr[2]}, {2, 0, 0} };
  return *(tbl[0].p);
}
int getq(void){
  static struct E tbl2[] = { {9, &gv, &arr[2]} };
  return *(tbl2[0].q);
}
" "
#include <stdio.h>
extern int getgv(void), getq(void);
int main(void){
  printf(\"%d %d\\n\", getgv(), getq());
  return (getgv()==42 && getq()==30) ? 0 : 1;
}
")))
    (should (equal "42 30" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-structtab-flexible-array-member-parses ()
  "A C99 flexible array member `T name[];' in a struct, and a
multi-dimensional field `T m[2][3];', parse (the `[]' dim -> `:array t');
the leading fields still lay out.  (libxml2's `_xmlDefAttrs' uses a FAM.)"
  (let* ((ast (nelisp-cfront-parse "
struct FAM { int nbAttrs; int maxAttrs; const char *values[]; };
struct MD { int n; int m[2][3]; };
"))
         (tbl (nelisp-cfront-type-build-structs ast))
         (fam-fields (nth 2 (cl-find-if (lambda (tp)
                                          (and (eq (car tp) 'struct-def)
                                               (equal (nth 1 tp) "FAM")))
                                        (cdr ast)))))
    ;; the FAM field parsed with an unknown (`t') array dimension
    (let ((vf (cl-find-if (lambda (f) (equal (nth 2 f) "values")) fam-fields)))
      (should vf)
      (should (eq t (plist-get (nth 1 vf) :array)))
      (should (= 1 (or (plist-get (nth 1 vf) :ptr) 0))))
    ;; MD lays out and its multi-dim field is sized 2*3*4 = 24
    (should (assoc "MD" tbl))
    (should (= (+ 4 24) (plist-get (cdr (assoc "MD" tbl)) :size)))))

(ert-deftest nelisp-cfront-structtab-multidim-array-e2e ()
  "A multi-dimensional local array indexes and sizes correctly (each index
peels one dimension; `sizeof' is the full product, not first-dim*element),
and a struct holding a 2-D field lays out at the true size.  Guards the
`--strip-array' one-dimension-peel fix."
  (unless (nelisp-cfront-structtab-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-structtab-test--run "
struct G { int cells[2][3]; int tag; };
int grid(void){
  int m[2][3];
  int i, j, s = 0;
  for (i = 0; i < 2; i++) for (j = 0; j < 3; j++) m[i][j] = i * 10 + j;
  for (i = 0; i < 2; i++) for (j = 0; j < 3; j++) s += m[i][j];
  return s + m[1][2];               /* 0+1+2+10+11+12 = 36, + m[1][2]=12 */
}
int gsize(void){ int m[2][3]; return (int)sizeof(m); }   /* 2*3*4 = 24 */
int gtag(void){ struct G g; g.cells[1][2] = 7; g.tag = 9; return g.cells[1][2] + g.tag; }
int gstructsz(void){ return (int)sizeof(struct G); } /* 24 + 4 = 28 */
" "
#include <stdio.h>
extern int grid(void), gsize(void), gtag(void), gstructsz(void);
int main(void){
  printf(\"%d %d %d %d\\n\", grid(), gsize(), gtag(), gstructsz());
  return (grid()==48 && gsize()==24 && gtag()==16 && gstructsz()==28) ? 0 : 1;
}
")))
    (should (equal "48 24 16 28" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-structtab-test)

;;; nelisp-cfront-structtab-test.el ends here
