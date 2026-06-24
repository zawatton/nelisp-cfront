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

(provide 'nelisp-cfront-structtab-test)

;;; nelisp-cfront-structtab-test.el ends here
