;;; nelisp-cfront-global-test.el --- read-only integer globals -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 / Doc 06 Step B — read-only integer global scalars and arrays.
;; cfront now retains a global array's dimensions on its type (so `G[i]'
;; types/indexes instead of signalling `:deref-non-pointer'), collects
;; const-initialized integer globals into `--globals', emits each as a
;; `data-blob' rodata symbol, and lowers a global reference through
;; `(data-addr NAME)' (scalar load / array address decay).

;;; Code:

(require 'ert)
(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-type)
(require 'nelisp-cfront-lower)
(require 'nelisp-cfront-cc)

;;; --- unit: collection + byte packing + lowering shape ---------------

(ert-deftest nelisp-cfront-global-collect-bytes ()
  "Read-only const integer globals collect into `.rodata' with correct
little-endian bytes; array dimensions are retained on the type."
  (let* ((ast (nelisp-cfront-parse "
const unsigned char tbl[4] = {10, 20, 250, 99};
const int words[3] = {1, -1, 256};
const int answer = 42;
"))
         (nelisp-cfront-lower--structs (nelisp-cfront-type-build-structs ast))
         (g (nelisp-cfront-lower--collect-globals ast)))
    (should (equal '("tbl" "words" "answer") (mapcar #'car g)))
    ;; never written + const integer -> read-only .rodata.
    (should (eq 'rodata (plist-get (cdr (assoc "tbl" g)) :section)))
    ;; array dimension retained on the type.
    (should (equal 4 (plist-get (plist-get (cdr (assoc "tbl" g)) :type) :array)))
    (should (equal (unibyte-string 10 20 250 99)
                   (plist-get (cdr (assoc "tbl" g)) :bytes)))
    ;; int words: 1, -1 (=0xffffffff), 256 -> 12 LE bytes.
    (should (equal (unibyte-string 1 0 0 0  255 255 255 255  0 1 0 0)
                   (plist-get (cdr (assoc "words" g)) :bytes)))
    (should (equal (unibyte-string 42 0 0 0)
                   (plist-get (cdr (assoc "answer" g)) :bytes)))))

(ert-deftest nelisp-cfront-global-section-selection ()
  "Section choice (Doc 06 Step C): read-only const int -> rodata; a written
scalar -> data; a zero/uninitialized struct/pointer -> bss."
  (let* ((ast (nelisp-cfront-parse "
const int ro = 7;
int counter = 5;
char *cursor;
struct P { int a; int b; } gp;
int bump(void){ counter += 1; return counter; }
char *getcur(void){ return cursor; }
int geta(void){ return gp.a; }
int getro(void){ return ro; }
"))
         (nelisp-cfront-lower--structs (nelisp-cfront-type-build-structs ast))
         (g (nelisp-cfront-lower--collect-globals ast)))
    (should (eq 'rodata (plist-get (cdr (assoc "ro" g)) :section)))
    (should (eq 'data   (plist-get (cdr (assoc "counter" g)) :section)))
    (should (eq 'bss    (plist-get (cdr (assoc "cursor" g)) :section)))
    (should (eq 'bss    (plist-get (cdr (assoc "gp" g)) :section)))
    ;; struct global sized from its layout (2 ints = 8 bytes of bss).
    (should (= 8 (length (plist-get (cdr (assoc "gp" g)) :bytes))))))

(ert-deftest nelisp-cfront-global-lowers-via-data-addr ()
  "A global array index lowers to a `data-addr'-based load and the program
emits a `data-blob' for the global; no `:deref-non-pointer'."
  (let* ((ast (nelisp-cfront-parse "
const unsigned char tbl[3] = {7, 8, 9};
int getb(int i){ return tbl[i]; }
"))
         (prog (nelisp-cfront-lower-program ast))
         (flat (format "%S" prog)))
    (should (string-match-p "(data-blob tbl " flat))
    (should (string-match-p "(data-addr tbl)" flat))))

;;; --- end-to-end: compile -> link -> run -----------------------------

(defun nelisp-cfront-global-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-global-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-global" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "global e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-global-readonly-arrays-e2e ()
  "Read-only integer global scalars/arrays read back correctly: unsigned
char (zero-extend), signed char (sign-extend), int array, int scalar, and
a loop summing a global array."
  (unless (nelisp-cfront-global-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-global-test--run "
const unsigned char tbl[8] = {10, 20, 30, 40, 250, 99, 7, 200};
signed char sgn[3] = {-1, -2, -128};
const int squares[6] = {0, 1, 4, 9, 16, 25};
const int answer = 42;
int getb(int i){ return tbl[i]; }
int getsgn(int i){ return sgn[i]; }
int getsq(int i){ return squares[i]; }
int getans(void){ return answer; }
int sumrow(void){ int s = 0; for (int i = 0; i < 6; i++) s += squares[i]; return s; }
" "
#include <stdio.h>
extern long getb(long), getsgn(long), getsq(long), getans(void), sumrow(void);
int main(void){
  printf(\"%ld %ld %ld %ld %ld %ld %ld %ld\\n\",
         getb(0), getb(4), getb(7), getsgn(0), getsgn(2),
         getsq(5), getans(), sumrow());
  return (getb(0)==10 && getb(4)==250 && getb(7)==200 &&
          getsgn(0)==-1 && getsgn(2)==-128 &&
          getsq(5)==25 && getans()==42 && sumrow()==55) ? 0 : 1;
}
")))
    (should (equal "10 250 200 -1 -128 25 42 55" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-global-writable-data-bss-e2e ()
  "Step C: a mutable scalar (.data), a zero-initialized struct and a
pointer global (.bss) all read/write correctly; a const stays read-only."
  (unless (nelisp-cfront-global-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-global-test--run "
struct Stat { long now[4]; long mx[4]; } sStat = { {0,}, {0,} };
int counter = 5;
char *cursor;
const int ro = 7;
void up(int i, long v){ sStat.now[i] += v; if (sStat.now[i] > sStat.mx[i]) sStat.mx[i] = sStat.now[i]; }
long getnow(int i){ return sStat.now[i]; }
long getmx(int i){ return sStat.mx[i]; }
int bump(void){ counter += 1; return counter; }
void setcur(char *p){ cursor = p; }
long curval(void){ return cursor ? *cursor : -1; }
int getro(void){ return ro; }
" "
#include <stdio.h>
extern void up(int,long); extern long getnow(int), getmx(int);
extern int bump(void), getro(void);
extern void setcur(char*); extern long curval(void);
int main(void){
  up(0,10); up(0,5); up(1,3);
  int b1 = bump();
  int b2 = bump();
  char c = 'Z'; setcur(&c);
  long cv = curval();
  printf(\"%ld %ld %ld %d %d %ld %d\\n\",
         getnow(0), getmx(0), getnow(1), b1, b2, cv, getro());
  return (getnow(0)==15 && getmx(0)==15 && getnow(1)==3 &&
          b1==6 && b2==7 && cv=='Z' && getro()==7) ? 0 : 1;
}
")))
    (should (equal "15 15 3 6 7 90 7" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-global-pointer-string-init-e2e ()
  "Step C-2: a pointer global initialized with a string literal, and a flat
array of string pointers, resolve via `.data' relocations to the rodata
string pool and read back correctly."
  (unless (nelisp-cfront-global-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-global-test--run "
const char *greeting = \"hello\";
const char *names[3] = { \"alpha\", \"beta\", \"gamma\" };
const char *get_greeting(void){ return greeting; }
const char *get_name(int i){ return names[i]; }
" "
#include <stdio.h>
#include <string.h>
extern const char *get_greeting(void);
extern const char *get_name(int);
int main(void){
  printf(\"%s %s %s %s\\n\", get_greeting(), get_name(0), get_name(1), get_name(2));
  return (strcmp(get_greeting(),\"hello\")==0 && strcmp(get_name(0),\"alpha\")==0 &&
          strcmp(get_name(1),\"beta\")==0 && strcmp(get_name(2),\"gamma\")==0) ? 0 : 1;
}
")))
    (should (equal "hello alpha beta gamma" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-global-aggregate-init-e2e ()
  "Step C-3: a non-zero aggregate global is laid out recursively — an array
of structs with a `double' field and an inline `char[]' field (implicit
`[]' size), and a positional `struct' global — read back correctly."
  (unless (nelisp-cfront-global-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-global-test--run "
typedef unsigned char u8;
static const struct { u8 nName; char zName[7]; double rLimit; } aXformType[] = {
  { 6, \"second\", 100.5 },
  { 4, \"hour\",   3600.0 },
  { 3, \"day\",    86400.0 },
};
struct Pt { int x; int y; } origin = { 3, 7 };
int getn(int i){ return aXformType[i].nName; }
int getc0(int i){ return aXformType[i].zName[0]; }
long lim_x10(int i){ return (long)(aXformType[i].rLimit * 10.0); }
int ox(void){ return origin.x; }
int oy(void){ return origin.y; }
" "
#include <stdio.h>
extern int getn(int), getc0(int), ox(void), oy(void);
extern long lim_x10(int);
int main(void){
  printf(\"%d %d %c %c %ld %ld %d %d\\n\",
         getn(0), getn(2), getc0(0), getc0(1), lim_x10(0), lim_x10(2), ox(), oy());
  return (getn(0)==6 && getn(2)==3 && getc0(0)=='s' && getc0(1)=='h' &&
          lim_x10(0)==1005 && lim_x10(2)==864000 && ox()==3 && oy()==7) ? 0 : 1;
}
")))
    (should (equal "6 3 s h 1005 864000 3 7" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-global-sizeof-implicit-dim-e2e ()
  "A global array with an implicit `[]' dimension is registered with its
resolved size, so the `sizeof(arr)/sizeof(arr[0])' count idiom works — for
both an init-list array and a string-initialized char array."
  (unless (nelisp-cfront-global-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-global-test--run "
static const unsigned char tbl[] = { 1, 2, 3, 4, 5 };
static const char magic[] = \"abcdef\";
int ntbl(void){ return (int)(sizeof(tbl)/sizeof(tbl[0])); }
int magicsz(void){ return (int)sizeof(magic); }
int sumtbl(void){ int s=0; int i;
  for(i=0;i<(int)(sizeof(tbl)/sizeof(tbl[0]));i++) s+=tbl[i]; return s; }
" "
#include <stdio.h>
extern int ntbl(void), magicsz(void), sumtbl(void);
int main(void){
  printf(\"%d %d %d\\n\", ntbl(), magicsz(), sumtbl());
  return (ntbl()==5 && magicsz()==7 && sumtbl()==15) ? 0 : 1;
}
")))
    (should (equal "5 7 15" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-global-test)

;;; nelisp-cfront-global-test.el ends here
