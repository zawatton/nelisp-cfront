;;; nelisp-cfront-narrowint-test.el --- narrow-int width handling -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 lowering — narrow integer (char/short/int) width correctness.
;; cfront carries every value as i64, so narrow ints need explicit
;; sign/zero extension at two boundaries:
;;   - function entry: SysV passes a narrow int arg in the low bits of a
;;     64-bit register (gcc zero-extends), so a negative `int' arg would
;;     read as a large positive i64 — each narrow param is re-normalized.
;;   - memory load: `--load-w' zero-extends, so a SIGNED narrow field is
;;     sign-extended after the load (unsigned is already exact).

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)

(defun nelisp-cfront-narrowint-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-narrowint-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-narrow" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "narrowint e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-narrowint-param-and-load-e2e ()
  "Negative narrow-int params sign-extend at entry (so signed `<' works),
and signed narrow struct fields sign-extend on load; unsigned stays
zero-extended."
  (unless (nelisp-cfront-narrowint-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-narrowint-test--run "
struct S { signed char c; short h; int w; unsigned char uc; };
int rdc(struct S *s){ return s->c; }
int rdh(struct S *s){ return s->h; }
int rdw(struct S *s){ return s->w; }
int rduc(struct S *s){ return s->uc; }
int classify(int x){
  if (x < 0) return -1;
  else if (x == 0) return 0;
  else if (x < 10) return 1;
  return 2;
}
" "
#include <stdio.h>
struct S { signed char c; short h; int w; unsigned char uc; };
extern int rdc(struct S*); extern int rdh(struct S*);
extern int rdw(struct S*); extern int rduc(struct S*); extern int classify(int);
int main(void){
  struct S s; s.c=-3; s.h=-300; s.w=-100000; s.uc=200;
  int c=rdc(&s), h=rdh(&s), w=rdw(&s), uc=rduc(&s);
  int cl = classify(-5)*1000 + (classify(0)+1)*100 + classify(5)*10 + classify(50);
  printf(\"%d %d %d %d %d\\n\", c,h,w,uc,cl);
  return (c==-3 && h==-300 && w==-100000 && uc==200 && cl==-888)?0:1;
}
")))
    (should (equal "-3 -300 -100000 200 -888" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-unsigned-shift-and-compare-e2e ()
  "Unsigned 64-bit operators behave as unsigned even when bit 63 is set:
`>>' is logical (zero-fill, not sign-extending `sar'), and the relational
operators order operands as unsigned.  This is the codegen the real SQLite
varint codec relies on (`v>>=7' / `v<=0x7f' on a `u64')."
  (unless (nelisp-cfront-narrowint-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-narrowint-test--run "
typedef unsigned long u64;
u64 ushr(u64 v){ return v >> 7; }                 /* logical shift */
u64 ushr_assign(u64 v){ v >>= 60; return v; }     /* compound, logical */
int ule(u64 v){ return v <= 0x7f; }               /* unsigned compare */
int ult(u64 a, u64 b){ return a < b; }
int sshr_ok(long v){ return (v >> 4) == -8; }     /* signed >> stays arithmetic */
" "
#include <stdio.h>
typedef unsigned long u64;
extern u64 ushr(u64); extern u64 ushr_assign(u64);
extern int ule(u64); extern int ult(u64,u64); extern int sshr_ok(long);
int main(void){
  u64 big = 0xfedcba9876543210UL;
  int ok = (ushr(big)==(big>>7))
        && (ushr_assign(0x8000000000000000UL)==8)
        && (ule(0x8000000000000000UL)==0) && (ule(0x7f)==1)
        && (ult(0x8000000000000000UL,1)==0) && (ult(1,2)==1)
        && (sshr_ok(-128)==1);
  printf(\"ushr=0x%lx ule_hi=%d ult_hi=%d %s\\n\",
         ushr(big), ule(0x8000000000000000UL), ult(0x8000000000000000UL,1),
         ok?\"OK\":\"FAIL\");
  return ok?0:1;
}
")))
    (should (equal "ushr=0x1fdb97530eca864 ule_hi=0 ult_hi=0 OK" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-narrowint-test)

;;; nelisp-cfront-narrowint-test.el ends here
