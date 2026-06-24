;;; nelisp-cfront-float-test.el --- end-to-end soft-float -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 float lowering — end-to-end ERT for the "soft-float in the i64
;; world" scheme: a C `double' is carried as its IEEE-754 i64 bit
;; pattern, so nested float expressions, double locals, mixed int/float
;; operands, conversions, and comparisons all lower onto the gp-class
;; machinery via per-object `nelisp_cfront__d*' helpers (which use the
;; upstream `f64-bits' keystone op).
;;
;; The cfront `double' ABI returns/receives bits in rax (= gp), so the C
;; driver passes/reads them as `long' + memcpy.  Skips (not fails) when
;; the AOT backend or cc are unavailable.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-float)

(defun nelisp-cfront-float-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-float-test--run (csource driver-c)
  "Compile CSOURCE, link with DRIVER-C, run; return (cons EXIT STDOUT)."
  (let* ((dir (make-temp-file "nlcf-float" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "float e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(defconst nelisp-cfront-float-test--driver "
#include <stdio.h>
#include <string.h>
extern long poly(long);
extern long mix(long);
extern long cmp(long,long);
extern long trunc1(long);
extern long neg(long);
static long B(double d){long x;memcpy(&x,&d,8);return x;}
static double U(long x){double d;memcpy(&d,&x,8);return d;}
int main(void){
  double p = U(poly(B(3.0)));     /* 2*9 + 3.5*3 + 1 = 29.5 */
  double m = U(mix(5));            /* 0+1+2+3+4 = 10.0 */
  long c1 = cmp(B(1.0),B(2.0));    /* -1 */
  long c2 = cmp(B(2.0),B(2.0));    /* 0  */
  long t  = trunc1(B(3.9));        /* 3  */
  double ng = U(neg(B(2.5)));      /* -2.5 */
  printf(\"%g %g %ld %ld %ld %g\\n\", p, m, c1, c2, t, ng);
  return (p==29.5 && m==10.0 && c1==-1 && c2==0 && t==3 && ng==-2.5)?0:1;
}
")

(ert-deftest nelisp-cfront-float-soft-float-e2e ()
  "Nested float arithmetic, double locals, int<->double conversion,
compound assignment, comparisons, and unary minus — all native."
  (unless (nelisp-cfront-float-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-float-test--run "
double poly(double x){
  double a = 2.0;
  double b = 3.5;
  return a*x*x + b*x + 1.0;
}
double mix(long n){
  double s = 0.0;
  for (long i = 0; i < n; i = i + 1) s += (double)i;
  return s;
}
long cmp(double a, double b){
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
}
long trunc1(double x){ return (long)x; }
double neg(double x){ return -x; }
"
                                              nelisp-cfront-float-test--driver)))
    (should (equal "29.5 10 -1 0 3 -2.5" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-float-arg-coercion-e2e ()
  "Call arguments are coerced to the callee's param class: an int arg to
a `double' param is lifted to double-bits (i2d) before the call."
  (unless (nelisp-cfront-float-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-float-test--run "
double scale(double x, double factor){ return x * factor; }
double half(double x){ return scale(x, 0.5); }
double fromint(int n){ return scale(n, 2.0); }
long trunc_scaled(double x){ return (long)scale(x, 3); }
" "
#include <stdio.h>
#include <string.h>
extern long half(long);
extern long fromint(long);
extern long trunc_scaled(long);
static long B(double d){long x;memcpy(&x,&d,8);return x;}
static double U(long x){double d;memcpy(&d,&x,8);return d;}
int main(void){
  double h = U(half(B(10.0)));    /* 5.0 */
  double fi = U(fromint(7));       /* scale(7.0,2.0)=14.0 (int arg coerced) */
  long ts = trunc_scaled(B(2.5));  /* (long)(2.5*3.0)=7 (int literal coerced) */
  printf(\"%g %g %ld\\n\", h, fi, ts);
  return (h==5.0 && fi==14.0 && ts==7)?0:1;
}
")))
    (should (equal "5 14 7" (cdr res)))
    (should (= 0 (car res)))))

(ert-deftest nelisp-cfront-float-double-to-bits-exact ()
  "The IEEE-754 encoder is bit-exact for representative literals."
  (should (= (nelisp-cfront-float--double-to-bits 1.5)
             #x3ff8000000000000))
  (should (= (nelisp-cfront-float--double-to-bits 0.0) 0))
  (should (= (nelisp-cfront-float--double-to-bits 42.0)
             #x4045000000000000))
  ;; 3.14 = 0x40091eb851eb851f, < 2^63 so it stays a positive i64
  (should (= (nelisp-cfront-float--double-to-bits 3.14)
             #x40091eb851eb851f))
  ;; -1.0 = 0xbff0000000000000 (signed)
  (should (= (nelisp-cfront-float--double-to-bits -1.0)
             (- #xbff0000000000000 (ash 1 64)))))

(ert-deftest nelisp-cfront-float-extern-libm-e2e ()
  "cfront can CALL a standard-ABI extern `double' function (libm).  cfront
carries a `double' as i64 bits in a gp slot, so a `double' extern argument
is bridged into the xmm register with `bits-to-f64' (MOVQ) and a `double'
return is bridged back with `f64-bits' (MOVQ xmm0 -> rax).  Exercises a
single f64 arg (`sqrt'/`sin'), two f64 args (`pow'), a mixed f64+int call
(`ldexp'), and an f64 expression feeding an extern arg (`a*a+b*b').  The
driver bit-casts across cfront's gp-bits `double' ABI (B/U) and links -lm."
  (skip-unless (nelisp-cfront-float-test--available-p))
  (let* ((dir (make-temp-file "nlcf-libm" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string "
extern double sqrt(double);
extern double pow(double, double);
extern double sin(double);
extern double ldexp(double, int);
double my_sqrt(double x){ return sqrt(x); }
double my_pow(double a, double b){ return pow(a, b); }
double my_hypot(double a, double b){ return sqrt(a*a + b*b); }
double my_ldexp(double x, int e){ return ldexp(x, e); }
double my_sin(double x){ return sin(x); }
" obj)
          (with-temp-file cdrv (insert "
#include <stdio.h>
#include <string.h>
#include <math.h>
static long B(double d){ long x; memcpy(&x,&d,8); return x; }
static double U(long x){ double d; memcpy(&d,&x,8); return d; }
extern long my_sqrt(long), my_pow(long,long), my_hypot(long,long),
            my_ldexp(long,int), my_sin(long);
int main(void){
  double s  = U(my_sqrt(B(2.0)));
  double p  = U(my_pow(B(2.0), B(10.0)));
  double h  = U(my_hypot(B(3.0), B(4.0)));
  double l  = U(my_ldexp(B(2.0), 3));
  double sn = U(my_sin(B(1.0)));
  int ok = (fabs(s-sqrt(2.0))<1e-12) && (fabs(p-1024.0)<1e-9)
        && (fabs(h-5.0)<1e-12) && (fabs(l-16.0)<1e-12)
        && (fabs(sn-sin(1.0))<1e-12);
  printf(\"%.6f|%.1f|%.1f|%.1f|%.6f|%s\\n\", s, p, h, l, sn, ok?\"OK\":\"FAIL\");
  return ok?0:1;
}
"))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin "-lm"))
              (error "libm e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (should (= 0 rc))
                (should (equal "1.414214|1024.0|5.0|16.0|0.841471|OK"
                               (string-trim (buffer-string))))))))
      (ignore-errors (delete-directory dir t)))))

(provide 'nelisp-cfront-float-test)

;;; nelisp-cfront-float-test.el ends here
