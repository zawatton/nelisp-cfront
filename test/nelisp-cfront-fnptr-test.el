;;; nelisp-cfront-fnptr-test.el --- M4 function pointers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M4 — function pointers: a function name used as a value decays to its
;; address (grammar `addr-of'); a call through a pointer value lowers to
;; an indirect call (grammar `call-ptr').  Covers internal dispatch and
;; passing a function pointer across the C boundary.  The `ret (*fp)(args)'
;; declarator syntax is not parsed yet; declare the pointer as void*/long*.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-e2e-test)

(ert-deftest nelisp-cfront-fnptr-dispatch-and-callback ()
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
long add(long a, long b){ return a + b; }
long mul(long a, long b){ return a * b; }
long dispatch(long op, long a, long b){
  void *fp; if (op == 0) fp = add; else fp = mul; return fp(a, b);
}
long apply2(void *fp, long a, long b){ return fp(a, b); }
void *get_add(void){ return add; }
void *get_mul(void){ return mul; }
")
         (drv "
#include <stdio.h>
extern long dispatch(long,long,long); extern long apply2(void*,long,long);
extern void *get_add(void); extern void *get_mul(void);
int main(void){
  long d0=dispatch(0,3,4), d1=dispatch(1,3,4), a=apply2(get_add(),5,6), m=apply2(get_mul(),5,6);
  printf(\"%ld %ld %ld %ld\\n\", d0, d1, a, m);
  return (d0==7 && d1==12 && a==11 && m==30) ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "7 12 11 30" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-fnptr-test)

;;; nelisp-cfront-fnptr-test.el ends here
