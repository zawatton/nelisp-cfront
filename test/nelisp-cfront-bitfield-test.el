;;; nelisp-cfront-bitfield-test.el --- M4 bitfields -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M4 — bitfields: `unsigned NAME : WIDTH' packed LSB-first into 4-byte
;; units (no straddling).  Read = (unit >> bit-offset) & mask; write =
;; clear+OR.  The driver reads the same struct via gcc's own bitfield
;; access to confirm the layout is byte-compatible.  Skips if backend/cc
;; unavailable.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-e2e-test)

(ert-deftest nelisp-cfront-bitfield-pack ()
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
struct Flags { unsigned a : 4; unsigned b : 4; unsigned c : 8; unsigned d : 16; };
void setf(struct Flags *f, long a, long b, long c, long d){ f->a=a; f->b=b; f->c=c; f->d=d; }
long getf(struct Flags *f){ return f->a + f->b + f->c + f->d; }
long geta(struct Flags *f){ return f->a; }
")
         (drv "
#include <stdio.h>
struct Flags { unsigned a:4; unsigned b:4; unsigned c:8; unsigned d:16; };
extern void setf(struct Flags*,long,long,long,long);
extern long getf(struct Flags*); extern long geta(struct Flags*);
int main(void){
  struct Flags f; setf(&f, 5, 10, 200, 40000);
  printf(\"%ld %ld %u %u %u %u %zu\\n\",
         geta(&f), getf(&f), f.a, f.b, f.c, f.d, sizeof(struct Flags));
  return (geta(&f)==5 && getf(&f)==40215
          && f.a==5 && f.b==10 && f.c==200 && f.d==40000
          && sizeof(struct Flags)==4) ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "5 40215 5 10 200 40000 4" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-bitfield-test)

;;; nelisp-cfront-bitfield-test.el ends here
