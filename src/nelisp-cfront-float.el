;;; nelisp-cfront-float.el --- Soft-float-in-i64 lowering support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 float lowering — the "soft-float in the i64 world" scheme.
;;
;; The upstream nelisp-cc f64 grammar is MVP "flat-only": f64-binop
;; operands must be leaves (no nesting — Doc 112 / xmm spill not yet
;; landed), there is no f64-class `let' slot, and SysV defuns require a
;; uniform param class (= no mixed int/double params).  None of that is
;; enough for real C double code (nested expressions, double locals,
;; mixed params).
;;
;; Instead we carry a C `double'/`float' value as its raw IEEE-754
;; 64-bit pattern in a *gp* (i64) register/slot — exactly the value
;; class cfront already supports everywhere.  Each float operation
;; lowers to a flat per-object helper call:
;;
;;   a + b   ->  (nelisp_cfront__dadd A-bits B-bits)   ; returns bits
;;   a < b   ->  (nelisp_cfront__dlt  A-bits B-bits)   ; returns i64 0/1
;;   (double)i  ->  (nelisp_cfront__i2d I)             ; int  -> bits
;;   (int)d     ->  (nelisp_cfront__d2i D-bits)        ; bits -> int (trunc)
;;
;; Each helper is one flat f64 op wrapped in `bits-to-f64' / `f64-bits'
;; (the keystone op added to nelisp-aot-compiler.el), so nesting,
;; locals, and mixed params all "just work" because every double is an
;; ordinary i64 the rest of the pipeline already handles.
;;
;; ABI note: a `double'-returning cfront function returns the *bits in
;; rax*, not an xmm0 double.  This is internally consistent for
;; cfront-compiled call graphs (= SQLite's own float math).  Interop
;; with external SysV `double' ABI (libm results, external callers)
;; needs a thin `extern-call-f64' + `f64-bits' shim — a documented
;; follow-on, not needed to compile SQLite's internal float code.

;;; Code:

(defconst nelisp-cfront-float--sign-bit -9223372036854775808
  "IEEE-754 double sign bit as a signed i64 (= 0x8000000000000000).")

(defun nelisp-cfront-float-type-p (ty)
  "Non-nil when TY is a scalar (non-pointer) `float'/`double'."
  (and (= 0 (or (plist-get ty :ptr) 0))
       (memq (plist-get ty :base) '(float double))))

(defun nelisp-cfront-float--double-to-bits (x)
  "Encode double X to its IEEE-754 64-bit pattern as a SIGNED i64.
Bit-exact with the platform C compiler's parse of the same literal
\(both go through strtod / the same IEEE round-to-nearest)."
  (let ((bits
         (cond
          ((= x 0.0)
           (if (< (/ 1.0 x) 0) (ash 1 63) 0))     ; -0.0 vs +0.0
          ((/= x x) #x7FF8000000000000)            ; NaN
          (t
           (let* ((sign (if (< x 0) 1 0))
                  (ax (abs x))
                  (fe (frexp ax))                  ; ax = m * 2^e, 0.5<=m<1
                  (m (car fe)) (e (cdr fe))
                  (biased (+ e 1022))              ; unbiased = e-1, +1023
                  (frac (- (* 2.0 m) 1.0))         ; in [0,1)
                  (mant (round (* frac (expt 2.0 52)))))
             (when (= mant (ash 1 52))             ; mantissa rounding carry
               (setq mant 0 biased (1+ biased)))
             (cond
              ((>= biased 2047)                    ; overflow -> +/-inf
               (logior (ash sign 63) #x7FF0000000000000))
              ((<= biased 0)                       ; underflow -> +/-0 (MVP)
               (ash sign 63))
              (t (logior (ash sign 63) (ash biased 52) mant))))))))
    (if (>= bits (ash 1 63)) (- bits (ash 1 64)) bits)))

;;; --- per-object helper defuns (emitted once when a program uses float) ---

(defconst nelisp-cfront-float--helper-names
  '(nelisp_cfront__dadd nelisp_cfront__dsub nelisp_cfront__dmul
    nelisp_cfront__ddiv nelisp_cfront__dlt nelisp_cfront__dgt
    nelisp_cfront__dle nelisp_cfront__dge nelisp_cfront__deq
    nelisp_cfront__i2d nelisp_cfront__d2i)
  "All soft-float helper function symbols.")

(defun nelisp-cfront-float-helper-defuns ()
  "Return the list of per-object soft-float helper `(defun ...)' forms.
All take/return i64 bit patterns (= the cfront `double' representation),
so they compose with the gp-class machinery used everywhere else."
  (list
   ;; arithmetic: bits, bits -> bits  (one flat f64 binop + MOVQ rax,xmm0)
   '(defun nelisp_cfront__dadd (a b)
      (f64-bits (f64-add (bits-to-f64 a) (bits-to-f64 b))))
   '(defun nelisp_cfront__dsub (a b)
      (f64-bits (f64-sub (bits-to-f64 a) (bits-to-f64 b))))
   '(defun nelisp_cfront__dmul (a b)
      (f64-bits (f64-mul (bits-to-f64 a) (bits-to-f64 b))))
   '(defun nelisp_cfront__ddiv (a b)
      (f64-bits (f64-div (bits-to-f64 a) (bits-to-f64 b))))
   ;; ordered comparisons: bits, bits -> i64 0/1 (NaN -> 0, matches C)
   '(defun nelisp_cfront__dlt (a b)
      (f64-lt (bits-to-f64 a) (bits-to-f64 b)))
   '(defun nelisp_cfront__dgt (a b)
      (f64-gt (bits-to-f64 a) (bits-to-f64 b)))
   '(defun nelisp_cfront__dle (a b)
      (f64-le (bits-to-f64 a) (bits-to-f64 b)))
   '(defun nelisp_cfront__dge (a b)
      (f64-ge (bits-to-f64 a) (bits-to-f64 b)))
   ;; exact equality via (a<=b) & (a>=b): true iff ordered-equal, NaN -> 0,
   ;; and -0.0 == +0.0 (matches C `==' semantics; f64-eq-eps would not).
   '(defun nelisp_cfront__deq (a b)
      (logand (nelisp_cfront__dle a b) (nelisp_cfront__dge a b)))
   ;; conversions
   '(defun nelisp_cfront__i2d (x)
      (f64-bits (i64-to-f64 x)))            ; signed int -> double bits
   '(defun nelisp_cfront__d2i (a)
      (f64-to-i64-trunc (bits-to-f64 a))))) ; double bits -> int (truncate)

(provide 'nelisp-cfront-float)

;;; nelisp-cfront-float.el ends here
