;;; nelisp-cfront-type.el --- C type sizing, struct layout, inference -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M2.3 — type sizes, struct field layout (offset/size + alignment), and
;; a light expression type-inference used by the lowering to pick the
;; right memory width for `p->f', `*p', `a[i]', and to scale pointer
;; arithmetic by the pointee size.
;;
;; A type is the parser's plist: (:base SYM :ptr N [:unsigned t]
;; [:struct NAME] [:fields ...] [:array SZ]).  BASE in
;; (void char short int long struct).  An LP64 model is assumed
;; (char1 short2 int4 long8 ptr8).
;;
;; A struct table maps "NAME" -> (:size N :align A :fields ALIST) where
;; ALIST is ((FNAME . (:type TY :offset O :size S)) ...).

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-cfront-type-error "nelisp-cfront type error")

(defun nelisp-cfront-type--scalar-size (base)
  (pcase base
    ('char 1) ('short 2) ('int 4) ('long 8) ('void 1)
    ('float 4) ('double 8)
    (_ (signal 'nelisp-cfront-type-error (list :unknown-base base)))))

(defun nelisp-cfront-type-size (ty structs)
  "Byte size of type TY given the STRUCTS table."
  (let ((ptr (or (plist-get ty :ptr) 0))
        (arr (plist-get ty :array)))
    (cond
     ((and arr (> ptr 0)) 8)            ; array of pointers element handled elsewhere
     (arr (* (nelisp-cfront-type-size
              (nelisp-cfront-type--strip-array ty) structs)
             (nelisp-cfront-type--const arr)))
     ((> ptr 0) 8)
     ((eq (plist-get ty :base) 'struct)
      (let ((s (nelisp-cfront-type-struct (plist-get ty :struct) structs)))
        (plist-get s :size)))
     (t (nelisp-cfront-type--scalar-size (plist-get ty :base))))))

(defun nelisp-cfront-type-align (ty structs)
  "Alignment of TY."
  (let ((ptr (or (plist-get ty :ptr) 0)))
    (cond
     ((> ptr 0) 8)
     ((plist-get ty :array)
      (nelisp-cfront-type-align (nelisp-cfront-type--strip-array ty) structs))
     ((eq (plist-get ty :base) 'struct)
      (plist-get (nelisp-cfront-type-struct (plist-get ty :struct) structs) :align))
     (t (nelisp-cfront-type--scalar-size (plist-get ty :base))))))

(defun nelisp-cfront-type--strip-array (ty)
  (let ((out nil) (p ty))
    (while p
      (unless (eq (car p) :array) (setq out (cons (car p) (cons (cadr p) out))))
      (setq p (cddr p)))
    (nreverse out)))

(defun nelisp-cfront-type--const (expr)
  "Evaluate a constant array-size EXPR (MVP: int literal only)."
  (if (and (consp expr) (eq (car expr) 'int)) (nth 1 expr)
    (signal 'nelisp-cfront-type-error (list :non-constant-array-size expr))))

(defun nelisp-cfront-type--round-up (n a) (* (/ (+ n a -1) a) a))

(defun nelisp-cfront-type-layout (fields structs &optional union-p)
  "Compute layout for struct/union FIELDS (a list of (field TY NAME BITS)).
When UNION-P, every field is at offset 0 and size = max field size.
Bitfields (BITS non-nil) pack LSB-first into 4-byte units (no straddling).
Returns (:size N :align A :fields ALIST)."
  (let ((off 0) (align 1) (maxsz 0) (alist nil)
        (bf-unit nil) (bf-cursor 0))
    (dolist (f fields)
      (let* ((ty (nth 1 f)) (name (nth 2 f)) (bits (nth 3 f)))
        (cond
         (bits                          ; --- bitfield ---
          (when (or (null bf-unit) (> (+ bf-cursor bits) 32))
            (setq off (nelisp-cfront-type--round-up off 4)
                  bf-unit off off (+ off 4) bf-cursor 0 align (max align 4)))
          (push (cons name (list :type ty :offset bf-unit :size 4
                                 :bit-offset bf-cursor :bits bits)) alist)
          (setq bf-cursor (+ bf-cursor bits)))
         (union-p                       ; --- union member ---
          (setq bf-unit nil bf-cursor 0)
          (let ((sz (nelisp-cfront-type-size ty structs)))
            (push (cons name (list :type ty :offset 0 :size sz)) alist)
            (setq maxsz (max maxsz sz)
                  align (max align (nelisp-cfront-type-align ty structs)))))
         (t                             ; --- normal struct member ---
          (setq bf-unit nil bf-cursor 0)
          (let ((sz (nelisp-cfront-type-size ty structs))
                (al (nelisp-cfront-type-align ty structs)))
            (setq off (nelisp-cfront-type--round-up off al))
            (push (cons name (list :type ty :offset off :size sz)) alist)
            (setq off (+ off sz) align (max align al)))))))
    (list :size (nelisp-cfront-type--round-up (if union-p maxsz off) align)
          :align align
          :fields (nreverse alist))))

(defun nelisp-cfront-type-build-structs (program)
  "Build the struct table from PROGRAM `(program TOP...)'.
Also scans inline `:fields' on struct types in params/decls."
  (let ((structs nil))
    (dolist (top (cdr program))
      (pcase (car top)
        ('struct-def
         (let ((name (nth 1 top)) (fields (nth 2 top)) (union-p (nth 3 top)))
           (when (and name fields)
             (push (cons name (nelisp-cfront-type-layout fields structs union-p)) structs))))
        ('typedef
         (let* ((ty (nth 2 top)) (name (plist-get ty :struct))
                (fields (plist-get ty :fields)) (union-p (plist-get ty :union)))
           (when (and name fields)
             (push (cons name (nelisp-cfront-type-layout fields structs union-p)) structs))))
        (_ nil)))
    structs))

(defun nelisp-cfront-type-struct (name structs)
  (or (cdr (assoc name structs))
      (signal 'nelisp-cfront-type-error (list :unknown-struct name))))

(defun nelisp-cfront-type-field (struct-name field structs)
  "Return (:type TY :offset O :size S) for FIELD of STRUCT-NAME."
  (or (cdr (assoc field (plist-get (nelisp-cfront-type-struct struct-name structs)
                                   :fields)))
      (signal 'nelisp-cfront-type-error (list :unknown-field struct-name field))))

;;; --- type plist helpers ---------------------------------------------

(defun nelisp-cfront-type-pointee (ty)
  "Type pointed to by pointer type TY (decrement :ptr)."
  (let ((ptr (or (plist-get ty :ptr) 0)))
    (when (= ptr 0)
      (signal 'nelisp-cfront-type-error (list :deref-non-pointer ty)))
    (plist-put (copy-sequence ty) :ptr (1- ptr))))

(defun nelisp-cfront-type-int () '(:base int :ptr 0))
(defun nelisp-cfront-type-long () '(:base long :ptr 0))
(defun nelisp-cfront-type-double () '(:base double :ptr 0))

(defun nelisp-cfront-type--float-p (ty)
  "Non-nil when TY is a scalar (non-pointer) float/double."
  (and (= 0 (or (plist-get ty :ptr) 0))
       (memq (plist-get ty :base) '(float double))))

;;; --- expression type inference --------------------------------------

(defun nelisp-cfront-type-of (expr tenv structs funcs)
  "Infer the C type of EXPR.
TENV is an alist NAME->type (params + locals); FUNCS is NAME->ret-type."
  (pcase (car expr)
    ('int (nelisp-cfront-type-long))
    ('str (list :base 'char :ptr 1))
    ('var (or (cdr (assoc (nth 1 expr) tenv)) (nelisp-cfront-type-long)))
    ('arrow
     (let* ((pty (nelisp-cfront-type-of (nth 1 expr) tenv structs funcs))
            (sname (plist-get pty :struct)))
       (plist-get (nelisp-cfront-type-field sname (nth 2 expr) structs) :type)))
    ('member
     (let* ((oty (nelisp-cfront-type-of (nth 1 expr) tenv structs funcs))
            (sname (plist-get oty :struct)))
       (plist-get (nelisp-cfront-type-field sname (nth 2 expr) structs) :type)))
    ('index
     (nelisp-cfront-type-pointee
      (nelisp-cfront-type-of (nth 1 expr) tenv structs funcs)))
    ('unop
     (pcase (nth 1 expr)
       ("*" (nelisp-cfront-type-pointee
             (nelisp-cfront-type-of (nth 2 expr) tenv structs funcs)))
       ("&" (let ((it (nelisp-cfront-type-of (nth 2 expr) tenv structs funcs)))
              (plist-put (copy-sequence it) :ptr (1+ (or (plist-get it :ptr) 0)))))
       ;; unary +/- preserve the operand type (so `-x' on a double stays
       ;; double); `!'/`~' yield integers.
       ((or "-" "+") (nelisp-cfront-type-of (nth 2 expr) tenv structs funcs))
       (_ (nelisp-cfront-type-long))))
    ('call
     (let ((fn (nth 1 expr)))
       (or (and (eq (car fn) 'var) (cdr (assoc (nth 1 fn) funcs)))
           (nelisp-cfront-type-long))))
    ('binop
     ;; pointer +/- integer keeps the pointer type; float arithmetic
     ;; yields double; comparisons and integer arithmetic yield long.
     (let* ((op (nth 1 expr))
            (lt (nelisp-cfront-type-of (nth 2 expr) tenv structs funcs))
            (rt (nelisp-cfront-type-of (nth 3 expr) tenv structs funcs)))
       (cond
        ((and (member op '("+" "-")) (> (or (plist-get lt :ptr) 0) 0)) lt)
        ((and (member op '("+" "-")) (> (or (plist-get rt :ptr) 0) 0)) rt)
        ((and (member op '("+" "-" "*" "/"))
              (or (nelisp-cfront-type--float-p lt)
                  (nelisp-cfront-type--float-p rt)))
         (nelisp-cfront-type-double))
        (t (nelisp-cfront-type-long)))))
    ('assign (nelisp-cfront-type-of (nth 2 expr) tenv structs funcs))
    ('ternary (nelisp-cfront-type-of (nth 2 expr) tenv structs funcs))
    ('cast (nth 1 expr))                ; a cast yields the cast-to type
    ((or 'sizeof 'sizeof-expr) (nelisp-cfront-type-long))
    ('va-arg (nth 2 expr))              ; va_arg(ap, TYPE) yields TYPE
    ('fnum '(:base double :ptr 0))
    ('comma (nelisp-cfront-type-of (nth 2 expr) tenv structs funcs))
    (_ (nelisp-cfront-type-long))))

(provide 'nelisp-cfront-type)

;;; nelisp-cfront-type.el ends here
