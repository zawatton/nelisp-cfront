;;; nelisp-cfront.el --- C front-end: lower C onto the nelisp-cc grammar -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; nelisp-cfront is the C *front-end* of the NeLisp toolchain.  It lowers
;; C onto the `nelisp-cc' grammar (the s-expression systems language that
;; `nelisp' AOT-compiles to native x86_64/arm64).
;;
;;   C source -> [front-end] -> C-AST -> [lowering] -> nelisp-cc grammar
;;            -> nelisp-aot-compile-to-object -> .o -> link -> native
;;
;; STATUS (2026-06-22): feasibility spike.  The C99 front-end (lex /
;; parse / typecheck) is DEFERRED.  The spike hand-lowers C to grammar
;; sexps so it measures the *semantic/runtime fit* (the unknown) rather
;; than the *parser* (known-doable).  This file therefore ships the
;; lowering-side helpers — small constructors that build verified
;; `nelisp-cc' grammar forms — plus stubbed front-end entry points.
;;
;; Every grammar op used here is verified against the nelisp source:
;;   - ptr-read/write-u8/u64   : lisp/nelisp-cc-atomic-raw-mem.el
;;   - syscall-direct          : lisp/nelisp-cc-alloc-mem.el
;;   - nl_mmap_alloc/dealloc   : lisp/nelisp-cc-alloc-mem.el
;;   - while                   : lisp/nelisp-cc-sf-while.el
;;   - syscall nr table        : lisp/nelisp-syscall-table.el
;; Do NOT add a constructor for an op you have not confirmed exists.

;;; Code:

(require 'cl-lib)

(defgroup nelisp-cfront nil
  "C front-end lowering onto the nelisp-cc grammar."
  :group 'nelisp)

(defconst nelisp-cfront-version "0.0.1-spike"
  "Version of the nelisp-cfront feasibility spike.")

;;; --- Lowering helpers: C constructs -> nelisp-cc grammar sexp ---------
;;
;; These return grammar S-expressions (data), not evaluated code.  The
;; spike stages compose them by hand; a future front-end emits them from
;; a typed C-AST.

;; Raw memory (a C pointer is an i64 byte address; offset is the field /
;; index byte offset that the front-end computes from struct layout).

(defun nelisp-cfront-lower-load (ptr offset width)
  "Lower a C load `*(WIDTH*)(PTR + OFFSET)' to a grammar form.
WIDTH is 1 or 8 (the natively-confirmed `ptr-read-u8' / `ptr-read-u64').
Narrower widths (2/4) are NOT yet a single grammar op — that gap is a
Stage 2 research item; signal so callers do not silently miscompile."
  (pcase width
    (1 `(ptr-read-u8  ,ptr ,offset))
    (8 `(ptr-read-u64 ,ptr ,offset))
    (_ (error "nelisp-cfront: load width %S has no confirmed grammar op (Stage 2 gap)" width))))

(defun nelisp-cfront-lower-store (ptr offset width val)
  "Lower a C store `*(WIDTH*)(PTR + OFFSET) = VAL' to a grammar form.
WIDTH is 1 or 8.  See `nelisp-cfront-lower-load' for the width gap."
  (pcase width
    (1 `(ptr-write-u8  ,ptr ,offset ,val))
    (8 `(ptr-write-u64 ,ptr ,offset ,val))
    (_ (error "nelisp-cfront: store width %S has no confirmed grammar op (Stage 2 gap)" width))))

;; Narrow integer truncation: C assignment to a sub-64-bit type masks to
;; the type width.  Composed from `logand' (confirmed arith op).
(defun nelisp-cfront-lower-trunc (expr bits)
  "Wrap EXPR so only its low BITS are kept (C narrow-int assignment)."
  (cl-assert (and (integerp bits) (> bits 0) (<= bits 64)))
  (if (= bits 64)
      expr
    `(logand ,expr ,(1- (ash 1 bits)))))

;; Heap: malloc/free -> mmap allocator already built in the grammar.
(defun nelisp-cfront-lower-malloc (size-expr &optional align)
  "Lower C `malloc(SIZE-EXPR)' to the grammar mmap allocator call."
  `(nl_mmap_alloc ,size-expr ,(or align 8)))

(defun nelisp-cfront-lower-free (ptr-expr size-expr &optional align)
  "Lower C `free(PTR-EXPR)' (size-tracked) to the grammar mmap dealloc."
  `(nl_mmap_dealloc ,ptr-expr ,size-expr ,(or align 8)))

;; Direct syscall (the libc/VFS bottom).  NR may be an integer or a
;; symbol resolvable via nelisp's syscall table at lowering time.
(defun nelisp-cfront-lower-syscall (nr &rest args)
  "Lower a syscall to `(syscall-direct NR a0..a5)', zero-padding ARGS."
  (let ((a (append args (make-list (max 0 (- 6 (length args))) 0))))
    (when (> (length a) 6)
      (error "nelisp-cfront: syscall takes at most 6 args, got %d" (length args)))
    `(syscall-direct ,nr ,@(seq-take a 6))))

;;; --- Front-end modules -----------------------------------------------
;;
;; The C front-end lives in dedicated modules, loaded on demand by the
;; driver (M2.5):
;;   - `nelisp-cfront-lex'   (M2.1) — tokenizer
;;   - `nelisp-cfront-parse' (M2.2) — recursive-descent parser → AST
;;   - lowering (M2.4) consumes the AST and emits grammar via the
;;     `nelisp-cfront-lower-*' helpers above.
;; They are intentionally NOT required here so this file stays a small,
;; dependency-light home for the lowering helpers.

(provide 'nelisp-cfront)

;;; nelisp-cfront.el ends here
