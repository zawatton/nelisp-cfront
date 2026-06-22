;;; nelisp-cfront-test.el --- ERT for nelisp-cfront lowering helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT for the pure lowering helpers (C construct -> nelisp-cc grammar
;; sexp).  These need no nelisp toolchain: they assert the *shape* of the
;; emitted grammar.  End-to-end compile/run is exercised by the spike
;; harness (spike/stage0-harness.el), which does require nelisp.

;;; Code:

(require 'ert)
(require 'nelisp-cfront)

;;; --- load / store ----------------------------------------------------

(ert-deftest nelisp-cfront-load-u8 ()
  (should (equal '(ptr-read-u8 p 0) (nelisp-cfront-lower-load 'p 0 1))))

(ert-deftest nelisp-cfront-load-u64 ()
  (should (equal '(ptr-read-u64 p 8) (nelisp-cfront-lower-load 'p 8 8))))

(ert-deftest nelisp-cfront-store-u8 ()
  (should (equal '(ptr-write-u8 p 0 v) (nelisp-cfront-lower-store 'p 0 1 'v))))

(ert-deftest nelisp-cfront-store-u64 ()
  (should (equal '(ptr-write-u64 p 8 99) (nelisp-cfront-lower-store 'p 8 8 99))))

(ert-deftest nelisp-cfront-load-unconfirmed-width-signals ()
  "u16/u32 have no single confirmed grammar op yet — must signal, not miscompile."
  (should-error (nelisp-cfront-lower-load 'p 0 2))
  (should-error (nelisp-cfront-lower-load 'p 0 4)))

;;; --- narrow-int truncation ------------------------------------------

(ert-deftest nelisp-cfront-trunc-u8 ()
  (should (equal '(logand x 255) (nelisp-cfront-lower-trunc 'x 8))))

(ert-deftest nelisp-cfront-trunc-u32 ()
  (should (equal '(logand x 4294967295) (nelisp-cfront-lower-trunc 'x 32))))

(ert-deftest nelisp-cfront-trunc-64-is-identity ()
  (should (equal 'x (nelisp-cfront-lower-trunc 'x 64))))

;;; --- heap ------------------------------------------------------------

(ert-deftest nelisp-cfront-malloc ()
  (should (equal '(nl_mmap_alloc 16 8) (nelisp-cfront-lower-malloc 16)))
  (should (equal '(nl_mmap_alloc n 16) (nelisp-cfront-lower-malloc 'n 16))))

(ert-deftest nelisp-cfront-free ()
  (should (equal '(nl_mmap_dealloc p 16 8) (nelisp-cfront-lower-free 'p 16))))

;;; --- syscall ---------------------------------------------------------

(ert-deftest nelisp-cfront-syscall-pads-to-6 ()
  "write(1, buf, 3) -> (syscall-direct 1 1 buf 3 0 0 0)."
  (should (equal '(syscall-direct 1 1 buf 3 0 0 0)
                 (nelisp-cfront-lower-syscall 1 1 'buf 3))))

(ert-deftest nelisp-cfront-syscall-too-many-args-signals ()
  (should-error (nelisp-cfront-lower-syscall 1 0 1 2 3 4 5 6)))

(provide 'nelisp-cfront-test)

;;; nelisp-cfront-test.el ends here
