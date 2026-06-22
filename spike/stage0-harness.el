;;; stage0-harness.el --- Stage 0: compile -> link -> run round-trip probe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 0 of the nelisp-cfront feasibility spike (Doc 01 §Stage 0).
;;
;; Proves the round-trip, with NO cargo/Rust in the run path:
;;
;;   hand-written nelisp-cc grammar source
;;     -> nelisp-aot-compile-to-object  (nelisp)  -> ET_REL .o
;;     -> cc links it with a tiny C driver         -> native binary
;;     -> running the binary returns the expected value
;;
;; This establishes that nelisp-cfront can drive the nelisp AOT toolchain
;; end-to-end before any C semantics are involved.
;;
;; nelisp API (verified 2026-06-22, nelisp/lisp/nelisp-aot-compiler.el):
;;   (nelisp-aot-compile-to-object SEXP FILE-PATH &key (arch 'x86_64) (format 'elf))
;;   - SEXP is the FIRST arg; FILE-PATH the second.
;;   - Each defun becomes a GLOBAL STT_FUNC, C-callable via System V ABI
;;     (args in rdi/rsi/..., i64 return in rax).  Bodies must not contain
;;     strings (the v1 object mode forbids `write' / rodata).

;;; Code:

(require 'nelisp-cfront)

;; Captured at LOAD time: under `make stage0' the function runs via
;; `--eval', where `load-file-name'/`buffer-file-name' are nil, so the
;; out dir must be anchored to this file's location now, not at call time.
(defconst nelisp-cfront-stage0--this-file
  (or load-file-name buffer-file-name
      (expand-file-name "spike/stage0-harness.el"))
  "Absolute path of this harness file, captured at load time.")

;; The probe source: a leaf i64 add (no allocation, no GC roots, no
;; strings) — the simplest function that exercises params + arith.
(defconst nelisp-cfront-stage0--source
  '(defun nelisp_cfront_stage0_add (a b) (+ a b))
  "Stage 0 probe: the simplest grammar defun the AOT path can consume.")

(defconst nelisp-cfront-stage0--sym "nelisp_cfront_stage0_add"
  "C linkage name of the probe (underscores preserved by the AOT path).")

(defun nelisp-cfront-stage0--dir ()
  "Absolute path of the spike/out directory (created on demand)."
  (let ((d (expand-file-name
            "out"
            (file-name-directory nelisp-cfront-stage0--this-file))))
    (make-directory d t)
    d))

(defun nelisp-cfront-stage0-run ()
  "Run the Stage 0 compile -> link -> run round-trip.
Returns t and messages PASS on success; signals on any failure so
`make stage0' exits non-zero."
  (let* ((out  (nelisp-cfront-stage0--dir))
         (obj  (expand-file-name "stage0_add.o" out))
         (csrc (expand-file-name "stage0_driver.c" out))
         (bin  (expand-file-name "stage0" out)))
    ;; 1. compile grammar source -> .o via nelisp
    (unless (require 'nelisp-aot-compiler nil t)
      (error "[stage0] cannot load nelisp-aot-compiler — set NELISP_REPO_ROOT (got %s)"
             (or (getenv "NELISP_REPO_ROOT") "<unset>")))
    (message "[stage0] compiling %S -> %s" nelisp-cfront-stage0--source obj)
    (nelisp-aot-compile-to-object nelisp-cfront-stage0--source obj
                                  :arch 'x86_64 :format 'elf)
    (unless (file-exists-p obj)
      (error "[stage0] AOT did not produce %s" obj))
    ;; 2. emit a tiny C driver that calls the symbol and checks the result
    (with-temp-file csrc
      (insert (format "#include <stdio.h>\n")
              (format "extern long %s(long, long);\n" nelisp-cfront-stage0--sym)
              "int main(void){\n"
              (format "  long r = %s(3, 4);\n" nelisp-cfront-stage0--sym)
              "  printf(\"add(3,4) = %ld\\n\", r);\n"
              "  return (r == 7) ? 0 : 1;\n"
              "}\n"))
    ;; 3. link with cc
    (let ((cc (or (executable-find "cc") (executable-find "gcc")
                  (error "[stage0] no cc/gcc on PATH"))))
      (message "[stage0] linking with %s" cc)
      (let ((rc (call-process cc nil nil nil csrc obj "-o" bin)))
        (unless (zerop rc)
          (error "[stage0] link failed (cc rc=%d)" rc))))
    ;; 4. run the native binary; capture stdout + exit code
    (with-temp-buffer
      (let ((rc (call-process bin nil t nil)))
        (let ((out-str (string-trim (buffer-string))))
          (message "[stage0] %s" out-str)
          (unless (zerop rc)
            (error "[stage0] FAIL: native run returned %d (expected add(3,4)=7)" rc))
          (message "[stage0] PASS — round-trip C-callable native code returned 7")
          t)))))

(provide 'stage0-harness)

;;; stage0-harness.el ends here
