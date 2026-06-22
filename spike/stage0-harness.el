;;; stage0-harness.el --- Stage 0: compile -> link -> run round-trip probe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 0 of the nelisp-cfront feasibility spike (Doc 01 §Stage 0).
;;
;; Goal: prove the round-trip
;;
;;   hand-written nelisp-cc grammar source
;;     -> nelisp-aot-compile-to-object  (nelisp)
;;     -> ET_REL .o
;;     -> link
;;     -> native call returns the expected value
;;
;; This establishes that nelisp-cfront can drive the nelisp AOT toolchain
;; before any C semantics are involved.  It is the FIRST spike task and is
;; expected to need wiring adjustments against the live nelisp API.
;;
;; nelisp entry point (verify against nelisp/lisp/nelisp-aot-compiler.el):
;;   (nelisp-aot-compile-to-object DEST SOURCE :arch ARCH :format 'elf)
;; observed call site: nelisp/lisp/nelisp-artifact.el ~L1413.
;;
;; The link + native-run step reuses nelisp's existing probe runner
;; (nelisp/scripts/compile-elisp-objects.el, which registers
;; `:source-var <X>--source' probes).  Stage 0's job is to confirm the
;; smallest path through it; wiring that link step is the concrete TODO.

;;; Code:

(require 'nelisp-cfront)

;; The trivial grammar source under test (NOT C — a hand-written grammar
;; defun, to isolate the toolchain round-trip from any C lowering).
(defconst nelisp-cfront-stage0--source
  '(defun nelisp_cfront_stage0_add (a b) (+ a b))
  "Stage 0 probe: the simplest grammar defun the AOT path can consume.")

(defun nelisp-cfront-stage0--nelisp-root ()
  "Resolve the sibling nelisp repo root."
  (or (getenv "NELISP_REPO_ROOT")
      (expand-file-name "../nelisp"
                        (file-name-directory (or load-file-name buffer-file-name default-directory)))))

(defun nelisp-cfront-stage0-run ()
  "Drive the Stage 0 compile-to-object probe and report status.
Returns t on success.  Does not yet perform the link + native-run step
\(documented TODO above\); emits the .o and reports so the next wiring
step is unambiguous."
  (let* ((root (nelisp-cfront-stage0--nelisp-root))
         (outdir (expand-file-name
                  "out"
                  (file-name-directory (or load-file-name buffer-file-name default-directory)))))
    (make-directory outdir t)
    (message "[stage0] nelisp root: %s" root)
    (message "[stage0] source: %S" nelisp-cfront-stage0--source)
    (unless (require 'nelisp-aot-compiler nil t)
      (error "[stage0] cannot load nelisp-aot-compiler from %s/lisp — set NELISP_REPO_ROOT and add it to load-path" root))
    (unless (fboundp 'nelisp-aot-compile-to-object)
      (error "[stage0] nelisp-aot-compile-to-object not defined after require — verify the nelisp API"))
    (let ((dest (expand-file-name "stage0_add.o" outdir)))
      (message "[stage0] compiling to %s ..." dest)
      ;; NOTE: keyword args mirror the observed nelisp call site; adjust
      ;; here if the live signature differs (first wiring task).
      (condition-case err
          (progn
            (nelisp-aot-compile-to-object dest nelisp-cfront-stage0--source
                                          :arch 'x86_64 :format 'elf)
            (if (file-exists-p dest)
                (progn (message "[stage0] OK: emitted %s (%d bytes). TODO: link + native run."
                                dest (nth 7 (file-attributes dest)))
                       t)
              (error "[stage0] compile returned without producing %s" dest)))
        (error
         (message "[stage0] compile step failed: %S" err)
         (signal (car err) (cdr err)))))))

(provide 'stage0-harness)

;;; stage0-harness.el ends here
