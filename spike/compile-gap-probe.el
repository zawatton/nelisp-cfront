;;; compile-gap-probe.el --- measure cfront's AOT-compile coverage -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Data-driven AOT-COMPILE gap tracking for M5 (Doc 05), the stage *after*
;; `lower-gap-probe.el'.  Lowering only proves a C function turns into
;; nelisp-cc grammar; this proves the grammar then survives the real AOT
;; back-end (`nelisp-aot-compile-to-link-unit' = parse -> two-pass emit ->
;; ELF link-unit).  Each lowering-OK `func' is compiled INDEPENDENTLY (its
;; own `defun'; same-unit calls and global/string `data-addr' references
;; become extern relocations, so this isolates per-function codegen, not
;; whole-program linking).  AOT-compile failures are bucketed by reason so
;; the histogram data-drives the next back-end gap.  Usage:
;;
;;   gcc -E -P sqlite3.c > /tmp/sqlite3.pp.c
;;   make compile-gap FILE=/tmp/sqlite3.pp.c

;;; Code:

(require 'cl-lib)
(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-type)
(require 'nelisp-cfront-lower)
(require 'nelisp-aot-compiler)

(defun nelisp-cfront-compile-gap-probe--reason (err)
  "Bucket key for a caught ERR (`(SYMBOL . DATA)')."
  (let ((sym (car err)) (data (cdr err)))
    (cond
     ((and (memq sym '(nelisp-cfront-lower-error nelisp-cfront-type-error
                       nelisp-aot-compiler-error))
           (keywordp (car data)))
      (car data))
     (t sym))))

(defun nelisp-cfront-compile-gap-probe ()
  "Parse FILE, lower each `func', AOT-compile it, report coverage + buckets."
  ;; The AOT back-end recurses over the grammar tree; a huge C function
  ;; (e.g. sqlite3PagerOpen, yy_reduce) overflows the default Emacs depth.
  ;; Raise it so `excessive-lisp-nesting' is not a false bucket (the real
  ;; cfront compile driver should do the same).
  (let ((max-lisp-eval-depth (max max-lisp-eval-depth 60000))
        (max-specpdl-size (max (or (and (boundp 'max-specpdl-size) max-specpdl-size) 0)
                               100000)))
  (let* ((file (or (getenv "FILE") (error "compile-gap: set FILE=foo.pp.c")))
         (src (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (ast (nelisp-cfront-parse src))
         (funcs (cl-remove-if-not (lambda (tp) (eq (car tp) 'func)) (cdr ast)))
         (total (length funcs))
         (lowered 0)                       ; survived lowering
         (compiled 0)                      ; survived AOT compile
         (lower-fail 0)
         (buckets (make-hash-table :test 'eq))   ; AOT reason -> count
         (examples (make-hash-table :test 'eq)))
    ;; Replicate `lower-program's whole-program context (so globals / static
    ;; locals / strings resolve during lowering exactly as in a real build).
    ;; `let*' (NOT `let'): `--collect-globals' reads `--structs' /
    ;; `--string-pool', which must already be bound when its init runs —
    ;; otherwise struct globals can't be sized and silently under-register.
    (let* ((nelisp-cfront-lower--structs (nelisp-cfront-type-build-structs ast))
           (nelisp-cfront-lower--funcs (nelisp-cfront-lower--collect-func-types ast))
           (nelisp-cfront-lower--defined-funcs (nelisp-cfront-lower--collect-defined-funcs ast))
           (nelisp-cfront-lower--variadic-funcs (nelisp-cfront-lower--collect-variadic-funcs ast))
           (nelisp-cfront-lower--func-params (nelisp-cfront-lower--collect-func-params ast))
           (nelisp-cfront-lower--string-pool nil)
           (nelisp-cfront-lower--string-counter 0)
           (nelisp-cfront-lower--globals (nelisp-cfront-lower--collect-globals ast))
           (nelisp-cfront-lower--static-blobs nil)
           (nelisp-cfront-lower--static-counter 0)
           (nelisp-cfront-lower--uses-float nil))
      ;; Two phases: lower EVERYTHING first (so the AOT back-end's state can
      ;; never perturb the lowering count), then AOT-compile each lowered
      ;; defun.  Keep (FN-NAME . GRAMMAR) for the lowered ones.
      (let ((kept nil))
        (dolist (top funcs)
          (let ((g (condition-case _e
                       (nelisp-cfront-lower--func top)
                     (error nil))))
            (if (null g)
                (setq lower-fail (1+ lower-fail))
              (setq lowered (1+ lowered))
              (push (cons (nth 2 top) g) kept))))
        (setq kept (nreverse kept))
        (dolist (pair kept)
          (condition-case e
              (progn
                (nelisp-aot-compile-to-link-unit (cdr pair))
                (setq compiled (1+ compiled)))
            (error
             (let ((r (nelisp-cfront-compile-gap-probe--reason e)))
               (puthash r (1+ (gethash r buckets 0)) buckets)
               (unless (gethash r examples)
                 (puthash r (car pair) examples)))))))
      ;; Report.
      (let ((pct (if (> total 0) (/ (* 100.0 compiled) total) 0.0))
            (lpct (if (> lowered 0) (/ (* 100.0 compiled) lowered) 0.0))
            (rows nil))
        (maphash (lambda (k v) (push (list k v (gethash k examples)) rows)) buckets)
        (setq rows (sort rows (lambda (a b) (> (nth 1 a) (nth 1 b)))))
        (message "[compile-gap] %s" file)
        (message "[compile-gap] functions: %d total, %d lowered, %d AOT-compiled"
                 total lowered compiled)
        (message "[compile-gap] AOT-compiled = %.1f%% of total, %.1f%% of lowered (%d lower-fail, %d compile-fail)"
                 pct lpct lower-fail (- lowered compiled))
        (when rows
          (message "[compile-gap] AOT-compile failure buckets (reason: count  e.g. fn):")
          (dolist (r rows)
            (message "[compile-gap]   %-34S %5d   %s" (nth 0 r) (nth 1 r) (nth 2 r)))))))))

(provide 'compile-gap-probe)

;;; compile-gap-probe.el ends here
