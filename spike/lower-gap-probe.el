;;; lower-gap-probe.el --- measure cfront's lowering coverage on real C -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Data-driven LOWERING gap tracking for M5 (Doc 05).  The parse gap
;; probe (`gap-probe.el') measures front-end coverage; this measures the
;; back-end: parse an already-preprocessed C file (FILE=...), then lower
;; each `func' top-level INDEPENDENTLY through `nelisp-cfront-lower--func'
;; and tally how many succeed.  Failures are bucketed by reason (the
;; lowering/type error keyword) so the histogram data-drives which
;; lowering feature to add next.  Usage:
;;
;;   gcc -E -P sqlite3.c > /tmp/sqlite3.pp.c
;;   make lower-gap FILE=/tmp/sqlite3.pp.c

;;; Code:

(require 'cl-lib)
(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-type)
(require 'nelisp-cfront-lower)

(defun nelisp-cfront-lower-gap-probe--reason (err)
  "Bucket key for a caught ERR (`(SYMBOL . DATA)')."
  (let ((sym (car err)) (data (cdr err)))
    (cond
     ;; lowering/type errors carry the reason keyword as the first datum
     ((and (memq sym '(nelisp-cfront-lower-error nelisp-cfront-type-error))
           (keywordp (car data)))
      (car data))
     (t sym))))

(defun nelisp-cfront-lower-gap-probe ()
  "Parse FILE and report per-function lowering coverage + failure buckets."
  (let* ((file (or (getenv "FILE") (error "lower-gap: set FILE=foo.pp.c")))
         (src (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (ast (nelisp-cfront-parse src))
         (funcs (cl-remove-if-not (lambda (tp) (eq (car tp) 'func)) (cdr ast)))
         (total (length funcs))
         (ok 0)
         (buckets (make-hash-table :test 'eq))   ; reason -> count
         (examples (make-hash-table :test 'eq)))  ; reason -> first fn name
    ;; Replicate `lower-program's whole-program context.
    (let* ((nelisp-cfront-lower--structs (nelisp-cfront-type-build-structs ast))
           (nelisp-cfront-lower--funcs (nelisp-cfront-lower--collect-func-types ast))
           (nelisp-cfront-lower--func-params (nelisp-cfront-lower--collect-func-params ast))
           (nelisp-cfront-lower--globals (nelisp-cfront-lower--collect-globals ast))
           (nelisp-cfront-lower--uses-float nil))
      (dolist (top funcs)
        (condition-case e
            (progn (nelisp-cfront-lower--func top) (setq ok (1+ ok)))
          (error
           (let ((r (nelisp-cfront-lower-gap-probe--reason e)))
             (puthash r (1+ (gethash r buckets 0)) buckets)
             (unless (gethash r examples) (puthash r (nth 2 top) examples))))))
      ;; Report.
      (let ((pct (if (> total 0) (/ (* 100.0 ok) total) 0.0))
            (rows nil))
        (maphash (lambda (k v) (push (list k v (gethash k examples)) rows)) buckets)
        (setq rows (sort rows (lambda (a b) (> (nth 1 a) (nth 1 b)))))
        (message "[lower-gap] %s" file)
        (message "[lower-gap] functions: %d total, %d lowered OK (%.1f%%), %d failed"
                 total ok pct (- total ok))
        (when rows
          (message "[lower-gap] failure buckets (reason: count  e.g. fn):")
          (dolist (r rows)
            (message "[lower-gap]   %-34S %5d   %s" (nth 0 r) (nth 1 r) (nth 2 r))))))))

(provide 'lower-gap-probe)

;;; lower-gap-probe.el ends here
