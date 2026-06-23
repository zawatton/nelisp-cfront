;;; gap-probe.el --- report cfront's first parse gap on a real C file -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Data-driven gap tracking for M5/M6 (Doc 05).  Parse an already-
;; preprocessed C file (FILE=...) and report the FIRST parse error with
;; its source line, so adding front-end features visibly advances the
;; first-error line.  Usage:
;;
;;   gcc -E -P sqlite3.c > /tmp/sqlite3.pp.c
;;   make gap FILE=/tmp/sqlite3.pp.c

;;; Code:

(require 'cl-lib)
(require 'nelisp-cfront-parse)

(defun nelisp-cfront-gap-probe--pos (data)
  "Best-effort: find a source byte offset inside parse-error DATA."
  (catch 'pos
    (cl-labels ((walk (x)
                  (when (consp x)
                    (when (and (= (length x) 3) (integerp (nth 2 x))
                               (memq (nth 0 x) '(ident keyword int char string punct eof)))
                      (throw 'pos (nth 2 x)))
                    (mapc #'walk x))))
      (walk data))
    nil))

(defun nelisp-cfront-gap-probe ()
  "Parse FILE and report the first parse gap with its source line."
  (let* ((file (or (getenv "FILE") (error "gap-probe: set FILE=foo.pp.c")))
         (src (with-temp-buffer (insert-file-contents file) (buffer-string))))
    (condition-case e
        (progn (nelisp-cfront-parse src)
               (message "[gap] %s parsed fully — no front-end gap!" file))
      (error
       (let ((pos (nelisp-cfront-gap-probe--pos (cdr e))))
         (if pos
             (let* ((line (1+ (cl-count ?\n src :end (min (length src) pos))))
                    (bol (or (cl-position ?\n src :end (min (length src) pos) :from-end t) -1))
                    (eol (or (cl-position ?\n src :start (min (length src) pos)) (length src))))
               (message "[gap] FIRST GAP: %S\n[gap] line %d: %s"
                        (car e) line (string-trim (substring src (1+ bol) eol))))
           (message "[gap] FIRST GAP: %S" e)))))))

(provide 'gap-probe)

;;; gap-probe.el ends here
