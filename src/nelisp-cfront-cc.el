;;; nelisp-cfront-cc.el --- Driver: C source -> native .o -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M2.5 — the end-to-end driver.  Glues the front-end to nelisp's AOT
;; backend:
;;
;;   C source -> lex -> parse -> lower -> nelisp-aot-compile-to-object -> .o
;;
;; `nelisp-cfront-compile-string' / `-compile-file' produce an ET_REL .o
;; whose functions are C-callable (System V ABI), linkable with `cc'.
;; `nelisp-cfront-cc-batch' is the `make cc FILE=foo.c' entry point.

;;; Code:

(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-lower)

(defun nelisp-cfront--ensure-backend ()
  (unless (require 'nelisp-aot-compiler nil t)
    (error "nelisp-cfront: cannot load nelisp-aot-compiler — set NELISP_REPO_ROOT")))

(defun nelisp-cfront-compile-string (csource objpath &optional arch)
  "Compile C source string CSOURCE to an ET_REL object at OBJPATH.
ARCH defaults to x86_64.  Returns the grammar form that was compiled."
  (nelisp-cfront--ensure-backend)
  (let* ((ast (nelisp-cfront-parse csource))
         (grammar (nelisp-cfront-lower-program ast)))
    (nelisp-aot-compile-to-object grammar objpath
                                  :arch (or arch 'x86_64) :format 'elf)
    grammar))

(defun nelisp-cfront-compile-file (cfile objpath &optional arch)
  "Compile C file CFILE to an ET_REL object at OBJPATH."
  (nelisp-cfront-compile-string
   (with-temp-buffer
     (insert-file-contents cfile)
     (buffer-string))
   objpath arch))

(defun nelisp-cfront-emit-el-string (csource elpath &optional feature)
  "Lower C source CSOURCE and WRITE its nelisp-cc grammar to ELPATH as a
nelisp-compliant `.el' file (the AOT back-end's input form), instead of an
object.  Needs only the front-end (lex/parse/lower) — no AOT back-end.
The file holds one top-level `(seq (defun ...) (data-blob ...) ...)' form,
one child per line, readable with `read' and feedable straight to
`nelisp-aot-compile-to-object'.  FEATURE is reserved for a future
`provide'.  Returns the grammar form."
  (ignore feature)
  (let* ((ast (nelisp-cfront-parse csource))
         (grammar (nelisp-cfront-lower-program ast))
         (name (file-name-nondirectory elpath))
         ;; escape raw bytes (data-blob unibyte strings) so they round-trip
         ;; through `read'; full structure, no abbreviation.
         (print-escape-nonascii t)
         (print-escape-control-characters t)
         (print-length nil)
         (print-level nil)
         (print-quoted t)
         (print-circle nil))
    (with-temp-file elpath
      (insert ";;; " name
              " --- nelisp-cc grammar generated from C by nelisp-cfront"
              "  -*- lexical-binding: t; -*-\n")
      (insert ";; Auto-generated — do not edit.  One `(seq ...)' nelisp form;\n")
      (insert ";; feed to `nelisp-aot-compile-to-object' (or load + compile).\n\n")
      (if (and (consp grammar) (eq (car grammar) 'seq))
          (progn
            (insert "(seq\n")
            (dolist (form (cdr grammar))
              (prin1 form (current-buffer))
              (insert "\n"))
            (insert ")\n"))
        (progn (prin1 grammar (current-buffer)) (insert "\n")))
      (insert "\n;;; " name " ends here\n"))
    grammar))

(defun nelisp-cfront-emit-el-file (cfile elpath &optional feature)
  "Lower C file CFILE and write its nelisp-cc grammar to ELPATH (a `.el')."
  (nelisp-cfront-emit-el-string
   (with-temp-buffer (insert-file-contents cfile) (buffer-string))
   elpath feature))

(defun nelisp-cfront-emit-el-batch ()
  "Batch entry: emit FILE=foo.c as OUT=foo.el (default: FILE with `.el').
The `make emit-el' entry point — C source to a nelisp-compliant grammar
file, no object produced."
  (let* ((file (or (getenv "FILE") (error "nelisp-cfront-emit-el-batch: set FILE=foo.c")))
         (out (or (getenv "OUT")
                  (concat (file-name-sans-extension file) ".el"))))
    (nelisp-cfront-emit-el-file file out)
    (message "[emit-el] %s -> %s" file out)))

(defun nelisp-cfront-cc-batch ()
  "Batch entry: compile FILE=foo.c to OUT=foo.o (or foo.o by default).
Reads FILE / OUT / ARCH from the environment (set by `make cc')."
  (let* ((file (or (getenv "FILE") (error "nelisp-cfront-cc-batch: set FILE=foo.c")))
         (out (or (getenv "OUT")
                  (concat (file-name-sans-extension file) ".o")))
         (arch (let ((a (getenv "ARCH"))) (and a (intern a)))))
    (nelisp-cfront-compile-file file out arch)
    (message "[cc] %s -> %s" file out)))

(provide 'nelisp-cfront-cc)

;;; nelisp-cfront-cc.el ends here
