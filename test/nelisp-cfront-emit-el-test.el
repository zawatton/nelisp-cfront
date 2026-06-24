;;; nelisp-cfront-emit-el-test.el --- C -> nelisp-compliant .el emission -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The core deliverable: turn a C source file into a nelisp-COMPLIANT `.el'
;; file (the nelisp-cc grammar written out), not an object.  These tests
;; check (1) the emitted file round-trips through `read' to exactly the
;; lowered grammar, and (2) it is genuinely nelisp source — readable,
;; AOT-compilable to native, linkable with cc, and correct at runtime
;; (incl. unibyte `data-blob' bytes with high/negative values).

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)

(ert-deftest nelisp-cfront-emit-el-roundtrips-to-grammar ()
  "The written `.el' `read's back to exactly the lowered grammar form,
including raw `data-blob' bytes (escaped so they survive `read')."
  (let* ((csrc "
const unsigned char tbl[5] = {10, 250, 0, 128, 255};
signed char sgn[2] = {-1, -128};
int getb(int i){ return tbl[i]; }
")
         (dir (make-temp-file "nlcf-emitel" t))
         (el (expand-file-name "out.el" dir)))
    (unwind-protect
        (let* ((grammar (nelisp-cfront-emit-el-string csrc el))
               (readback (with-temp-buffer
                           (insert-file-contents el)
                           (goto-char (point-min))
                           (read (current-buffer)))))
          (should (file-exists-p el))
          (should (eq (car readback) 'seq))
          ;; the file's form is `equal' to what was lowered in memory
          (should (equal grammar readback))
          ;; the raw bytes survived the text round-trip exactly
          (let ((blob (cl-find-if (lambda (f) (and (eq (car-safe f) 'data-blob)
                                                   (eq (nth 1 f) 'tbl)))
                                  (cdr readback))))
            (should blob)
            (should (equal (unibyte-string 10 250 0 128 255) (nth 2 blob)))))
      (ignore-errors (delete-directory dir t)))))

(defun nelisp-cfront-emit-el-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(ert-deftest nelisp-cfront-emit-el-compiles-and-runs-e2e ()
  "The emitted `.el', read back and fed to the AOT back-end, links with cc
and runs correctly — proving it is real nelisp source, not just text."
  (unless (nelisp-cfront-emit-el-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc "
const unsigned char tbl[8] = {10, 20, 30, 40, 250, 99, 7, 200};
signed char sgn[3] = {-1, -2, -128};
int getb(int i){ return tbl[i]; }
int getsgn(int i){ return sgn[i]; }
int fib(int n){ if (n < 2) return n; return fib(n-1) + fib(n-2); }
const char *hello(void){ return \"hi!\"; }
")
         (dir (make-temp-file "nlcf-emitel-e2e" t))
         (el (expand-file-name "prog.el" dir))
         (obj (expand-file-name "prog.o" dir))
         (drv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          ;; C -> .el (front-end only)
          (nelisp-cfront-emit-el-file
           (let ((c (expand-file-name "in.c" dir)))
             (with-temp-file c (insert csrc)) c)
           el)
          ;; .el -> read -> AOT -> .o (back-end consumes the generated file)
          (let ((grammar (with-temp-buffer
                           (insert-file-contents el)
                           (goto-char (point-min))
                           (read (current-buffer)))))
            (nelisp-aot-compile-to-object grammar obj :arch 'x86_64 :format 'elf))
          (with-temp-file drv
            (insert "#include <stdio.h>\n#include <string.h>\n"
                    "extern long getb(long),getsgn(long),fib(long);\n"
                    "extern const char *hello(void);\n"
                    "int main(void){\n"
                    "  printf(\"%ld %ld %ld %ld %s\\n\","
                    " getb(4),getsgn(2),getb(7),fib(10),hello());\n"
                    "  return (getb(4)==250 && getsgn(2)==-128 && getb(7)==200"
                    " && fib(10)==55 && strcmp(hello(),\"hi!\")==0)?0:1; }\n"))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (should (zerop (call-process cc nil nil nil drv obj "-o" bin)))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (should (equal "250 -128 200 55 hi!"
                               (string-trim (buffer-string))))
                (should (= 0 rc))))))
      (ignore-errors (delete-directory dir t)))))

(provide 'nelisp-cfront-emit-el-test)

;;; nelisp-cfront-emit-el-test.el ends here
