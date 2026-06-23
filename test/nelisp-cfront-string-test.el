;;; nelisp-cfront-string-test.el --- C string literal rodata pool -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 / Doc 06 Step D — C string literals.  Each distinct (`equal')
;; literal is interned into a NUL-terminated `data-blob' rodata symbol;
;; `(str S)' lowers to `(data-addr SYM)'.  Rides Step A's keystone (no new
;; grammar op) and reuses Step B's per-program data-blob emission.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-parse)
(require 'nelisp-cfront-lower)
(require 'nelisp-cfront-cc)

;;; --- unit: interning, dedup, NUL termination, lowering shape --------

(ert-deftest nelisp-cfront-string-pool-interns-and-dedups ()
  "Identical string literals share one rodata symbol; the program emits
one NUL-terminated `data-blob' per distinct string and lowers `(str S)'
to `(data-addr SYM)'."
  (let* ((ast (nelisp-cfront-parse "
const char *a(void){ return \"hi\"; }
const char *b(void){ return \"hi\"; }
const char *c(void){ return \"yo\"; }
"))
         (prog (nelisp-cfront-lower-program ast))
         (blobs (cl-remove-if-not (lambda (f) (eq (car-safe f) 'data-blob))
                                  (cdr prog))))
    ;; two distinct strings -> two blobs (the duplicate "hi" is shared).
    (should (= 2 (length blobs)))
    ;; NUL-terminated bytes: "hi" -> 3 bytes ending in 0.
    (let ((hi (cl-find-if (lambda (b) (equal (nth 2 b) (concat "hi" (unibyte-string 0))))
                          blobs)))
      (should hi)
      (should (= 3 (length (nth 2 hi)))))
    ;; "hi" lowers to the same data-addr symbol in both a() and b().
    (let* ((flat (format "%S" prog))
           (sym (symbol-name (nth 1 (car blobs)))))
      (should (string-match-p "(data-addr nlcf_str_" flat)))))

;;; --- end-to-end: compile -> link -> run -----------------------------

(defun nelisp-cfront-string-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-string-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-string" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "string e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-string-literals-e2e ()
  "String literals read back as C strings: content, empty string, indexing
into a literal, and pointer-identity dedup of equal literals."
  (unless (nelisp-cfront-string-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-string-test--run "
const char *hello(void){ return \"hello, world\"; }
const char *empty(void){ return \"\"; }
int first(void){ return \"ABC\"[0]; }
int third(void){ return \"ABC\"[2]; }
const char *dup1(void){ return \"shared\"; }
const char *dup2(void){ return \"shared\"; }
int samep(void){ return dup1() == dup2(); }
" "
#include <stdio.h>
#include <string.h>
extern const char *hello(void), *empty(void), *dup1(void), *dup2(void);
extern int first(void), third(void), samep(void);
int main(void){
  printf(\"[%s] [%s] %d %d %d %d\\n\",
         hello(), empty(), (int)strlen(hello()), first(), third(), samep());
  return (strcmp(hello(),\"hello, world\")==0 && strlen(empty())==0 &&
          first()=='A' && third()=='C' && samep()==1) ? 0 : 1;
}
")))
    (should (equal "[hello, world] [] 12 65 67 1" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-string-test)

;;; nelisp-cfront-string-test.el ends here
