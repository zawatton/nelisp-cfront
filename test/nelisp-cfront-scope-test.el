;;; nelisp-cfront-scope-test.el --- nested-scope local type tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M5 / Doc 06 follow-on — local-variable type inference must reach
;; declarations nested inside `switch'/`do-while'/labeled statements, not
;; just the top-level block and if/while/for bodies.  A typed pointer
;; declared in a `switch' case used to fall back to `long', so `p->field'
;; signalled `:unknown-struct'/`:deref-non-pointer'.  `--collect-decls' /
;; `--collect-decl-types' now recurse into those forms.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)

(defun nelisp-cfront-scope-test--available-p ()
  (and (require 'nelisp-aot-compiler nil t)
       (fboundp 'nelisp-aot-compile-to-object)
       (or (executable-find "cc") (executable-find "gcc"))))

(defun nelisp-cfront-scope-test--run (csource driver-c)
  (let* ((dir (make-temp-file "nlcf-scope" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-cfront-compile-string csource obj)
          (with-temp-file cdrv (insert driver-c))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (unless (zerop (call-process cc nil nil nil cdrv obj "-o" bin))
              (error "scope e2e: link failed"))
            (with-temp-buffer
              (let ((rc (call-process bin nil t nil)))
                (cons rc (string-trim (buffer-string)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-cfront-scope-nested-pointer-decl-e2e ()
  "A typed pointer declared inside a `switch' case (and a `while' body) is
tracked as its declared type, so `p->field' traverses a linked list."
  (unless (nelisp-cfront-scope-test--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let ((res (nelisp-cfront-scope-test--run "
struct Node { int val; struct Node *next; };
int sum_from(struct Node *head, int mode){
  int s = 0;
  switch(mode){
    case 1: {
      struct Node *p = head;
      while(p){ s += p->val; p = p->next; }
      break;
    }
    default:
      s = -1;
  }
  return s;
}
" "
#include <stdio.h>
struct Node { int val; struct Node *next; };
extern int sum_from(struct Node*, int);
int main(void){
  struct Node c = {30, 0}, b = {20, &c}, a = {10, &b};
  printf(\"%d %d\\n\", sum_from(&a, 1), sum_from(&a, 9));
  return (sum_from(&a,1)==60 && sum_from(&a,9)==-1) ? 0 : 1;
}
")))
    (should (equal "60 -1" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-scope-test)

;;; nelisp-cfront-scope-test.el ends here
