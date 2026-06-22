;;; nelisp-cfront-libc-test.el --- M3 libc-in-C compiled by cfront -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M3 — compile examples/libc.c (a libc subset written in C) with
;; nelisp-cfront itself, link it with a C driver, and verify the
;; functions behave like libc.  Dogfoods M2 (loops/pointers/arrays).
;; Skips when the backend or cc are unavailable.

;;; Code:

(require 'ert)
(require 'nelisp-cfront-cc)
(require 'nelisp-cfront-e2e-test)        ; reuse the run helper

(defconst nelisp-cfront-libc-test--dir
  (file-name-directory (or load-file-name buffer-file-name
                           (expand-file-name "test/x")))
  "Directory of this test file, captured at load time.")

(defun nelisp-cfront-libc-test--file ()
  (expand-file-name "../examples/libc.c" nelisp-cfront-libc-test--dir))

(ert-deftest nelisp-cfront-libc-string-mem ()
  (unless (nelisp-cfront-e2e--available-p)
    (ert-skip "nelisp AOT backend or cc unavailable"))
  (let* ((csrc (with-temp-buffer
                 (insert-file-contents (nelisp-cfront-libc-test--file))
                 (buffer-string)))
         (drv "
#include <stdio.h>
extern void *nlcf_memcpy(char*, char*, long);
extern void *nlcf_memset(char*, long, long);
extern long  nlcf_strlen(char*);
extern int   nlcf_strcmp(char*, char*);
extern char *nlcf_strcpy(char*, char*);
extern int   nlcf_memcmp(char*, char*, long);
int main(void){
  char buf[16]; char buf2[16];
  nlcf_memset(buf, 'A', 5); buf[5]=0;
  nlcf_memcpy(buf2, buf, 6);
  long l = nlcf_strlen(buf);
  int c1 = nlcf_strcmp(\"abc\",\"abc\");
  int c2 = nlcf_strcmp(\"abc\",\"abd\");
  char dst[8]; nlcf_strcpy(dst, \"hi\");
  int m = nlcf_memcmp(buf, buf2, 6);
  printf(\"%s %s %ld %d %d %s %d\\n\", buf, buf2, l, c1, (c2<0)?-1:1, dst, m);
  return (l==5 && c1==0 && c2<0 && m==0
          && buf[0]=='A' && buf2[4]=='A' && dst[0]=='h' && dst[1]=='i' && dst[2]==0)
         ? 0 : 1;
}
")
         (res (nelisp-cfront-e2e--run csrc drv)))
    (should (equal "AAAAA AAAAA 5 0 -1 hi 0" (cdr res)))
    (should (= 0 (car res)))))

(provide 'nelisp-cfront-libc-test)

;;; nelisp-cfront-libc-test.el ends here
