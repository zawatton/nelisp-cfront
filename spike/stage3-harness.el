;;; stage3-harness.el --- Stage 3: one syscall / file I/O -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 3 of the nelisp-cfront feasibility spike (Doc 01 §Stage 3).
;;
;; Proves the libc/VFS boundary with NO Rust: direct Linux syscalls via
;; the grammar `syscall-direct' op.
;;
;;   A. write(1, buf, n)         -> bytes to stdout
;;   B. file round-trip:  openat(O_WRONLY|O_CREAT|O_TRUNC) -> write -> close
;;                        openat(O_RDONLY)                 -> read  -> close
;;      then verify the bytes read back equal the bytes written.
;;
;; Design: the grammar provides THIN single-syscall wrappers; the C
;; driver orchestrates the sequence.  Orchestrating multiple syscalls
;; *inside one grammar function* needs to hold the fd in a local across
;; calls — which is the Stage 1 wall (let/setq are env-backed; params
;; survive only the proven forward-threading pattern).  Driver-side
;; orchestration keeps Stage 3 runtime-free and isolates the boundary
;; result from that separate concern (-> cost-curve, Doc 01 §4).
;;
;; syscall numbers (nelisp/lisp/nelisp-syscall-table.el): read=0 write=1
;; close=3 openat=257.  ABI (nelisp-cc-alloc-mem): syscall-direct NR A0..A5
;; -> rax=NR rdi=A0 rsi=A1 rdx=A2 r10=A3 r8=A4 r9=A5; rax = kernel return
;; (negative = -errno).
;;
;; openat: dirfd AT_FDCWD = -100; O_WRONLY|O_CREAT|O_TRUNC = 1|64|512 =
;; 577; mode 0644 = 420; O_RDONLY = 0.

;;; Code:

(require 'nelisp-cfront)

(defconst nelisp-cfront-stage3--this-file
  (or load-file-name buffer-file-name
      (expand-file-name "spike/stage3-harness.el"))
  "Absolute path of this harness file, captured at load time.")

(defconst nelisp-cfront-stage3--source
  '(seq
    ;; write(fd, buf, n)
    (defun nelisp_cfront_stage3_write (fd buf n)
      (syscall-direct 1 fd buf n 0 0 0))
    ;; read(fd, buf, n)
    (defun nelisp_cfront_stage3_read (fd buf n)
      (syscall-direct 0 fd buf n 0 0 0))
    ;; openat(AT_FDCWD, path, O_WRONLY|O_CREAT|O_TRUNC, 0644)
    (defun nelisp_cfront_stage3_openat_wr (path)
      (syscall-direct 257 -100 path 577 420 0 0))
    ;; openat(AT_FDCWD, path, O_RDONLY, 0)
    (defun nelisp_cfront_stage3_openat_rd (path)
      (syscall-direct 257 -100 path 0 0 0 0))
    ;; close(fd)
    (defun nelisp_cfront_stage3_close (fd)
      (syscall-direct 3 fd 0 0 0 0 0)))
  "Stage 3 probe: thin syscall-direct wrappers (write/read/openat/close).")

(defun nelisp-cfront-stage3--dir ()
  (let ((d (expand-file-name
            "out" (file-name-directory nelisp-cfront-stage3--this-file))))
    (make-directory d t)
    d))

(defun nelisp-cfront-stage3-run ()
  "Run the Stage 3 compile -> link -> run round-trip.
Prints `hi' via write(1,...), then writes + reads back a temp file and
verifies the bytes.  Signals on failure."
  (let* ((out  (nelisp-cfront-stage3--dir))
         (obj  (expand-file-name "stage3.o" out))
         (csrc (expand-file-name "stage3_driver.c" out))
         (bin  (expand-file-name "stage3" out)))
    (unless (require 'nelisp-aot-compiler nil t)
      (error "[stage3] cannot load nelisp-aot-compiler — set NELISP_REPO_ROOT"))
    (message "[stage3] compiling syscall-direct wrappers (write/read/openat/close)")
    (nelisp-aot-compile-to-object nelisp-cfront-stage3--source obj
                                  :arch 'x86_64 :format 'elf)
    (unless (file-exists-p obj)
      (error "[stage3] AOT did not produce %s" obj))
    (with-temp-file csrc
      (insert "#include <stdio.h>\n"
              "#include <string.h>\n"
              "extern long nelisp_cfront_stage3_write(long, const void*, long);\n"
              "extern long nelisp_cfront_stage3_read(long, void*, long);\n"
              "extern long nelisp_cfront_stage3_openat_wr(const char*);\n"
              "extern long nelisp_cfront_stage3_openat_rd(const char*);\n"
              "extern long nelisp_cfront_stage3_close(long);\n"
              "int main(void){\n"
              "  const char *msg = \"hi\\n\";\n"
              "  long w = nelisp_cfront_stage3_write(1, msg, 3);\n"
              "  const char *path = \"/tmp/nelisp_cfront_stage3.txt\";\n"
              "  const char *data = \"nelisp-cfront stage3\\n\";\n"
              "  long len = (long)strlen(data);\n"
              "  long fd1 = nelisp_cfront_stage3_openat_wr(path);\n"
              "  long nw = (fd1>=0) ? nelisp_cfront_stage3_write(fd1, data, len) : fd1;\n"
              "  if (fd1>=0) nelisp_cfront_stage3_close(fd1);\n"
              "  char buf[64]; memset(buf,0,sizeof buf);\n"
              "  long fd2 = nelisp_cfront_stage3_openat_rd(path);\n"
              "  long nr = (fd2>=0) ? nelisp_cfront_stage3_read(fd2, buf, (long)sizeof buf) : fd2;\n"
              "  if (fd2>=0) nelisp_cfront_stage3_close(fd2);\n"
              "  int ok = (nr==len && memcmp(buf,data,len)==0);\n"
              "  printf(\"write=%ld open_wr=%ld nwrite=%ld open_rd=%ld nread=%ld roundtrip=%s\\n\",\n"
              "         w, fd1, nw, fd2, nr, ok?\"OK\":\"BAD\");\n"
              "  return (w==3 && fd1>=0 && nw==len && fd2>=0 && nr==len && ok) ? 0 : 1;\n"
              "}\n"))
    (let ((cc (or (executable-find "cc") (executable-find "gcc")
                  (error "[stage3] no cc/gcc on PATH"))))
      (let ((rc (call-process cc nil nil nil csrc obj "-o" bin)))
        (unless (zerop rc) (error "[stage3] link failed (cc rc=%d)" rc))))
    (with-temp-buffer
      (let ((rc (call-process bin nil t nil)))
        (let ((out-str (string-trim (buffer-string))))
          (message "[stage3] %s" out-str)
          (unless (zerop rc)
            (error "[stage3] FAIL: native run returned %d (want write=3 + file roundtrip OK)" rc))
          (message "[stage3] PASS — libc/VFS boundary via syscall-direct (write/openat/read/close), no Rust")
          t)))))

(provide 'stage3-harness)

;;; stage3-harness.el ends here
