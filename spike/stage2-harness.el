;;; stage2-harness.el --- Stage 2: pointer + memory + struct -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 2 of the nelisp-cfront feasibility spike (Doc 01 §Stage 2).
;;
;; Proves the "memory machine" — REAL native memory (not emulated) read /
;; written through the grammar's raw pointer ops — and measures the first
;; concrete gap (no single u16/u32 load op).
;;
;; Three probes, one object:
;;
;;   A. struct field rw on a C-provided pointer
;;        struct P { long x; long y; };          // x@0, y@8
;;        long rw(void*p){ p->x=5; p->y=99; return p->x + p->y; }   // 104
;;      Grammar: ptr-write-u64 / ptr-read-u64 at computed offsets.
;;
;;   B. self-allocated heap via the grammar mmap allocator
;;        long heap(void){ P*p=malloc(16); p->x=7; p->y=35;
;;                         long r=p->x+p->y; free(p); return r; }    // 42
;;      Reuses nelisp's verified nl_mmap_alloc / nl_mmap_dealloc
;;      (syscall-direct mmap/munmap — runtime-free).  The pointer is
;;      threaded through PARAMS (not let — let is env-backed, Stage 1).
;;
;;   C. GAP — u16/u32 load has no single grammar op (only u8 / u64).
;;        Compose a little-endian u32 from 4 u8 reads + shl + logior:
;;          u32 = b0 | b1<<8 | b2<<16 | b3<<24
;;      Works (functionally complete) but costs 4 reads + 3 shifts + 3
;;      ors vs one instruction -> cost-curve note (a native u16/u32 op
;;      would be a worthwhile grammar addition).
;;
;; Verified grammar ops: ptr-read/write-u8/u64 (nelisp-cc-atomic-raw-mem),
;; syscall-direct + nl_mmap_alloc/dealloc (nelisp-cc-alloc-mem), shl /
;; logior / logand / + (nelisp-cc-jit-arith, nelisp-cc-jit-secure-hash).

;;; Code:

(require 'nelisp-cfront)
(require 'nelisp-cc-alloc-mem)          ; nl_mmap_alloc / nl_mmap_dealloc sources

(defconst nelisp-cfront-stage2--this-file
  (or load-file-name buffer-file-name
      (expand-file-name "spike/stage2-harness.el"))
  "Absolute path of this harness file, captured at load time.")

(defconst nelisp-cfront-stage2--source
  (append
   '(seq)
   ;; Reuse nelisp's grammar-level mmap allocator (splice its defuns in).
   (cdr nelisp-cc-alloc-mem--alloc-source)     ; nl_mmap_alloc (+ helpers)
   (cdr nelisp-cc-alloc-mem--dealloc-source)   ; nl_mmap_dealloc (+ helpers)
   '(;; A. struct field rw on a caller-provided pointer
     (defun nelisp_cfront_stage2_struct_rw (p)
       (seq
        (ptr-write-u64 p 0 5)
        (ptr-write-u64 p 8 99)
        (+ (ptr-read-u64 p 0) (ptr-read-u64 p 8))))
     ;; B. self-allocated heap (pointer threaded through params, no let)
     (defun nelisp_cfront_stage2_heap_finish (p result)
       (seq (nl_mmap_dealloc p 16 8) result))
     (defun nelisp_cfront_stage2_heap_use (p)
       (if (= p 0)
           -1
         (seq
          (ptr-write-u64 p 0 7)
          (ptr-write-u64 p 8 35)
          (nelisp_cfront_stage2_heap_finish
           p (+ (ptr-read-u64 p 0) (ptr-read-u64 p 8))))))
     (defun nelisp_cfront_stage2_heap ()
       (nelisp_cfront_stage2_heap_use (nl_mmap_alloc 16 8)))
     ;; C. u16/u32 GAP — compose a little-endian u32 from u8 reads
     (defun nelisp_cfront_stage2_read_u32_le (p off)
       (logior
        (ptr-read-u8 p off)
        (logior
         (shl (ptr-read-u8 p (+ off 1)) 8)
         (logior
          (shl (ptr-read-u8 p (+ off 2)) 16)
          (shl (ptr-read-u8 p (+ off 3)) 24)))))))
  "Stage 2 probe: pointer/struct/heap + u16/u32-compose, one object.")

(defun nelisp-cfront-stage2--dir ()
  (let ((d (expand-file-name
            "out" (file-name-directory nelisp-cfront-stage2--this-file))))
    (make-directory d t)
    d))

(defun nelisp-cfront-stage2-run ()
  "Run the Stage 2 compile -> link -> run round-trip.
Asserts struct_rw=104, heap=42, read_u32_le=0x12345678.  Signals on failure."
  (let* ((out  (nelisp-cfront-stage2--dir))
         (obj  (expand-file-name "stage2.o" out))
         (csrc (expand-file-name "stage2_driver.c" out))
         (bin  (expand-file-name "stage2" out)))
    (unless (require 'nelisp-aot-compiler nil t)
      (error "[stage2] cannot load nelisp-aot-compiler — set NELISP_REPO_ROOT"))
    (message "[stage2] compiling Stage 2 object (struct rw / mmap heap / u32 compose)")
    (nelisp-aot-compile-to-object nelisp-cfront-stage2--source obj
                                  :arch 'x86_64 :format 'elf)
    (unless (file-exists-p obj)
      (error "[stage2] AOT did not produce %s" obj))
    (with-temp-file csrc
      (insert "#include <stdio.h>\n"
              "#include <stdlib.h>\n"
              "#include <string.h>\n"
              "extern long nelisp_cfront_stage2_struct_rw(void*);\n"
              "extern long nelisp_cfront_stage2_heap(void);\n"
              "extern long nelisp_cfront_stage2_read_u32_le(void*, long);\n"
              "int main(void){\n"
              "  void *p = malloc(16);\n"
              "  long a = nelisp_cfront_stage2_struct_rw(p);\n"
              "  long h = nelisp_cfront_stage2_heap();\n"
              "  unsigned char buf[8]; unsigned u = 0x12345678u; memcpy(buf,&u,4);\n"
              "  long r = nelisp_cfront_stage2_read_u32_le(buf, 0);\n"
              "  printf(\"struct_rw=%ld heap=%ld read_u32_le=0x%lx\\n\", a, h, r);\n"
              "  free(p);\n"
              "  return (a==104 && h==42 && r==0x12345678) ? 0 : 1;\n"
              "}\n"))
    (let ((cc (or (executable-find "cc") (executable-find "gcc")
                  (error "[stage2] no cc/gcc on PATH"))))
      (let ((rc (call-process cc nil nil nil csrc obj "-o" bin)))
        (unless (zerop rc) (error "[stage2] link failed (cc rc=%d)" rc))))
    (with-temp-buffer
      (let ((rc (call-process bin nil t nil)))
        (let ((out-str (string-trim (buffer-string))))
          (message "[stage2] %s" out-str)
          (unless (zerop rc)
            (error "[stage2] FAIL: native run returned %d (want struct_rw=104 heap=42 u32=0x12345678)" rc))
          (message "[stage2] PASS — native memory machine: ptr-read/write-u8/u64 + mmap heap + u32 compose")
          t)))))

(provide 'stage2-harness)

;;; stage2-harness.el ends here
