;;; nelisp-cfront-rt.el --- nelisp-cfront runtime: arena allocator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; M3 — the allocation runtime.  A POSIX `malloc()' needs a hidden global
;; heap pointer that persists across calls; cfront does not have C globals
;; yet (M2 deferred them), so the heap state would have nowhere to live.
;; Instead this provides an ARENA (region) allocator whose state lives in
;; an explicit handle the program threads around — a legitimate, common
;; allocation model (bump-allocate, free the whole region at once) that is
;; runtime-free and needs no globals.  A POSIX-style `malloc' wrapper with
;; a hidden global arena is a thin follow-on once C globals land.
;;
;; The runtime is hand-written grammar (not cfront-compiled C) because it
;; calls nelisp's grammar-level mmap allocator and does raw memory ops.
;; It links into any cfront-compiled object that calls these symbols.
;;
;; Arena handle layout (at base):  [0]=cur (i64)  [8]=end (i64)
;; usable memory begins at base+16.  Allocations are 8-byte aligned.
;;
;; API (C-callable):
;;   char *nlcf_arena_new(long total_bytes);  // mmap; 0 on failure
;;   void *nlcf_arena_alloc(char *arena, long n); // bump; 0 when full
;;   long  nlcf_arena_free(char *arena);          // munmap whole region

;;; Code:

(require 'nelisp-cc-alloc-mem)          ; nl_mmap_alloc / nl_mmap_dealloc grammar

(defconst nelisp-cfront-rt--arena-source
  '(;; arena_new: mmap TOTAL bytes; init header [cur=base+16, end=base+total]
    (defun nlcf_arena_new (total)
      (nlcf_arena_init (nl_mmap_alloc total 8) total))
    (defun nlcf_arena_init (base total)
      (if (= base 0)
          0
        (seq
         (ptr-write-u64 base 0 (+ base 16))
         (ptr-write-u64 base 8 (+ base total))
         base)))
    ;; arena_alloc: 8-byte-align N, bump if it fits, else 0
    (defun nlcf_arena_alloc (arena n)
      (nlcf_arena_bump arena (logand (+ n 7) -8)
                       (ptr-read-u64 arena 0) (ptr-read-u64 arena 8)))
    (defun nlcf_arena_bump (arena n cur end)
      (if (> (+ cur n) end)
          0
        (seq (ptr-write-u64 arena 0 (+ cur n)) cur)))
    ;; arena_free: munmap the whole region (size = end - base)
    (defun nlcf_arena_free (arena)
      (if (= arena 0)
          1
        (nl_mmap_dealloc arena (- (ptr-read-u64 arena 8) arena) 8))))
  "Grammar defuns for the arena allocator (spliced after the mmap source).")

(defun nelisp-cfront-rt-source ()
  "Return the full runtime grammar `(seq ...)' (mmap allocator + arena)."
  (append '(seq)
          (cdr nelisp-cc-alloc-mem--alloc-source)     ; nl_mmap_alloc (+ helpers)
          (cdr nelisp-cc-alloc-mem--dealloc-source)   ; nl_mmap_dealloc (+ helpers)
          nelisp-cfront-rt--arena-source))

(defun nelisp-cfront-rt-compile (objpath &optional arch)
  "Compile the runtime to an ET_REL object at OBJPATH."
  (unless (require 'nelisp-aot-compiler nil t)
    (error "nelisp-cfront-rt: cannot load nelisp-aot-compiler — set NELISP_REPO_ROOT"))
  (nelisp-aot-compile-to-object (nelisp-cfront-rt-source) objpath
                                :arch (or arch 'x86_64) :format 'elf)
  objpath)

(provide 'nelisp-cfront-rt)

;;; nelisp-cfront-rt.el ends here
