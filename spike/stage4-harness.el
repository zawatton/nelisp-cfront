;;; stage4-harness.el --- Stage 4: native loop + frame-slot C locals -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 4 of the nelisp-cfront feasibility spike — the GATING ITEM
;; resolved (Doc 02): lower C locals + loops to native WITHOUT the
;; env-backed grammar `let'.
;;
;; Mechanism (verified in nelisp-aot-compiler.el):
;;   - A `let' binding with a NON-foldable init allocates a frame SLOT
;;     (`:class gp :slot N', rbp-relative stack), not an env value-cell.
;;     A foldable init (e.g. literal 0) is const-folded into the compile
;;     env and is NOT a mutable slot -> a later `setq' on it falls to the
;;     env bridge (nl_alloc_symbol / nelisp_env_set_value).  THIS was the
;;     Stage 1 wall.
;;   - `setq' on a gp-slot var emits `setq-local' (frame write), runtime-free.
;;   - frame slots are rbp-relative => they SURVIVE function calls.
;;   - grammar `while' is a native loop => no recursion => no stack
;;     overflow (the Stage 1 no-TCO wall is gone for loops).
;;
;; So a C `for/while` lowers to: (let ((v <non-foldable-init>)...) (seq
;; (while COND (seq STMT... )) RESULT)) with setq-local updates.
;;
;; Front-end rule used here: a mutable C local with a constant init C is
;; given a non-foldable init that evaluates to C.  For a function with a
;; param p, `(- p p)' is a reliable non-foldable 0.  (The no-param case
;; is a front-end detail noted in Doc 02; not exercised here.)
;;
;; Probes:
;;   A. long sum(long n){ long s=0; for(i=0;i<n;i++) s+=i; return s; }
;;      run with n=1,000,000 -> 499999500000.  A recursion lowering would
;;      overflow the native stack ~650k; the native while does not.
;;   B. slots survive a CALL: i is updated via inc(i) each iteration;
;;      s and i (both slots) stay correct across the call.  -> 4950 at n=100.

;;; Code:

(require 'nelisp-cfront)

(defconst nelisp-cfront-stage4--this-file
  (or load-file-name buffer-file-name
      (expand-file-name "spike/stage4-harness.el"))
  "Absolute path of this harness file, captured at load time.")

(defconst nelisp-cfront-stage4--source
  '(seq
    ;; A. big native loop with frame-slot locals (no recursion).
    (defun nelisp_cfront_stage4_sum (n)
      (let ((s (- n n)) (i (- n n)))          ; non-foldable 0 -> frame slots
        (seq
         (while (< i n)
           (seq (setq s (+ s i)) (setq i (+ i 1))))
         s)))
    ;; B. slots survive a call: i updated via inc(i) each iteration.
    (defun nelisp_cfront_stage4_inc (x) (+ x 1))
    (defun nelisp_cfront_stage4_sum_call (n)
      (let ((s (- n n)) (i (- n n)))
        (seq
         (while (< i n)
           (seq (setq s (+ s i))
                (setq i (nelisp_cfront_stage4_inc i))))   ; CALL mid-loop
         s))))
  "Stage 4 probe: native while-loop + frame-slot C locals, runtime-free.")

(defun nelisp-cfront-stage4--dir ()
  (let ((d (expand-file-name
            "out" (file-name-directory nelisp-cfront-stage4--this-file))))
    (make-directory d t)
    d))

(defun nelisp-cfront-stage4-run ()
  "Run the Stage 4 compile -> link -> run round-trip.
Asserts sum(1000000)=499999500000 (no stack overflow) and
sum_call(100)=4950 (slots survive calls).  Signals on failure."
  (let* ((out  (nelisp-cfront-stage4--dir))
         (obj  (expand-file-name "stage4.o" out))
         (csrc (expand-file-name "stage4_driver.c" out))
         (bin  (expand-file-name "stage4" out)))
    (unless (require 'nelisp-aot-compiler nil t)
      (error "[stage4] cannot load nelisp-aot-compiler — set NELISP_REPO_ROOT"))
    (message "[stage4] compiling native-loop frame-slot locals")
    (nelisp-aot-compile-to-object nelisp-cfront-stage4--source obj
                                  :arch 'x86_64 :format 'elf)
    (unless (file-exists-p obj)
      (error "[stage4] AOT did not produce %s" obj))
    (with-temp-file csrc
      (insert "#include <stdio.h>\n"
              "extern long nelisp_cfront_stage4_sum(long);\n"
              "extern long nelisp_cfront_stage4_sum_call(long);\n"
              "int main(void){\n"
              "  long big = nelisp_cfront_stage4_sum(1000000L);\n"
              "  long c   = nelisp_cfront_stage4_sum_call(100L);\n"
              "  printf(\"sum(1e6)=%ld sum_call(100)=%ld\\n\", big, c);\n"
              "  return (big==499999500000L && c==4950L) ? 0 : 1;\n"
              "}\n"))
    (let ((cc (or (executable-find "cc") (executable-find "gcc")
                  (error "[stage4] no cc/gcc on PATH"))))
      (let ((rc (call-process cc nil nil nil csrc obj "-o" bin)))
        (unless (zerop rc) (error "[stage4] link failed (cc rc=%d)" rc))))
    (with-temp-buffer
      (let ((rc (call-process bin nil t nil)))
        (let ((out-str (string-trim (buffer-string))))
          (message "[stage4] %s" out-str)
          (unless (zerop rc)
            (error "[stage4] FAIL: native run returned %d (want sum(1e6)=499999500000 sum_call=4950)" rc))
          (message "[stage4] PASS — native while-loop + frame-slot locals, survive calls, no overflow, runtime-free")
          t)))))

(provide 'stage4-harness)

;;; stage4-harness.el ends here
