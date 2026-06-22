;;; stage1-harness.el --- Stage 1: int-only C -> grammar -> native -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 1 of the nelisp-cfront feasibility spike (Doc 01 §Stage 1).
;;
;; Hand-lowers int-only C to the nelisp-cc grammar and proves the native
;; round-trip for: locals (let), loops (while), mutation (setq),
;; sequencing (seq), comparison (<), arithmetic (+), and narrow-int
;; truncation (logand mask).
;;
;; C being modelled:
;;
;;   long sum(long n){ long s=0; for(long i=0;i<n;i++) s+=i; return s; }
;;   long narrow(long x){ return (uint8_t)x; }     // x & 0xFF
;;   long fact(long n){ return n<=1 ? 1 : n*fact(n-1); }
;;
;; KEY FINDING (Stage 1 wall — verified by linker symbols 2026-06-22):
;;   The grammar `let'/`setq' do NOT lower to raw machine locals — they
;;   bind into the nelisp RUNTIME ENVIRONMENT.  An iterative
;;   `(let ((s 0)(i 0)) (while (< i n) (setq s (+ s i)) ...))' compiled to
;;   undefined refs `nl_alloc_symbol' + `nelisp_env_set_value', i.e. it
;;   needs the nelisp runtime linked in — it is NOT standalone.
;;
;;   Therefore C locals must NOT be lowered to grammar let/setq for a
;;   runtime-free object.  Instead, model C locals as FUNCTION PARAMETERS
;;   threaded through recursion (params are raw i64 registers, as stage0
;;   proved).  nelisp-cc has no TCO, so depth = iteration count; fine for
;;   small loops, a real limit for large ones (-> cost-curve, Doc 01 §4).
;;
;; Grammar facts used (verified: nelisp-cc-fact-i64.el, stage0):
;;   - if / <= / < / + / - / * / logand + self-recursion are grammar ops
;;     and link with NO runtime externs (pure i64, params in registers).
;;   - multiple/forward-referencing defuns => wrap in (seq (defun ...) ...)
;;     so the parser pre-scan registers names before bodies are parsed.

;;; Code:

(require 'nelisp-cfront)

(defconst nelisp-cfront-stage1--this-file
  (or load-file-name buffer-file-name
      (expand-file-name "spike/stage1-harness.el"))
  "Absolute path of this harness file, captured at load time.")

(defconst nelisp-cfront-stage1--source
  '(seq
    ;; long sum(long n){ long s=0; for(i=0;i<n;i++) s+=i; return s; }
    ;; C locals -> recursion params (i, acc) so the object is runtime-free
    ;; (no let/setq -> no nl_alloc_symbol / nelisp_env_set_value).
    (defun nelisp_cfront_stage1_sum_iter (i n acc)
      (if (< i n)
          (nelisp_cfront_stage1_sum_iter (+ i 1) n (+ acc i))
        acc))
    (defun nelisp_cfront_stage1_sum (n)
      (nelisp_cfront_stage1_sum_iter 0 n 0))
    ;; long narrow(long x){ return (uint8_t)x; }  -> x & 0xFF
    (defun nelisp_cfront_stage1_narrow (x)
      (logand x 255))
    ;; long fact(long n){ return n<=1 ? 1 : n*fact(n-1); }
    (defun nelisp_cfront_stage1_fact (n)
      (if (<= n 1) 1 (* n (nelisp_cfront_stage1_fact (- n 1))))))
  "Stage 1 probe: int-only C hand-lowered to the nelisp-cc grammar.
Locals are threaded through recursion params (runtime-free); see the
let/setq=env finding in this file's commentary.")

(defun nelisp-cfront-stage1--dir ()
  (let ((d (expand-file-name
            "out" (file-name-directory nelisp-cfront-stage1--this-file))))
    (make-directory d t)
    d))

(defun nelisp-cfront-stage1-run ()
  "Run the Stage 1 compile -> link -> run round-trip.
Asserts sum(10)=45 and narrow(300)=44.  Signals on failure."
  (let* ((out  (nelisp-cfront-stage1--dir))
         (obj  (expand-file-name "stage1.o" out))
         (csrc (expand-file-name "stage1_driver.c" out))
         (bin  (expand-file-name "stage1" out)))
    (unless (require 'nelisp-aot-compiler nil t)
      (error "[stage1] cannot load nelisp-aot-compiler — set NELISP_REPO_ROOT"))
    (message "[stage1] compiling %S" nelisp-cfront-stage1--source)
    (nelisp-aot-compile-to-object nelisp-cfront-stage1--source obj
                                  :arch 'x86_64 :format 'elf)
    (unless (file-exists-p obj)
      (error "[stage1] AOT did not produce %s" obj))
    (with-temp-file csrc
      (insert "#include <stdio.h>\n"
              "extern long nelisp_cfront_stage1_sum(long);\n"
              "extern long nelisp_cfront_stage1_narrow(long);\n"
              "extern long nelisp_cfront_stage1_fact(long);\n"
              "int main(void){\n"
              "  long s = nelisp_cfront_stage1_sum(10);\n"
              "  long w = nelisp_cfront_stage1_narrow(300);\n"
              "  long f = nelisp_cfront_stage1_fact(5);\n"
              "  printf(\"sum(10)=%ld narrow(300)=%ld fact(5)=%ld\\n\", s, w, f);\n"
              "  return (s==45 && w==44 && f==120) ? 0 : 1;\n"
              "}\n"))
    (let ((cc (or (executable-find "cc") (executable-find "gcc")
                  (error "[stage1] no cc/gcc on PATH"))))
      (let ((rc (call-process cc nil nil nil csrc obj "-o" bin)))
        (unless (zerop rc) (error "[stage1] link failed (cc rc=%d)" rc))))
    (with-temp-buffer
      (let ((rc (call-process bin nil t nil)))
        (let ((out-str (string-trim (buffer-string))))
          (message "[stage1] %s" out-str)
          (unless (zerop rc)
            (error "[stage1] FAIL: native run returned %d (want sum=45 narrow=44 fact=120)" rc))
          (message "[stage1] PASS — recursion/if/cmp(< <=)/arith(+ - *)/logand mask, native, runtime-free")
          t)))))

(provide 'stage1-harness)

;;; stage1-harness.el ends here
