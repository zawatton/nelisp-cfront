.PHONY: test compile clean stage0 stage1 help

EMACS ?= emacs

# Sibling nelisp repo (provides nelisp-aot-compile-to-object + grammar).
NELISP_REPO_ROOT ?= ../nelisp
export NELISP_REPO_ROOT

# Load-path: this repo's src + nelisp lisp/src.
LOADPATH = -L src \
           -L $(NELISP_REPO_ROOT)/lisp \
           -L $(NELISP_REPO_ROOT)/src

# MSYS sane temp defaults (mirrors nelisp Makefile).
export TMPDIR ?= /tmp
export TEMP   ?= /tmp
export TMP    ?= /tmp

help:
	@echo "targets:"
	@echo "  make test     — run ERT suite (test/*-test.el)"
	@echo "  make compile  — byte-compile src/"
	@echo "  make stage0   — run the compile->link->run round-trip probe"
	@echo "  make clean     — remove build artifacts"
	@echo ""
	@echo "vars: NELISP_REPO_ROOT=$(NELISP_REPO_ROOT)  EMACS=$(EMACS)"

test:
	$(EMACS) -Q --batch $(LOADPATH) \
	  -l nelisp-cfront \
	  -l test/nelisp-cfront-test.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch $(LOADPATH) \
	  --eval '(setq byte-compile-error-on-warn nil)' \
	  -f batch-byte-compile src/nelisp-cfront.el

# Stage 0 feasibility probe: grammar source -> .o (nelisp AOT) -> cc-linked
# native binary -> run -> assert. VERIFIED PASS 2026-06-22 (no cargo in the
# run path). Requires NELISP_REPO_ROOT to point at a working nelisp checkout.
stage0:
	$(EMACS) -Q --batch $(LOADPATH) \
	  -l nelisp-cfront \
	  -l spike/stage0-harness.el \
	  --eval '(nelisp-cfront-stage0-run)'

# Stage 1: int-only C (loop sum + narrow-int) -> grammar -> native.
stage1:
	$(EMACS) -Q --batch $(LOADPATH) \
	  -l nelisp-cfront \
	  -l spike/stage1-harness.el \
	  --eval '(nelisp-cfront-stage1-run)'

clean:
	rm -f src/*.elc test/*.elc spike/*.elc
	rm -rf spike/out target build
