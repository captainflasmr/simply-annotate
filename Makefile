# Makefile for simply-annotate
#
# Common targets:
#   make test       run the ERT suite
#   make compile    byte-compile (warnings are errors)
#   make checkdoc   run checkdoc on the package
#   make lint       run package-lint (needs `make deps')
#   make deps       install dependencies (transient, package-lint) into ./.elpa
#   make all        compile + test + checkdoc
#   make clean      remove byte-compiled files
#
# Override the Emacs binary with, e.g.:  make EMACS=emacs-29.4 test

EMACS ?= emacs

PACKAGE = simply-annotate.el
TESTS   = tests/simply-annotate-tests.el
INIT    = tests/init.el

# Every invocation loads the bootstrap so transient is on `load-path'
# (bundled on Emacs 28.1+, installed into ./.elpa by `make deps' otherwise)
# and the working copy shadows any installed simply-annotate.
BATCH = $(EMACS) -Q --batch -l $(INIT)

.PHONY: all compile test checkdoc lint deps clean

all: compile test checkdoc

deps:
	$(BATCH) \
	  --eval "(unless package-archive-contents (package-refresh-contents))" \
	  --eval "(dolist (p '(transient package-lint)) \
	            (unless (package-installed-p p) (package-install p)))"

compile:
	$(BATCH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(PACKAGE)
	@rm -f $(PACKAGE)c

test:
	$(BATCH) -l ert -l $(TESTS) -f ert-run-tests-batch-and-exit

checkdoc:
	$(BATCH) --eval "(checkdoc-file \"$(PACKAGE)\")"

lint:
	$(BATCH) --eval "(require 'package-lint)" \
	  -f package-lint-batch-and-exit $(PACKAGE)

clean:
	rm -f $(PACKAGE)c tests/*.elc
