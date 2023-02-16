# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

## Use Bourne shell syntax.
SHELL = /bin/sh
UNAME := $(shell uname)

LISP ?= sbcl
SBCL_FLAGS =
ifeq ($(LISP), sbcl)
	SBCL_FLAGS=--dynamic-space-size $(shell sbcl --noinform --no-userinit --non-interactive --eval '(prin1 (max 3072 (/ (sb-ext:dynamic-space-size) 1024 1024)))' --quit | tail -1)
endif
## We use --non-interactive with SBCL so that errors don't interrupt the CI.
LISP_FLAGS ?= $(SBCL_FLAGS) --no-userinit --non-interactive

FLATPAK_COMMAND = flatpak
FLATPAK_BUILDER = flatpak-builder
FLATPAK_APP_ID = engineer.atlas.Nyxt
FLATPAK_MANIFEST := $(FLATPAK_APP_ID).yaml

export NYXT_SUBMODULES=true
export NYXT_RENDERER=gi-gtk
export NASDF_USE_LOGICAL_PATHS=true

.PHONY: help
help:
	@cat INSTALL

makefile_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

# The CFFI-specific snippet is useful when running in a Guix environment to register its libraries in CFFI.
# TODO: Find a better way to do it.
lisp_eval:=$(LISP) $(LISP_FLAGS) \
	--eval '(require "asdf")' \
	--eval '(when (string= "$(NYXT_SUBMODULES)" "true") (setf asdf:*default-source-registries* (list (quote asdf/source-registry:environment-source-registry))) (asdf:clear-configuration) (asdf:load-asd "$(makefile_dir)/libraries/nasdf/nasdf.asd") (asdf:load-asd "$(makefile_dir)/nyxt.asd") (asdf:load-system :nyxt/submodules))' \
	--eval '(asdf:load-asd "$(makefile_dir)/libraries/nasdf/nasdf.asd")' \
	--eval '(asdf:load-asd "$(makefile_dir)/nyxt.asd")' \
  --eval '(when (find-package :ql) (funcall (read-from-string "ql:quickload") :cffi))' \
  --eval '(when (and (find-package :cffi) (uiop:getenv "GUIX_ENVIRONMENT")) (pushnew (pathname (format nil "~a/lib/" (uiop:getenv "GUIX_ENVIRONMENT"))) (symbol-value (read-from-string "cffi:*foreign-library-directories*" )) :test (quote equal)))' \
	--eval
lisp_quit:=--eval '(uiop:quit)'

## asdf:load-system is a bit slow on :nyxt/$(NYXT_RENDERER)-application, so we
## keep a Make dependency on the Lisp files.
lisp_files := nyxt.asd $(shell find . -type f -name '*.lisp')
nyxt: $(lisp_files)
	$(lisp_eval) '(asdf:load-system :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(asdf:make :nyxt/$(NYXT_RENDERER)-application)' \
		$(lisp_quit) || (printf "\n%s\n%s\n" "Compilation failed, see the above stacktrace." && exit 1)

web-extensions:
	$(MAKE) -C libraries/web-extensions/ all

.PHONY: all
all: nyxt

.PHONY: install
install: all
	$(lisp_eval) '(asdf:load-system :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(asdf:make :nyxt/install)' $(lisp_quit)

.PHONY: doc
doc:
	$(lisp_eval) '(asdf:load-system :nyxt)' \
		--eval '(asdf:load-system :nyxt/documentation)' $(lisp_quit)

.PHONY: check
check:
	$(lisp_eval) '(asdf:load-system :nyxt)' \
		--eval '(asdf:test-system :nyxt)' $(lisp_quit)

.PHONY: clean-submodules
clean-submodules:
	git submodule deinit  --all

.PHONY: clean
clean: clean-submodules

.PHONY: flatpak-build
flatpak-build:
	@$(FLATPAK_BUILDER) --user --install --force-clean build-dir $(FLATPAK_MANIFEST)

.PHONY: flatpak-run
flatpak-run:
	@$(FLATPAK_COMMAND) run $(FLATPAK_APP_ID)
