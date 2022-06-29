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

export NYXT_SUBMODULES=true
export NYXT_RENDERER=gi-gtk
export NYXT_USE_LOGICAL_PATHS=true

.PHONY: help
help:
	@cat INSTALL

makefile_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

# The CFFI-specific snippet is useful when running in a Guix environment to register its libraries in CFFI.
# TODO: Find a better way to do it.
lisp_eval:=$(LISP) $(LISP_FLAGS) \
	--eval '(require "asdf")' \
	--eval '(when (string= "$(NYXT_SUBMODULES)" "true") (setf asdf:*default-source-registries* (list (quote asdf/source-registry:environment-source-registry))) (asdf:clear-configuration) (asdf:load-asd "$(makefile_dir)/nyxt-asdf.asd") (asdf:load-asd "$(makefile_dir)/nyxt.asd") (asdf:load-system :nyxt/submodules))' \
	--eval '(asdf:load-asd "$(makefile_dir)/nyxt-asdf.asd")' \
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

.PHONY: app-bundle
app-bundle:
	mkdir -p ./Nyxt.app/Contents/MacOS
	mkdir -p ./Nyxt.app/Contents/Resources
	mv ./nyxt ./Nyxt.app/Contents/MacOS
	cp ./assets/Info.plist ./Nyxt.app/Contents
	cp ./assets/nyxt.icns ./Nyxt.app/Contents/Resources

.PHONY: install-app-bundle
install-app-bundle:
	cp -r Nyxt.app $(DESTDIR)/Applications

.PHONY: all
all: nyxt
ifeq ($(UNAME), Darwin)
all: nyxt app-bundle
endif

.PHONY: install
ifeq ($(UNAME), Darwin)
install: install-app-bundle
else
install: all
	$(lisp_eval) '(asdf:load-system :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(asdf:make :nyxt/install)' $(lisp_quit)
endif

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
