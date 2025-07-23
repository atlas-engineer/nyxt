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

LISP_FLAGS ?= $(SBCL_FLAGS) --no-userinit --non-interactive

NYXT_SUBMODULES ?= true
NYXT_RENDERER ?= electron
NASDF_USE_LOGICAL_PATHS ?= true
NODE_SETUP ?= true

export NYXT_SUBMODULES
export NYXT_RENDERER
export NASDF_USE_LOGICAL_PATHS
export NODE_SETUP

.PHONY: help
help:
	@cat INSTALL

makefile_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

ifeq ($(NYXT_SUBMODULES),true)
	CL_SOURCE_REGISTRY = $(makefile_dir)_build//
	export CL_SOURCE_REGISTRY
endif

lisp_eval:=$(LISP) $(LISP_FLAGS) \
	--eval '(require "asdf")' \
	--eval '(asdf:load-asd "$(makefile_dir)/libraries/nasdf/nasdf.asd")' \
	--eval '(asdf:load-asd "$(makefile_dir)/nyxt.asd")' \
	--eval

lisp_quit:=--eval '(uiop:quit 0 \#+bsd nil)'

## asdf:load-system is a bit slow on :nyxt/$(NYXT_RENDERER)-application, so we
## keep a Make dependency on the Lisp files.
lisp_files := nyxt.asd $(shell find . -type f -name '*.lisp')
nyxt: $(lisp_files)
	if [ "$(NYXT_RENDERER)" = "electron" ] && \
	   [ "$(NODE_SETUP)" = "true" ] && \
	   [ "$(NYXT_SUBMODULES)" = "true" ]; then \
		$(MAKE) -C $(makefile_dir)_build/cl-electron install; \
	fi
	$(lisp_eval) '(asdf:load-system :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(asdf:make :nyxt/$(NYXT_RENDERER)-application)' \
		$(lisp_quit) || (printf "\n%s\n%s\n" "Compilation failed, see the above stacktrace." && exit 1)

.PHONY: all
all: nyxt

.PHONY: doc
doc:
	$(lisp_eval) '(asdf:load-system :nyxt)' \
		--eval '(asdf:load-system :nyxt/documentation)' $(lisp_quit)

.PHONY: check
check:
	$(lisp_eval) '(asdf:test-system :nyxt)'

.PHONY: clean
clean:
	rm nyxt
