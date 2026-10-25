# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

## Use Bourne shell syntax.
SHELL = /bin/sh
LISP ?= sbcl
SBCL_FLAGS =
ifeq ($(LISP), sbcl)
	SBCL_FLAGS=--dynamic-space-size $(shell sbcl --noinform --no-userinit --non-interactive --eval '(prin1 (max 3072 (/ (sb-ext:dynamic-space-size) 1024 1024)))' --quit | tail -1)
endif

LISP_FLAGS ?= $(SBCL_FLAGS) --no-userinit --non-interactive

NYXT_RENDERER ?= electron
NASDF_USE_LOGICAL_PATHS ?= true
NODE_SETUP ?= true
OCICL ?= ocicl
OCICL_RUNTIME ?= $(HOME)/.local/share/ocicl/ocicl-runtime.lisp

export NYXT_RENDERER
export NASDF_USE_LOGICAL_PATHS
export NODE_SETUP

.PHONY: help
help:
	@cat INSTALL

makefile_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

## Directory of an ocicl-pinned system, looked up in ocicl.csv.
ocicl_system_dir = $(makefile_dir)ocicl/$(patsubst %/,%,$(dir $(shell awk -F', ' '$$1 == "$(1)" { print $$3; exit }' $(makefile_dir)ocicl.csv)))

lisp_eval:=$(LISP) $(LISP_FLAGS) \
	--eval '(require "asdf")' \
	--eval '(load "$(OCICL_RUNTIME)")' \
	--eval '(setf ocicl-runtime:*download* nil)' \
	--eval '(push (lambda (name) (let ((name (asdf:coerce-name name))) (when (or (string= name "nyxt") (uiop:string-prefix-p "nyxt/" name)) (pathname "$(makefile_dir)nyxt.asd")))) asdf:*system-definition-search-functions*)' \
	--eval '(asdf:load-asd "$(makefile_dir)/libraries/nasdf/nasdf.asd")' \
	--eval '(asdf:load-asd "$(makefile_dir)/nyxt.asd")' \
	--eval

lisp_quit:=--eval '(uiop:quit 0 \#+bsd nil)'

.PHONY: ocicl-install
ocicl-install:
	cd $(makefile_dir) && CL_SOURCE_REGISTRY= $(OCICL) -c never install

## asdf:load-system is a bit slow on :nyxt/$(NYXT_RENDERER)-application, so we
## keep a Make dependency on the Lisp files.
lisp_files := nyxt.asd $(shell find source libraries tests -type f -name '*.lisp')
nyxt: $(lisp_files) ocicl-install
	if [ "$(NYXT_RENDERER)" = "electron" ] && [ "$(NODE_SETUP)" = "true" ]; then \
		$(MAKE) -C $(call ocicl_system_dir,cl-electron) install; \
	fi
	if [ "$(NYXT_RENDERER)" = "cef" ] && [ -n "$(CEF_ROOT)" ]; then \
		cef_dir="$(call ocicl_system_dir,cl-cef)"; \
		cmake -S "$$cef_dir" \
			-B "$$cef_dir/build" \
			-DCEF_ROOT="$(CEF_ROOT)" && \
		cmake --build "$$cef_dir/build"; \
	fi
	$(lisp_eval) '(asdf:load-system :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(nyxt:prepare-relocatable-libraries)' \
		--eval '(asdf:make :nyxt/$(NYXT_RENDERER)-application)' \
		$(lisp_quit) || (printf "\n%s\n%s\n" "Compilation failed, see the above stacktrace." && exit 1)

.PHONY: all
all: nyxt

.PHONY: doc
doc: ocicl-install
	$(lisp_eval) '(asdf:load-system :nyxt)' \
		--eval '(asdf:load-system :nyxt/documentation)' $(lisp_quit)

.PHONY: check
check: ocicl-install
	$(lisp_eval) '(asdf:test-system :nyxt)'

.PHONY: integration
integration: ocicl-install
	XDG_SESSION_TYPE=x11 DISPLAY=$${DISPLAY:-:1} $(lisp_eval) \
		"(progn (push (lambda (name) (let ((name (asdf:coerce-name name))) (when (or (string= name \"nyxt\") (uiop:string-prefix-p \"nyxt/\" name)) #p\"$(makefile_dir)nyxt.asd\"))) asdf:*system-definition-search-functions*) (asdf:test-system :nyxt/integration))"

.PHONY: clean
clean:
	rm -f nyxt
