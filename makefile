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
NYXT_RENDERER ?= gi-gtk
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
		npm install --verbose $(makefile_dir)_build/cl-electron; \
	fi
	$(lisp_eval) '(asdf:load-system :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(asdf:make :nyxt/$(NYXT_RENDERER)-application)' \
		$(lisp_quit) || (printf "\n%s\n%s\n" "Compilation failed, see the above stacktrace." && exit 1)

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
	$(lisp_eval) '(asdf:test-system :nyxt)'

.PHONY: clean-submodules
clean-submodules:
	git submodule deinit --force --all

.PHONY: clean
clean: clean-submodules
	rm -rf build

# Flatpak

FLATPAK_COMMAND = flatpak
FLATPAK_BUILDER = flatpak-builder

FLATPAK_WEBKITGTK_ID = engineer.atlas.Nyxt-WebKitGTK
FLATPAK_WEBKITGTK_MANIFEST := $(FLATPAK_WEBKITGTK_ID).yaml
FLATPAK_WEBKITGTK_EXPORT_REPOSITORY = build/nyxt-webkitgtk-flatpak-repository

.PHONY: flatpak-webkitgtk-build
flatpak-webkitgtk-build:
# To start a shell before building add --build-shell=nyxt.
	@$(FLATPAK_BUILDER) --force-clean --user --install --default-branch=local build $(FLATPAK_WEBKITGTK_MANIFEST)

.PHONY: flatpak-webkitgtk-run
flatpak-webkitgtk-run:
	@$(FLATPAK_COMMAND) run --branch=local $(FLATPAK_WEBKITGTK_ID)

.PHONY: flatpak-webkitgtk-repository
flatpak-webkitgtk-repository:
	mkdir -p $(FLATPAK_WEBKITGTK_EXPORT_REPOSITORY)
	@$(FLATPAK_BUILDER) --force-clean --repo=$(FLATPAK_WEBKITGTK_EXPORT_REPOSITORY) build $(FLATPAK_WEBKITGTK_MANIFEST)

.PHONY: flatpak-webkitgtk-bundle
flatpak-webkitgtk-bundle:
	@$(FLATPAK_COMMAND) build-bundle $(FLATPAK_WEBKITGTK_EXPORT_REPOSITORY) nyxt-webkitgtk.flatpak $(FLATPAK_WEBKITGTK_ID)

FLATPAK_ELECTRON_ID = engineer.atlas.Nyxt-Electron
FLATPAK_ELECTRON_MANIFEST := $(FLATPAK_ELECTRON_ID).yaml
FLATPAK_ELECTRON_EXPORT_REPOSITORY = build/nyxt-electron-flatpak-repository

.PHONY: flatpak-electron-build
flatpak-electron-build:
# To start a shell before building add --build-shell=nyxt.
	@$(FLATPAK_BUILDER) --force-clean --user --install --default-branch=local build $(FLATPAK_ELECTRON_MANIFEST)

.PHONY: flatpak-electron-run
flatpak-electron-run:
	@$(FLATPAK_COMMAND) run --branch=local $(FLATPAK_ELECTRON_ID)

.PHONY: flatpak-electron-repository
flatpak-electron-repository:
	mkdir -p $(FLATPAK_ELECTRON_EXPORT_REPOSITORY)
	@$(FLATPAK_BUILDER) --force-clean --repo=$(FLATPAK_ELECTRON_EXPORT_REPOSITORY) build $(FLATPAK_ELECTRON_MANIFEST)

.PHONY: flatpak-electron-bundle
flatpak-electron-bundle:
	@$(FLATPAK_COMMAND) build-bundle $(FLATPAK_ELECTRON_EXPORT_REPOSITORY) nyxt-electron.flatpak $(FLATPAK_ELECTRON_ID)
