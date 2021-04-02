# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

## Use Bourne shell syntax.
SHELL = /bin/sh
UNAME := $(shell uname)

LISP ?= sbcl
## We use --non-interactive with SBCL so that errors don't interrupt the CI.
LISP_FLAGS ?= --no-userinit --non-interactive

NYXT_INTERNAL_QUICKLISP=true
NYXT_RENDERER=gtk

PREFIX = /usr/local
prefix = $(PREFIX)
BINDIR = $(PREFIX)/bin
DATADIR = $(PREFIX)/share
APPLICATIONSDIR = /Applications

.PHONY: help
help:
	@cat INSTALL

makefile_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

load_or_quickload=asdf:load-system
ifeq ($(NYXT_INTERNAL_QUICKLISP), true)
load_or_quickload=ql:quickload
endif

lisp_eval:=$(LISP) $(LISP_FLAGS) \
	--eval '(require "asdf")' \
	--eval '(asdf:load-asd "$(makefile_dir)/nyxt.asd")' \
  --eval '(when (string= "$(NYXT_INTERNAL_QUICKLISP)" "true") (asdf:load-system :nyxt/quicklisp))' \
	--eval
lisp_quit:=--eval '(uiop:quit)'

# .PHONY: clean-fasls
# clean-fasls:
# 	$(NYXT_INTERNAL_QUICKLISP) && \
# 	$(LISP) $(LISP_FLAGS) \
# 		--eval '(require "asdf")' \
# 		--load $(QUICKLISP_DIR)/setup.lisp \
# 		--eval '(asdf:load-asd "$(makefile_dir)/nyxt.asd")' \
# 		--eval '(ql:quickload :swank)' \
# 		--eval '(load (merge-pathnames  "contrib/swank-asdf.lisp" swank-loader:*source-directory*))' \
# 		--eval '(swank:delete-system-fasls "nyxt")' \
# 		--eval '(uiop:quit)' || true

## We need lisp_files to avoid building binary when nothing has changed.
## TODO: Can ASDF be this smart?
lisp_files := nyxt.asd $(shell find . -type f -name '*.lisp')
nyxt: $(lisp_files)
	$(lisp_eval) '($(load_or_quickload) :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(asdf:make :nyxt/$(NYXT_RENDERER)-application)' \
		|| (printf "\n%s\n%s\n" "Compilation failed, see the above stacktrace." && exit 1)

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

## We use a temporary "version" file to generate the final nyxt.desktop with the
## right version number.  Since "version" is a file target, third-party
## packaging systems can choose to generate "version" in advance before calling
## "make install-assets", so that they won't need to rely on Quicklisp.
version:
	$(lisp_eval) '($(load_or_quickload) :nyxt)' \
		--eval '(asdf:make :nyxt/version)' $(lisp_quit)

.PHONY: install-assets
install-assets: version
	mkdir -p "$(DESTDIR)$(DATADIR)/applications/"
	sed "s/VERSION/$$(cat version)/" assets/nyxt.desktop > "$(DESTDIR)$(DATADIR)/applications/nyxt.desktop"
	rm version
	for i in 16 32 128 256 512; do \
		mkdir -p "$(DESTDIR)$(DATADIR)/icons/hicolor/$${i}x$${i}/apps/" ; \
		cp -f assets/nyxt_$${i}x$${i}.png "$(DESTDIR)$(DATADIR)/icons/hicolor/$${i}x$${i}/apps/nyxt.png" ; \
		done

.PHONY: install-nyxt
install-nyxt: nyxt
	mkdir -p "$(DESTDIR)$(BINDIR)"
	cp -f $< "$(DESTDIR)$(BINDIR)/"
	chmod 755 "$(DESTDIR)$(BINDIR)/"$<

.PHONY: install
install:
ifeq ($(UNAME), Linux)
install: install-nyxt install-assets
endif
ifeq ($(UNAME), FreeBSD)
install: install-nyxt install-assets
endif
ifeq ($(UNAME), Darwin)
install: install-app-bundle
endif

.PHONY: doc
doc:
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '$(quicklisp_maybe_load)' \
		--eval '(asdf:load-asd "$(makefile_dir)/nyxt.asd")' \
		--eval '(asdf:load-system :nyxt/documentation)' \
		--eval '(uiop:quit)'

.PHONY: check
check: check-asdf check-binary

## TODO: Test that Nyxt starts even with broken init file.

.PHONY: check-asdf
check-asdf: deps
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '$(quicklisp_maybe_load)' \
		--eval '(asdf:load-asd "$(makefile_dir)/nyxt.asd")' \
		--eval '(asdf:test-system :nyxt)' \
		--eval '(uiop:quit)'

.PHONY: check-binary
check-binary: nyxt
	./nyxt -h

.PHONY: clean-deps
clean-deps:
	rm -rf $(QUICKLISP_DIR)

.PHONY: clean
clean: clean-fasls clean-deps
