## Use Bourne shell syntax.
SHELL = /bin/sh
UNAME := $(shell uname)

LISP ?= sbcl
## We use --non-interactive with SBCL so that errors don't interrupt the CI.
LISP_FLAGS ?= --no-userinit --non-interactive
QUICKLISP_DIR=quicklisp-client
QUICKLISP_LIBRARIES=quicklisp-libraries

NYXT_INTERNAL_QUICKLISP = true
NYXT_RENDERER = gtk

PREFIX = /usr/local
prefix = $(PREFIX)
BINDIR = $(PREFIX)/bin
DATADIR = $(PREFIX)/share
APPLICATIONSDIR = /Applications

.PHONY: help
help:
	@cat INSTALL

lisp_files := nyxt.asd $(shell find . -type f -name '*.lisp')
quicklisp_set_dir=(push \#p"$(QUICKLISP_LIBRARIES)/" (symbol-value (find-symbol "*LOCAL-PROJECT-DIRECTORIES*" (find-package (quote ql)))))
quicklisp_maybe_load=(when (string= (uiop:getenv "NYXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp") $(quicklisp_set_dir))

.PHONY: clean-fasls
clean-fasls:
	$(NYXT_INTERNAL_QUICKLISP) && \
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load $(QUICKLISP_DIR)/setup.lisp \
		--load nyxt.asd \
		--eval '(ql:quickload :swank)' \
		--eval '(load (merge-pathnames  "contrib/swank-asdf.lisp" swank-loader:*source-directory*))' \
		--eval '(swank:delete-system-fasls "nyxt")' \
		--eval '(uiop:quit)' || true

nyxt: $(lisp_files)
	$(MAKE) application

.PHONE: application
application: deps
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '$(quicklisp_maybe_load)' \
		--load nyxt.asd \
		--eval '(asdf:make :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(uiop:quit)' || (printf "\n%s\n%s\n" "Compilation failed, see the above stacktrace." && exit 1)

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
version: deps
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '$(quicklisp_maybe_load)' \
		--load nyxt.asd \
		--eval '(with-open-file (stream "version" :direction :output :if-exists :supersede) (format stream "~a" (asdf/component:component-version (asdf:find-system :nyxt))))' \
		--eval '(uiop:quit)'

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
install:
ifeq ($(UNAME), Linux)
install: install-nyxt install-assets
endif
ifeq ($(UNAME), Darwin)
install: install-app-bundle
endif

.PHONY: quicklisp-extra-libs
quicklisp-extra-libs:
	$(NYXT_INTERNAL_QUICKLISP) && git submodule update --init || true

## This rule only updates the internal distribution.
.PHONY: quicklisp-update
quicklisp-update:
	$(NYXT_INTERNAL_QUICKLISP) && $(LISP) $(LISP_FLAGS) \
		--load $(QUICKLISP_DIR)/setup.lisp \
		--eval '(require "asdf")' \
		--eval '(ql:update-dist "quicklisp" :prompt nil)' \
		--eval '(uiop:quit)' || true

.PHONY: build-deps
build-deps: quicklisp-extra-libs
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load $(QUICKLISP_DIR)/setup.lisp \
		--eval '$(quicklisp_set_dir)' \
		--load nyxt.asd \
		--eval '(ql:quickload :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(uiop:quit)' || true
	$(MAKE) quicklisp-update

.PHONY: deps
deps:
	$(NYXT_INTERNAL_QUICKLISP) && $(MAKE) build-deps || true

manual.html: $(lisp_files)
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '$(quicklisp_maybe_load)' \
		--load nyxt.asd \
		--eval '(asdf:load-system :nyxt)' \
		--eval '(with-open-file  (out "manual.html" :direction :output) (write-string (nyxt::manual-content) out))' \
		--eval '(uiop:quit)'

.PHONY: doc
doc: manual.html

.PHONY: check
check: check-asdf check-binary

## TODO: Test that Nyxt starts even with broken init file.

.PHONY: check-build
check-build: deps
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '$(quicklisp_maybe_load)' \
		--load nyxt.asd \
		--eval '(ql:quickload :nyxt)' \
		--eval '(uiop:quit)'

.PHONY: check-asdf
check-asdf: deps
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '$(quicklisp_maybe_load)' \
		--load nyxt.asd \
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
