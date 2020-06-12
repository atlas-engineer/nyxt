## Use Bourne shell syntax.
SHELL = /bin/sh
UNAME := $(shell uname)

LISP ?= sbcl
## We use --non-interactive with SBCL so that errors don't interrupt the CI.
LISP_FLAGS ?= --no-userinit --non-interactive

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

.PHONY: clean-fasls
clean-fasls:
	$(NYXT_INTERNAL_QUICKLISP) && \
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NYXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
		--eval '(ql:quickload :swank)' \
		--eval '(load (merge-pathnames  "contrib/swank-asdf.lisp" swank-loader:*source-directory*))' \
		--eval '(swank:delete-system-fasls "nyxt")' \
		--eval '(uiop:quit)' || true

nyxt: $(lisp_files)
	$(MAKE) application

.PHONE: application
application: clean-fasls quicklisp-update
	$(NYXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NYXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
		--load nyxt.asd \
		--eval '(asdf:make :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(uiop:quit)' || (printf "\n%s\n%s\n" "Compilation failed, see the above stacktrace." "Also make sure 'xclip' and latest cl-webkit are available on your system." && exit 1)

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
all:
ifeq ($(UNAME), Linux)
all: nyxt
endif
ifeq ($(UNAME), Darwin)
all: nyxt app-bundle
endif

## We use a temporary "version" file to generate the final nyxt.desktop with the
## right version number.  Since "version" is a file target, third-party
## packaging systems can choose to generate "version" in advance before calling
## "make install-assets", so that they won't need to rely on Quicklisp.
version:
	$(NYXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NYXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
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

QUICKLISP_URL = https://beta.quicklisp.org/quicklisp.lisp
DOWNLOAD_AGENT = curl
DOWNLOAD_AGENT_FLAGS = --output
QUICKLISP_DIR = quicklisp

quicklisp.lisp:
	$(NYXT_INTERNAL_QUICKLISP) && $(DOWNLOAD_AGENT) $(DOWNLOAD_AGENT_FLAGS) $@ $(QUICKLISP_URL) || true

$(QUICKLISP_DIR)/setup.lisp: quicklisp.lisp
	$(NYXT_INTERNAL_QUICKLISP) && rm -rf $(QUICKLISP_DIR) || true
	mkdir -p $(QUICKLISP_DIR)
	$(NYXT_INTERNAL_QUICKLISP) && $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load $< \
		--eval '(quicklisp-quickstart:install :path "$(QUICKLISP_DIR)/")' \
		--eval '(uiop:quit)' || true

# TODO: This updated package is not on Quicklisp yet, but once it is we can
# remove this special rule.
.PHONY: cl-webkit
cl-webkit: $(QUICKLISP_DIR)/setup.lisp
	$(NYXT_INTERNAL_QUICKLISP) && git submodule update --init --remote || true

.PHONY: deps
deps: $(QUICKLISP_DIR)/setup.lisp cl-webkit
	$(NYXT_INTERNAL_QUICKLISP) && $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load $< \
		--load nyxt.asd \
		--eval '(ql:quickload :nyxt/$(NYXT_RENDERER)-application)' \
		--eval '(uiop:quit)' || true

## This rule only updates the internal distribution.
.PHONY: quicklisp-update
quicklisp-update: $(QUICKLISP_DIR)/setup.lisp
	$(NYXT_INTERNAL_QUICKLISP) && $(LISP) $(LISP_FLAGS) \
		--load $(QUICKLISP_DIR)/setup.lisp \
		--eval '(require "asdf")' \
		--eval '(ql:update-dist "quicklisp" :prompt nil)' \
		--eval '(uiop:quit)' || true

.PHONY: check
check: check-asdf check-binary

## TODO: Test that Nyxt starts even with broken init file.

.PHONY: check-build
check-build:
	$(NYXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NYXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
		--load nyxt.asd \
		--eval '(ql:quickload :nyxt)' \
		--eval '(uiop:quit)'

.PHONY: check-asdf
check-asdf:
	$(NYXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NYXT_INTERNAL_QUICKLISP=$(NYXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NYXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
		--load nyxt.asd \
		--eval '(asdf:test-system :nyxt)' \
		--eval '(uiop:quit)'

.PHONY: check-binary
check-binary: nyxt
	./nyxt -h

.PHONY: clean-deps
clean-deps:
	rm -rf quicklisp.lisp
	rm -rf $(QUICKLISP_DIR)

.PHONY: clean
clean: clean-fasls clean-deps
