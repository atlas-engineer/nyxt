## Use Bourne shell syntax.
SHELL = /bin/sh
UNAME := $(shell uname)

LISP ?= sbcl
## We use --non-interactive with SBCL so that errors don't interrupt the CI.
LISP_FLAGS ?= --no-userinit --non-interactive
QUICKLISP_DIR=quicklisp-client
QUICKLISP_LIBRARIES=quicklisp-libraries

NYXT_INTERNAL_QUICKLISP=true
NYXT_RENDERER = gtk

PREFIX = /usr/local
prefix = $(PREFIX)
BINDIR = $(PREFIX)/bin
DATADIR = $(PREFIX)/share
APPLICATIONSDIR = /Applications

.PHONY: help
help:
	@cat INSTALL

lisp_eval:=$(LISP) $(LISP_FLAGS) \
	--eval '(require "asdf")' \
	--eval '(when (string= "true" "$(NYXT_INTERNAL_QUICKLISP)") (push :nyxt-internal-quicklisp *features*))' \
	--eval '(asdf:load-asd "nyxt.asd")' --eval
lisp_quit:=--eval '(uiop:quit)'

.PHONY: clean-fasls
clean-fasls:
	$(lisp_eval) '(asdf:make :nyxt/clean-fasls)' $(lisp_quit)

.PHONY: nyxt
nyxt:
	$(lisp_eval) '(asdf:make :nyxt/$(NYXT_RENDERER)-application)' \
		$(lisp_quit) || \
		(printf "\n%s\n%s\n" "Compilation failed, see the above stacktrace." && exit 1)

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
	$(lisp_eval) '(asdf:make :nyxt/version)' $(lisp_quit)

.PHONY: install
ifeq ($(UNAME), Linux)
install:
	$(lisp_eval) '(asdf:make :nyxt/install)' $(lisp_quit)
endif
ifeq ($(UNAME), Darwin)
install: install-app-bundle
endif

.PHONY: doc
doc:
	$(lisp_eval) '(asdf:load-system :nyxt/documentation)' $(lisp_quit)

.PHONY: check
check:
	$(lisp_eval) '(asdf:test-system :nyxt)' $(lisp_quit)

.PHONY: clean-deps
clean-deps:
	rm -rf $(QUICKLISP_DIR)

.PHONY: clean
clean: clean-fasls clean-deps
