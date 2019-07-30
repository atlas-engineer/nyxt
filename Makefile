## Use Bourne shell syntax.
SHELL = /bin/sh
UNAME := $(shell uname)

LISP ?= sbcl
LISP_FLAGS ?= --no-userinit

NEXT_INTERNAL_QUICKLISP = true

PREFIX = /usr/local
prefix = $(PREFIX)
BINDIR = $(PREFIX)/bin
DATADIR = $(PREFIX)/share
APPLICATIONSDIR = /Applications

.PHONY: help
help:
	@cat INSTALL

lisp_files := next.asd source/*.lisp source/ports/*.lisp

next: $(lisp_files)
	$(NEXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NEXT_INTERNAL_QUICKLISP=$(NEXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NEXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
		--eval '(ql:quickload :trivial-features)' \
		--eval '(ql:quickload :prove-asdf)' \
		--load next.asd \
		--eval '(asdf:make :next)' \
		--eval '(uiop:quit)'

.PHONEY: app-bundle
app-bundle: next
	mkdir -p ./Next.app/Contents/MacOS
	mkdir -p ./Next.app/Resources
	mv ./next ./Next.app/Contents/MacOS
	cp -r ./ports/pyqt-webengine/ ./Next.app/Contents/MacOS
	mv ./Next.app/Contents/MacOS/next-pyqt-webengine.py ./Next.app/Contents/MacOS/next-pyqt-webengine
	chmod +x ./Next.app/Contents/MacOS/next-pyqt-webengine
	cp ./assets/Info.plist ./Next.app/Contents
	cp ./assets/next.icns ./Next.app/Resource

.PHONEY: install-app-bundle
install-app-bundle:
	mv Next.app $(APPLICATIONSDIR)

.PHONY: gtk-webkit
gtk-webkit:
	$(MAKE) -C ports/gtk-webkit

.PHONY: all
all: next gtk-webkit

.PHONY: install-gtk-webkit
install-gtk-webkit: gtk-webkit
	$(MAKE) -C ports/gtk-webkit install DESTDIR=$(DESTDIR) PREFIX=$(PREFIX)

## We use a temporary "version" file to generate the final next.desktop with the
## right version number.  Since "version" is a file target, third-party
## packaging systems can choose to generate "version" in advance before calling
## "make install-assets", so that they won't need to rely on Quicklisp.
version:
	$(NEXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NEXT_INTERNAL_QUICKLISP=$(NEXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NEXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
		--eval '(ql:quickload :trivial-features)' \
		--eval '(ql:quickload :prove-asdf)' \
		--load next.asd \
		--eval '(with-open-file (stream "version" :direction :output :if-exists :supersede) (format stream "~a" (asdf/component:component-version (asdf:find-system :next))))' \
		--eval '(uiop:quit)'


.PHONY: install-assets
install-assets: version
	mkdir -p "$(DESTDIR)$(DATADIR)/applications/"
	sed "s/VERSION/$$(cat version)/" assets/next.desktop > "$(DESTDIR)$(DATADIR)/applications/next.desktop"
	rm version
	for i in 16 32 128 256 512; do \
		mkdir -p "$(DESTDIR)$(DATADIR)/icons/hicolor/$${i}x$${i}/apps/" ; \
		cp -f assets/next_$${i}x$${i}.png "$(DESTDIR)$(DATADIR)/icons/hicolor/$${i}x$${i}/apps/next.png" ; \
		done

.PHONY: install-next
install-next: next
	mkdir -p "$(DESTDIR)$(BINDIR)"
	cp -f $< "$(DESTDIR)$(BINDIR)/"
	chmod 755 "$(DESTDIR)$(BINDIR)/"$<

.PHONY: install
install: install-next install-gtk-webkit install-assets

.PHONY: clean
clean:
	rm -rf build

QUICKLISP_URL = https://beta.quicklisp.org/quicklisp.lisp
DOWNLOAD_AGENT = curl
DOWNLOAD_AGENT_FLAGS = --output
QUICKLISP_DIR = quicklisp

quicklisp.lisp:
	$(DOWNLOAD_AGENT) $(DOWNLOAD_AGENT_FLAGS) $@ $(QUICKLISP_URL)

$(QUICKLISP_DIR)/setup.lisp: quicklisp.lisp
	rm -rf $(QUICKLISP_DIR)
	mkdir -p $(QUICKLISP_DIR)
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load $< \
		--eval '(quicklisp-quickstart:install :path "$(QUICKLISP_DIR)/")' \
		--eval '(uiop:quit)'

.PHONY: deps
deps: $(QUICKLISP_DIR)/setup.lisp
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load $< \
		--eval '(ql:quickload :trivial-features)' \
		--eval '(ql:quickload :prove-asdf)' \
		--load next.asd \
		--eval '(ql:quickload :next)' \
		--eval '(uiop:quit)'

.PHONY: clean-deps
clean-deps:
	rm -rf quicklisp.lisp
	rm -rf $(QUICKLISP_DIR)

.PHONY: clean-all
clean-all: clean clean-deps
