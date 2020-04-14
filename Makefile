## Use Bourne shell syntax.
SHELL = /bin/sh
UNAME := $(shell uname)

LISP ?= sbcl
## We use --non-interactive with SBCL so that errors don't interrupt the CI.
LISP_FLAGS ?= --no-userinit --non-interactive

NEXT_INTERNAL_QUICKLISP = true
NEXT_RENDERER = gtk

PREFIX = /usr/local
prefix = $(PREFIX)
BINDIR = $(PREFIX)/bin
DATADIR = $(PREFIX)/share
APPLICATIONSDIR = /Applications

.PHONY: help
help:
	@cat INSTALL

lisp_files := next.asd source/*.lisp

.PHONY: clean-fasls
clean-fasls:
	$(NEXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NEXT_INTERNAL_QUICKLISP=$(NEXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NEXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
		--eval '(ql:quickload :swank)' \
		--eval '(load (merge-pathnames  "contrib/swank-asdf.lisp" swank-loader:*source-directory*))' \
		--eval '(swank:delete-system-fasls "next")' \
		--eval '(uiop:quit)' || true

next: $(lisp_files) clean-fasls quicklisp-update
	$(NEXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NEXT_INTERNAL_QUICKLISP=$(NEXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NEXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
		--eval '(ql:quickload :prove-asdf)' \
		--load next.asd \
		--eval '(asdf:make :next/$(NEXT_RENDERER))' \
		--eval '(uiop:quit)' || (printf "\n%s\n%s\n" "Compilation failed." "Make sure 'xclip' and latest cl-webkit are available on your system." && exit 1)

.PHONY: app-bundle
app-bundle:
	mkdir -p ./Next.app/Contents/MacOS
	mkdir -p ./Next.app/Contents/Resources
	mv ./next ./Next.app/Contents/MacOS
	cp ./assets/Info.plist ./Next.app/Contents
	cp ./assets/next.icns ./Next.app/Contents/Resources

.PHONY: install-app-bundle
install-app-bundle:
	cp -r Next.app $(DESTDIR)/Applications

.PHONY: all
all:
ifeq ($(UNAME), Linux)
all: next
endif
ifeq ($(UNAME), Darwin)
all: next app-bundle
endif

## We use a temporary "version" file to generate the final next.desktop with the
## right version number.  Since "version" is a file target, third-party
## packaging systems can choose to generate "version" in advance before calling
## "make install-assets", so that they won't need to rely on Quicklisp.
version:
	$(NEXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NEXT_INTERNAL_QUICKLISP=$(NEXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NEXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
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
install:
install:
ifeq ($(UNAME), Linux)
install: install-next install-assets
endif
ifeq ($(UNAME), Darwin)
install: install-app-bundle
endif

QUICKLISP_URL = https://beta.quicklisp.org/quicklisp.lisp
DOWNLOAD_AGENT = curl
DOWNLOAD_AGENT_FLAGS = --output
QUICKLISP_DIR = quicklisp

quicklisp.lisp:
	$(NEXT_INTERNAL_QUICKLISP) && $(DOWNLOAD_AGENT) $(DOWNLOAD_AGENT_FLAGS) $@ $(QUICKLISP_URL) || true

$(QUICKLISP_DIR)/setup.lisp: quicklisp.lisp
	$(NEXT_INTERNAL_QUICKLISP) && rm -rf $(QUICKLISP_DIR) || true
	mkdir -p $(QUICKLISP_DIR)
	$(NEXT_INTERNAL_QUICKLISP) && $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load $< \
		--eval '(quicklisp-quickstart:install :path "$(QUICKLISP_DIR)/")' \
		--eval '(uiop:quit)' || true

# TODO: This updated package is not on Quicklisp yet, but once it is we can
# remove this special rule.
.PHONY: cl-webkit
cl-webkit: $(QUICKLISP_DIR)/setup.lisp
	if $(NEXT_INTERNAL_QUICKLISP); then \
			[ -e "$(QUICKLISP_DIR)"/local-projects/cl-webkit ] && \
				git -C "$(QUICKLISP_DIR)"/local-projects/cl-webkit pull || \
				git clone https://github.com/joachifm/cl-webkit "$(QUICKLISP_DIR)"/local-projects/cl-webkit ; \
		else \
			mkdir -p ~/common-lisp && \
			[ -e ~/common-lisp/cl-webkit ] && \
				git -C ~/common-lisp/cl-webkit pull || \
				git clone https://github.com/joachifm/cl-webkit ~/common-lisp/cl-webkit ; \
		fi

.PHONY: deps
deps: $(QUICKLISP_DIR)/setup.lisp cl-webkit
	$(NEXT_INTERNAL_QUICKLISP) && $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load $< \
		--eval '(ql:quickload :prove-asdf)' \
		--load next.asd \
		--eval '(ql:quickload :next/$(NEXT_RENDERER))' \
		--eval '(uiop:quit)' || true

## This rule only updates the internal distribution.
.PHONY: quicklisp-update
quicklisp-update: $(QUICKLISP_DIR)/setup.lisp
	$(NEXT_INTERNAL_QUICKLISP) && $(LISP) $(LISP_FLAGS) \
		--load $(QUICKLISP_DIR)/setup.lisp \
		--eval '(require "asdf")' \
		--eval '(ql:update-dist "quicklisp" :prompt nil)' \
		--eval '(uiop:quit)' || true

## Testing that next loads is a first test.
## TODO: Test that Next starts even with broken init file.
.PHONY: test
test: $(lisp_files)
	$(NEXT_INTERNAL_QUICKLISP) && $(MAKE) deps || true
	env NEXT_INTERNAL_QUICKLISP=$(NEXT_INTERNAL_QUICKLISP) $(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--eval '(when (string= (uiop:getenv "NEXT_INTERNAL_QUICKLISP") "true") (load "$(QUICKLISP_DIR)/setup.lisp"))' \
		--eval '(ql:quickload :prove-asdf)' \
		--load next.asd \
		--eval '(ql:quickload :next)' \
		--eval '(uiop:quit)'

.PHONY: clean-deps
clean-deps:
	rm -rf quicklisp.lisp
	rm -rf $(QUICKLISP_DIR)

.PHONY: clean
clean: clean-fasls clean-deps
