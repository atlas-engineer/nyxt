## We use some Bourne shell syntax.
SHELL = /bin/sh

LISP ?= sbcl
LISP_FLAGS ?= --non-interactive --no-userinit
## If you want to enable SBCL's user init file:
# LISP_FLAGS = --non-interactive

NEXT_INTERNAL_QUICKLISP = true

PREFIX = /usr/local
prefix = $(PREFIX)
BINDIR = $(PREFIX)/bin
DATADIR = $(PREFIX)/share

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
		--load next.asd \
		--eval '(asdf:make :next)'

## TODO: Update the rule once we have the resulting .app.
next-cocoa: next
	xcodebuild -project ports/cocoa-webkit/cocoa-webkit.xcodeproj
	mkdir -p build/Next.app
	mkdir -p build/Next.app/Contents/MacOS
	mkdir -p build/Next.app/Contents/Resources
	mkdir -p build/Next.app/Contents/Frameworks
	cp assets/Info.plist build/Next.app/Contents/Info.plist
	cp assets/next.icns build/Next.app/Contents/Resources/next.icns
	cp ports/cocoa-webkit/libxmlrpc.3.39.dylib              build/Next.app/Contents/Frameworks
	cp ports/cocoa-webkit/libxmlrpc_abyss.3.39.dylib        build/Next.app/Contents/Frameworks
	cp ports/cocoa-webkit/libxmlrpc_client.3.39.dylib       build/Next.app/Contents/Frameworks
	cp ports/cocoa-webkit/libxmlrpc_packetsocket.8.39.dylib build/Next.app/Contents/Frameworks
	cp ports/cocoa-webkit/libxmlrpc_server.3.39.dylib       build/Next.app/Contents/Frameworks
	cp ports/cocoa-webkit/libxmlrpc_server_abyss.3.39.dylib build/Next.app/Contents/Frameworks
	cp ports/cocoa-webkit/libxmlrpc_server_cgi.3.39.dylib   build/Next.app/Contents/Frameworks
	cp ports/cocoa-webkit/libxmlrpc_util.3.39.dylib         build/Next.app/Contents/Frameworks
	cp ports/cocoa-webkit/libxmlrpc_xmlparse.3.39.dylib     build/Next.app/Contents/Frameworks
	cp ports/cocoa-webkit/libxmlrpc_xmltok.3.39.dylib       build/Next.app/Contents/Frameworks
	mv next build/Next.app/Contents/MacOS
	mv ports/cocoa-webkit/build/Release/cocoa-webkit build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc_server_cgi.3.39.dylib    @executable_path/../Frameworks/libxmlrpc_server_cgi.3.39.dylib   build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc.3.39.dylib		    @executable_path/../Frameworks/libxmlrpc.3.39.dylib              build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc_xmltok.3.39.dylib	    @executable_path/../Frameworks/libxmlrpc_xmltok.3.39.dylib       build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc_util.3.39.dylib	    @executable_path/../Frameworks/libxmlrpc_util.3.39.dylib         build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc_packetsocket.8.39.dylib  @executable_path/../Frameworks/libxmlrpc_packetsocket.8.39.dylib build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc_server_abyss.3.39.dylib  @executable_path/../Frameworks/libxmlrpc_server_abyss.3.39.dylib build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc_server.3.39.dylib	    @executable_path/../Frameworks/libxmlrpc_server.3.39.dylib       build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc_abyss.3.39.dylib	    @executable_path/../Frameworks/libxmlrpc_abyss.3.39.dylib        build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc_client.3.39.dylib	    @executable_path/../Frameworks/libxmlrpc_client.3.39.dylib       build/Next.app/Contents/MacOS/cocoa-webkit
	install_name_tool -change /usr/local/lib/libxmlrpc_xmlparse.3.39.dylib	    @executable_path/../Frameworks/libxmlrpc_xmlparse.3.39.dylib     build/Next.app/Contents/MacOS/cocoa-webkit

.PHONY: next-gtk
next-gtk:
	$(MAKE) -C ports/gtk-webkit

.PHONY: all
all: next next-gtk

.PHONY: install-next-gtk
install-next-gtk: next-gtk
	$(MAKE) -C ports/gtk-webkit install

## TODO: Add install rule for Cocoa?
## TODO: Set version in next.desktop.
.PHONY: install-next
install-next: next
	mkdir -p "$(DESTDIR)$(BINDIR)"
	cp -f $< "$(DESTDIR)$(BINDIR)/"
	chmod 755 "$(DESTDIR)$(BINDIR)/"$<
	mkdir -p "$(DESTDIR)$(DATADIR)/xsessions/"
	$(LISP) $(LISP_FLAGS) --eval '(require "asdf")' --load next.asd \
		--eval '(with-open-file (stream "version" :direction :output :if-exists :supersede) (format stream "~a" (asdf/component:component-version (asdf:find-system :next))))'
	sed "s/VERSION/$$(cat version)/" assets/next.desktop > "$(DESTDIR)$(DATADIR)/xsessions/next.desktop"
	rm version
	for i in 16 32 128 256 512; do \
		mkdir -p "$(DESTDIR)$(DATADIR)/icons/hicolor/$${i}x$${i}/apps/" ; \
		cp -f assets/next_$${i}x$${i}.png "$(DESTDIR)$(DATADIR)/icons/hicolor/$${i}x$${i}/apps/next.png" ; \
		done

.PHONY: install
install: install-next install-next-gtk

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
		--eval '(quicklisp-quickstart:install :path "$(QUICKLISP_DIR)/")'

.PHONY: deps
deps: $(QUICKLISP_DIR)/setup.lisp
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load $< \
		--eval '(ql:quickload :trivial-features)' \
		--load next.asd \
		--eval '(ql:quickload :next)'

clean_deps:
	rm -rf quicklisp.lisp
	rm -rf $(QUICKLISP_DIR)
