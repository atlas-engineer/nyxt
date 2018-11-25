LISP ?= sbcl
LISP_FLAGS ?= --non-interactive

PREFIX = /usr/local
prefix = $(PREFIX)
BINDIR = $(PREFIX)/bin
DATADIR = $(PREFIX)/share

.PHONY: help
help:
	@echo 'Usage:                                                         '
	@echo '   make core        Create an executable of the Next core.     '
	@echo '   make next-cocoa  Create Next with the Cocoa port.           '
	@echo '   make next-gtk    Create Next with the GTK port.             '
	@echo '   make install     Install the GTK port.  Set DESTDIR to      '
	@echo '                      change the target destinatation.         '
	@echo '                                                               '
	@echo 'Set LISP and LISP_FLAGS to accommodate to your Lisp compiler.  '

deps := next.asd source/*.lisp source/ports/*lisp

.PHONY: core
core: $(deps)
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load next.asd \
		--eval '(asdf:make :next)'

next: $(deps)
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load next.asd \
		--eval '(asdf:make :next/release)'

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

.PHONY: next-gtk
next-gtk: next
	$(MAKE) -C ports/gtk-webkit

## TODO: Add install rule for Cocoa?
## TODO: Set version in next.desktop.
.PHONY: install
install: next next-gtk
	mkdir -p "$(DESTDIR)$(BINDIR)"
	cp -f $< "$(DESTDIR)$(BINDIR)/"
	cp -f ports/gtk-webkit/next-gtk-webkit "$(DESTDIR)$(BINDIR)/"
	chmod 755 "$(DESTDIR)$(BINDIR)/"$<
	mkdir -p "$(DESTDIR)$(DATADIR)/xsessions/"
	cp -f assets/next.desktop "$(DESTDIR)$(DATADIR)/xsessions/"
	mkdir -p "$(DESTDIR)$(DATADIR)/icons/hicolor/16x16/apps/"
	cp -f assets/next_16x16.png "$(DESTDIR)$(DATADIR)/icons/hicolor/16x16/apps/next.png"
	mkdir -p "$(DESTDIR)$(DATADIR)/icons/hicolor/32x32/apps/"
	cp -f assets/next_32x32.png "$(DESTDIR)$(DATADIR)/icons/hicolor/32x32/apps/next.png"
	mkdir -p "$(DESTDIR)$(DATADIR)/icons/hicolor/128x128/apps/"
	cp -f assets/next_128x128.png "$(DESTDIR)$(DATADIR)/icons/hicolor/128x128/apps/next.png"
	mkdir -p "$(DESTDIR)$(DATADIR)/icons/hicolor/256x256/apps/"
	cp -f assets/next_256x256.png "$(DESTDIR)$(DATADIR)/icons/hicolor/256x256/apps/next.png"
	mkdir -p "$(DESTDIR)$(DATADIR)/icons/hicolor/512x512/apps/"
	cp -f assets/next_512x512.png "$(DESTDIR)$(DATADIR)/icons/hicolor/512x512/apps/next.png"

.PHONY: clean
clean:
	rm -rf build

# TODO: Fetch dependencies with Quicklisp?
