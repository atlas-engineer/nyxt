LISP?=sbcl
LISP_FLAGS?=--non-interactive


## TODO: Use ifeq() to detect system.

.PHONY: help
help:
	@echo 'Usage:                                                         '
	@echo '   make core        Create an executable of the Next core.     '
	@echo '   make next-cocoa  Create Next with the Cocoa port.           '
	@echo '   make next-gtk    Create Next with the GTK port.             '
	@echo '   make install     Install the GTK port.  Set DESTDIR to      '
	@echo '                      change the target destinatation.         '
	@echo '                                                               '

.PHONY: core
core:
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load next.asd \
		--eval '(asdf:make :next)'

next:
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

ports/gtk-webkit/next-gtk-webkit:
	$(MAKE) -C ports/gtk-webkit
.PHONY: next-gtk
next-gtk: next ports/gtk-webkit/next-gtk-webkit

## TODO: Add install rule for Cocoa?
.PHONY: install
install: next next-gtk
	mkdir -p "$(DESTDIR)$(PREFIX)/bin"
	cp -f $< "$(DESTDIR)$(PREFIX)/bin/"
	cp -f ports/gtk-webkit/next-gtk-webkit "$(DESTDIR)$(PREFIX)/bin/"
	chmod 755 $(DESTDIR)$(PREFIX)/bin/$<
	mkdir -p "$(DESTDIR)$(PREFIX)/share/xsessions/"
	cp -f assets/next.desktop "$(DESTDIR)$(PREFIX)/share/xsessions/"
	$(foreach i, 16 32 128 256 512, mkdir -p "$(DESTDIR)$(PREFIX)/share/icons/hicolor/$(i)x$(i)/apps/" && \
		cp -f "assets/next_$(i)x$(i).png" "$(DESTDIR)$(PREFIX)/share/icons/hicolor/$(i)x$(i)/apps/next.png";)

.PHONY: clean
clean:
	rm -rf build

# TODO: Fetch dependencies with Quicklisp?
