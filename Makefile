LISP?=sbcl
LISP_FLAGS?=--non-interactive

help:
	@echo 'Makefile for Next. Please run this Makefile from the directory '
	@echo 'it is located in.                                              '
	@echo '                                                               '
	@echo 'Usage:                                                         '
	@echo '   make core        create an executable of the Next core.     '
	@echo '   make next-cocoa  create Next with the Cocoa port.           '
	@echo '                                                               '

core:
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load next.asd \
		--eval '(asdf:make :next)'

next-cocoa:
	$(LISP) $(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load next.asd \
		--eval '(asdf:make :next/release)'
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

clean:
	rm -rf build

.PHONY: clean next-cocoa core help
