LISP?=sbcl
LISP_FLAGS?=--non-interactive

help:
	@echo 'Makefile for Next. Please run this Makefile from the directory '
	@echo 'it is located in.                                              '
	@echo '                                                               '
	@echo '                                                               '
	@echo 'Usage:                                                         '
	@echo '   make core   create an executable of the Next core.          '
	@echo '   make next-cocoa  create Next with the Cocoa port            '
	@echo '                                                               '

core:
	$(LISP)	--eval '(require "asdf")' \
		--load next.asd \
		--eval '(asdf:make :next)'

next-cocoa: core
	mkdir -p build/Next.app
	mkdir -p build/Next.app/Contents/MacOS
	mkdir -p build/Next.app/Contents/Resources
	cp assets/Info.plist build/Next.app/Contents/Info.plist
	cp assets/next.icns build/Next.app/Contents/Resources/next.icns
	mv next build/Next.app/Contents/MacOS
clean:
	rm -rf build

.PHONY: clean cocoa core help
