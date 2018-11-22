LISP?=sbcl
LISP_FLAGS?=--non-interactive

help:
	@echo 'Makefile for Next. Please run this Makefile from the directory '
	@echo 'it is located in.                                              '
	@echo '                                                               '
	@echo '                                                               '
	@echo 'Usage:                                                         '
	@echo '   make next_core   create an executable of the Next core.     '
	@echo '                                                               '

next_core:
	$(LISP)	$(LISP_FLAGS) \
		--eval '(require "asdf")' \
		--load next.asd \
		--eval '(asdf:make :next)'
