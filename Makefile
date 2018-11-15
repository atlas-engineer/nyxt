LISP?=sbcl

build-gtk:
	$(LISP)	--non-interactive \
		--eval '(require "asdf")' \
		--load next.asd \
		--eval '(ql:quickload :next/gtk)' \
		--eval '(setq *exit-timeout* nil)' \
		--eval '(asdf:make :next/gtk)'
