LISP?=sbcl

build-gtk:
	$(LISP)	--non-interactive \
		--load next.asd \
		--eval '(ql:quickload :next/gtk)' \
		--eval '(asdf:make :next/gtk)'
