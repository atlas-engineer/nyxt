LISP?=sbcl

build-gtk:
	$(LISP) --load next.asd \
		--eval '(ql:quickload :next/gtk)' \
		--eval '(asdf:make :next/gtk)' \
		--eval '(quit)'
