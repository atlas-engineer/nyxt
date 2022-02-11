(uiop:define-package :nyxt/no-procrastinate-mode
    (:use :common-lisp :nyxt :nyxt/blocker-mode)
  (:import-from #:class-star #:define-class)
  (:documentation "Block resource queries for listed hosts."))
(in-package :nyxt/no-procrastinate-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-nyxt-package-nicknames))

(define-mode no-procrastinate-mode ()
  "Mode to block the loading of websites associated to procrastination from the
user perspective."
  ((constructor
    (lambda ()
      (echo "teste")))))
