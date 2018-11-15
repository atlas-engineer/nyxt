;;; package.lisp --- definition of package used by next

(in-package :cl-user)

(defpackage :next
  (:use :common-lisp)
  (:export
   #:start
   #:push-key-chord))

(defpackage :interface
  (:use :common-lisp)
  (:export
   #:initialize
   #:start
   #:kill
   #:copy
   #:paste
   #:cut
   #:process-event
   #:set-visible-view
   #:delete-view
   #:make-web-view
   #:web-view-set-url
   #:web-view-set-url-loaded-callback
   #:web-view-get-url
   #:web-view-execute
   #:make-minibuffer
   #:minibuffer-show
   #:minibuffer-hide
   #:minibuffer-set-input
   #:minibuffer-get-input
   #:minibuffer-get-input-complete
   #:minibuffer-select-next
   #:minibuffer-select-previous
   #:minibuffer-set-completion-function))
