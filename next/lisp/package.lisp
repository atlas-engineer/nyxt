;;;; package.lisp --- definition of package used by next

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
   #:set-visible-view
   #:add-to-stack-layout
   #:delete-view
   #:make-web-view
   #:web-view-scroll-down
   #:web-view-scroll-up
   #:web-view-set-url
   #:web-view-set-url-loaded-callback
   #:web-view-get-url
   #:make-minibuffer
   #:minibuffer-show
   #:minibuffer-hide
   #:minibuffer-get-input
   #:minibuffer-set-completion-function))
