;;;; cocoa.lisp --- cocoa helper functions & data

(in-package :interface)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :interface-packages)
  (require :objc-initialize)
  (require :dev-tools)
  (require :window-utils)
  (require :window-controller)
  (require :text-views)
  (require :constraint-layout))

(defparameter *window* nil)
(defparameter *next-view* nil)

(defclass minibuffer-view (ns:ns-view)
  ((input-buffer :accessor input-buffer)
   (completion-table :accessor completion-table))
    (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self minibuffer-view)
				       &key &allow-other-keys)
  (let ((input-field (make-instance 'ns:ns-text-field))
	(candidate-table (make-instance 'ns:ns-table-view)))
    (#/setTranslatesAutoresizingMaskIntoConstraints: self #$NO)
    (#/setTranslatesAutoresizingMaskIntoConstraints: input-field #$NO)
    (#/setTranslatesAutoresizingMaskIntoConstraints: candidate-table #$NO)
    (setf (input-buffer self) input-field)
    (setf (completion-table self) candidate-table)
    (#/addSubview: self input-field)
    (#/addSubview: self candidate-table)
    (constrain (= (center-x input-field) (center-x self)))
    (constrain (= (width input-field) (width self)))
    (constrain (= (top input-field) (top self)))
    (constrain (= (height input-field) 20))
    (constrain (= (top candidate-table) (bottom input-field)))
    (constrain (= (bottom candidate-table) (bottom self)))
    (constrain (= (width candidate-table) (width self)))))

(defclass fill-container-view (ns:ns-view)
  ((fill-view :accessor fill-view
		    :initarg :fill-view))
  (:default-initargs
   :fill-view nil)
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self fill-container-view)
                                       &key (fill-view nil) &allow-other-keys)
  (when (and fill-view (view-p fill-view))
    (#/addSubview: self fill-view)
    (constrain-size-relative-to fill-view self :rel :=))
  (#/setTranslatesAutoresizingMaskIntoConstraints: self #$NO))

(defmethod set-fill-view ((self fill-container-view) view)
  (on-main-thread
   (when (fill-view self)
     (#/removeFromSuperview (fill-view self)))
   (#/addSubview: self view)
   (constrain-size-relative-to view self :rel :=))
  (setf (fill-view self) view))

(defclass next-view (ns:ns-view)
  ((%fill-container-view :accessor fill-container-view)
   (%minibuffer-view :accessor minibuffer-view)
   (minibuffer-height-constraint :accessor minibuffer-height-constraint))
  (:metaclass ns:+ns-object))

(defmethod initialize-instance :after ((self next-view)
				       &key &allow-other-keys)
  (let ((fvc (make-instance 'fill-container-view))
	(mb (make-instance 'minibuffer-view)))
    (#/addSubview: self fvc)
    (#/addSubview: self mb)
    (setf (fill-container-view self) fvc)
    (setf (minibuffer-view self) mb)
    (constrain (= (top fvc) (top self)))
    (constrain (= (width fvc) (width self)))
    (constrain (= (bottom fvc) (top mb)))
    (constrain (= (bottom mb) (bottom self)))
    (constrain (= (width mb) (width self)))
    (setf (minibuffer-height-constraint self)
	  (constrain (= (height mb) 100)))))

(defmethod hide-minibuffer ((self next-view))
    (#/setConstant: (minibuffer-height-constraint self) 0))

(defmethod show-minibuffer ((self next-view))
    (#/setConstant: (minibuffer-height-constraint self) 0))

(defmacro on-main-thread (&rest actions)
  `(ccl::call-in-event-process
     #'(lambda ()
         ,@actions)))

(defun ns-to-lisp-string (ns-str)
  (if (and (not (eql (%null-ptr) ns-str)) (plusp (#/length ns-str)))
    (ccl::%get-utf-8-cstring (#/UTF8String ns-str))
    ""))

(defclass next-window (ns:ns-window) ()
  (:metaclass ns:+ns-object))

(objc:defmethod (#/acceptsFirstResponder :<BOOL>) ((self next-window)) t)

(objc:defmethod (#/keyDown: :void) ((self next-window) event)
  (let* ((flags (#/modifierFlags event))
	 (character (ns-to-lisp-string (#/charactersIgnoringModifiers event))))
    (next:push-key-chord
     (> (logand flags #$NSControlKeyMask) 0)
     (> (logand flags #$NSAlternateKeyMask) 0)
     (> (logand flags #$NSCommandKeyMask) 0)
     character)
    (call-next-method event)))

(defun make-window ()
  (gui::assume-cocoa-thread)
  (ns:with-ns-rect (r 100.0 100.0 1024.0 768.0)
    (ccl::with-autorelease-pool 
      (let* ((.window. (make-instance
		      'next-window
		      :with-content-rect r
		      :style-mask (logior #$NSTitledWindowMask
					  #$NSClosableWindowMask
					  #$NSMiniaturizableWindowMask
					  #$NSResizableWindowMask)
		      :backing #$NSBackingStoreBuffered
		      :defer t))
	     (.next-view. (make-instance 'next-view)))
	(#/setContentView: .window. .next-view.)
	(#/makeKeyAndOrderFront: .window. +null-ptr+)
	(setf *window* .window.)
	(setf *next-view* .next-view.)
	*window*))))

(defun initialize ()
  (on-main-thread
   (make-window)))

(defun start ())

(defun kill ()
  (quit))

(defun set-visible-view (view)
  (set-fill-view (fill-container-view *next-view*) view))

(defun make-web-view ()
  (on-main-thread
   (let ((view
	  (make-instance
	   'ns:web-view
	   :frame-name #@"frame"
	   :group-name #@"group")))
     view)))

(defun url-from-string (s)
  (ccl::with-autorelease-pool
    (#/retain (#/URLWithString: ns:ns-url (ccl::%make-nsstring (string s))))))

(defun web-view-set-url (view url)
  (on-main-thread
   (let* ((nsurl (url-from-string url))
	  (webframe (#/mainFrame view))
	  (request (#/requestWithURL: ns:ns-url-request nsurl)))
     (#/loadRequest: webframe request))))

(defun add-to-stack-layout (view))
(defun delete-view (view))
(defun web-view-scroll-down (view))
(defun web-view-scroll-up (view))
(defun web-view-set-url-loaded-callback ())
(defun web-view-get-url (view))
(defun make-minibuffer ()
  (minibuffer-view *next-view*))
(defun minibuffer-show ())
(defun minibuffer-hide ())
(defun minibuffer-get-input ())
(defun minibuffer-set-completion-function ())
