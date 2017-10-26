;;;; cocoa.lisp --- cocoa helper functions & data

(in-package :interface)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "COCOA"))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (objc:load-framework "WebKit" :webkit))

(defun url-from-string (s)
  (ccl::with-autorelease-pool
    (#/retain (#/URLWithString: ns:ns-url (ccl::%make-nsstring (string s))))))

(defun %browser-window (urlspec)
  (gui::assume-cocoa-thread)
  ;; Content rect for window, bounds rect for view.
  (ns:with-ns-rect (r 100.0 100.0 800.0 600.0)
    (ccl::with-autorelease-pool 
      (let* ((url (url-from-string urlspec))
             ;; Create a window with titlebar, close & iconize buttons
             (w (make-instance
                 'ns:ns-window
                 :with-content-rect r
                 :style-mask (logior #$NSTitledWindowMask
                                     #$NSClosableWindowMask
                                     #$NSMiniaturizableWindowMask
                                     #$NSResizableWindowMask)
                 ;; Backing styles other than #$NSBackingStoreBuffered
                 ;; don't work at all in Cocoa.
                 :backing #$NSBackingStoreBuffered
                 :defer t)))
        (#/setTitle: w (#/absoluteString url))
        ;; Create a web-view instance,
        (let* ((v (make-instance
                   'ns:web-view
                   :with-frame r
                   :frame-name #@"frame" ; could be documented a bit better ...
                   :group-name #@"group"))) ; as could this
          ;; Make the view be the window's content view.
          (#/setContentView: w v)
          ;; Start a URL request.  The request is processed
          ;; asynchronously, but apparently needs to be initiated
          ;; from the event-handling thread.
          (let* ((webframe (#/mainFrame v))
                 (request (#/requestWithURL: ns:ns-url-request url)))
            ;; Failing to wait until the main thread has
            ;; initiated the request seems to cause
            ;; view-locking errors.  Maybe that's just
            ;; an artifact of some other problem.
            (#/loadRequest: webframe request)
            ;; Make the window visible & activate it
            ;; The view knows how to draw itself and respond
            ;; to events.
            (#/makeKeyAndOrderFront: w +null-ptr+))
          v)))))

(defun browser-window (urlspec)
  (let* ((ip ccl::*initial-process*))
    (if (eq ccl::*current-process* ip)
      (%browser-window urlspec)
      (let* ((s (make-semaphore))
             (v nil))
        (process-interrupt ip (lambda ()
                                (setq v (%browser-window urlspec))
                                (signal-semaphore s)))
        (wait-on-semaphore s)
        v))))

(defun initialize ())
(defun start ()
  (browser-window "https://github.com/nEXT-Browser/nEXT"))
(defun kill ())
(defun set-visible-view ())
(defun add-to-stack-layout ())
(defun delete-view ())
(defun make-web-view ())
(defun web-view-scroll-down ())
(defun web-view-scroll-up ())
(defun web-view-set-url ())
(defun web-view-set-url-loaded-callback ())
(defun web-view-get-url ())
(defun make-minibuffer ())
(defun minibuffer-show ())
(defun minibuffer-hide ())
(defun minibuffer-get-input ())
(defun minibuffer-set-completion-function ())

