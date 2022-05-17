;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmacro define-ffi-generic (name arguments &body options)
  "Like `defgeneric' but export NAME and define default dummy method if none is
provided."
  (let* ((methods (sera:filter (sera:eqs :method) options :key #'first))
         (options-sans-methods (set-difference options methods :key #'first)))
    `(progn
       (export-always ',name)
       (defgeneric ,name (,@arguments)
         ,@(if methods
               methods
               `((:method (,@arguments)
                   (declare (ignore ,@(set-difference arguments lambda-list-keywords))))))
         ,@options-sans-methods))))

(define-ffi-generic ffi-window-delete (window))
(define-ffi-generic ffi-window-fullscreen (window))
(define-ffi-generic ffi-window-unfullscreen (window))
(define-ffi-generic ffi-buffer-url (buffer))
(define-ffi-generic ffi-buffer-title (buffer))
(define-ffi-generic ffi-window-make (browser)
  (:method ((browser t))
    (declare (ignore browser))
    (make-instance 'window)))
(define-ffi-generic ffi-window-to-foreground (window)
  (:method ((window t))
    (setf (slot-value *browser* 'last-active-window) window)))
(define-ffi-generic ffi-window-set-title (window title))
(define-ffi-generic ffi-window-active (browser))
(define-ffi-generic ffi-window-set-buffer (window buffer &key focus))
(define-ffi-generic ffi-window-add-panel-buffer (window buffer side))
(define-ffi-generic ffi-window-delete-panel-buffer (window buffer))

(define-ffi-generic ffi-window-panel-buffer-width (window buffer)
  (:documentation "Return the panel BUFFER width as a number.
Setf-able."))
(define-ffi-generic ffi-window-prompt-buffer-height (window)
  (:documentation "Return the WINDOW prompt buffer height as a number.
Setf-able."))
(define-ffi-generic ffi-window-status-buffer-height (window)
  (:documentation "Return the WINDOW status buffer height as a number.
Setf-able."))
(define-ffi-generic ffi-window-message-buffer-height (window)
  (:documentation "Return the WINDOW message buffer height as a number.
Setf-able."))

(define-ffi-generic ffi-buffer-make (buffer))
(define-ffi-generic ffi-buffer-delete (buffer))
(define-ffi-generic ffi-buffer-load (buffer url))
(define-ffi-generic ffi-buffer-evaluate-javascript (buffer javascript &optional world-name))
(define-ffi-generic ffi-buffer-evaluate-javascript-async (buffer javascript &optional world-name))
(define-ffi-generic ffi-buffer-add-user-style (buffer css &key
                                                      world-name all-frames-p inject-as-author-p
                                                      allow-list block-list))
(define-ffi-generic ffi-buffer-remove-user-style (buffer style-sheet))
(define-ffi-generic ffi-buffer-add-user-script (buffer javascript &key
                                                       world-name all-frames-p at-document-start-p
                                                       run-now-p allow-list block-list))
(define-ffi-generic ffi-buffer-remove-user-script (buffer script))
(define-ffi-generic ffi-buffer-enable-javascript (buffer value))
(define-ffi-generic ffi-buffer-enable-javascript-markup (buffer value))
(define-ffi-generic ffi-buffer-enable-smooth-scrolling (buffer value))
(define-ffi-generic ffi-buffer-enable-media (buffer value))
(define-ffi-generic ffi-buffer-webgl-enabled-p (buffer))
(define-ffi-generic ffi-buffer-enable-webgl (buffer value))
(define-ffi-generic ffi-buffer-auto-load-image (buffer value))
(define-ffi-generic ffi-buffer-enable-sound (buffer value))

(define-ffi-generic ffi-buffer-user-agent (buffer)
  (:documentation "Return the user agent as a string.
Setf-able."))

(define-ffi-generic ffi-buffer-proxy (buffer)
  (:documentation "Return the proxy URL as a `quri:uri'.
Return the list of ignored hosts (list of strings) as a second value.

Setf-able.  The value is either a PROXY-URL or a pair of (PROXY-URL IGNORE-HOSTS).
PROXY-URL is a `quri:uri' and IGNORE-HOSTS a list of strings."))

(define-ffi-generic ffi-buffer-download (buffer url))

(define-ffi-generic ffi-buffer-zoom-level (buffer)
  (:method ((buffer t))
    (with-current-buffer buffer
      (peval (ps:chain document body style zoom))))
  (:documentation "Return the zoom level of the document.
Setf-able."))
(defmethod (setf ffi-buffer-zoom-level) (value (buffer buffer))
  (with-current-buffer buffer
      (peval (ps:let ((style (ps:chain document body style)))
               (setf (ps:@ style zoom)
                     (ps:lisp value))))))

(define-ffi-generic ffi-buffer-get-document (buffer)
  (:method ((buffer t))
    (pflet ((get-html (start end)
                      (ps:chain document document-element |innerHTML| (slice (ps:lisp start)
                                                                             (ps:lisp end))))
            (get-html-length ()
                             (ps:chain document document-element |innerHTML| length)))
      (with-current-buffer buffer
        (let ((slice-size 10000))
          (reduce #'str:concat
                  (loop for i from 0 to (truncate (get-html-length)) by slice-size
                        collect (get-html i (+ i slice-size)))))))))
(define-ffi-generic ffi-generate-input-event (window event))
(define-ffi-generic ffi-generated-input-event-p (window event))
(define-ffi-generic ffi-within-renderer-thread (browser thunk)
  (:method ((browser t) thunk)
    (declare (ignore browser))
    (funcall thunk)))
(define-ffi-generic ffi-kill-browser (browser))
(define-ffi-generic ffi-initialize (browser urls startup-timestamp)
  (:method ((browser t) urls startup-timestamp)
    (finalize browser urls startup-timestamp)))
(define-ffi-generic ffi-inspector-show (buffer))
(define-ffi-generic ffi-print-status (window text))
(define-ffi-generic ffi-print-message (window message)
  (:documentation "Print MESSAGE which is an HTML string."))
(define-ffi-generic ffi-display-url (text))
(define-ffi-generic ffi-buffer-cookie-policy (buffer value))
(define-ffi-generic ffi-set-preferred-languages (buffer value))
(define-ffi-generic ffi-focused-p (buffer))

(define-ffi-generic ffi-tracking-prevention (buffer)
  (:documentation "Return if Intelligent Tracking Prevention (ITP) is enabled.
Setf-able."))

(define-ffi-generic ffi-buffer-copy (buffer)
  (:method ((buffer t))
    (with-current-buffer buffer
      ;; On some systems like Xorg, clipboard pasting happens just-in-time.  So if we
      ;; copy something from the context menu 'Copy' action, upon pasting we will
      ;; retrieve the text from the GTK thread.  This is prone to create
      ;; dead-locks (e.g. when executing a Parenscript that acts upon the clipboard).
      ;;
      ;; To avoid this, we can 'flush' the clipboard to ensure that the copied text
      ;; is present the clipboard and need not be retrieved from the GTK thread.
      ;; TODO: Do we still need to flush now that we have multiple threads?
      ;; (trivial-clipboard:text (trivial-clipboard:text))
      (let ((input (%copy)))
        (copy-to-clipboard input)
        (echo "Text copied: ~s" input)))))
(define-ffi-generic ffi-buffer-paste (buffer)
  (:method ((buffer t))
    (with-current-buffer buffer
      (%paste))))
(define-ffi-generic ffi-buffer-cut (buffer)
  (:method ((buffer t))
    (with-current-buffer buffer
      (let ((input (%cut)))
        (when input
          (copy-to-clipboard input)
          (echo "Text cut: ~s" input))))))
(define-ffi-generic ffi-buffer-select-all (buffer)
  (:method ((buffer t))
    (with-current-buffer buffer
      (%select-all))))
(define-ffi-generic ffi-buffer-undo (buffer)
  (:method ((buffer t))
    (echo-warning "Undoing the edits is not yet implemented for this renderer.")))
(define-ffi-generic ffi-buffer-redo (buffer)
  (:method ((buffer t))
    (echo-warning "Redoing the edits is not yet implemented for this renderer.")))
