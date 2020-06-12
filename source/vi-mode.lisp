(uiop:define-package :nyxt/vi-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "VI-style bindings."))
(in-package :nyxt/vi-mode)

(define-mode vi-normal-mode ()
  "Enable VI-style modal bindings (normal mode).
To enable these bindings by default, add the mode to the list of default modes
in your configuration file.

Example:

\(define-configuration buffer
  ((default-modes (append '(vi-normal-mode) %slot-default))))"
  ((previous-keymap-scheme-name
    :accessor previous-keymap-scheme-name
    :type keymap:scheme-name
    :documentation "The previous keymap scheme that will be used when ending
vi-normal-mode.")
   (keymap-scheme
    :initform
    (define-scheme "vi"
      scheme:vi-normal
      (list
        "i" 'vi-insert-mode
        "button1" 'vi-button1)))
   (destructor
    :initform
    (lambda (mode)
      (setf (keymap-scheme-name (buffer mode))
            (previous-keymap-scheme-name mode))
      (setf (forward-input-events-p (buffer mode)) t)))
   (constructor
    :initform
    (lambda (mode)
      (with-accessors ((buffer buffer)) mode
        (let ((vi-insert (find-submode buffer 'vi-insert-mode)))
          (setf (previous-keymap-scheme-name mode)
                (if vi-insert
                    (previous-keymap-scheme-name vi-insert)
                    (keymap-scheme-name buffer))))
        ;; Destroy vi-normal mode after setting previous-keymap-scheme-name, or
        ;; else we can't save the previous keymap scheme.
        (vi-insert-mode :activate nil :buffer buffer)
        (setf (keymap-scheme-name buffer) scheme:vi-normal)
        (setf (forward-input-events-p buffer) nil))))))

;; TODO: Move ESCAPE binding to the override map?
(define-mode vi-insert-mode ()
  "Enable VI-style modal bindings (insert mode).
See `vi-normal-mode'."
  ;; We could inherit from vi-normal-mode to save the declaration of this slot
  ;; but then (find-submode ... 'vi-normal-mode) would match vi-insert-mode.
  ((previous-keymap-scheme-name
    :accessor previous-keymap-scheme-name
    :type keymap:scheme-name
    :documentation "The previous keymap scheme that will be used when ending
vi-normal-mode.")
   (keymap-scheme
    :initform
    (define-scheme "vi"
      scheme:vi-insert
      (list
       "C-i" 'autofill
       "C-v" 'nyxt/web-mode:paste
       "button2" 'nyxt/web-mode:paste-or-set-url
       "escape" 'vi-normal-mode
       "button1" 'vi-button1)))
   (destructor
    :initform
    (lambda (mode)
      (setf (keymap-scheme-name (buffer mode))
            (previous-keymap-scheme-name mode))))
   (constructor
    :initform
    (lambda (mode)
      (with-accessors ((buffer buffer)) mode
        (let ((vi-normal (find-submode buffer 'vi-normal-mode)))
          (setf (previous-keymap-scheme-name mode)
                (if vi-normal
                    (previous-keymap-scheme-name vi-normal)
                    (keymap-scheme-name buffer))))
        (vi-normal-mode :activate nil :buffer buffer)
        (setf (keymap-scheme-name buffer) scheme:vi-insert))))))

(define-command vi-button1 (&optional (buffer (current-buffer)))
  "Enable VI insert mode when focus is on an input element on the web page."
  ;; First we generate a button1 event so that the web view element is clicked
  ;; (e.g. a text field gets focus).
  (ffi-generate-input-event
   (current-window)
   (nyxt::last-event buffer))
  (with-result (response (nyxt/web-mode:%clicked-in-input?))
    (cond
      ((and (nyxt/web-mode:input-tag-p response)
            (find-submode buffer 'vi-normal-mode))
       (vi-insert-mode))
      ((and (not (nyxt/web-mode:input-tag-p response))
            (find-submode buffer 'vi-insert-mode))
       (vi-normal-mode)))))

(defmethod on-signal-load-finished ((mode vi-insert-mode) url)
  (declare (ignore url))
  (vi-normal-mode))
