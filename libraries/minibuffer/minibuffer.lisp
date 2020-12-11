;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :minibuffer)

;; Same as `minibuffer-source' as to why we wrap in `eval-always'.
(sera:eval-always
  (define-class minibuffer ()
    ((input ""
            :accessor nil
            :reader :input
            :documentation
            "User input.")

     (prompt ""
             :documentation
             "Prefix to the user input.")

     (sources '()
              :type (or null (cons minibuffer-source))
              :documentation "List of `minibuffer-source's.")

     (selection nil                     ; TODO: Make use of it.
                :type list
                :documentation "A pair of source and suggestion index.")

     (initializer nil
                  :type (or null function)
                  :documentation
                  "Function called with the minibuffer as argument.")

     (before-destructor nil
                     :type (or null function)
                     :documentation
                     "First function called with no parameters when calling the
`destructor' function over this minibuffer.
It's called before the sources are cleaned up.")
     (after-destructor nil
                    :type (or null function)
                    :documentation
                    "Last function called with no parameters when calling the
`destructor' function over this minibuffer.
It's called after the sources are cleaned up.

Note that the function is executed *before* performing an action.")

     ;; (history (make-history)              ; TODO: Move to `minibuffer' class?
     ;;     :type (or containers:ring-buffer-reverse null)
     ;;     :documentation
     ;;     "")

     (keymap nil
             :type (or null keymap:keymap)
             :documentation
             "Keymap for the minibuffer.
Useful, say, to switch source.
It takes precedence over individual source keymaps.")

     (help-message ""
                   :type (or string function)
                   :documentation
                   "Help message for this minibuffer.
It can be a function of one argument, the minibuffer, which returns a string."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer #'class*:name-identity)
    (:documentation "The minibuffer is an interface for user interactions.
A minibuffer object holds multiple sources (of type `minibuffer-source') which
contain a list of `suggestion's.

You can call `destructor' to call the registered termination functions of the
minibuffer and its sources.

Suggestions are computed asynchronously when `input' is updated.
Use `ready-p' to know when the minibuffer is ready.
Sources suggestions can be retrieved even when the compution is not
finished.")))

(defmethod initialize-instance :after ((minibuffer minibuffer) &key)
  (maybe-funcall (initializer minibuffer) minibuffer)
  minibuffer)

(export-always 'input)
(defmethod (setf input) (text (minibuffer minibuffer)) ; TODO: (str:replace-all " " " " input) in the caller.
  "Update MINIBUFFER sources and return TEXT."
  (setf (slot-value minibuffer 'input) text)
  (mapc (lambda (source) (update source text)) (sources minibuffer))
  text)

(export-always 'destructor)
(defmethod destructor ((minibuffer minibuffer))
  "First call `before-destructor', then clean up all sources, finally call
`after-destructor'."
  (maybe-funcall (before-destructor minibuffer))
  (mapc #'destructor (sources minibuffer))
  (maybe-funcall (after-destructor minibuffer)))

(export-always 'ready-p)
(defun ready-p (minibuffer &optional timeout)
  "Return non-nil when all minibuffer sources are ready.
After timeout has elapsed for one source, return nil."
  (every (lambda (source)
            (nth-value 1 (calispel:? (ready-notifier source) timeout)))
          (sources minibuffer)))

(export-always 'make)
(define-function make
    (append '(&rest args)
            `(&key ,@(initargs 'minibuffer)))
  "Return minibuffer object."
  (apply #'make-instance 'minibuffer args))
