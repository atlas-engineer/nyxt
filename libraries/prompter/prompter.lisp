;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

;; Same as `prompter-source' as to why we wrap in `eval-always'.
(sera:eval-always
  (define-class prompter ()
    ((input ""
            :accessor nil
            :reader :input
            :documentation
            "User input.")

     (prompt ""
             :documentation
             "Prefix to the user input.")

     (sources '()
              :type (or null (cons prompter-source))
              :documentation "List of `prompter-source's.")

     (selection nil                     ; TODO: Make use of it.
                :type list
                :documentation "A pair of source and suggestion index.")

     (initializer nil
                  :type (or null function)
                  :documentation
                  "Function called with the prompter as argument.")

     (before-destructor nil
                     :type (or null function)
                     :documentation
                     "First function called with no parameters when calling the
`destructor' function over this prompter.
It's called before the sources are cleaned up.")
     (after-destructor nil
                    :type (or null function)
                    :documentation
                    "Last function called with no parameters when calling the
`destructor' function over this prompter.
It's called after the sources are cleaned up.

Note that the function is executed *before* performing an action.")

     ;; (history (make-history)              ; TODO: Move to `prompter' class?
     ;;     :type (or containers:ring-buffer-reverse null)
     ;;     :documentation
     ;;     "")

     (keymap nil
             :type (or null keymap:keymap)
             :documentation
             "Keymap for the prompter.
Useful, say, to switch source.
It takes precedence over individual source keymaps.")

     (help-message ""
                   :type (or string function)
                   :documentation
                   "Help message for this prompter.
It can be a function of one argument, the prompter, which returns a string."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer #'class*:name-identity)
    (:documentation "The prompter is an interface for user interactions.
A prompter object holds multiple sources (of type `prompter-source') which
contain a list of `suggestion's.

You can call `destructor' to call the registered termination functions of the
prompter and its sources.

Suggestions are computed asynchronously when `input' is updated.
Use `ready-p' to know when the prompter is ready.
Sources suggestions can be retrieved even when the compution is not
finished.")))

(defmethod initialize-instance :after ((prompter prompter) &key)
  (maybe-funcall (initializer prompter) prompter)
  prompter)

(export-always 'input)
(defmethod (setf input) (text (prompter prompter)) ; TODO: (str:replace-all " " " " input) in the caller.
  "Update PROMPTER sources and return TEXT."
  (setf (slot-value prompter 'input) text)
  (mapc (lambda (source) (update source text)) (sources prompter))
  text)

(export-always 'destructor)
(defmethod destructor ((prompter prompter))
  "First call `before-destructor', then clean up all sources, finally call
`after-destructor'."
  (maybe-funcall (before-destructor prompter))
  (mapc #'destructor (sources prompter))
  (maybe-funcall (after-destructor prompter)))

(export-always 'ready-p)
(defun ready-p (prompter &optional timeout)
  "Return non-nil when all prompter sources are ready.
After timeout has elapsed for one source, return nil."
  (every (lambda (source)
            (nth-value 1 (calispel:? (ready-notifier source) timeout)))
          (sources prompter)))

(export-always 'make)
(define-function make
    (append '(&rest args)
            `(&key ,@(initargs 'prompter)))
  "Return prompter object."
  (apply #'make-instance 'prompter args))
