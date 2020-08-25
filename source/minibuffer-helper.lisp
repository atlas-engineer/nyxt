;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defparameter %callback nil)            ; TODO: Make a monad?

(export-always 'use-empty-result)
(defun use-empty-result ()
  (funcall-safely %callback nil))

(export-always 'with-result)
(defmacro with-result ((symbol async-form) &body body)
  "Call ASYNC-FORM and lexically bind `%callback' to BODY.
SYMBOL is bound to the result of ASYNC-FORM in the lexical context of BODY.

If ASYNC-FORM does not funcall `%callback', BODY won't be called.
Call `use-empty-result' without argument in ASYNC-FORM to force a call to BODY.

Example:

  (with-result (url (read-from-minibuffer
                     (make-minibuffer
                      :input-prompt \"Bookmark URL\"))
    (bookmark-add url))"
  `(let ((%callback (lambda (,symbol) ,@body)))
     ,async-form))

(export-always 'with-result*)
(defmacro with-result* (bindings &body body)
  "Like WITH-RESULT but allows for chained asynchronous bindings.

Example:

  (with-result* ((links-json (add-link-hints))
                 (selected-hint (read-from-minibuffer
                                 (make-minibuffer
                                  :input-prompt \"Bookmark hint\"
                                  :cleanup-function #'remove-link-hints))))
    ...)"
  (if (null bindings)
    `(progn ,@body)
    `(with-result ,(first bindings)
       (with-result* ,(rest bindings) ,@body))))

(export-always '*yes-no-choices*)
(defparameter *yes-no-choices* '(:yes "yes" :no "no")
  "The suggestions when asking the user for a yes/no choice.
See `if-confirm'.
The first suggestion poses as a default.")

(defun yes-no-suggestion-filter ()
  (lambda (minibuffer)
    (fuzzy-match (input-buffer minibuffer) (sera:plist-values *yes-no-choices*))))

(defun confirmed-p (answer)
  (string-equal answer (getf *yes-no-choices* :yes)))

(export-always 'if-confirm)
(defmacro if-confirm (prompt yes-form &optional no-form)
  "Ask the user for confirmation before executing either YES-FORM of NO-FORM.
YES-FORM is executed on  \"yes\" answer, NO-FORM -- on \"no\".
PROMPT is a list fed to `format nil'.

Example usage defaulting to \"no\":

\(let ((*yes-no-choices* '(:no \"no\" :yes \"yes\")))
  (if-confirm (\"Are you sure to kill ~a buffers?\" count)
     (delete-buffers)))"
  `(with-result (answer (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt (format nil ,@prompt)
                          :suggestion-function (yes-no-suggestion-filter)
                          :hide-suggestion-count-p t)))
     (if (confirmed-p answer)
         ,yes-form
         ,no-form)))
