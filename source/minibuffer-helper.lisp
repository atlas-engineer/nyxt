;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

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
  `(let ((answer (prompt-minibuffer
                  :input-prompt (format nil ,@prompt)
                  :suggestion-function (yes-no-suggestion-filter)
                  :hide-suggestion-count-p t)))
     (if (confirmed-p answer)
         ,yes-form
         ,no-form)))
