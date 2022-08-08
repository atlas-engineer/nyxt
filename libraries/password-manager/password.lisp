;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(define-class password-interface ()
  ((executable nil
               :type (or null string)
               :documentation "The program to query for password information.")
   (sleep-timer 15
                :documentation "The amount of time to sleep, in seconds."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always 'list-passwords)
(defgeneric list-passwords (password-interface)
  (:documentation "Retrieve all available passwords."))

(export-always 'clip-password)
(defgeneric clip-password (password-interface &key password-name service)
  (:documentation "Retrieve specific password by name."))

(export-always 'clip-username)
(defgeneric clip-username (password-interface &key password-name service)
  (:documentation "Retrieve specific login by name of the password entry."))

(export-always 'save-password)
(defgeneric save-password (password-interface
                           &key password-name username password service)
  (:documentation "Save password to database.
If PASSWORD-NAME is empty, then generate a new password."))

(export-always 'password-correct-p)
(defgeneric password-correct-p (password-interface)
  (:documentation "Return T if set password is correct, NIL otherwise."))

(export-always 'complete-interface)
(defgeneric complete-interface (password-interface)
  (:method ((password-interface password-interface))
    password-interface)
  (:documentation "Return the PASSWORD-INTERFACE with all the misfilled fields corrected."))

(defmacro execute (interface arguments &rest run-program-args &key (wait-p t) &allow-other-keys)
  "Execute the command matching the INTERFACE, with ARGS.

`uiop:run-program' is used underneath, with RUN-PROGRAM-ARGS being its
arguments.

When the WAIT-P is NIL, `uiop:launch-program' is used instead of
`uiop:run-program'."
  `(,(if wait-p
         'uiop:run-program
         'uiop:launch-program)
    (append (list (executable ,interface)) ,arguments)
    ,@(alexandria:remove-from-plist run-program-args :wait-p)))

(defun safe-clipboard-text ()
  "Return clipboard content, or \"\" if the content is not textual."
  ;; xclip errors out when the clipboard contains non-text:
  ;; https://github.com/astrand/xclip/issues/38#issuecomment-466625564.
  (ignore-errors (trivial-clipboard:text)))

;;; Prerequisite Functions
(defmethod clip-password-string ((password-interface password-interface) pass)
  (trivial-clipboard:text pass)
  (bt:make-thread
   (lambda ()
     (sleep (sleep-timer password-interface))
     (when (string= (safe-clipboard-text) pass)
       ;; Reset the clipboard so that the user does not accidentally paste
       ;; something else.
       (trivial-clipboard:text "")))))


;;; Commands to wrap together.
(defun pathname->string (pathname)
  "Like `namestring' but return NIL if PATHNAME is NIL."
  (when pathname
    (namestring pathname)))

(export-always '*interfaces*)
(defvar *interfaces* '())
