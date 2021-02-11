;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(defclass password-interface ()
  ((executable :accessor executable :initarg :executable
               :documentation "The program to query for password information.")
   (sleep-timer :accessor sleep-timer :initarg :sleep-timer :initform 15
                :documentation "The amount of time to sleep, in seconds.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'list-passwords))
(defgeneric list-passwords (password-interface)
  (:documentation "Retrieve all available passwords."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'clip-password))
(defgeneric clip-password (password-interface &key password-name service)
  (:documentation "Retrieve specific password by name."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'save-password))
(defgeneric save-password (password-interface &key password-name password service)
  (:documentation "Save password to database.
If PASSWORD-NAME is empty, then generate a new password."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'password-correct-p))
(defgeneric password-correct-p (password-interface)
  (:documentation "Return T if set password is correct, NIL otherwise."))

(defmacro execute (interface arguments &body body)
  `(uiop:run-program (append (list (executable ,interface)) ,arguments) ,@body))

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

(defun executable-find (command)
  "Search for COMMAND in the PATH and return the absolute file name.
Return nil if COMMAND is not found anywhere."
  (multiple-value-bind (path)
      (ignore-errors
       (uiop:run-program (format nil "command -v ~A" command)
                         :output '(:string :stripped t)))
    path))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '*interfaces*))
(defvar *interfaces* (list))
