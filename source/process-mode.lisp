;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/process-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:class-star #:define-class)
  (:documentation "Act on file/directory based on a certain condition."))
(in-package :nyxt/process-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)
  (trivial-package-local-nicknames:add-package-local-nickname :file-attributes :org.shirakumo.file-attributes))

(defun initialize-process-mode (mode)
  (sera:and-let* ((url (quri:uri (prompt
                                  :prompt "URL of the document to act on"
                                  :input (object-string (url (current-buffer)))))))
    (setf (path-url mode) url
          (thread mode) (bt:make-thread
                         #'(lambda ()
                             (loop with cond = (firing-condition mode)
                                   with cond-func = (typecase cond
                                                      (function cond)
                                                      (boolean (constantly cond)))
                                   when (funcall cond-func url mode)
                                     do (funcall mode url mode)))))))

(defun clean-up-process-mode (mode)
  (and (cleanup mode)
       (funcall (cleanup mode) (path-url mode) mode))
  (bt:destroy-thread (thread mode)))

(define-mode process-mode ()
  "Conditional execution a file/directory-related actions in a separate thread.

Possible applications:
- Web server.
- Live preview of documents (`preview-mode').
- Refreshing the website at regular intervals (`watch-mode').
- Live tracking of filesystem/data in a file/directory."
  ((path-url nil
             :type (or quri:uri null)
             :documentation "The path to where `process-mode' needs to track things at.
Is not necessarily the same as current buffer URL.")
   (firing-condition t
                     :type (or boolean function)
                     :documentation "The condition for the action firing.
Can be boolean (T to always fire, NIL to never fire), or function over URL and mode instance.")
   (action nil
           :type (or (function (quri:uri root-mode)) null)
           :documentation "The action (function) to do with file URL and `process-mode' instance.")
   (cleanup nil
            :type (or (function (quri:uri root-mode)) null)
            :documentation "Function to run when process ends.
Accepts the path to the acted-on document and `process-mode' instance.")
   (thread nil
           :type (or bt:thread null)
           :export nil
           :documentation "The thread that `action' happen in.")
   (constructor #'initialize-process-mode)
   (destructor #'clean-up-process-mode)))
