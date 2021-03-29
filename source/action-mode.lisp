;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/action-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:class-star #:define-class)
  (:documentation "Act on file/directory based on a certain condition."))
(in-package :nyxt/action-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)
  (trivial-package-local-nicknames:add-package-local-nickname :file-attributes :org.shirakumo.file-attributes))

(defun updated-file-p (path-url mode)
  (and
   (quri:uri-file-p path-url)
   (or (null (last-access (action mode)))
       (local-time:timestamp>
        (local-time:universal-to-timestamp
         (file-attributes:modification-time (quri:uri-path path-url)))
        (last-access (action mode))))))

;; (define-class line-count-action (nyxt/action-mode:preview-action)
;;   ((action
;;     #'(lambda (path-url mode)
;;         (let ((lines (length (uiop:read-file-lines (quri:uri-path path-url)))))
;;           (when (\= (lines (action mode)) lines)
;;             (nyxt:echo "~a has ~a lines of text." (quri:uri-path path-url) lines)))))
;;    (lines :documentation "The number of lines that file had the last time it was checked."))
;;   (:export-class-name-p t)
;;   (:export-accessor-names-p t)
;;   (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))l

(defun initialize-action-mode (mode)
  (sera:and-let* ((url (quri:uri (prompt-minibuffer
                                  :input-prompt "URL of the document to act on"
                                  :input-buffer (object-string (url (current-buffer)))))))
    (setf (path-url mode) url
          (thread mode) (bt:make-thread
                         #'(lambda ()
                             (loop with cond = (firing-condition mode)
                                   with cond-func = (typecase cond
                                                      (function cond)
                                                      (boolean (constantly cond)))
                                   when (funcall cond-function url mode)
                                     do (funcall (action mode) url mode)))))))

(defun clean-up-action-mode (mode)
  (and (cleanup mode)
       (funcall (cleanup mode) (path-url mode) mode))
  (bt:destroy-thread (thread mode)))

(define-mode action-mode ()
  "Conditional execution a file/directory-related actions in a separate thread.

Possible applications:
- Web server.
- Live preview of documents (`preview-mode').
- Refreshing the website at regular intervals (`watch-mode').
- Live tracking of filesystem/data in a file/directory."
  ((path-url nil
             :type (or quri:uri null)
             :documentation "The path to where `action-mode' needs to track things at.
Is not necessarily the same as current buffer URL.")
   (firing-condition t
                     :type (or boolean function)
                     :documentation "The condition for the action firing.
Can be boolean (T to always fire, NIL to never fire), or function over URL and mode instance.")
   (action nil
           :type (or (function (quri:uri root-mode)) null)
           :documentation "The action (function) to do with file URL and `action-mode' instance.")
   (cleanup nil
            :type (or (function (quri:uri root-mode)) null)
            :documentation "Function to run when action ends.
Accepts the path to the acted-on document and action-mode instance.")
   (thread nil
           :type (or bt:thread null)
           :export nil
           :documentation "The thread that `actions' happen in.")
   (constructor #'initialize-action-mode)
   (destructor #'clean-up-action-mode)))
