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

;; REVIEW: `action' and `cleanup' resemble `constructor' and `destructor'.
;; TODO: Maybe subclass `action-mode' instead?
;; This will sacrifice the flexibility of `action's, though...
(define-class action ()
  ((firing-condition t
                         :type (or boolean function)
                         :documentation "The condition for the action firing.
Can be boolean (T to always fire, NIL to never fire), or function over URL and mode.")
   (action nil
           :type (or (function (quri:uri root-mode)) null)
           :documentation "The action (function) to do with file URL and action-mode.")
   (cleanup nil
            :type (or (function (quri:uri root-mode)) null)
            :documentation "Function to run when action ends.
Accepts the path to the acted-on document and action-mode instance."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defmethod nyxt:object-string ((action action))
  (str:downcase (symbol-name (sera:class-name-of action))))

(defmethod nyxt:object-display ((action action))
  (str:downcase (symbol-name (sera:class-name-of action))))

(defun updated-file-p (path-url mode)
  (and
   (quri:uri-file-p path-url)
   (or (null (last-access (action mode)))
       (local-time:timestamp>
        (local-time:universal-to-timestamp
         (file-attributes:modification-time (quri:uri-path path-url)))
        (last-access (action mode))))))

(define-class preview-action (action)
  ((firing-condition #'updated-file-p)
   (action
    #'(lambda (path-url mode)
        (buffer-load path-url :buffer (buffer mode))
        (setf (last-access (action mode)) (local-time:now))))
   (last-access nil
                :type (or local-time:timestamp null)
                :documentation "The time `path-url' file was last accessed."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class server-action (preview-action)
  ((action #'(lambda (path-url mode)
               (setf (acceptor (action mode))
                     (if (acceptor (action mode))
                         (hunchentoot:start
                          (hunchentoot:stop (acceptor (action mode))))
                         (hunchentoot:start
                          (or (acceptor (action mode))
                              (make-instance 'hunchentoot:easy-acceptor
                                             :document-root (uiop:pathname-directory-pathname
                                                             (pathname (quri:uri-path path-url)))
                                             :address (address (action mode))
                                             :port (port (action mode)))))))
               (buffer-load (quri:make-uri :host (address (action mode))
                                           :port (port (action mode)))
                            :buffer (buffer mode))))
   (cleanup #'(lambda (path-url mode)
                (declare (ignore path-url))
                (when (acceptor (action mode))
                  (hunchentoot:stop (acceptor (action mode))))))
   (acceptor nil
             :type (or hunchentoot:acceptor null)
             :export nil
             :documentation "Current server acceptor.")
   (address "localhost"
            :type (or string null)
            :documentation "What address to set the server listening on.
Listens on all machine IPs if not set (see `hunchentoot:acceptor'.)")
   (port 8080
         :type integer
         :documentation "The port server starts on."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defun seconds-from-user-input ()
  "Read from the minibuffer numerical time inputs and collate them into
seconds."
  (let* ((time-units '("days" "hours" "minutes" "seconds"))
         (to-seconds-alist (pairlis time-units '(86400 3600 60 1)))
         (active-time-units (prompt-minibuffer
                             :input-prompt "Time unit(s)"
                             :multi-selection-p t
                             :suggestion-function (lambda (minibuffer)
                                                    (fuzzy-match
                                                     (input-buffer minibuffer)
                                                     time-units))))
         (times (mapcar (lambda (unit)
                          (parse-integer
                           (prompt-minibuffer
                            :input-prompt (format nil "Time interval (~a)" unit)
                            :hide-suggestion-count-p t)
                           :junk-allowed t))
                        active-time-units))
         (to-seconds-multipliers
           (mapcar
            (lambda (elem) (cdr (assoc elem to-seconds-alist :test 'string-equal)))
            active-time-units)))
    (echo "Refreshing every ~:@{~{~d ~}~a~:@}"
          (list times active-time-units))
    (apply '+ (mapcar (lambda (time multiplier) (* time multiplier))
                      times to-seconds-multipliers))))

(define-class watch-action (action)
  ((action #'(lambda (path-url mode)
               (unless (sleep-time (action mode))
                 (setf (sleep-time (action mode)) (seconds-from-user-input)))
               (loop
                 (if (quri:uri= path-url (url (buffer mode)))
                     (reload-current-buffer (buffer mode))
                     (buffer-load path-url :buffer (buffer mode)))
                 (sleep (sleep-time mode)))))
   (sleep-time :documentation "The amount of time to sleep between reloads."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class line-count-action (nyxt/action-mode:preview-action)
  ((action
    #'(lambda (path-url mode)
        (let ((lines (length (uiop:read-file-lines (quri:uri-path path-url)))))
          (when (\= (lines (action mode)) lines)
            (nyxt:echo "~a has ~a lines of text." (quri:uri-path path-url) lines)))))
   (lines :documentation "The number of lines that file had the last time it was checked."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defun initialize-action-mode (mode)
  (sera:and-let* ((url (quri:uri (prompt-minibuffer
                                  :input-prompt "URL of the document to act on"
                                  :input-buffer (object-string (url (current-buffer)))))))
    (setf (path-url mode) url
          (thread mode) (bt:make-thread
                         #'(lambda ()
                             (loop
                               with condition = (firing-condition (action mode))
                               when (typecase condition
                                      (function (funcall condition url mode))
                                      (boolean condition))
                                 do (funcall (action (action mode)) url mode)))))))

(defun clean-up-action-mode (mode)
  (and (cleanup (action mode))
       (funcall (cleanup (action mode)) (path-url mode) mode))
  (bt:destroy-thread (thread mode)))

(define-mode action-mode ()
  "Conditional execution a file/directory-related actions in a separate thread.

Possible applications:
- Web server (`server-action').
- Live preview of documents (`preview-action').
- Refreshing the website at regular intervals (`watch-action').
- Live tracking of filesystem/data in a file/directory."
  ((path-url nil
             :type (or quri:uri null)
             :documentation "The path to where `display-mode' needs to track things at.
Is not necessarily the same as current buffer URL.")
   (possible-actions (list (make-instance 'preview-action)
                           (make-instance 'server-action)
                           (make-instance 'watch-action))
                     :type list
                     :documentation "The list of action symbols to choose suitable action from.
Customize this to add your own action to the list.")
   (action (make-instance 'preview-action)
           :type action
           :documentation "Action to run when `action-mode' is activated.
Defaults to `preview-action'.")
   (thread nil
           :type (or bt:thread null)
           :export nil
           :documentation "The thread that actions happen in.")
   (constructor #'initialize-action-mode)
   (destructor #'clean-up-action-mode)))

(in-package :nyxt)

(define-command action-mode-with-action (&optional (buffer (current-buffer)))
  "Enable `action-mode' in BUFFER and prompt about the action to use with it."
  ;; There seems to be no better way to get the list of actions.
  (let* ((mode (make-instance 'nyxt/action-mode:action-mode))
         (action (prompt-minibuffer :input-prompt "Action to use"
                                    :suggestion-function (lambda (minibuffer)
                                                           (fuzzy-match
                                                            (input-buffer minibuffer)
                                                            (nyxt/action-mode:possible-actions mode))))))
    (nyxt/action-mode:action-mode :buffer buffer :activate t :action action)))
