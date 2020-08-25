;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defclass hook-description ()
  ((name :accessor name :initarg :name
         :initform ""
         :type string
         :documentation "The hook name.")
   (value :accessor value :initarg :value
          :documentation "The hook value.")))

(defmethod object-string ((hook-desc hook-description))
  (name hook-desc))
(defmethod object-display ((hook-desc hook-description))
  (name hook-desc))

(defmethod object-string ((handler hooks:handler))
  (str:downcase (hooks:name handler)))
(defmethod object-display ((handler hooks:handler))
  (str:downcase (hooks:name handler)))

(defun command-suggestion-filter (&optional mode-symbols)
  (let* ((commands
           (sort (apply #'list-commands mode-symbols) #'> :key #'access-time))
         (pretty-commands (mapcar #'command-display commands)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) commands :suggestions-display pretty-commands))))

(define-command execute-command ()
  "Execute a command by name."
  (unless (active-minibuffers (current-window))
    (with-result (command (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Execute command"
                            :suggestion-function (command-suggestion-filter
                                                  (mapcar (alex:compose #'class-name #'class-of)
                                                          (modes (current-buffer))))
                            :hide-suggestion-count-p t)))
      (setf (access-time command) (get-internal-real-time))
      (run command))))


(defun hook-suggestion-filter ()
  (flet ((list-hooks (object)
           (mapcar (lambda (hook)
                     (make-instance 'hook-description
                                    :name (str:downcase (closer-mop:slot-definition-name hook))
                                    :value (funcall (symbol-function (closer-mop:slot-definition-name hook))
                                                    object)))
                   (remove-if-not (lambda (s)
                                    (let ((name (closer-mop:slot-definition-name s)))
                                      (and (str:ends-with-p "-hook" (string name) :ignore-case t)
                                           (fboundp name))))
                                  (closer-mop:class-slots (class-of object))))))
    (let ((window-hooks (list-hooks (current-window)))
          (buffer-hooks (list-hooks (current-buffer)))
          (browser-hooks (list-hooks *browser*)))
      (lambda (minibuffer)
        (fuzzy-match (input-buffer minibuffer)
                     (append window-hooks
                             buffer-hooks
                             browser-hooks))))))

(defun handler-suggestion-filter (hook)
  (lambda (minibuffer)
    (fuzzy-match (input-buffer minibuffer)
                 (hooks:handlers hook))))

(defun disabled-handler-suggestion-filter (hook)
  (lambda (minibuffer)
    (fuzzy-match (input-buffer minibuffer)
                 (hooks:disabled-handlers hook))))

(define-command disable-hook-handler ()
  "Remove handler(s) from a hook."
  (with-result (hook-desc (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Hook where to disable handler"
                            :suggestion-function (hook-suggestion-filter))))
    (with-result (handler
                  (read-from-minibuffer
                   (make-minibuffer
                    :input-prompt (format nil "Disable handler from ~a" (name hook-desc))
                    :suggestion-function (handler-suggestion-filter (value hook-desc)))))
      (hooks:disable-hook (value hook-desc) handler))))

(define-command enable-hook-handler ()
  "Remove handler(s) from a hook."
  (with-result (hook-desc (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Hook where to enable handler"
                            :suggestion-function (hook-suggestion-filter))))
    (with-result (handler
                  (read-from-minibuffer
                   (make-minibuffer
                    :input-prompt (format nil "Enable handler from ~a" (name hook-desc))
                    :suggestion-function (disabled-handler-suggestion-filter (value hook-desc)))))
      (hooks:enable-hook (value hook-desc) handler))))
