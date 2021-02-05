;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class hook-description ()
  ((name ""
         :documentation "The hook name.")
   (value nil
          :type t
          :documentation "The hook value."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

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
    (let ((command (prompt-minibuffer
                    :input-prompt "Execute command"
                    :suggestion-function (command-suggestion-filter
                                          (mapcar #'mode-name
                                                  (modes (current-buffer))))
                    :hide-suggestion-count-p t)))
      (setf (access-time command) (get-internal-real-time))
      (run-async command))))

(define-command execute-extended-command ()
   "Execute a command by name, also supply required, optional, and
keyword parameters."
  ;; TODO: prefill default-values when prompting optional/key arguments
   (let* ((command (prompt-minibuffer
                    :input-prompt "Execute extended command"
                    :suggestion-function (command-suggestion-filter
                                          (mapcar #'mode-name (modes (current-buffer))))
                    :hide-suggestion-count-p t))
          (command-symbol (sym command))
          (argument-list (swank::arglist command-symbol))
          (required-arguments (nth-value 0 (alex:parse-ordinary-lambda-list
                                            argument-list)))
          (optional-arguments (nth-value 1 (alex:parse-ordinary-lambda-list
                                            argument-list)))
          (key-arguments (nth-value 3 (alex:parse-ordinary-lambda-list
                                       argument-list))))
     (apply command-symbol
            (append
             (when required-arguments
               (loop for argument in required-arguments
                     collect (read-from-string
                              (prompt-minibuffer
                               :input-prompt argument))))
             (when optional-arguments
               (loop for argument in optional-arguments
                     collect (read-from-string
                              (prompt-minibuffer
                               :input-prompt (first argument)))))
             (when key-arguments
               (loop for argument in key-arguments
                     collect (first (car argument))
                     collect (read-from-string
                              (prompt-minibuffer
                               :input-prompt (second (car argument))))))))
     (setf (access-time command) (get-internal-real-time))))

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
  (let* ((hook-desc (prompt-minibuffer
                     :input-prompt "Hook where to disable handler"
                     :suggestion-function (hook-suggestion-filter)))
         (handler (prompt-minibuffer
                   :input-prompt (format nil "Disable handler from ~a" (name hook-desc))
                   :suggestion-function (handler-suggestion-filter (value hook-desc)))))
    (hooks:disable-hook (value hook-desc) handler)))

(define-command enable-hook-handler ()
  "Remove handler(s) from a hook."
  (let* ((hook-desc (prompt-minibuffer
                     :input-prompt "Hook where to enable handler"
                     :suggestion-function (hook-suggestion-filter)))
         (handler (prompt-minibuffer
                   :input-prompt (format nil "Enable handler from ~a" (name hook-desc))
                   :suggestion-function (disabled-handler-suggestion-filter (value hook-desc)))))
    (hooks:enable-hook (value hook-desc) handler)))
