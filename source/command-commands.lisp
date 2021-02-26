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

(defmethod prompter:object-properties ((hook-description hook-description))
  (list :name (name hook-description)
        :value (value hook-description)))

(defmethod prompter:object-properties ((handler hooks:handler))
  (list :name (str:downcase (hooks:name handler))))

(defun get-commands (&optional (buffer (current-buffer)))
  (sort (apply #'list-commands
               (mapcar #'mode-name (modes buffer)))
        #'> :key #'access-time))

(defmethod prompter:object-properties ((command command))
  (let* ((buffer (active-buffer (current-window :no-rescan)))
         (scheme-name (keymap-scheme-name buffer))
         (bindings '()))
    (loop for mode in (modes buffer)
          for scheme-keymap = (keymap:get-keymap scheme-name (keymap-scheme mode))
          when scheme-keymap
            do (setf bindings (keymap:binding-keys (sym command) scheme-keymap))
          when (not (null bindings))
            return bindings)
    (list :name (string-downcase (sym command))
          :bindings (format nil "~{~a~^, ~}" bindings))))

(define-class command-source (prompter:source)
  ((prompter:name "Commands")
   (prompter:must-match-p t)
   (prompter:initial-suggestions (get-commands))))

(define-command execute-command ()
  "Execute a command by name."
  (unless (active-minibuffers (current-window))
    (let ((command (prompt
                    :prompt "Execute command"
                    :sources (make-instance 'command-source)
                    :hide-suggestion-count-p t)))
      (setf (access-time command) (get-internal-real-time))
      (run-async command))))

(define-command execute-extended-command ()
   "Execute a command by name, also supply required, optional, and
keyword parameters."
  ;; TODO: prefill default-values when prompting optional/key arguments
   (let* ((command (prompt
                    :prompt "Execute extended command"
                    :sources (make-instance 'command-source)
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

(defun get-hooks ()
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
      (append window-hooks
              buffer-hooks
              browser-hooks))))

(define-class hook-source (prompter:source)
  ((prompter:name "Hooks")
   (prompter:must-match-p t)
   (prompter:initial-suggestions (get-hooks))))

(define-class handler-source (prompter:source)
  ((prompter:name "Handlers")
   (prompter:must-match-p t)
   (hook :accessor hook
         :initarg :hook
         :documentation "The hook for which to retrieve handlers for.")
   (prompter:constructor (lambda (source)
                           (hooks:handlers (hook source))))))

(define-class disabled-handler-source (handler-source)
  ((prompter:constructor (lambda (source)
                           (hooks:disabled-handlers (hook source))))))

(define-command disable-hook-handler ()
  "Remove handler(s) from a hook."
  (let* ((hook-desc (prompt
                     :prompt "Hook where to disable handler"
                     :sources (make-instance 'hook-source)))
         (handler (prompt
                   :prompt (format nil "Disable handler from ~a" (name hook-desc))
                   :sources (make-instance 'handler-source
                                           :hook (value hook-desc)))))
    (hooks:disable-hook (value hook-desc) handler)))

(define-command enable-hook-handler ()
  "Remove handler(s) from a hook."
  (let* ((hook-desc (prompt
                     :prompt "Hook where to enable handler"
                     :sources (make-instance 'hook-source)))
         (handler (prompt
                   :prompt (format nil "Enable handler from ~a" (name hook-desc))
                   :sources (make-instance 'disabled-handler-source
                                           :hook (value hook-desc)))))
    (hooks:enable-hook (value hook-desc) handler)))
