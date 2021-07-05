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

(defmethod prompter:object-attributes ((hook-description hook-description))
  `(("Name" ,(name hook-description))
    ("Value" ,(princ-to-string (value hook-description)))))

(defmethod prompter:object-attributes ((handler hooks:handler))
  `(("Name" ,(str:downcase (hooks:name handler)))))

(defun command-attributes (command &optional (buffer (active-buffer (current-window :no-rescan))))
  (let ((scheme-name (keymap-scheme-name buffer))
        (bindings '()))
    (loop for mode in (modes buffer)
          for scheme-keymap = (keymap:get-keymap scheme-name (keymap-scheme mode))
          when scheme-keymap
            do (setf bindings (keymap:binding-keys (name command) scheme-keymap))
          when (not (null bindings))
            return bindings)
    `(("Name" ,(string-downcase (name command)))
      ("Bindings" ,(format nil "~{~a~^, ~}" bindings))
      ("Docstring" ,(or (first (sera::lines (nyxt::docstring command)))
                        ""))
      ("Mode" ,(let ((package-name (str:downcase (uiop:symbol-package-name (name command)))))
                 (if (sera:in package-name "nyxt" "nyxt-user")
                     ""
                     (str:replace-first "nyxt/" "" package-name)))))))

(defmethod prompter:object-attributes ((command command))
  (command-attributes command))

(define-class command-source (prompter:source)
  ((prompter:name "Commands")
   (global-p t
             :type boolean
             :documentation "If non-nil, include global commands in the suggestions.")
   (buffer (current-buffer)
           :type buffer)
   (prompter:constructor (lambda (source)
                           (sort-by-time
                            (list-commands
                             :global-p (global-p source)
                             :mode-symbols (mapcar #'mode-name (modes (buffer source))))))))
  (:export-class-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Prompter source to execute commands."))
(define-user-class command-source)

(define-command execute-command ()
  "Execute a command by name."
  (unless (active-prompt-buffers (current-window))
    (let ((command (first (prompt
                           :prompt "Execute command"
                           :sources (make-instance 'user-command-source)
                           :hide-suggestion-count-p t))))
      (setf (last-access command) (local-time:now))
      (run-async command))))

(define-command execute-extended-command ()
   "Execute a command by name, also supply required, optional, and
keyword parameters."
  ;; TODO: prefill default-values when prompting optional/key arguments
   (let* ((command (first (prompt
                           :prompt "Execute extended command"
                           :sources (make-instance 'user-command-source)
                           :hide-suggestion-count-p t)))
          (argument-list (swank::arglist (fn command)))
          (required-arguments (nth-value 0 (alex:parse-ordinary-lambda-list
                                            argument-list)))
          (optional-arguments (nth-value 1 (alex:parse-ordinary-lambda-list
                                            argument-list)))
          (key-arguments (nth-value 3 (alex:parse-ordinary-lambda-list
                                       argument-list))))
     (apply command
            (append
             (when required-arguments
               (loop for argument in required-arguments
                     collect (read-from-string
                              (first (prompt
                                      :prompt argument
                                      :sources (make-instance 'prompter:raw-source))))))
             (when optional-arguments
               (loop for argument in optional-arguments
                     collect (read-from-string
                              (first (prompt
                                      :prompt (first argument)
                                      :sources (make-instance 'prompter:raw-source))))))
             (when key-arguments
               (loop for argument in key-arguments
                     collect (first (car argument))
                     collect (read-from-string
                              (first (prompt
                                      :prompt (second (car argument))
                                      :sources (make-instance 'prompter:raw-source))))))))
     (setf (last-access command) (local-time:now))))

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
   (prompter:constructor (get-hooks))))

(define-class handler-source (prompter:source)
  ((prompter:name "Handlers")
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
  (let* ((hook-desc (first (prompt
                            :prompt "Hook where to disable handler"
                            :sources (make-instance 'hook-source))))
         (handler (first (prompt
                          :prompt (format nil "Disable handler from ~a" (name hook-desc))
                          :sources (make-instance 'handler-source
                                                  :hook (value hook-desc))))))
    (hooks:disable-hook (value hook-desc) handler)))

(define-command enable-hook-handler ()
  "Remove handler(s) from a hook."
  (let* ((hook-desc (first (prompt
                            :prompt "Hook where to enable handler"
                            :sources (make-instance 'hook-source))))
         (handler (first (prompt
                          :prompt (format nil "Enable handler from ~a" (name hook-desc))
                          :sources (make-instance 'disabled-handler-source
                                                  :hook (value hook-desc))))))
    (hooks:enable-hook (value hook-desc) handler)))
