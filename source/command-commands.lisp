;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class hook-description ()
  ((name ""
         :documentation "The hook name.")
   (value nil
          :type t
          :documentation "The hook value."))
  (:accessor-name-transformer (class*:make-name-transformer name)))

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
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompter source to execute commands.
Global commands are listed if `global-p' is non-nil.
Mode commands of enabled modes are also listed.
While disabled-mode commands are not listed, it's still possible to call them
from a key binding."))
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

(defun parse-function-lambda-list-types (fn)
  #-sbcl
  (declare (ignore fn))
  #-sbcl
  (warn "Function type parsing is not supported on this Lisp implementation.")
  #+sbcl
  (let* ((types (second (sb-introspect:function-type fn)))
         (keywords '())
         (type-batches (sera:split-sequence-if (lambda (type)
                                                 (when (find type lambda-list-keywords)
                                                   (push type keywords)))
                                               types))
         (keyword-type-pairs (pairlis keywords (rest type-batches))))
    (alex:nreversef keywords)
    (values (first type-batches)
            (alex:assoc-value keyword-type-pairs '&optional)
            (alex:assoc-value keyword-type-pairs '&rest)
            (alex:assoc-value keyword-type-pairs '&key))))

(defun prompt-argument (prompt &optional type input)
  (let ((value
          (first
           (evaluate
            (first (prompt
                    :prompt (if type
                                (format nil "~a (~a)" prompt type)
                                prompt)
                    :input (write-to-string input)
                    :sources (make-instance 'prompter:raw-source
                                            :name "Evaluated input")))))))
    (if (or (not type)
            (typep value type))
        value
        (progn
          (echo "~s has type ~s, expected ~s."
                value (type-of value) type)
         (prompt-argument prompt type input)))))

(define-command execute-extended-command (&optional command)
  "Query the user for the arguments to pass to a given COMMAND.
User input is evaluated Lisp."
  ;; TODO: Add support for &rest arguments.
  (let* ((command (or command
                      (first (prompt
                              :prompt "Execute extended command"
                              :sources (make-instance 'user-command-source)
                              :hide-suggestion-count-p t))))
         (lambda-list (swank::arglist (fn command))))
    (multiple-value-match (alex:parse-ordinary-lambda-list lambda-list)
      ((required-arguments optional-arguments _ keyword-arguments)
       (multiple-value-match (parse-function-lambda-list-types (fn command))
         ((required-types optional-types _ keyword-types)
          (flet ((parse-args (params)
                   (alex:mappend
                    (lambda-match
                      ((cons (and param (type symbol)) type)
                       (list (prompt-argument param type)))
                      ((cons (list (list keyword name) default _) type)
                       (list keyword
                             (prompt-argument name type default)))
                      ((cons (list name default _) type)
                       (list (prompt-argument name type default))))
                    params)))
            (setf (last-access command) (local-time:now))
            (apply #'run-async
                   command
                   (alex:mappend #'parse-args
                                 (list (pairlis required-arguments required-types)
                                       (pairlis optional-arguments optional-types)
                                       (pairlis keyword-arguments (mapcar #'second keyword-types))))))))))))

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
