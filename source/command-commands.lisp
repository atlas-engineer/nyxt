;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class hook-description ()
  ((name
    ""
    :documentation "The hook name.")
   (value
    nil
    :documentation "The hook value.")))

(defun command-attributes (command &optional buffer)
  (let ((command-name (name command)))
    `(("Name" ,(string-downcase command-name) (:width 1))
      ("Bindings" ,(if buffer
                       (format nil "~{~a~^, ~}"
                               (keymaps:pretty-binding-keys
                                command-name
                                (current-keymaps buffer)
                                :print-style (keymaps:name (keyscheme buffer))))
                       "")
                     (:width 1))
      ("Docstring" ,(documentation-line command 'function "") (:width 4))
      ("Mode" ,(let ((package-name (uiop:symbol-package-name command-name)))
                 (if (str:starts-with-p "NYXT/MODE/" package-name)
                     (string-downcase (str:replace-first "NYXT/MODE/" ""
                                                         package-name))
                     ""))
              (:width 1)))))

(define-class command-source (prompter:source)
  ((prompter:name "Commands")
   (global-p
    t
    :type boolean
    :documentation "Whether global commands are included in the suggestions.")
   (buffer
    nil
    :type (maybe buffer))
   (prompter:constructor
    (lambda (source)
      (sort-by-time
       (list-commands
        :global-p (global-p source)
        :mode-symbols (when (buffer source)
                        (mapcar #'sera:class-name-of
                                (sera:filter #'enabled-p
                                             (enabled-modes (buffer source)))))))))
   (prompter:active-attributes-keys
    '("Name" "Bindings" "Docstring")
    :accessor nil)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches))
  (:export-class-name-p t)
  (:documentation "Prompter source to execute commands.
Global commands are listed if `global-p' is non-nil.
Mode commands of enabled modes are also listed.
While disabled-mode commands are not listed, it's still possible to call them
from a key binding.")
  (:metaclass user-class))

(defmethod prompter:object-attributes
    ((command command) (source command-source))
  (command-attributes command (buffer source)))

(defmethod prompter:object-attributes
    ((command command) (source prompter:source))
  (declare (ignore source))
  (command-attributes command))

(define-command execute-command (buffer)
  "Execute a command by name.

Also accepts arbitrary Lisp expressions (even without the outermost level of
parentheses), and lists the possible completions for the incomplete symbols,
together with the arglists and documentations of the functions typed in."
  (let ((window (window buffer)))
    (unless (and window (active-prompt-buffers window))
      (prompt
       :prompt "Execute command"
       :sources
       (list (make-instance
              'command-source
              :buffer buffer
              :actions-on-return
              (list (lambda-command run-command* (commands)
                      "Run the chosen command."
                      (let* ((command (first commands))
                             (mode (find-if (lambda (m)
                                              (eq (symbol-package (name command))
                                                  (symbol-package (name m))))
                                            (enabled-modes buffer)))
                             (target-buffer (if mode (buffer mode) buffer)))
                        (setf (last-access command) (local-time:now))
                        (dispatch-command command target-buffer)))
                    (lambda-command describe-command* (commands)
                      "Show the documentation and properties of this command."
                      (describe-command :command (name (first commands)))))))
       :hide-suggestion-count-p t))))

(defun get-hooks (buffer)
  (flet ((list-hooks (object)
           (when object
             (mapcar (lambda (hook)
                       (make-instance
                        'hook-description
                        :name (str:downcase (closer-mop:slot-definition-name hook))
                        :value (funcall (symbol-function
                                         (closer-mop:slot-definition-name hook))
                                        object)))
                     (remove-if-not
                      (lambda (s)
                        (let ((name (closer-mop:slot-definition-name s)))
                          (and (str:ends-with-p "-hook" (string name)
                                                :ignore-case t)
                               (fboundp name))))
                      (closer-mop:class-slots (class-of object)))))))
    (let ((window-hooks (list-hooks (window buffer)))
          (buffer-hooks (list-hooks buffer))
          (browser-hooks (list-hooks *browser*)))
      (append window-hooks
              buffer-hooks
              browser-hooks))))

(define-class hook-source (prompter:source)
  ((prompter:name "Hooks")
   (prompter:actions-on-return (lambda-mapped-command value))))

(defmethod prompter:object-attributes
    ((hook-description hook-description) (source hook-source))
  (declare (ignore source))
  `(("Name" ,(name hook-description))))

(define-class handler-source (prompter:source)
  ((prompter:name "Handlers")
   (hook
    nil
    :documentation "The hook for which to retrieve handlers for.")
   (prompter:constructor (lambda (source) (hooks:handlers (hook source))))))

(defmethod prompter:object-attributes ((handler symbol) (source handler-source))
  (declare (ignore source))
  `(("Name" ,(str:downcase (hooks:name handler)))))

(define-class disabled-handler-source (handler-source)
  ((prompter:constructor
    (lambda (source) (hooks:disabled-handlers (hook source))))))

(defun manage-hook-handler (action buffer)
  (let ((hook (prompt1 :prompt "Hook"
                       :sources (make-instance 'hook-source
                                               :constructor (get-hooks buffer)))))
    (funcall (case action
               (:enable #'hooks:enable-hook)
               (:disable #'hooks:disable-hook))
             hook
             (prompt1 :prompt "Handler"
                      :sources (make-instance
                                (case action
                                  (:enable 'disabled-handler-source)
                                  (:disable 'handler-source))
                                :hook hook)))))

(define-command-global disable-hook-handler (buffer)
  "Remove handler of a hook."
  (manage-hook-handler :disable buffer))

(define-command-global enable-hook-handler (buffer)
  "Add handler of a hook."
  (manage-hook-handler :enable buffer))
