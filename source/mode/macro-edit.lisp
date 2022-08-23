;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/macro-edit-mode
    (:documentation "Mode for editing macros."))
(in-package :nyxt/macro-edit-mode)

(define-mode macro-edit-mode ()
  "Mode for creating and editing macros."
  ((visible-in-status-p nil)
   (name
    ""
    :accessor nil
    :documentation "The name used for the macro.")
   (functions
    '()
    :documentation "Functions the user has added to their macro."))
  (:toggler-command-p nil))

(define-internal-page-command-global edit-macro ()
    (buffer "*Macro edit*" 'nyxt/macro-edit-mode:macro-edit-mode)
  "Edit a macro."
  (spinneret:with-html-string
    (:style (style buffer))
    (:h1 "Macro editor")
    (:p "Name")
    (:input :type "text" :id "macro-name")
    (:p "Commands")
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval (:title "add-command") (nyxt/macro-edit-mode::add-command)))
                 "+ Add command"))
    (:div :id "commands" "")
    (:br)
    (:hr)
    (:button :class "button"
             :onclick (ps:ps (nyxt/ps:lisp-eval (:title "save-macro") (nyxt/macro-edit-mode::save-macro)))
             "Save macro")
    (:button :class "button"
             :onclick (ps:ps (nyxt/ps:lisp-eval (:title "evaluate-macro") (nyxt/macro-edit-mode::evaluate-macro)))
             "Compile macro")))

(defmethod render-functions ((macro-editor macro-edit-mode))
  (flet ((render-functions ()
           (spinneret:with-html-string
             (:table
              (loop for function in (functions macro-editor)
                    for index from 0
                    collect (:tr (:td (:button :class "button"
                                               :onclick (ps:ps (nyxt/ps:lisp-eval
                                                                (:title "remove-function")
                                                                (nyxt/macro-edit-mode::remove-function
                                                                 (find-submode 'macro-edit-mode)
                                                                 index)))
                                               "✕"))
                                 (:td (:button :class "button"
                                               :onclick (ps:ps (nyxt/ps:lisp-eval
                                                                (:title "command-help")
                                                                (nyxt/macro-edit-mode::command-help
                                                                 (find-submode 'macro-edit-mode)
                                                                 index)))
                                               "ℹ"))
                                 (:td (let ((name (symbol-name (name function))))
                                        (if (str:upcase? name)
                                            (string-downcase name)
                                            name)))))))))
    (ps-eval :async t :buffer (buffer macro-editor)
      (setf (ps:chain document (get-element-by-id "commands") |innerHTML|)
            (ps:lisp
             (render-functions))))))

(defmethod command-help ((macro-editor macro-edit-mode) command-index)
  (nyxt::describe-command :command (name (nth command-index (functions macro-editor)))))

(defmethod add-function ((macro-editor macro-edit-mode) command)
  (alex:appendf (functions macro-editor)
                (list command))
  (render-functions macro-editor))

(defun delete-nth (n list)
  (nconc (subseq list 0 n) (nthcdr (1+ n) list)))

(defmethod remove-function ((macro-editor macro-edit-mode) command-index)
  (setf (functions macro-editor) (delete-nth command-index (functions macro-editor)))
  (render-functions macro-editor))

(defmethod name ((macro-editor macro-edit-mode))
  (let ((name (ps-eval :buffer (buffer macro-editor)
                (ps:chain document (get-element-by-id "macro-name") value))))
    (cond ((not (str:emptyp name)) (setf (slot-value macro-editor 'name) (string-upcase name)))
          ((slot-value macro-editor 'name) (slot-value macro-editor 'name))
          (t nil))))

(defmethod generate-macro-form ((macro-editor macro-edit-mode))
  (let ((name (intern (name macro-editor)))
        (commands (mapcar
                   (lambda (command) `(,(name command)))
                   (functions macro-editor))))
    `(define-command-global ,name () "User generated macro form." ,@commands)))

(define-command add-command (&optional (macro-editor (find-submode 'macro-edit-mode)))
  "Add a command to the macro."
  (add-function macro-editor (prompt1
                              :prompt "Add command"
                              :sources 'command-source)))

(defmethod macro-form-valid-p ((macro-editor macro-edit-mode))
  (and (name macro-editor)
       (functions macro-editor)))

(define-command save-macro (&optional (macro-editor (find-submode 'macro-edit-mode)))
  "Save the macro to the `*auto-config-file*' file."
  (if (macro-form-valid-p macro-editor)
      (progn
        (nyxt::auto-configure :form (generate-macro-form macro-editor))
        (echo "Saved macro to ~s." (files:expand *auto-config-file*)))
      (echo "Macro form is invalid; check it has a title and functions.")))

(define-command evaluate-macro (&optional (macro-editor (find-submode 'macro-edit-mode)))
  "Evaluate the macro for testing."
  (if (macro-form-valid-p macro-editor)
      (progn (eval (generate-macro-form macro-editor))
             (echo "Macro compiled, you may now use the ~s command."
                   (name macro-editor)))
      (echo "Macro form is invalid; check it has a title and functions.")))
