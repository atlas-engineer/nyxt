;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/macro-edit
    (:documentation "Mode for editing macros."))
(in-package :nyxt/mode/macro-edit)

(define-mode macro-edit-mode ()
  "Mode for creating and editing macros."
  ((visible-in-status-p nil)
   (macro-name
    ""
    :accessor nil
    :documentation "The name used for the macro.")
   (functions
    '()
    :documentation "Functions the user has added to their macro."))
  (:toggler-command-p nil))

(defmethod render-functions ((macro-editor macro-edit-mode))
  (spinneret:with-html-string
    (:table
     (loop for function in (functions macro-editor)
           for index from 0
           collect (:tr (:td (:nbutton :class "button"
                               :text "✕"
                               :title "Remove from the macro"
                               `(nyxt/mode/macro-edit::remove-function
                                 (find-submode 'macro-edit-mode)
                                 ,index)))
                        (:td
                         (:a.button
                          :title "Help"
                          :target "_blank"
                          :href (nyxt-url 'describe-function
                                          :fn (name (nth index (functions macro-editor))))
                          "ℹ"))
                        (:td (let ((name (symbol-name (name function))))
                               (if (str:upcase? name)
                                   (string-downcase name)
                                   name))))))))

(define-internal-page-command-global edit-macro ()
    (buffer "*Macro edit*" 'nyxt/mode/macro-edit:macro-edit-mode)
  "Edit a macro."
  (spinneret:with-html-string
    (:nstyle (style buffer))
    (:h1 "Macro editor")
    (:p "Name")
    (:input :type "text" :id "macro-name")
    (:p "Commands")
    (:p (:nbutton
          :text "+ Add command"
          '(nyxt/mode/macro-edit::add-command)))
    (:div
     :id "commands"
     (:raw
      (render-functions
       (find-submode 'nyxt/mode/macro-edit:macro-edit-mode))))
    (:br)
    (:hr)
    (:nbutton
      :text "Save macro"
      '(nyxt/mode/macro-edit::save-macro))
    (:nbutton
      :text "Compile macro"
      '(nyxt/mode/macro-edit::evaluate-macro))))

(defmethod add-function ((macro-editor macro-edit-mode) command)
  (alex:appendf (functions macro-editor)
                (list command))
  (reload-buffer (buffer macro-editor)))

(defun delete-nth (n list)
  (nconc (subseq list 0 n) (nthcdr (1+ n) list)))

(defmethod remove-function ((macro-editor macro-edit-mode) command-index)
  (setf (functions macro-editor) (delete-nth command-index (functions macro-editor)))
  (reload-buffer (buffer macro-editor)))

(defmethod macro-name ((macro-editor macro-edit-mode))
  (let ((name (ps-eval :buffer (buffer macro-editor)
                (ps:chain (nyxt/ps:qs document "#macro-name") value))))
    (cond ((not (str:emptyp name))
           (setf (slot-value macro-editor 'macro-name) (string-upcase name)))
          ((slot-value macro-editor 'macro-name)
           (slot-value macro-editor 'macro-name))
          (t nil))))

(defmethod generate-macro-form ((macro-editor macro-edit-mode))
  (let ((name (intern (macro-name macro-editor)))
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
  (and (macro-name macro-editor)
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
      (progn
        (eval (generate-macro-form macro-editor))
        (echo "Macro compiled, you may now use the ~s command."
              (name macro-editor)))
      (echo "Macro form is invalid; check it has a title and functions.")))
