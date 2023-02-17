;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class prompt-source (prompter:source)
  ;; Override prompter:actions-* slots.  In Nyxt, we want actions to be either
  ;; commands or symbols bound to commands since functions are opaque.
  ;; Concretely, it's important when issuing prompt buffer commands such as
  ;; set-action-on-return.  See
  ;; `nyxt/prompt-buffer-mode::make-action-suggestion'.
  ((prompter:actions-on-return
    #'identity
    :type (or null function sym:function-symbol (cons (or function sym:function-symbol) *))
    :accessor actions-on-return
    :documentation "List of commands that run on `prompter:suggestion's.
This is a low-level implementation, see `prompter:actions-on-return' for the
public interface.
For convenience, it may be initialized with a single symbol or command, in which case it
will be automatically turned into a list.")
   (prompter:actions-on-current-suggestion
    #'identity
    :type (or null function sym:function-symbol (cons (or function sym:function-symbol) *))
    :accessor actions-on-current-suggestion
    :documentation "The first command of this list is called automatically on
the current-suggestion when it's changed.
It does not interrupt or return the prompter.
For convenience, it may be initialized with a single function, in which case it
will be automatically turned into a list.")
   (prompter:actions-on-marks
    #'identity
    :type (or null function sym:function-symbol (cons (or function sym:function-symbol) *))
    :accessor actions-on-marks
    :documentation "The first command of this list is called automatically when
the marks change.
It does not interrupt or return the `prompt-buffer'.
For convenience, it may be initialized with a single function, in which case it
will be automatically turned into a list."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A prompt buffer source is meant to be used by `prompt-buffer'
objects.

For a detailed description see `prompter:source', the parent class of
`prompt-source'.")
  (:metaclass user-class))

(defmethod customize-instance :after ((source prompt-source) &key)
  (setf (actions-on-return source)
        (slot-value source 'prompter:actions-on-return))
  (setf (actions-on-current-suggestion source)
        (slot-value source 'prompter:actions-on-current-suggestion))
  (setf (actions-on-marks source)
        (slot-value source 'prompter:actions-on-marks)))

(defmethod (setf actions-on-return) :after (value (source prompt-source))
  (let ((%value (%actions value)))
    (unless (valid-actions-p %value)
      (cerror "Use this value anyway." 'invalid-prompt-buffer-actions :arg %value))
    (setf (slot-value source 'prompter:actions-on-return) %value)))

(defmethod (setf actions-on-current-suggestion) :after (value (source prompt-source))
  (let ((%value (%actions value)))
    (unless (valid-actions-p %value)
      (cerror "Use this value anyway." 'invalid-prompt-buffer-actions :arg %value))
    (setf (slot-value source 'prompter:actions-on-current-suggestion) %value)))

(defmethod (setf actions-on-marks) :after (value (source prompt-source))
  (let ((%value (%actions value)))
    (unless (valid-actions-p %value)
      (cerror "Use this value anyway." 'invalid-prompt-buffer-actions :arg %value))
    (setf (slot-value source 'prompter:actions-on-marks) %value)))

(defun valid-actions-p (actions)
  "Ensure that ACTIONS are funcallables."
  (every (alex:disjoin 'functionp 'sym:function-symbol-p) (uiop:ensure-list actions)))

(defun %actions (actions)
  "Replace nil ACTIONS with `#'identity'.

Although the nil and #'identity actions are equivalent, it's semantically easier
for users to think of the nil action as the #'identity."
  (if actions
      (substitute #'identity nil (uiop:ensure-list actions))
      (list #'identity)))

(export-always 'invalid-prompt-buffer-actions)
(define-condition invalid-prompt-buffer-actions (error)
  ((arg :reader invalid-prompt-buffer-actions :initarg :arg))
  (:report (lambda (condition stream)
             (format stream "Not all elements of ~S are funcallable."
                     (invalid-prompt-buffer-actions condition)))))
