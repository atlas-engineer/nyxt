;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/repeat-mode
    (:use :common-lisp :nyxt)
  (:documentation "Mode to infinitely repeat commands."))
(in-package :nyxt/repeat-mode)

(serapeum:export-always 'repeat-seconds)
(defun repeat-seconds (seconds)
  (lambda (&rest args)
    (declare (ignore args))
    (sleep seconds)
    t))

(defun initialize-repeat-mode (mode)
  (unless (repetitive-action mode)
    (setf (repetitive-action mode)
          (first
           (prompt :prompt "Command to repeat"
                   :sources (list (make-instance 'nyxt:command-source))))))
  (nyxt/process-mode::initialize-process-mode mode))

(define-mode repeat-mode (nyxt/process-mode:process-mode)
  "Mode to repeat a simple action/function repetitively until stopped."
  ((nyxt/process-mode:firing-condition (repeat-seconds 5))
   (nyxt/process-mode:action
    #'(lambda (path-url mode)
        (declare (ignore path-url))
        (when (repetitive-action mode)
          (funcall (repetitive-action mode) mode))))
   (repetitive-action nil
                      :type (or null (function (repeat-mode)))
                      :documentation "The action to repeat.
Function taking a repeat-mode instance.")))

(define-command-global repeat-every (&optional seconds)
  "Repeat a command every SECONDS (prompts if SECONDS are not set)."
  (let ((seconds (or seconds
                     (ignore-errors
                      (parse-integer
                       (first (prompt :prompt "Repeat every X seconds"
                                      :input "5"
                                      :sources (list (make-instance 'prompter:raw-source)))))))))
    (when seconds
      (enable-modes 'repeat-mode (current-buffer) (list :firing-condition (repeat-seconds seconds))))))
