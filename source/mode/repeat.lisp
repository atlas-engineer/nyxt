;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/repeat-mode
    (:documentation "Mode to repeat actions."))
(in-package :nyxt/repeat-mode)

(define-mode repeat-mode (nyxt/process-mode:process-mode)
  "Repeat the execution of a command while enabled."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (repeat-count
    nil
    :type (or integer null)
    :documentation "The number of times to repeat the commands for.")
   (repeat-interval
    1.0
    :type alex:non-negative-real
    :documentation "Time in seconds after which `repeat-action' is repeated.")
   (repeat-action
    nil
    :type (or null (function (repeat-mode)))
    :documentation "The action to repeat.
It takes a `repeat-mode' instance as argument.")
   (nyxt/process-mode:firing-condition
    #'(lambda (path-url mode)
        (declare (ignore path-url))
        (sleep (repeat-interval mode))
        (cond ((repeat-count mode)
               (if (zerop (repeat-count mode))
                   :return
                   (decf (repeat-count mode))))
              (t t)))
    :documentation "See `nyxt/process-mode:firing-condition'.")
   (nyxt/process-mode:action
    #'(lambda (path-url mode)
        (declare (ignore path-url))
        (funcall* (repeat-action mode) mode))
    :documentation "See `nyxt/process-mode:action'.")
   (nyxt/process-mode:cleanup
    #'(lambda (path-url mode)
        (declare (ignore path-url))
        ;; Needed since the mode object might not have been garbage collected.
        (setf (repeat-action mode) nil
              (repeat-count mode) nil
              (repeat-interval mode) 1.0))
    :documentation "See `nyxt/process-mode:cleanup'.")))

(defmethod enable ((mode repeat-mode) &key)
  ;; TODO: Remember prompt input now that we have prompt-buffer hooks.
  (unless (repeat-action mode)
    (let* ((nyxt::*interactive-p* t)
           (prompted-action (prompt1 :prompt "Command to repeat"
                                     :sources 'nyxt:command-source)))
      (setf (repeat-action mode)
            #'(lambda (mode)
                (declare (ignore mode))
                (funcall prompted-action))))))

(define-command-global repeat-every (&optional seconds function)
  "Prompt for FUNCTION to be run every SECONDS."
  (alex:when-let ((seconds (or seconds
                               (ignore-errors
                                (parse-integer
                                 (prompt1 :prompt "Repeat every X seconds"
                                          :input "5"
                                          :hide-suggestion-count-p t
                                          :sources 'prompter:raw-source))))))
    (enable-modes* 'repeat-mode
                   (current-buffer)
                   :repeat-interval seconds
                   :repeat-action function)))

(define-command-global repeat-times (&optional times function)
  "Prompt for FUNCTION to be run a number of TIMES."
  (alex:when-let ((times (or times
                             (ignore-errors
                              (parse-integer
                               (prompt1 :prompt "Repeat for X times"
                                        :input "4"
                                        :hide-suggestion-count-p t
                                        :sources 'prompter:raw-source))))))
    (enable-modes* 'repeat-mode
                   (current-buffer)
                   :repeat-count times
                   :repeat-action function)))

(defvar *repeat-times-stack* 0
  "The current number of repetitions.")

(defun make-repeat-command-dispatcher (times)
  (lambda (command)
    (if (eq 'repeat-key command)
        (dispatch-command command)
        (unwind-protect
             (repeat-times times (symbol-function command))
          (setf (command-dispatcher (current-window)) #'dispatch-command
                (input-skip-dispatcher (current-window)) #'dispatch-input-skip
                *repeat-times-stack* 0)))))

(defun skip-repeat-dispatch (keyspec)
  (declare (ignore keyspec))
  (echo "Canceled repeat-key.")
  (setf (command-dispatcher (current-window)) #'dispatch-command
        (input-skip-dispatcher (current-window)) #'dispatch-input-skip))

(define-command-global repeat-key
    (&key (times (let ((nyxt::*interactive-p* t))
                   (or
                    (ignore-errors
                     (parse-integer
                      (keymaps:key-value (nyxt::last-key (current-window)))))
                    (ignore-errors
                     (parse-integer
                      (prompt1 :prompt "Repeat for X times"
                               :input "4"
                               :hide-suggestion-count-p t
                               :sources 'prompter:raw-source)))))))
  "Repeat the command bound to the user-pressed keybinding TIMES times."
  (setf *repeat-times-stack* (+ times (* 10 *repeat-times-stack*))
        (command-dispatcher (current-window)) (make-repeat-command-dispatcher *repeat-times-stack*)
        (input-skip-dispatcher (current-window)) #'skip-repeat-dispatch)
  (echo "Press a key sequence for command to repeat ~R times:" *repeat-times-stack*))
