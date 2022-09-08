;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/repeat-mode
    (:documentation "Mode to infinitely repeat commands."))
(in-package :nyxt/repeat-mode)

(define-mode repeat-mode (nyxt/process-mode:process-mode)
  "Mode to repeat a simple action/function repetitively until stopped."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (nyxt/process-mode:firing-condition
    #'(lambda (path-url mode)
        (declare (ignore path-url))
        (cond
          ((repeat-count mode)
           (if (zerop (repeat-count mode))
               :return
               (decf (repeat-count mode))))
          (t (sleep (repeat-interval mode)) t))))
   (nyxt/process-mode:action
    #'(lambda (path-url mode)
        (declare (ignore path-url))
        (when (repeat-action mode)
          (funcall (repeat-action mode) mode))))
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
It takes a `repeat-mode' instance as argument.")))

(defmethod enable ((mode repeat-mode) &key)
  ;; TODO: Remember prompt input now that we have prompt-buffer hooks.
  (unless (repeat-action mode)
    (let ((prompted-action (prompt1 :prompt "Command to repeat"
                                    :sources 'nyxt:command-source)))
      (setf (repeat-action mode)
            #'(lambda (mode)
                (declare (ignore mode))
                (funcall prompted-action)))))
  (call-next-method))

(define-command-global repeat-every (&optional seconds function)
  "Prompt for FUNCTION to be run every SECONDS."
  (let ((seconds (or seconds
                     (ignore-errors
                      (parse-integer
                       (prompt1 :prompt "Repeat every X seconds"
                                :input "5"
                                :sources 'prompter:raw-source))))))
    (when seconds
      (enable-modes 'repeat-mode (current-buffer)
                    (list :repeat-interval seconds :repeat-action function)))))

(define-command-global repeat-times (&optional times function)
  "Prompt for FUNCTION to be run a number of TIMES."
  (let ((times (or times
                   (ignore-errors
                    (parse-integer
                     (prompt1 :prompt "Repeat for X times"
                              :input "4"
                              :sources 'prompter:raw-source))))))
    (when times
      (enable-modes 'repeat-mode (current-buffer)
                    (list :repeat-count times
                          :repeat-action #'(lambda (mode)
                                             (declare (ignore mode))
                                             (nyxt::run function)))))))

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
    (&key (times (or
                  (ignore-errors
                   (parse-integer
                    (keymaps:key-value (nyxt::last-key (current-window)))))
                  (ignore-errors
                   (parse-integer
                    (prompt1 :prompt "Repeat for X times"
                             :input "4"
                             :sources 'prompter:raw-source))))))
  "Repeat the command bound to the user-pressed keybinding TIMES times."
  (setf *repeat-times-stack* (+ times (* 10 *repeat-times-stack*))
        (command-dispatcher (current-window)) (make-repeat-command-dispatcher *repeat-times-stack*)
        (input-skip-dispatcher (current-window)) #'skip-repeat-dispatch)
  (echo "Press a key sequence for command to repeat ~R times:" *repeat-times-stack*))
