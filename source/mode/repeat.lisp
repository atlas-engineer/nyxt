;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/repeat
  (:documentation "Package for `repeat-mode', a mode to repeat actions.

Parts of the API are:
- `repeat-mode' slots:
  - `repeat-action': the action to repeat.
  - `repeat-count': the number of times to repeat the `repeat-action'.
  - `repeat-interval': the interval to repeat `repeat-action' at.
- Internal keyboard dispatchers (can be a good example of Nyxt input handling):
  - `make-repeat-command-dispatcher'.
  - `skip-repeat-dispatch'.

`repeat-mode' is based on `nyxt/mode/process:process-mode' and customizes the
`nyxt/mode/process:firing-condition', `nyxt/mode/process:action', and
`nyxt/mode/process:cleanup' to repeat the action on the dedicated thread.

See the `repeat-mode' for the external user-facing APIs."))
(in-package :nyxt/mode/repeat)

(define-mode repeat-mode (nyxt/mode/process:process-mode)
  "Repeat the execution of a command while enabled.

The commands that `repeat-mode' exposes are:
- `repeat-every' to repeat an action every X seconds.
- `repeat-times' to repeat an action only X times.
- `repeat-key' to repeat the command bound to the keybinding.

See `nyxt/mode/repeat' package documentation for implementation details and
internal programming APIs."
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
   (nyxt/mode/process:firing-condition
    #'(lambda (path-url mode)
        (declare (ignore path-url))
        (when (repeat-interval mode)
          (sleep (repeat-interval mode)))
        (cond ((repeat-count mode)
               (if (zerop (repeat-count mode))
                   :return
                   (decf (repeat-count mode))))
              (t t)))
    :documentation "See `nyxt/mode/process:firing-condition'.")
   (nyxt/mode/process:action
    #'(lambda (path-url mode)
        (declare (ignore path-url))
        (funcall* (repeat-action mode) mode))
    :documentation "See `nyxt/mode/process:action'.")
   (nyxt/mode/process:cleanup
    #'(lambda (path-url mode)
        (declare (ignore path-url))
        ;; Needed since the mode object might not have been garbage collected.
        (setf (repeat-action mode) nil
              (repeat-count mode) nil
              (repeat-interval mode) 1.0))
    :documentation "See `nyxt/mode/process:cleanup'.")))

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

(define-command-global repeat-every (&key (seconds (sera:parse-float
                                                    (prompt1 :prompt "Repeat every X seconds"
                                                             :input "5"
                                                             :hide-suggestion-count-p t
                                                             :sources 'prompter:raw-source)
                                                    :type 'single-float))
                                     function)
  "Prompt for FUNCTION to be run every SECONDS."
  (enable-modes* 'repeat-mode
                 (current-buffer)
                 :repeat-interval seconds
                 :repeat-action function))

(define-command-global repeat-times (&key (times
                                           (parse-integer
                                            (prompt1 :prompt "Repeat for X times"
                                                     :input "4"
                                                     :hide-suggestion-count-p t
                                                     :sources 'prompter:raw-source)))
                                     function)
  "Prompt for FUNCTION to be run a number of TIMES."
  (enable-modes* 'repeat-mode
                 (current-buffer)
                 :repeat-count times
                 :repeat-action function))

(defvar *repeat-times-stack* 0
  "The current number of repetitions.")

(defun make-repeat-command-dispatcher (times)
  "Create a command dispatcher that counts the M-digit keys and adds them together.
Once a non-number key is pressed, it dispatches this key to a command and starts
repeating it like a regular `repeat-mode' does."
  (lambda (command)
    (if (eq 'repeat-key command)
        (dispatch-command command)
        (unwind-protect
             (repeat-times times (lambda (mode)
                                   (declare (ignore mode))
                                   (nyxt::run command)))
          (setf (command-dispatcher (current-window)) #'dispatch-command
                (input-skip-dispatcher (current-window)) #'dispatch-input-skip
                *repeat-times-stack* 0)))))

(defun skip-repeat-dispatch (keyspec)
  "A stub copy of `dispatch-input-skip' customized for `repeat-mode'."
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
