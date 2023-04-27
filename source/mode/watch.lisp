;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/watch
  (:documentation "Mode for reloading buffers at regular time intervals."))
(in-package :nyxt/mode/watch)

(defun seconds-from-user-input ()
  "Query the numerical time inputs and collate them into seconds."
  (let* ((time-units '("days" "hours" "minutes" "seconds"))
         (to-seconds-alist (pairlis time-units '(86400 3600 60 1)))
         (active-time-units (prompt
                             :prompt "Time unit(s)"
                             :sources (make-instance 'prompter:source
                                                     :name "Units"
                                                     :constructor time-units
                                                     :enable-marks-p t)))
         (times (mapcar (lambda (unit)
                          (parse-integer
                           (prompt1
                             :prompt (format nil "Time interval (~a)" unit)
                             :sources 'prompter:raw-source)
                           :junk-allowed t))
                        active-time-units))
         (to-seconds-multipliers
           (mapcar
            (lambda (elem) (rest (assoc elem to-seconds-alist :test 'string-equal)))
            active-time-units)))
    (echo "Refreshing every ~:@{~{~d ~}~a~:@}"
          (list times active-time-units))
    (reduce #'+ (mapcar (lambda (time multiplier) (* time multiplier))
                        times to-seconds-multipliers))))

(define-mode watch-mode (nyxt/mode/repeat:repeat-mode)
  "Reload the current buffer every 5 minutes."
  ((rememberable-p t)
   (nyxt/mode/repeat:repeat-interval 300.0)
   (nyxt/mode/repeat:repeat-action
    #'(lambda (mode)
        (reload-buffer (buffer mode)))
    :type (maybe (function (nyxt/mode/repeat:repeat-mode))))))

(define-command-global watch-buffer (&optional (buffer (current-buffer)))
  "Reload BUFFER at a prompted interval."
  (let ((interval (seconds-from-user-input)))
    (enable-modes* 'watch-mode buffer :repeat-interval interval :remember-p t)))
