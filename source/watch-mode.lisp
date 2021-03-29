;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/watch-mode
    (:use :common-lisp :nyxt)
  (:documentation "Mode for reloading buffers at regular time
  intervals."))
(in-package :nyxt/watch-mode)

(defun seconds-from-user-input ()
  "Query the numerical time inputs and collate them into seconds."
  (let* ((time-units '("days" "hours" "minutes" "seconds"))
         (to-seconds-alist (pairlis time-units '(86400 3600 60 1)))
         (active-time-units (prompt
                             :prompt "Time unit(s)"
                             :sources (make-instance 'prompter:source
                                                     :name "Units"
                                                     :constructor time-units
                                                     :multi-selection-p t)))
         (times (mapcar (lambda (unit)
                          (parse-integer
                           (first (prompt
                                   :prompt (format nil "Time interval (~a)" unit)
                                   :sources (make-instance 'prompter:raw-source)))
                           :junk-allowed t))
                        active-time-units))
         (to-seconds-multipliers
           (mapcar
            (lambda (elem) (cdr (assoc elem to-seconds-alist :test 'string-equal)))
            active-time-units)))
    (echo "Refreshing every ~:@{~{~d ~}~a~:@}"
          (list times active-time-units))
    (apply '+ (mapcar (lambda (time multiplier) (* time multiplier))
                      times to-seconds-multipliers))))

(define-mode watch-mode (nyxt/process-mode:process-mode)
  "Reload the current buffer at regular time intervals."
  ((action #'(lambda (path-url mode)
               (unless (sleep-time mode)
                 (setf (sleep-time mode) (seconds-from-user-input)))
               (loop
                 (if (quri:uri= path-url (url (buffer mode)))
                     (nyxt::reload-buffer (buffer mode))
                     (buffer-load path-url :buffer (buffer mode)))
                 (sleep (sleep-time mode)))))
   (sleep-time :documentation "The amount of time to sleep between reloads.")))
