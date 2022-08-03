;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/watch-mode
  (:documentation "Mode for reloading buffers at regular time intervals."))
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
                           (prompt1
                             :prompt (format nil "Time interval (~a)" unit)
                             :sources 'prompter:raw-source)
                           :junk-allowed t))
                        active-time-units))
         (to-seconds-multipliers
           (mapcar
            (lambda (elem) (cdr (assoc elem to-seconds-alist :test 'string-equal)))
            active-time-units)))
    (echo "Refreshing every ~:@{~{~d ~}~a~:@}"
          (list times active-time-units))
    (reduce #'+ (mapcar (lambda (time multiplier) (* time multiplier))
                        times to-seconds-multipliers))))

(define-mode watch-mode (nyxt/repeat-mode:repeat-mode)
  "Reload the current buffer every 5 minutes."
  ((rememberable-p t)
   (nyxt/repeat-mode:repeat-interval 300)
   (nyxt/repeat-mode:repeat-action
    #'(lambda (mode)
        (buffer-load (nyxt/process-mode:path-url mode) :buffer (buffer mode))))))

(define-command-global watch-buffer (&optional (buffer (current-buffer)))
  "Reload BUFFER at a prompted interval."
  (let ((interval (seconds-from-user-input)))
    (enable-modes :modes 'watch-mode :buffers buffer :repeat-interval interval)))
