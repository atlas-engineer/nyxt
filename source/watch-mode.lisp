;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/watch-mode
    (:use :common-lisp :trivia :nyxt)
  (:documentation "Mode for reloading buffers at regular time
  intervals."))
(in-package :nyxt/watch-mode)

(defun seconds-from-user-input ()
  "Read from the minibuffer numerical time inputs and collate them into
seconds."
  (let* ((time-units '("days" "hours" "minutes" "seconds"))
         (to-seconds-alist (pairlis time-units '(86400 3600 60 1)))
         (active-time-units (prompt-minibuffer
                             :input-prompt "Time unit(s)"
                             :multi-selection-p t
                             :suggestion-function (lambda (minibuffer)
                                                    (fuzzy-match
                                                     (input-buffer minibuffer)
                                                     time-units))))
         (times (mapcar (lambda (unit)
                          (parse-integer
                           (prompt-minibuffer
                            :input-prompt (format nil "Time interval (~a)" unit)
                            :hide-suggestion-count-p t)
                           :junk-allowed t))
                        active-time-units))
         (to-seconds-multipliers
           (mapcar
            (lambda (elem) (cdr (assoc elem to-seconds-alist :test 'string-equal)))
            active-time-units)))
    (prompt-minibuffer
     ;; TODO better way to format the string?
     :input-prompt (format nil "Confirm refreshing every ~:@{~{~d ~}~a~:@}?"
                           (list times active-time-units)))
    (apply '+ (mapcar (lambda (time multiplier) (* time multiplier))
                      times to-seconds-multipliers))))

(define-mode watch-mode ()
  "Reload the current buffer at regular time intervals."
  ((thread (let ((secs (seconds-from-user-input)))
             (bt:make-thread (lambda () (loop (reload-current-buffer)
                                         (sleep secs))))))
   (destructor (lambda (mode) (bt:destroy-thread (thread mode))))
   (constructor (lambda (mode) (thread mode)))))
