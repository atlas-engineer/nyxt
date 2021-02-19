;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/watch-mode
    (:use :common-lisp :trivia :nyxt)
  (:documentation "Mode for reloading buffers at regular time
  intervals."))
(in-package :nyxt/watch-mode)

(define-mode watch-mode ()
  "Reload the current buffer at regular time intervals."
  ((thread-label (str:concat "watch-mode-" (id (current-buffer)))
                 :export nil
                 :documentation "Label that identifies a thread.")
   (time-interval
    (let* ((time-units '("days" "hours" "minutes" "seconds"))
           (to-seconds-alist (pairlis time-units '(86400 3600 60 1)))
           (active-time-units (prompt-minibuffer
                               :input-prompt "Time unit(s)"
                               :multi-selection-p t
                               :suggestion-function (lambda (minibuffer) time-units)))
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
                        times to-seconds-multipliers)))
    :type number
    :documentation "Time interval in seconds.")
   (destructor
    (lambda (mode)
      (let ((threads (bt:all-threads)))
        (bt:destroy-thread
         (nth (position (thread-label mode)
                        (mapcar 'bt:thread-name threads) :test 'string-equal)
              threads)))))
   (constructor
    (lambda (mode) (bt:make-thread
               (lambda () (loop (buffer-load (url (buffer mode)))
                           (sleep (time-interval mode))))
               :name (thread-label mode))))))
