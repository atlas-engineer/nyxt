;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/cruise-control-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for scrolling continuously at a pre-defined speed."))

(in-package :nyxt/cruise-control-mode)

(define-mode cruise-control-mode ()
  "Mode for automatically scrolling up and down the page."
  ((velocity 0 :documentation "The distance the page is scrolling up or down
  each update interval. A positive velocity corresponds to scrolling down, a
  negative velocity corresponds to scrolling up.")
   (poll-sleep-time 0.10 :documentation "The amount of time spent sleeping between
   each update interval (movement of the page). A poll-sleep-time of 2 means
   that every 2 seconds the page will update its position.")
   (thread :documentation "Runs the operations to scroll the page up and down.")
   (keymap-scheme
    (define-scheme "cruise-control-mode"
      scheme:cua
      (list
       "0" 'velocity-zero
       "up" 'velocity-decf
       "down" 'velocity-incf)
      scheme:emacs
      (list
       "p" 'velocity-decf
       "n" 'velocity-incf)
      scheme:vi-normal
      (list
       "k" 'velocity-decf
       "j" 'velocity-incf)))
   (constructor
    (lambda (mode)
      (setf (thread mode)
            (bt:make-thread
             (lambda ()
               (loop while t
                     do (sleep (poll-sleep-time mode))
                        (when (not (zerop (velocity mode)))
                          (with-current-buffer (buffer mode)
                            (nyxt/web-mode::scroll-down
                             :scroll-distance (velocity mode))))))))))
   (destructor
    (lambda (mode)
      (bt:destroy-thread (thread mode))))))

(defun current-cruise-control (&key (buffer (current-buffer)))
  (find-submode buffer 'cruise-control-mode))

(define-command velocity-incf (&key (cruise-control (current-cruise-control)))
  "Increase the velocity."
  (incf (velocity cruise-control)))

(define-command velocity-decf (&key (cruise-control (current-cruise-control)))
  "Decrease the velocity."
  (decf (velocity cruise-control)))

(define-command velocity-zero (&key (cruise-control (current-cruise-control)))
  "Zero the velocity."
  (setf (velocity cruise-control) 0))
