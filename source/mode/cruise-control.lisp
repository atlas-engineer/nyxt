;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/cruise-control
    (:documentation "Mode for scrolling continuously at a pre-defined speed."))
(in-package :nyxt/mode/cruise-control)

(define-mode cruise-control-mode (nyxt/mode/repeat:repeat-mode)
  "Mode for automatically scrolling up and down the page."
  ((rememberable-p t)
   (velocity 0 :documentation "The distance the page is scrolling up or down
  each update interval. A positive velocity corresponds to scrolling down, a
  negative velocity corresponds to scrolling up.")
   (nyxt/mode/repeat:repeat-interval 0.10)
   (keyscheme-map
    (define-keyscheme-map "cruise-control-mode" ()
      keyscheme:default
      (list
       "escape" 'cruise-control-mode
       "0" 'velocity-zero)
      keyscheme:cua
      (list
       "up" 'velocity-decf
       "down" 'velocity-incf)
      keyscheme:emacs
      (list
       "p" 'velocity-decf
       "n" 'velocity-incf)
      keyscheme:vi-normal
      (list
       "K" 'velocity-decf
       "J" 'velocity-incf)))
   (nyxt/mode/repeat:repeat-action
    (lambda (mode)
      (unless (zerop (velocity mode))
        (with-current-buffer (buffer mode)
          (nyxt/mode/document::scroll-down
           :scroll-distance (velocity mode))))))))

(define-command velocity-incf (&key (cruise-control (find-submode 'cruise-control-mode)))
  "Increase the velocity."
  (incf (velocity cruise-control)))

(define-command velocity-decf (&key (cruise-control (find-submode 'cruise-control-mode)))
  "Decrease the velocity."
  (decf (velocity cruise-control)))

(define-command velocity-zero (&key (cruise-control (find-submode 'cruise-control-mode)))
  "Zero the velocity. Scrolling will stop."
  (setf (velocity cruise-control) 0))
