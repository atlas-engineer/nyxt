;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/cruise-control-mode
    (:documentation "Mode for scrolling continuously at a pre-defined speed."))
(in-package :nyxt/cruise-control-mode)

(define-mode cruise-control-mode (nyxt/repeat-mode:repeat-mode)
  "Mode for automatically scrolling up and down the page."
  ((rememberable-p t)
   (velocity 0 :documentation "The distance the page is scrolling up or down
  each update interval. A positive velocity corresponds to scrolling down, a
  negative velocity corresponds to scrolling up.")
   (nyxt/repeat-mode:repeat-interval 0.10)
   (keymap-scheme
    (define-keyscheme-map "cruise-control-mode" ()
      keyscheme:cua
      (list
       "escape" 'cruise-control-mode
       "0" 'velocity-zero
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
   (nyxt/repeat-mode:repeat-action
    (lambda (mode)
      (unless (zerop (velocity mode))
        (with-current-buffer (buffer mode)
          (nyxt/document-mode::scroll-down
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
