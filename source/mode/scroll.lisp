;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-command scroll-to-top ()
  "Scroll to the top of the current page."
  (peval (ps:chain window (scroll-by 0 (- (ps:chain document document-element scroll-height))))))

(define-command scroll-to-bottom ()
  "Scroll to the bottom of the current page."
  (peval (ps:chain window (scroll-by 0 (ps:chain document document-element scroll-height)))))

(define-command scroll-down (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll down the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (peval (ps:chain window (scroll-by 0 (ps:lisp scroll-distance)))))

(define-command scroll-up (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll up the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (peval (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance))))))

(define-command scroll-left (&key (horizontal-scroll-distance
                                   (horizontal-scroll-distance (current-buffer))))
  "Scroll left the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (peval (ps:chain window (scroll-by (ps:lisp (- horizontal-scroll-distance)) 0))))

(define-command scroll-right (&key (horizontal-scroll-distance
                                    (horizontal-scroll-distance (current-buffer))))
  "Scroll right the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (peval (ps:chain window (scroll-by (ps:lisp horizontal-scroll-distance) 0))))

(define-command scroll-page-down ()
  "Scroll down by one page height."
  (peval (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                              (ps:@ window inner-height))))))

(define-command scroll-page-up ()
  "Scroll up by one page height."
  (peval (ps:chain window (scroll-by 0 (- (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                             (ps:@ window inner-height)))))))
