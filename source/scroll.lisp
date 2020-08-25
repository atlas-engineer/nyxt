(in-package :nyxt/web-mode)

(define-command scroll-to-top ()
  "Scroll to the top of the current page."
  (pflet ((scroll-to-top ()
            (ps:chain window (scroll-by 0 (- (ps:chain document body scroll-height))))))
    (scroll-to-top)))

(define-command scroll-to-bottom ()
  "Scroll to the bottom of the current page."
  (pflet ((scroll-to-bottom ()
            (ps:chain window (scroll-by 0 (ps:chain document body scroll-height)))))
    (scroll-to-bottom)))

(define-command scroll-down (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll down the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (pflet ((scroll-down ()
            (ps:chain window (scroll-by 0 (ps:lisp scroll-distance)))))
    (scroll-down)))

(define-command scroll-up (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll up the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (pflet ((scroll-up ()
            (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance))))))
    (scroll-up)))

(define-command scroll-left (&key (horizontal-scroll-distance
                                   (horizontal-scroll-distance (current-buffer))))
  "Scroll left the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (pflet ((scroll-left ()
            (ps:chain window (scroll-by (ps:lisp (- horizontal-scroll-distance)) 0))))
    (scroll-left)))

(define-command scroll-right (&key (horizontal-scroll-distance
                                    (horizontal-scroll-distance (current-buffer))))
  "Scroll right the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (pflet ((scroll-right ()
            (ps:chain window (scroll-by (ps:lisp horizontal-scroll-distance) 0))))
    (scroll-right)))

(define-command scroll-page-down ()
  "Scroll down by one page height."
  (pflet ((scroll-page-down ()
            (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                             (ps:@ window inner-height))))))
    (scroll-page-down)))

(define-command scroll-page-up ()
  "Scroll up by one page height."
  (pflet ((scroll-page-up ()
            (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                             (- (ps:@ window inner-height)))))))
    (scroll-page-up)))
