;;; scroll.lisp --- scrolling functions

(in-package :next)

(define-parenscript %scroll-to-top ()
  (ps:chain window (scroll-by 0 (- (ps:chain document body scroll-height)))))

(define-parenscript %scroll-to-bottom ()
  (ps:chain window (scroll-by 0 (ps:chain document body scroll-height))))

(define-parenscript %scroll-down ((scroll-distance (scroll-distance %buffer)))
  (ps:chain window (scroll-by 0 (ps:lisp scroll-distance))))

(define-parenscript %scroll-up ((scroll-distance (scroll-distance %buffer)))
  (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance)))))

(define-parenscript %scroll-left ((horizontal-scroll-distance (horizontal-scroll-distance %buffer)))
  (ps:chain window (scroll-by (ps:lisp (- horizontal-scroll-distance)) 0)))

(define-parenscript %scroll-right ((horizontal-scroll-distance (horizontal-scroll-distance %buffer)))
  (ps:chain window (scroll-by (ps:lisp horizontal-scroll-distance) 0)))

(define-command scroll-to-top ()
  "Scroll to the top of the current page."
  (%scroll-to-top))

(define-command scroll-to-bottom ()
  "Scroll to the bottom of the current page."
  (%scroll-to-bottom))

(define-command scroll-down ()
  "Scroll down the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (%scroll-down))

(define-command scroll-up ()
  "Scroll up the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (%scroll-up))

(define-command scroll-left ()
  "Scroll left the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (%scroll-left))

(define-command scroll-right ()
  "Scroll right the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (%scroll-right))
