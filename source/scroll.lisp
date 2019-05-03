;;; scroll.lisp --- scrolling functions

(in-package :next)

(define-parenscript scroll-to-top ()
  (ps:chain window (scroll-by 0 (- (ps:chain document body scroll-height)))))

(define-parenscript scroll-to-bottom ()
  (ps:chain window (scroll-by 0 (ps:chain document body scroll-height))))

(define-parenscript scroll-down ((scroll-distance (scroll-distance %buffer)))
  (ps:chain window (scroll-by 0 (ps:lisp scroll-distance))))

(define-parenscript scroll-up ((scroll-distance (scroll-distance %buffer)))
  (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance)))))

(define-parenscript scroll-left ((horizontal-scroll-distance (horizontal-scroll-distance %buffer)))
  (ps:chain window (scroll-by (ps:lisp (- horizontal-scroll-distance)) 0)))

(define-parenscript scroll-right ((horizontal-scroll-distance (horizontal-scroll-distance %buffer)))
  (ps:chain window (scroll-by (ps:lisp horizontal-scroll-distance) 0)))
