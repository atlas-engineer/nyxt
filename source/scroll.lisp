;;; scroll.lisp --- scrolling helper functions

(in-package :next)

(define-parenstatic scroll-to-top
    (ps:chain window (scroll-by 0 (- (ps:chain document body scroll-height)))))

(define-parenstatic scroll-to-bottom
    (ps:chain window (scroll-by 0 (ps:chain document body scroll-height))))

(define-parenstatic scroll-down
    (ps:chain window (scroll-by 0 (ps:lisp *scroll-distance*))))

(define-parenstatic scroll-up
    (ps:chain window (scroll-by 0 (ps:lisp (- *scroll-distance*)))))

(define-parenstatic scroll-left
    (ps:chain window (scroll-by (ps:lisp (- *horizontal-scroll-distance*)) 0)))

(define-parenstatic scroll-right
    (ps:chain window (scroll-by (ps:lisp *horizontal-scroll-distance*) 0)))
