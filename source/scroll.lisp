;;; scroll.lisp --- scrolling functions

(in-package :next)

(define-parenstatic scroll-to-top
    (ps:chain window (scroll-by 0 (- (ps:chain document body scroll-height)))))

(define-parenstatic scroll-to-bottom
    (ps:chain window (scroll-by 0 (ps:chain document body scroll-height))))

(define-parenscript scroll-down ()
    (ps:chain window (scroll-by 0 (ps:lisp (scroll-distance (active-buffer *interface*))))))

(define-parenscript scroll-up ()
    (ps:chain window (scroll-by 0 (ps:lisp (- (scroll-distance (active-buffer *interface*)))))))

(define-parenscript scroll-left ()
    (ps:chain window (scroll-by (ps:lisp (- (horizontal-scroll-distance (active-buffer *interface*)))) 0)))

(define-parenscript scroll-right ()
    (ps:chain window (scroll-by (ps:lisp (horizontal-scroll-distance (active-buffer *interface*))) 0)))
