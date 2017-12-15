;;;; jump-heading.lisp --- functions to enable efficient heading traversal

(in-package :next)

(defparenstatic get-headings
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
    (ps:chain -j-s-o-n (stringify
                        (loop for heading in headings
                           collect (ps:chain heading inner-text))))))

(defparen jump-to-heading (heading-inner-text)
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
    (loop for heading in headings do
         (when (equal (ps:lisp heading-inner-text) (ps:chain heading inner-text))
           (ps:chain heading (scroll-into-view t))))))
