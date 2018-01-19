;;; jump-heading.lisp --- functions to enable efficient heading traversal

(in-package :next)

(defun setup-headings-jump ()
  (with-parenscript (headings get-headings)
    (setf *current-completions* headings)))

(defparenstatic get-headings
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
    (ps:chain -j-s-o-n (stringify
                        (loop for heading in headings
                           collect (ps:chain heading inner-text))))))

(defparen paren-jump-to-heading (heading-inner-text)
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
    (loop for heading in headings do
         (when (equal (ps:lisp heading-inner-text) (ps:chain heading inner-text))
           (ps:chain heading (scroll-into-view t))))))

(defun heading-complete (input)
  (fuzzy-match input *current-completions*))

(defun jump-to-heading (input)
  (interface:web-view-execute (view *active-buffer*) (paren-jump-to-heading input)))
