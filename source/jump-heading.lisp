;;; jump-heading.lisp --- functions to enable efficient heading traversal

(in-package :next)

(define-parenstatic get-headings
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
    (ps:chain -j-s-o-n (stringify
                        (loop for heading in headings
                           collect (ps:chain heading inner-text))))))

(defun setup-headings-jump ()
  (with-parenscript (headings get-headings)
    (setf *current-completions* headings)))

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

(define-command jump-to-heading ()
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6"
  (with-result (input (read-from-minibuffer
                       *minibuffer*
                       :completion-function 'heading-complete
                       :setup-function 'setup-headings-jump))
    (buffer-execute-javascript *interface* (view *active-buffer*) (paren-jump-to-heading input))))
