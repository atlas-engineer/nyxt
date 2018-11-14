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

(defparen paren-jump-to-heading (heading-inner-text)
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
    (loop for heading in headings do
         (when (equal (ps:lisp heading-inner-text) (ps:chain heading inner-text))
           (ps:chain heading (scroll-into-view t))))))

(define-command jump-to-heading ()
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6"
  (with-result* ((headings (get-headings))
                 (heading (read-from-minibuffer
                           *minibuffer*
                           :input-prompt "Jump to heading:"
                           :completion-function (lambda (input)
                                                  (fuzzy-match
                                                   input
                                                   (cl-json:decode-json-from-string
                                                    headings))))))
    (buffer-execute-javascript *interface*
                               (active-buffer *interface*)
                               (paren-jump-to-heading heading))))
