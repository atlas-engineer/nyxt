;;; search-buffer.lisp --- functions to enable searching within a buffer

(in-package :next)

(define-parenscript query-buffer (query)
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (defun matches-from-element (element)
    ((ps:chain element inner-h-t-m-l includes) (ps:lisp query)))
  (ps:let ((elements (qsa document (list "*"))))
    (loop for i from 0 to (- (length elements) 1)
          collect (matches-from-element (elt elements i)))))

(define-command search-buffer ()
  "Add search boxes for a given search string."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Search for"
                        :history (minibuffer-search-history *interface*))))
    (print input)))

(define-command remove-search-hints ()
  "Remove all search hints.")

(define-command next-search-hint ()
  "Go to next search hint.")

(define-command previous-search-hint ()
  "Go to previous search hint.")

(define-deprecated-command add-search-hints ()
  "Deprecated by `search-buffer'."
  (search-buffer))
