;;; help.lisp --- functions for the user to get help on Next

(in-package :next)

(defun package-symbols (p)
  (let (l) (do-symbols (s p l)
	     (push s l))))

(defun load-package-symbols ()
  (setf *package-symbols* (package-symbols :next)))

(defun load-package-globals ()
  (setf *package-globals* (filter-globals (package-symbols :next))))

(defun filter-globals (symbol-list)
  (remove-if-not
   (lambda (symbol)
     (cl-string-match:match-re "[*]+[a-zA-Z\-]+[*]" (symbol-name symbol)))
   symbol-list))

(defun variable-complete (input)
  (fuzzy-match input *package-globals* #'symbol-name))

(defun function-complete (input)
  (fuzzy-match input (alexandria:hash-table-keys *available-commands*)))

(define-command variable-inspect ()
  "Inspect a variable and show it in a help buffer."
  (load-package-globals)
  (with-result (input (read-from-minibuffer
                       *minibuffer*
                       :completion-function 'variable-complete
                       :input-prompt "Inspect variable:"))
    (let* ((help-buffer (make-buffer
                         (concatenate 'string "HELP-" (symbol-name input))))
           (help-contents (cl-markup:markup
                           (:h1 (symbol-name input))
                           (:p (documentation input 'variable))
                           (:h2 "Current Value:")
                           (:p (write-to-string (symbol-value input)))))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (buffer-evaluate-javascript *interface* help-buffer insert-help)
      (set-active-buffer *interface* help-buffer))))

(define-command command-inspect ()
  "Inspect a function and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       *minibuffer*
                       :input-prompt "Inspect command:"
                       :completion-function 'function-complete))
    (let* ((help-buffer (make-buffer
                         (concatenate 'string "HELP-" input)))
           (help-contents (cl-markup:markup
                           (:h1 input)
                           (:h2 "Documentation")
                           (:p (write-to-string (doc (gethash input *available-commands*))))))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (buffer-evaluate-javascript *interface* help-buffer insert-help)
      (set-active-buffer *interface* help-buffer))))
