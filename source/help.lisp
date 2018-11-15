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
  (with-result (input (read-from-minibuffer
                       (mode *minibuffer*)
                       :completion 'variable-complete
                       :setup 'load-package-globals))
    (let* ((help-buffer (generate-new-buffer
                         (concatenate 'string "HELP-" (symbol-name input)) (document-mode)))
           (help-contents (concatenate 'string "<h1>" (symbol-name input) "</h1>"
                                       (documentation input 'variable)
                                       "<h2>Current Value:</h2>"
                                       (write-to-string (symbol-value input))))
           (insert-help (ps:ps (setf (ps:@ document Body inner-H-T-M-L) (ps:lisp help-contents)))))
      (interface:web-view-execute (view help-buffer) insert-help)
      (set-visible-active-buffer help-buffer))))

(define-command command-inspect ()
  "Inspect a function and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (mode *minibuffer*)
                       :completion 'function-complete))
    (let* ((help-buffer (generate-new-buffer
                         (concatenate 'string "HELP-" input) (document-mode)))
           (help-contents (concatenate 'string "<h1>" input "</h1>"
                                       (documentation input 'variable)
                                       "<h2>Documentation:</h2>"
                                       (write-to-string (doc (gethash input *available-commands*)))))
           (insert-help (ps:ps (setf (ps:@ document Body inner-H-T-M-L) (ps:lisp help-contents)))))
      (interface:web-view-execute (view help-buffer) insert-help)
      (set-visible-active-buffer help-buffer))))
