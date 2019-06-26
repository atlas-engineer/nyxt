;;; help.lisp --- functions for the user to get help on Next

(in-package :next)

(define-mode help-mode ()
    "Mode for displaying documentation."
    ((keymap-schemes
      :initform
      (let ((emacs-map (make-keymap))
            (vi-map (make-keymap)))
        (define-key "C-p" 'scroll-up
          "C-n" 'scroll-down
          :keymap emacs-map)
        (define-key :keymap vi-map
          "k" 'scroll-up
          "j" 'scroll-down)
        (list :emacs emacs-map
              :vi-normal vi-map)))))

(defun package-symbols (p)
  (let (l) (do-symbols (s p l)
             (push s l))))

(defun variable-complete (input)
  (fuzzy-match input (package-variables) :accessor-function #'symbol-name))

(defun function-complete (input)
  (fuzzy-match input (list-commands)
               :accessor-function #'(lambda (c)
                                      (closer-mop:generic-function-name
                                       (closer-mop:method-generic-function c)))))

;; TODO: This is barely useful as is since we don't have any global.  We need to
;; augment the latter function so that we can inspect *INTERFACE* and classes.
(define-command variable-inspect ()
  "Inspect a variable and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (minibuffer *interface*)
                       :completion-function 'variable-complete
                       :input-prompt "Inspect variable:"))
    (let* ((help-buffer (make-buffer
                         :name (concatenate 'string "HELP-" (symbol-name input))
                         :default-modes (cons 'help-mode
                                                (get-default 'buffer 'default-modes))))
           (help-contents (cl-markup:markup
                           (:h1 (symbol-name input))
                           (:p (documentation input 'variable))
                           (:h2 "Current Value:")
                           (:p (write-to-string (symbol-value input)))))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (%%buffer-evaluate-javascript *interface* help-buffer insert-help)
      (set-active-buffer *interface* help-buffer))))

;; TODO: Have both "function-inspect" and "command-inspect"?
(define-command command-inspect ()
  "Inspect a function and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (minibuffer *interface*)
                       :input-prompt "Inspect command:"
                       :completion-function 'function-complete))
    (let* ((input-sym (closer-mop:generic-function-name
                       (closer-mop:method-generic-function input)))

           (help-buffer (make-buffer
                         :name (concatenate 'string "HELP-" (symbol-name input-sym))
                         :default-modes (cons 'help-mode
                                              (get-default 'buffer 'default-modes))))
           (help-contents (cl-markup:markup
                           (:h1 (symbol-name input-sym))
                           (:h2 "Documentation")
                           (:p (write-to-string
                                (documentation input t)))))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (%%buffer-evaluate-javascript *interface* help-buffer insert-help)
      (set-active-buffer *interface* help-buffer))))

(defun evaluate (string)
  "Evaluate all expressions in string and return a list of values.
This does not use an implicit PROGN to allow evaluating top-level expressions."
  (with-input-from-string (input string)
    (loop for object = (read input nil :eof)
          until (eq object :eof)
          collect (eval object))))

(define-command command-evaluate ()
  "Evaluate a form."
  (with-result (input (read-from-minibuffer
                       (minibuffer *interface*)
                       :input-prompt "Evaluate Lisp:"))
    (let* ((result-buffer (make-buffer
                           :name (concatenate 'string "EVALUATION RESULT-" input)
                           :default-modes (cons 'help-mode
                                                (get-default 'buffer 'default-modes))))
           (results (handler-case
                        (mapcar #'write-to-string (evaluate input))
                      (error (c) (format nil "~a" c))))
           (result-contents (apply #'concatenate 'string
                                   (cl-markup:markup
                                    (:h1 "Form")
                                    (:p input)
                                    (:h1 "Result"))
                                   (loop for result in results
                                         collect (cl-markup:markup (:p result)))))
           (insert-results (ps:ps (setf (ps:@ document Body |innerHTML|)
                                        (ps:lisp result-contents)))))
      (%%buffer-evaluate-javascript *interface* result-buffer insert-results)
      (set-active-buffer *interface* result-buffer))))

(define-command next-version ()
  "Version number of this version of Next.
The version number is stored in the clipboard."
  (trivial-clipboard:text +version+)
  (echo "Version ~a" +version+))
