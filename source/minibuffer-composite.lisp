(in-package :nyxt)

(defstruct minibuffer-source
  (suggestion-function)
  (result-function))

(define-command meta-search ()
  "Search a composite set of sources simultaneously."
  (let ((minibuffers (list
                      (make-minibuffer-source
                       :suggestion-function (lambda (i) (fuzzy-match i (list "Turtle" "Sea Turtle" "Box Turtle")))
                       :result-function (lambda (i)
                                          (print i)))
                      (make-minibuffer-source
                       :suggestion-function (lambda (i) (fuzzy-match i (list "Salmon" "Carp" "Swordfish")))
                       :result-function (lambda (i)
                                          (print i))))))
    (with-result (selection (read-from-minibuffer
                             (make-minibuffer
                              :input-prompt "Open bookmarks in new buffers"
                              :default-modes '(minibuffer-tag-mode minibuffer-mode)
                              :suggestion-function
                              (lambda (minibuffer)
                                (apply #'intertwine
                                       (mapcar (lambda (i)
                                                 (funcall (minibuffer-source-suggestion-function i)
                                                          (input-buffer minibuffer)))
                                               minibuffers))))))
      (print selection))))

(defun intertwine (&rest lists)
  (let ((heads (copy-list lists))
        (result '()))
    (loop (loop for list on heads do
                   (when (car list)
                     (push (pop (car list)) result))
                   (when (every #'null heads)
                     (return-from intertwine (nreverse result)))))))

