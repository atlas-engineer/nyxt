;;; recent-buffer.lisp --- lisp subroutines for creating and managing recent buffers

(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass recent-buffer()
  ((name :accessor name :initarg :name
        :initform nil)
   (title :accessor title :initarg :title
          :initform nil)))

(defmethod object-string ((recent-buffer recent-buffer))
  (format nil "~a  ~a" (name recent-buffer) (title recent-buffer)))

(defun recent-buffer-completion-fn ()
  (let ((buffers (ring-recent-list (recent-buffers *interface*))))
    (lambda (input)
      (fuzzy-match input buffers))))

(define-command reopen-buffer ()
  "Delete the buffer via minibuffer input."
  (with-result (buffer (read-from-minibuffer
                        (minibuffer *interface*)
                        :input-prompt "Reopen buffer:"
                        :completion-function (recent-buffer-completion-fn)))
    (make-buffers (list (name buffer)))))

(define-command undo-buffer-deletion ()
  "Go to the first url in the list of killed buffers."
  (if (> (item-count (recent-buffers *interface*)) 0)
      (make-buffers (list (name (ring-pop-most-recent (recent-buffers *interface*)))))))

@export
(defun make-recent-buffer (name title)
  (make-instance 'recent-buffer :name name :title title))
