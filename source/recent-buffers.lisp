;;; recent-buffers.lisp --- Manage list of recent buffers.

(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass recent-buffer ()
  ((name :accessor name :initarg :name
        :initform nil :type string)
   (title :accessor title :initarg :title
          :initform nil :type string)))

(defmethod object-string ((recent-buffer recent-buffer))
  (format nil "~a  ~a" (name recent-buffer) (title recent-buffer)))

@export
(defmethod buffer-match-predicate ((buffer recent-buffer))
  (lambda (other-buffer)
    (when other-buffer
      (and (string= (name buffer) (name other-buffer))
           (string= (title buffer) (title other-buffer))))))

(defun recent-buffer-completion-fn ()
  (let ((buffers (ring:recent-list (recent-buffers *interface*))))
    (lambda (input)
      (fuzzy-match input buffers))))

(define-command reopen-buffer ()
  "Reopen a deleted buffer via minibuffer input."
  (with-result (buffer (read-from-minibuffer
                        (make-instance 'minibuffer
                                       :input-prompt "Reopen buffer:"
                                       :completion-function (recent-buffer-completion-fn))))
    (ring:delete-match (recent-buffers *interface*) (buffer-match-predicate buffer))
    (make-buffers (list (name buffer)))))

(define-command undo-buffer-deletion ()
  "Open a new buffer with the URL of the most recently deleted buffer."
  (if (plusp (ring:item-count (recent-buffers *interface*)))
      (make-buffers (list (name (ring:pop-most-recent (recent-buffers *interface*)))))
      (echo "There are no recently-deleted buffers.")))

@export
(defun make-recent-buffer (name title)
  (make-instance 'recent-buffer :name name :title title))
