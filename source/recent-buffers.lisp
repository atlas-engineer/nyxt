;;; recent-buffers.lisp --- Manage list of recent buffers.

(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass recent-buffer ()
  ((url :accessor url :initarg :url
         :initform nil :type string)
   (title :accessor title :initarg :title
          :initform nil :type string)))

(defmethod object-string ((recent-buffer recent-buffer))
  (format nil "~a  ~a" (url recent-buffer) (title recent-buffer)))

@export
(defmethod buffer-match-predicate ((buffer recent-buffer))
  (lambda (other-buffer)
    (when other-buffer
      (and (string= (url buffer) (url other-buffer))
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
    (open-urls (list (url buffer)))))

(define-command undo-buffer-deletion ()
  "Open a new buffer with the URL of the most recently deleted buffer."
  (if (plusp (ring:item-count (recent-buffers *interface*)))
      (open-urls (list (url (ring:pop-most-recent (recent-buffers *interface*)))))
      (echo "There are no recently-deleted buffers.")))

(define-key "C-/" #'reopen-buffer)
(define-key :scheme :vi-normal
  "u" #'reopen-buffer)

@export
(defun make-recent-buffer (url title)
  (make-instance 'recent-buffer :url url :title title))
