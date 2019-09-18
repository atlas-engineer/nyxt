;;; recent-buffers.lisp --- Manage list of recent buffers

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

;; (defmethod EQUALS ((rb1 recent-buffer) (rb2 recent-buffer))
;;   (and (string= (name rb1) (name rb2))
;;        (string= (title rb1) (title rb2))))

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
    (let ((buffer-index (ring:index-if (recent-buffers *interface*) (lambda (other-buffer)
                                                                     (when other-buffer
                                                                       (and (string= (name buffer) (name other-buffer))
                                                                            (string= (title buffer) (title other-buffer))))))))
    (ring:delete-index (recent-buffers *interface*) buffer-index))
    (make-buffers (list (name buffer)))))

(define-command undo-buffer-deletion ()
  "Open a new buffer and go to the URL of the most recently deleted buffer."
  (when (plusp (ring:item-count (recent-buffers *interface*)))
      (make-buffers (list (name (ring:pop-most-recent (recent-buffers *interface*)))))))

@export
(defun make-recent-buffer (name title)
  (make-instance 'recent-buffer :name name :title title))
