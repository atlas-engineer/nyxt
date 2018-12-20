;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)

(defmethod object-string ((buffer buffer))
  (name buffer))

(define-command make-buffer (&optional (name "default")
                                       (mode (funcall *default-new-buffer-mode*)))
  "Create a new buffer."
  (%buffer-make *interface* name mode))

(defun buffer-completion-fn ()
  (let ((buffers (alexandria:hash-table-values (buffers *interface*))))
    (lambda (input)
      (fuzzy-match input buffers :accessor-function #'name))))

(define-command switch-buffer ()
  "Switch the active buffer in the current window."
  (with-result (buffer (read-from-minibuffer
                        *minibuffer*
                        :input-prompt "Switch to buffer:"
                        :completion-function (buffer-completion-fn)))
    (set-active-buffer *interface* buffer)))

(define-command make-visible-new-buffer ()
  "Make a new empty buffer with the *default-new-buffer-url* loaded"
  (let ((buffer (make-buffer)))
    (set-active-buffer *interface* buffer)
    (set-url-buffer *default-new-buffer-url* buffer)))

(define-command delete-buffer ()
  "Delete the buffer via minibuffer input."
  (with-result (buffer (read-from-minibuffer
                        *minibuffer*
                        :input-prompt "Kill buffer:"
                        :completion-function (buffer-completion-fn)))
    (buffer-delete *interface* buffer)))

(define-command delete-active-buffer ()
  "Delete the currently active buffer, and make the next buffer the
visible buffer. If no other buffers exist, set the url of the current
buffer to the start page."
  (buffer-delete *interface* (active-buffer *interface*)))

(define-parenstatic buffer-get-url
    (ps:chain window location href))

(define-parenstatic buffer-get-title
    (ps:chain document title))

(define-parenscript buffer-set-url (url)
  ((lambda () (setf (ps:chain this document location href) (ps:lisp url)))))

(defmethod set-url-buffer (input-url (buffer buffer) &optional disable-history)
  (let ((url (parse-url input-url)))
    (setf (name buffer) url)
    (unless disable-history
      (history-typed-add input-url))
    (buffer-evaluate-javascript *interface*
                                buffer
                                (buffer-set-url url))))

(defun set-url (input-url &optional disable-history)
  (let ((url (parse-url input-url)))
    (set-url-buffer url (active-buffer *interface*) disable-history)))

(define-command set-url-current-buffer ()
  "Set the URL for the current buffer, completing with history."
  (with-result (url (read-from-minibuffer
                     *minibuffer*
                     :input-prompt "Open URL in buffer:"
                     :completion-function 'history-typed-complete
                     :empty-complete-immediate t))
    (set-url url)))

(define-command reload-current-buffer ()
  "Reload current buffer."
  (with-result (url (buffer-get-url))
    (set-url url 'disable-history)))

(define-command set-url-new-buffer ()
  "Prompt the user for a URL and set it in a new active / visible
buffer"
  (with-result (url (read-from-minibuffer
                     *minibuffer*
                     :input-prompt "Open URL in new buffer:"
                     :completion-function 'history-typed-complete
                     :empty-complete-immediate t))
    (let ((buffer (make-buffer)))
      (set-url-buffer url buffer)
      (set-active-buffer *interface* buffer))))

(defmethod get-active-buffer-index ((active-buffer buffer) buffers)
  (position active-buffer buffers :test #'equal))

(define-command switch-buffer-previous ()
  "Switch to the previous buffer in the list of buffers, if the
first item in the list, jump to the last item."
  (let* ((buffers (alexandria:hash-table-values (buffers *interface*)))
         (active-buffer (active-buffer *interface*))
         (active-buffer-index (get-active-buffer-index active-buffer buffers)))
    (if (equalp 0 active-buffer-index)
        (set-active-buffer *interface* (nth (- (length buffers) 1) buffers))
        (set-active-buffer *interface* (nth (- active-buffer-index 1) buffers)))))

(define-command switch-buffer-next ()
  "Switch to the next buffer in the list of buffers, if the last
item in the list, jump to the first item."
  (let* ((buffers (alexandria:hash-table-values (buffers *interface*)))
         (active-buffer (active-buffer *interface*))
         (active-buffer-index (get-active-buffer-index active-buffer buffers)))
    (if (< (+ active-buffer-index 1) (length buffers))
        (set-active-buffer *interface* (nth (+ active-buffer-index 1) buffers))
        (set-active-buffer *interface* (nth 0 buffers)))))

(defmethod add-mode ((buffer buffer) mode &optional (overwrite nil))
  (let ((found-mode (gethash (class-name (class-of mode)) (modes buffer))))
    (when (or (not found-mode) (and found-mode overwrite))
      (setf (buffer mode) buffer)
      (setf (gethash (class-name (class-of mode)) (modes buffer)) mode))))

(defmethod switch-mode ((buffer buffer) mode)
  (let ((found-mode (gethash (class-name (class-of mode)) (modes buffer))))
    (when found-mode
      (setf (mode buffer) found-mode))))

(defmethod add-or-switch-to-mode ((buffer buffer) mode)
  (add-mode buffer mode)
  (switch-mode buffer mode))
