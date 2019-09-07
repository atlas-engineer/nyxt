;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)
(annot:enable-annot-syntax)

;; TODO: Use standard `print-object' instead?
(defmethod object-string ((buffer buffer))
  (format nil "~a  ~a" (name buffer) (title buffer)))

(defun make-buffer (&key (name "default")
                         default-modes)
  "Create a new buffer.
MODE is a mode symbol.
This function is meant to be used on the Lisp side."
  (rpc-buffer-make *interface* :name name :default-modes default-modes))

(define-command new-buffer (&optional (name "default")
                                      modes)
  ;; TODO: Ask for modes interactively?
  "Create a new buffer.
This command is meant to be used interactively.
See the `make-buffer' function for Lisp code."
  (make-buffer :name name :default-modes modes))

(defun buffer-completion-fn ()
  (let ((buffers (alexandria:hash-table-values (buffers *interface*)))
        (active-buffer (active-buffer *interface*)))
    ;; For commodity, the current buffer shouldn't be the first one on the list.
    (when (equal (first buffers)
                 active-buffer)
      (setf buffers (alexandria:rotate (copy-seq buffers) -1)))
    (lambda (input)
      (fuzzy-match input buffers))))

(define-command switch-buffer ()
  "Switch the active buffer in the current window."
  (with-result (buffer (read-from-minibuffer
                        :input-prompt "Switch to buffer:"
                        :completion-function (buffer-completion-fn)))
    (set-active-buffer *interface* buffer)))

(define-command make-visible-new-buffer ()
  "Make a new empty buffer with the default-new-buffer-url loaded."
  (let ((buffer (make-buffer)))
    (set-active-buffer *interface* buffer)
    (set-url (default-new-buffer-url buffer) :buffer buffer)))

(define-command delete-buffer ()
  "Delete the buffer via minibuffer input."
  (with-result (buffer (read-from-minibuffer
                        :input-prompt "Kill buffer:"
                        :completion-function (buffer-completion-fn)))
    (rpc-buffer-delete *interface* buffer)))

(define-command delete-current-buffer ()
  "Delete the currently active buffer, and make the next buffer the
visible buffer. If no other buffers exist, set the url of the current
buffer to the start page."
  (rpc-buffer-delete *interface* (active-buffer *interface*)))

@export
(define-parenscript buffer-get-url ()
  (ps:chain window location href))

@export
(define-parenscript buffer-get-title ()
  (ps:chain document title))

;; This needs to be a funtion call in Javascript.
(define-parenscript buffer-set-url (url)
  ((lambda () (setf (ps:chain this document location href) (ps:lisp url)))))

@export
(defun set-url (input-url &key (buffer (active-buffer *interface*)) disable-history)
  "Load INPUT-URL in BUFFER.
URL is first transformed by `parse-url', then by BUFFER's `load-hook'.
If DISABLE-HISTORY is non-nil, don't add the resulting URL to history."
  (let ((url (parse-url input-url)))
    (setf (name buffer) url)
    (unless disable-history
      (history-typed-add input-url))
    (setf url (run-composed-hook (load-hook buffer) url))
    (if (str:starts-with-p "file://" url)
        (rpc-buffer-load *interface* buffer url)
        ;; We need to specify the buffer here since we may reach this point
        ;; on initialization before ACTIVE-BUFFER can be used.
        (buffer-set-url :url url :buffer buffer))))

(define-command set-url-current-buffer ()
  "Set the URL for the current buffer, completing with history."
  (with-result (url (read-from-minibuffer
                     :input-prompt "Open URL in buffer:"
                     :completion-function 'history-typed-complete
                     :empty-complete-immediate t))
    (set-url url)))

(define-command reload-current-buffer ()
  "Reload current buffer."
  (with-result (url (buffer-get-url))
    (set-url url :disable-history t)))

(define-command set-url-new-buffer ()
  "Prompt the user for a URL and set it in a new active / visible
buffer"
  (with-result (url (read-from-minibuffer
                     :input-prompt "Open URL in new buffer:"
                     :completion-function 'history-typed-complete
                     :empty-complete-immediate t))
    (let ((buffer (make-buffer)))
      (set-url url :buffer buffer)
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
