;;; document-mode.lisp --- document major mode for internet documents

(in-package :next)

(defvar *document-mode-map* (make-hash-table :test 'equalp))

(defclass document-mode (mode)
  ((active-history-node :accessor active-history-node :initarg :active-node)
   (link-hints :accessor link-hints)))

(define-parenstatic scroll-to-top
    (ps:chain window (scroll-by 0 (- (ps:chain document body scroll-height)))))

(define-parenstatic scroll-to-bottom
    (ps:chain window (scroll-by 0 (ps:chain document body scroll-height))))

(define-parenstatic scroll-down
    (ps:chain window (scroll-by 0 (ps:lisp *scroll-distance*))))

(define-parenstatic scroll-up
    (ps:chain window (scroll-by 0 (ps:lisp (- *scroll-distance*)))))

(define-parenstatic scroll-left
    (ps:chain window (scroll-by (ps:lisp (- *horizontal-scroll-distance*)) 0)))

(define-parenstatic scroll-right
    (ps:chain window (scroll-by (ps:lisp *horizontal-scroll-distance*) 0)))

(defun ensure-zoom-ratio-range (zoom)
  (let ((ratio (funcall zoom *current-zoom-ratio* *zoom-ratio-step*)))
    (setf ratio (max ratio *zoom-ratio-min*))
    (setf ratio (min ratio *zoom-ratio-max*))
    (setf *current-zoom-ratio* ratio)))

(defparen %zoom-in-page ()
  (ps:lisp (ensure-zoom-ratio-range #'+))
  (ps:let ((style (ps:chain document body style)))
    (setf (ps:@ style zoom) (ps:lisp *current-zoom-ratio*))))

(define-command zoom-in-page ()
  "Zoom in the current page."
  (buffer-execute-javascript *interface* (view *active-buffer*) (%zoom-in-page)))

(defparen %zoom-out-page ()
  (ps:lisp (ensure-zoom-ratio-range #'-))
  (ps:let ((style (ps:chain document body style)))
    (setf (ps:@ style zoom) (ps:lisp *current-zoom-ratio*))))

(define-command zoom-out-page ()
  "Zoom out the current page."
  (buffer-execute-javascript *interface* (view *active-buffer*) (%zoom-out-page)))

(defparen %unzoom-page ()
  (ps:lisp (setf *current-zoom-ratio* *zoom-ratio-default*))
  (setf (ps:chain document body style zoom) (ps:lisp *zoom-ratio-default*)))

(define-command unzoom-page ()
  "Unzoom the page."
  (buffer-execute-javascript *interface* (view *active-buffer*) (%unzoom-page)))

(define-command history-backwards ()
  "Move up to parent node to iterate backwards in history tree."
  (let ((parent (node-parent (active-history-node (mode *active-buffer*)))))
    (when parent
      (set-url (node-data parent) t))))

(define-command history-forwards ()
  "Move forwards in history selecting the first child."
  (let ((children (node-children (active-history-node (mode *active-buffer*)))))
    (unless (null children)
      (set-url (node-data (nth 0 children)) t))))

(defun history-fowards-query-complete (input)
  ;; provide completion candidates to the history-forwards-query function
  (let ((children
	  ;; Find children of active document-mode instance
	  (node-children (active-history-node
			  ;; Find active document-mode instance from minibuffer callback
			  (mode (callback-buffer (mode *minibuffer*)))))))
    (when children
      (fuzzy-match input (mapcar #'node-data children)))))

(define-command history-forwards-query ()
  "Move forwards in history querying if more than one child present."
  (with-result (input (read-from-minibuffer
                       *minibuffer*
                       :completion-function 'history-fowards-query-complete))
    (let ((children (node-children (active-history-node (mode *active-buffer*)))))
      (loop for child in children do
        (when (equalp (node-data child) input)
          (set-url (node-data child) t))))))

(defun add-or-traverse-history (mode)
  (with-result (url (buffer-get-url))
    (let ((active-node (active-history-node mode)))
      ;; only add element to the history if it is different than the current
      (when (equalp url (node-data active-node))
        (return-from add-or-traverse-history t))
      ;; check if parent exists
      (when (node-parent active-node)
        ;; check if parent node's url is equal
        (when (equalp url (node-data (node-parent active-node)))
    	  ;; set active-node to parent
    	  (setf (active-history-node mode) (node-parent active-node))
    	  (return-from add-or-traverse-history t)))
      ;; loop through children to make sure node does not exist in children
      (loop for child in (node-children active-node) do
        (when (equalp (node-data child) url)
    	  (setf (active-history-node mode) child)
    	  (return-from add-or-traverse-history t)))
      ;; if we made it this far, we must create a new node
      (when url
        (history-add url)) ; add to history database
      (let ((new-node (make-node :parent active-node :data url)))
        (push new-node (node-children active-node))
        (setf (active-history-node mode) new-node)
        (return-from add-or-traverse-history t)))))

(define-command set-url-new-buffer ()
  "Prompt the user for a url and set it in a new active / visible
buffer"
  (with-result (input-url (read-from-minibuffer
                           *minibuffer*
                           :completion-function 'history-typed-complete
                           :empty-complete-immediate t))
    (let ((new-buffer (generate-new-buffer "default" (document-mode))))
      (set-visible-active-buffer new-buffer)
      (set-url input-url))))

(defun set-url-buffer (input-url buffer &optional disable-history)
  (setf (name buffer) input-url)
  (unless disable-history
    (history-typed-add input-url))
  (web-view-set-url *interface* (view buffer) input-url))

(defun %set-url-new-buffer (input-url &optional disable-history)
  (let ((new-buffer (generate-new-buffer "default" (document-mode))))
    (set-visible-active-buffer new-buffer)
    (set-url-buffer input-url new-buffer disable-history)))

(defun setup-url ()
  (with-result (url (buffer-get-url))
    (set-input *minibuffer* url)))

(defun set-url (input-url &optional disable-history)
  (let ((url (parse-url input-url)))
    (set-url-buffer url *active-buffer* disable-history)))

(define-command set-url-current-buffer ()
  "Set the url for the current buffer, completing with history."
  (with-result (url (read-from-minibuffer
                     *minibuffer*
                     :completion-function 'history-typed-complete
                     :setup-function 'setup-url
                     :empty-complete-immediate t))
    (set-url url)))

(define-command set-url-from-bookmark ()
  "Set the url for the current buffer from a bookmark."
  (with-result (url (read-from-minibuffer
                     *minibuffer*
                     :completion-function 'bookmark-complete))
    (set-url url)))

(defun setup-anchor ()
  (erase-document (mode *minibuffer*))
  (let ((current-mode (mode *active-buffer*)))
    (with-parenscript (link-hints add-link-hints)
      (setf (link-hints current-mode) link-hints))))

(define-command go-anchor ()
  "Show a set of link hints, and go to the user inputted one in the
currently active buffer."
  (with-result (input (read-from-minibuffer
                       *minibuffer*
                       :setup-function 'setup-anchor
                       :cleanup-function 'remove-link-hints))
    (loop for hint in (link-hints (mode *active-buffer*))
          do (when (equalp (nth 0 hint) input)
               (set-url-buffer (nth 1 hint) *active-buffer* t)))))

(define-command go-anchor-new-buffer ()
  "Show a set of link hints, and open the user inputted one in a new
buffer (not set to visible active buffer)."
  (with-result (input (read-from-minibuffer
                       *minibuffer*
                       :setup-function 'setup-anchor
                       :cleanup-function 'remove-link-hints))
    (let ((new-buffer (generate-new-buffer "default" (document-mode))))
      (loop for hint in (link-hints (mode *active-buffer*))
            do (when (equalp (nth 0 hint) input)
                 (set-url-buffer (nth 1 hint) new-buffer t))))))

(define-command go-anchor-new-buffer-focus ()
  "Show a set of link hints, and open the user inputted one in a new
visible active buffer."
  (with-result (input (read-from-minibuffer
                       *minibuffer*
                       :setup-function 'setup-anchor))
    (let ((new-buffer (generate-new-buffer "default" (document-mode))))
      (loop for hint in (link-hints (mode *active-buffer*))
            do (when (equalp (nth 0 hint) input)
                 (set-url-buffer (nth 1 hint) new-buffer t)))
      (remove-link-hints)
      (set-visible-active-buffer new-buffer))))

(defun document-mode ()
  "Base mode for interacting with documents"
  (let* ((root (make-node :data "about:blank"))
	 (mode (make-instance 'document-mode
			      :name "Document-Mode"
			      :keymap *document-mode-map*
			      :active-node root)))
    mode))

(defmethod setup ((mode document-mode) buffer)
  (call-next-method)
  (web-view-set-url-loaded-callback *interface*
                                    (view buffer)
                                    (lambda () (add-or-traverse-history mode))))
