;;; buffer.lisp --- lisp subroutines for creating / managing buffers

(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass buffer-description ()
  ((url :accessor url :initarg :url
        :initform "" :type string)
   (title :accessor title :initarg :title
          :initform "" :type string)))

(defmethod object-string ((buffer-description buffer-description))
  (format nil "~a  ~a" (url buffer-description) (title buffer-description)))

(defmethod equals ((bd1 buffer-description) (bd2 buffer-description))
  "Comparison function for buffer history entries.
An entry is uniquely identified from its URL.  Not that we should not take the
title into accound as it may vary from one load to the next."
  (string= (url bd1) (url bd2)))

(defmethod object-string ((buffer buffer))
  (format nil "~a  ~a" (url buffer) (title buffer)))

(define-command make-buffer (&key (title "default")
                                  modes)
  "Create a new buffer.
MODES is a list of mode symbols."
  (rpc-buffer-make :title title :default-modes modes))

(define-deprecated-command new-buffer ()
  "Deprecated by `make-buffer'."
  (make-buffer))

(defun buffer-completion-filter (&key current-is-last-p)
  (let ((buffers (alexandria:hash-table-values (buffers *interface*)))
        (active-buffer (current-buffer)))
    (when (and current-is-last-p
               (equal (first buffers)
                      active-buffer))
      ;; TODO: No need for copy-seq since buffers is already a copy, isn't it?
      (setf buffers (alexandria:rotate (copy-seq buffers) -1)))
    (lambda (input)
      (fuzzy-match input buffers))))

(define-command switch-buffer ()
  "Switch the active buffer in the current window."
  (with-result (buffer (read-from-minibuffer
                        (make-minibuffer
                         :input-prompt "Switch to buffer:"
                         ;; For commodity, the current buffer shouldn't be the first one on the list.
                         :completion-function (buffer-completion-filter :current-is-last-p t))))
    (set-current-buffer buffer)))

(define-command make-buffer-focus ()
  "Switch to a new buffer showing default-new-buffer-url."
  (let ((buffer (make-buffer)))
  (set-current-buffer buffer)
    (set-url (default-new-buffer-url buffer) :buffer buffer)))

(define-deprecated-command make-visible-new-buffer ()
  "Deprecated by `make-buffer-focus'."
  (make-buffer-focus))

(define-command delete-buffer ()
  "Delete the buffer(s) via minibuffer input."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Delete buffer(s):"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (mapcar #'rpc-buffer-delete buffers)))

(define-command delete-current-buffer (&optional (buffer (current-buffer)))
  "Delete the currently active buffer, and make the next buffer the
visible buffer. If no other buffers exist, set the url of the current
buffer to the start page."
  (rpc-buffer-delete buffer))

;; WARNING: Don't use this parenscript, use the TITLE buffer slot instead.
@export
(define-parenscript %%buffer-get-title () ; TODO: `did-commit-navigation' should
                                          ; pass the title so that we don't have
                                          ; to call this.
  (ps:chain document title))

@export
(defun set-url (input-url &key (buffer (current-buffer))
                            raw-url-p)
  "Load INPUT-URL in BUFFER.
URL is first transformed by `parse-url', then by BUFFER's `load-hook'."
  (let ((url (if raw-url-p
                 input-url
                 (parse-url input-url))))
    (setf url (next-hooks:run-hook (load-hook buffer) url))
    (setf (url buffer) url)
    (rpc-buffer-load buffer url)))

@export
(defun set-url-to-buffer (input-url &key new-buffer-p)
  "Load INPUT-URL in the current buffer or a new one when NEW-BUFFER-P is t."
  (if new-buffer-p
      (let ((buffer (make-buffer)))
        (set-url input-url :buffer buffer)
        (set-current-buffer buffer))
      (set-url input-url)))

(define-command set-url-current-buffer (&key new-buffer-p)
  "Set the URL for the current buffer, completing with history."
  (let ((history (minibuffer-set-url-history *interface*)))
    (when history
      (ring:insert history (url (current-buffer))))
    (with-result (url (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt (format nil "Open URL in ~A buffer:"
                                              (if new-buffer-p
                                                  "new"
                                                  "current"))
                        :completion-function (history-completion-filter)
                        :history history
                        :empty-complete-immediate t)))
      (when (typep url 'history-entry)
        ;; In case read-from-minibuffer returned a string upon
        ;; empty-complete-immediate.
        (setf url (url url)))
      (set-url-to-buffer url :new-buffer-p new-buffer-p))))

(define-command set-url-new-buffer ()
  "Prompt the user for a URL and set it in a new focused buffer."
  (set-url-current-buffer :new-buffer-p t))

(define-command reload-current-buffer (&optional (buffer (current-buffer)))
  "Reload of BUFFER or current buffer if unspecified."
  (set-url (url buffer) :buffer buffer))

(define-command reload-buffer ()
  "Reload queried buffer(s)."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Reload buffer(s):"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (mapcar #'reload-current-buffer buffers)))

(defmethod get-active-buffer-index ((active-buffer buffer) buffers)
  (position active-buffer buffers :test #'equal))

(define-command switch-buffer-previous ()
  "Switch to the previous buffer in the list of buffers, if the
first item in the list, jump to the last item."
  (let* ((buffers (alexandria:hash-table-values (buffers *interface*)))
         (active-buffer (current-buffer))
         (active-buffer-index (get-active-buffer-index active-buffer buffers)))
    (if (equalp 0 active-buffer-index)
    (set-current-buffer (nth (- (length buffers) 1) buffers))
    (set-current-buffer (nth (- active-buffer-index 1) buffers)))))

(define-command switch-buffer-next ()
  "Switch to the next buffer in the list of buffers, if the last
item in the list, jump to the first item."
  (let* ((buffers (alexandria:hash-table-values (buffers *interface*)))
         (active-buffer (current-buffer))
         (active-buffer-index (get-active-buffer-index active-buffer buffers)))
    (if (< (+ active-buffer-index 1) (length buffers))
    (set-current-buffer (nth (+ active-buffer-index 1) buffers))
    (set-current-buffer (nth 0 buffers)))))

(defun active-mode-completion-filter (buffers)
  "Return the union of the active modes in BUFFERS."
  (let ((modes (delete-duplicates (mapcar (lambda (m)
                                            (class-name (class-of m)))
                                          (apply #'append (mapcar #'modes buffers))))))
    (lambda (input)
      (fuzzy-match input modes))))

(defun inactive-mode-completion-filter (buffers)
  "Return the list of all modes minus those present in all BUFFERS."
  (let ((all-non-minibuffer-modes
         (delete-if (lambda (m)
                      (closer-mop:subclassp (find-class m)
                                            (find-class 'minibuffer-mode)))
                    (mode-list)))
        (common-modes (reduce #'intersection
                              (mapcar (lambda (b)
                                        (mapcar (lambda (m) (class-name (class-of m)))
                                                (modes b)))
                                      buffers))))
    (lambda (input)
      (fuzzy-match input (set-difference all-non-minibuffer-modes common-modes)))))

(define-command disable-mode-for-current-buffer (&key (buffers (list (current-buffer))))
  "Disable queried mode(s)."
  (with-result (modes (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Disable mode(s):"
                        :multi-selection-p t
                        :completion-function (active-mode-completion-filter buffers))))
    (dolist (buffer buffers)
      (dolist (mode modes)
        (funcall (sym (mode-command mode))
                 :buffer buffer :activate nil)))))

(define-command disable-mode-for-buffer ()
  "Disable queried mode(s) for select buffer(s)."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Disable mode(s) for buffer(s):"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (disable-mode-for-current-buffer :buffers buffers)))

(define-command enable-mode-for-current-buffer (&key (buffers (list (current-buffer))))
  "Enable queried mode(s)."
  (with-result (modes (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Enable mode(s):"
                        :multi-selection-p t
                        :completion-function (inactive-mode-completion-filter buffers))))
    (dolist (buffer buffers)
      (dolist (mode modes)
        (funcall (sym (mode-command mode))
                 :buffer buffer :activate t)))))

(define-command enable-mode-for-buffer ()
  "Enable queried mode(s) for select buffer(s)."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Enable mode(s) for buffer(s):"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (enable-mode-for-current-buffer :buffers buffers)))
