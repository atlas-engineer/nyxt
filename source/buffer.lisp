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
  (url buffer-description))

(defmethod object-display ((buffer-description buffer-description))
  (format nil "~a  ~a" (url buffer-description) (title buffer-description)))

(defmethod equals ((bd1 buffer-description) (bd2 buffer-description))
  "Comparison function for buffer history entries.
An entry is uniquely identified from its URL.  Not that we should not take the
title into accound as it may vary from one load to the next."
  (string= (url bd1) (url bd2)))

(defmethod object-string ((buffer buffer))
  (url buffer))

(defmethod object-display ((buffer buffer))
  (format nil "~a  ~a" (url buffer) (title buffer)))

(define-command make-buffer (&key (title "default")
                             modes
                             url)
  "Create a new buffer.
MODES is a list of mode symbols.
If URL is `:default', use `default-new-buffer-url'."
  (let* ((buffer (ipc-buffer-make *interface* :title title :default-modes modes))
         (url (if (eq url :default)
                  (default-new-buffer-url buffer)
                  url)))
    (when url
      (set-url url :buffer buffer))
    buffer))

(define-deprecated-command new-buffer ()
  "Deprecated by `make-buffer'."
  (make-buffer))

@export
(defun buffer-list (&key sort-by-time)
  (let ((buf-list (alexandria:hash-table-values (buffers *interface*))))
    (if sort-by-time
        (sort buf-list
              #'local-time:timestamp>
              :key #'last-access)
        buf-list)))

(defun buffer-completion-filter (&key current-is-last-p)
  (let ((buffers (buffer-list :sort-by-time t)))
    (when current-is-last-p
      (setf buffers (alexandria:rotate buffers -1)))
    (lambda (input)
      (fuzzy-match input buffers))))

(define-command switch-buffer ()
  "Switch the active buffer in the current window."
  (with-result (buffer (read-from-minibuffer
                        (make-minibuffer
                         :input-prompt "Switch to buffer"
                         ;; For commodity, the current buffer shouldn't be the first one on the list.
                         :completion-function (buffer-completion-filter :current-is-last-p t))))
    (set-current-buffer buffer)))

(define-command make-buffer-focus (&key (url :default))
  "Switch to a new buffer.
See `make-buffer'."
  (let ((buffer (make-buffer :url url)))
    (set-current-buffer buffer)
    buffer))

(define-deprecated-command make-visible-new-buffer ()
  "Deprecated by `make-buffer-focus'."
  (make-buffer-focus))

(define-command delete-buffer ()
  "Delete the buffer(s) via minibuffer input."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Delete buffer(s)"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (mapcar #'rpc-buffer-delete buffers)))

(defun delete-buffers ()
  "Delete all buffers."
  (mapcar #'rpc-buffer-delete (buffer-list)))

(define-command delete-all-buffers ()
  "Delete all buffers, with confirmation."
  (let ((count (hash-table-count (buffers *interface*))))
    (with-confirm ("Are you sure to delete ~a buffer~p?" count count)
      (delete-buffers))))

(define-command delete-current-buffer (&optional (buffer (current-buffer)))
  "Delete the currently active buffer, and make the next buffer the
visible buffer. If no other buffers exist, set the url of the current
buffer to the start page."
  (rpc-buffer-delete buffer))

(define-command delete-other-buffers (&optional (buffer (current-buffer)))
  "Delete all other buffers but `buffer` which if not explicitly set defaults
to the currently active buffer."
  (let* ((all-buffers (buffer-list))
         (buffers-to-delete (remove buffer all-buffers))
         (count (list-length buffers-to-delete)))
    (with-confirm ("Are you sure to delete ~a buffer~p?" count count)
      (mapcar #'rpc-buffer-delete buffers-to-delete))))

;; WARNING: Don't use this parenscript, use the TITLE buffer slot instead.
@export
(define-parenscript %%buffer-get-title () ; TODO: `did-commit-navigation' should
                                          ; pass the title so that we don't have
                                          ; to call this.
  (ps:chain document title))

@export
(defun set-url (input-url &key (buffer (current-buffer)) raw-url-p)
  "Load INPUT-URL in BUFFER.
URL is first transformed by `parse-url', then by BUFFER's `load-hook'."
  (let* ((url (if raw-url-p
                  input-url
                  (parse-url input-url))))
    (handler-case
        (progn
          (let ((new-url (next-hooks:run-hook (load-hook buffer) url)))
            (check-type new-url string)
            (setf url new-url)))
      (error (c)
        (log:error "In `load-hook': ~a" c)))
    (setf (url buffer) url)
    (ipc-buffer-load buffer url)))

(define-command insert-candidate-or-search-engine (&optional (minibuffer (current-minibuffer)))
  "Paste clipboard text or to input.
If minibuffer input is not empty and the selection is on first position,
complete against a search engine."
  (let ((candidate (get-candidate minibuffer)))
    (cond
      ;; Complete a search engine name.
      ((and (not (str:emptyp (input-buffer minibuffer)))
            (zerop (completion-cursor minibuffer)))
       (let ((name (search-engine-starting-with candidate)))
         (when name
           (kill-whole-line minibuffer)
           (insert (str:concat name " ")))))
      (t
       (when candidate
         (kill-whole-line minibuffer)
         (insert candidate minibuffer))))))

(define-mode set-url-mode (minibuffer-mode)
  "Minibuffer mode for setting the URL of a buffer."
  ((keymap-schemes
    :initform
    (let ((map (make-keymap)))
      (define-key :keymap map
        "TAB" #'insert-candidate-or-search-engine)
      (list :emacs map
            :vi-normal map)))))

(define-command set-url-current-buffer (&key new-buffer-p)
  "Set the URL for the current buffer, completing with history."
  (let ((history (minibuffer-set-url-history *interface*)))
    (when history
      (ring:insert history (url (current-buffer))))
    (with-result (url (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt (format nil "Open URL in ~A buffer"
                                              (if new-buffer-p
                                                  "new"
                                                  "current"))
                        :default-modes '(set-url-mode minibuffer-mode)
                        :completion-function (history-completion-filter)
                        :history history
                        :empty-complete-immediate t)))
      (when (typep url 'history-entry)
        ;; In case read-from-minibuffer returned a string upon
        ;; empty-complete-immediate.
        (setf url (url url)))
      (set-url url :buffer (if new-buffer-p
                               (make-buffer-focus :url nil)
                               (current-buffer))))))

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
                          :input-prompt "Reload buffer(s)"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (mapcar #'reload-current-buffer buffers)))

(define-command switch-buffer-previous ()
  "Switch to the previous buffer in the list of buffers.
That is to say, the one with the most recent access time after the current buffer.
The current buffer access time is set to be the last."
  (let* ((buffers (buffer-list :sort-by-time t))
         (last-buffer (alexandria:last-elt buffers)))
    (setf (last-access (current-buffer))
          (local-time:timestamp- (last-access last-buffer) 1 :sec))
    (set-current-buffer (second buffers))))

(define-command switch-buffer-next ()   ; TODO: Rename switch-buffer-oldest
  "Switch to the oldest buffer in the list of buffers."
  (set-current-buffer (alexandria:last-elt (buffer-list :sort-by-time t))))

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
                        :input-prompt "Disable mode(s)"
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
                          :input-prompt "Disable mode(s) for buffer(s)"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (disable-mode-for-current-buffer :buffers buffers)))

(define-command enable-mode-for-current-buffer (&key (buffers (list (current-buffer))))
  "Enable queried mode(s)."
  (with-result (modes (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Enable mode(s)"
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
                          :input-prompt "Enable mode(s) for buffer(s)"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (enable-mode-for-current-buffer :buffers buffers)))
