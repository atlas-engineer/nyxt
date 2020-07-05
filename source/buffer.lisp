(in-package :nyxt)

(defclass-export buffer-description ()
  ((url :accessor url :initarg :url
        :initform "" :type string)
   (title :accessor title :initarg :title
          :initform "" :type string)))

(defmethod object-string ((buffer-description buffer-description))
  (url buffer-description))

(defmethod object-display ((buffer-description buffer-description))
  (format nil "~a  ~a"
          (title buffer-description)
          (quri:url-decode (url buffer-description))))

(export-always 'equals)
(defmethod equals ((bd1 buffer-description) (bd2 buffer-description))
  "Comparison function for buffer history entries.
An entry is uniquely identified from its URL.  We do not take the
title into accound as it may vary from one load to the next."
  (string= (url bd1) (url bd2)))

(defmethod object-string ((buffer buffer))
  (object-string (url buffer)))

(defmethod object-display ((buffer buffer))
  (format nil "~a  ~a" (title buffer) (object-display (url buffer))))

(define-command make-buffer (&key (title "") modes (url ""))
  "Create a new buffer.
MODES is a list of mode symbols.
If URL is `:default', use `default-new-buffer-url'."
  (let* ((buffer (buffer-make *browser* :title title :default-modes modes))
         (url (if (eq url :default)
                  (object-string (default-new-buffer-url buffer))
                  url)))
    (unless (str:emptyp url)
      (set-url* url :buffer buffer))
    buffer))

(define-class-type buffer)
(declaim (type (buffer-type) *buffer-class*))
(export-always '*buffer-class*)
(defvar *buffer-class* 'buffer)

(declaim (ftype (function (browser &key (:title string) (:default-modes list) (:dead-buffer buffer))) buffer-make))
(defun buffer-make (browser &key title default-modes dead-buffer)
  "Make buffer with title TITLE and modes DEFAULT-MODES.
Run `*browser*'s `buffer-make-hook' over the created buffer before returning it.
If DEAD-BUFFER is a dead buffer, recreate its web view and give it a new ID."
  (let* ((buffer (if dead-buffer
                     (ffi-buffer-make dead-buffer)
                     (apply #'make-instance *buffer-class*
                            (append (when title `(:title ,title))
                                    (when default-modes `(:default-modes ,default-modes)))))))
    (setf (id buffer) (get-unique-buffer-identifier *browser*))
    (hooks:run-hook (buffer-before-make-hook *browser*) buffer)
    ;; Modes might require that buffer exists, so we need to initialize them
    ;; after the view has been created.
    (initialize-modes buffer)
    (if dead-buffer
        (progn
          (setf (url buffer) (url dead-buffer))
          (setf (slot-value buffer 'load-status) :unloaded))
        (setf (slot-value buffer 'load-status) :void))
    (when (expand-path (cookies-path buffer))
      (ensure-parent-exists (expand-path (cookies-path buffer))))
    (setf (gethash (id buffer) (buffers browser)) buffer)
    (unless (last-active-buffer browser)
      ;; When starting from a REPL, it's possible that the window is spawned in
      ;; the background and current-buffer would then return nil.
      (setf (last-active-buffer browser) buffer))
    ;; Run hooks before `initialize-modes' to allow for last-minute modification
    ;; of the default modes.
    (hooks:run-hook (buffer-make-hook browser) buffer)
    buffer))

(declaim (ftype (function (buffer)) buffer-delete))
(defun buffer-delete (buffer)
  (hooks:run-hook (buffer-delete-hook buffer) buffer)
  (let ((parent-window (find buffer (window-list) :key 'active-buffer)))
    (when parent-window
      (let ((replacement-buffer (or (first (get-inactive-buffers))
                                    (buffer-make *browser*))))
        (window-set-active-buffer parent-window
                                  replacement-buffer)))
    (ffi-buffer-delete buffer)
    (remhash (id buffer) (buffers *browser*))
    (setf (id buffer) "")
    (add-to-recent-buffers buffer)
    (match (session-store-function *browser*)
      ((guard f f)
       (funcall-safely f)))))

(export-always 'buffer-list)
(defun buffer-list (&key sort-by-time domain)
  (let* ((buffer-list (alex:hash-table-values (buffers *browser*)))
         (buffer-list (if sort-by-time (sort
                                        buffer-list #'local-time:timestamp> :key #'last-access)
                          buffer-list))
         (buffer-list (if domain (remove-if-not
                                  (lambda (i) (equal domain (quri:uri-domain (url i)))) buffer-list)
                          buffer-list)))
    buffer-list))

(export-always 'window-list)
(defun window-list ()
  (alex:hash-table-values (windows *browser*)))

(export-always 'buffer-suggestion-filter)
(defun buffer-suggestion-filter (&key current-is-last-p domain)
  (let ((buffers (buffer-list :sort-by-time t :domain domain)))
    (when current-is-last-p
      (setf buffers (alex:rotate buffers -1)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) buffers))))

(define-command copy-url ()
  "Save current URL to clipboard."
  (copy-to-clipboard (object-string (url (current-buffer))))
  (echo "~a copied to clipboard." (object-string (url (current-buffer)))))

(define-command copy-title ()
  "Save current page title to clipboard."
  (copy-to-clipboard (title (current-buffer)))
  (echo "~a copied to clipboard." (title (current-buffer))))

(define-command switch-buffer ()
  "Switch the active buffer in the current window."
  (with-result (buffer (read-from-minibuffer
                        (make-minibuffer
                         :input-prompt "Switch to buffer"
                         ;; For commodity, the current buffer shouldn't be the first one on the list.
                         :suggestion-function (buffer-suggestion-filter :current-is-last-p t))))
    (set-current-buffer buffer)))

(define-command switch-buffer-domain (&optional (buffer (current-buffer)))
  "Switch the active buffer in the current window from the current domain."
  (let ((domain (quri:uri-domain (url buffer))))
    (with-result (buffer (read-from-minibuffer
                          (make-minibuffer
                           :input-prompt "Switch to buffer in current domain:"
                           :suggestion-function (buffer-suggestion-filter
                                                 :domain domain
                                                 :current-is-last-p t))))
      (set-current-buffer buffer))))

(define-command make-buffer-focus (&key (url :default))
  "Switch to a new buffer.
See `make-buffer'."
  (let ((buffer (make-buffer :url url)))
    (set-current-buffer buffer)
    buffer))

(define-command delete-buffer ()
  "Delete the buffer(s) via minibuffer input."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Delete buffer(s)"
                          :multi-selection-p t
                          :suggestion-function (buffer-suggestion-filter))))
    (mapcar #'buffer-delete buffers)))

(defun delete-buffers ()
  "Delete all buffers."
  (mapcar #'buffer-delete (buffer-list)))

(define-command delete-all-buffers ()
  "Delete all buffers, with confirmation."
  (let ((count (hash-table-count (buffers *browser*))))
    (with-confirm ("Are you sure to delete ~a buffer~p?" count count)
      (delete-buffers))))

(define-command delete-current-buffer (&optional (buffer (current-buffer)))
  "Delete the currently active buffer, and make the next buffer the
visible buffer. If no other buffers exist, set the url of the current
buffer to the start page."
  (buffer-delete buffer))

(define-command delete-other-buffers (&optional (buffer (current-buffer)))
  "Delete all other buffers but `buffer` which if not explicitly set defaults
to the currently active buffer."
  (let* ((all-buffers (buffer-list))
         (buffers-to-delete (remove buffer all-buffers))
         (count (list-length buffers-to-delete)))
    (with-confirm ("Are you sure to delete ~a buffer~p?" count count)
      (mapcar #'buffer-delete buffers-to-delete))))

;; WARNING: Don't use this parenscript, use the TITLE buffer slot instead.
(define-parenscript %%buffer-get-title () ; TODO: `on-signal-load-finished' should
                                          ; pass the title so that we don't have
                                          ; to call this.
  (ps:chain document title))

(export-always 'set-url*)
(declaim (ftype (function (string &key (:buffer buffer) (:raw-url-p boolean)) t) set-url*))
(defun set-url* (input-url &key (buffer (current-buffer)) raw-url-p)
  "Load INPUT-URL in BUFFER.
URL is first transformed by `parse-url', then by BUFFER's `set-url-hook'."
  (let* ((url (if raw-url-p
                  (quri:uri input-url)
                  (parse-url input-url))))
    (handler-case
        (progn
          (let ((new-url (hooks:run-hook (slot-value buffer 'set-url-hook) url)))
            (check-type new-url quri:uri)
            (setf url new-url)))
      (error (c)
        (log:error "In `set-url-hook': ~a" c)))
    (ffi-buffer-load buffer url)))

(defun search-engine-suggestion-filter (minibuffer)
  (with-slots (input-buffer) minibuffer
    (let* ((matched-engines
             (remove-if-not
              (lambda (engine)
                (str:starts-with-p (text-buffer::string-representation input-buffer)
                                   (shortcut engine)
                                   :ignore-case t))
              (search-engines *browser*)))
           (fuzzy-matched-engines
            (fuzzy-match (input-buffer minibuffer)
                         (set-difference (search-engines *browser*) matched-engines))))
      (append matched-engines fuzzy-matched-engines))))

(define-command insert-suggestion-or-search-engine (&optional (minibuffer (current-minibuffer)))
  "Paste selected suggestion or search engine to input.
If minibuffer input is not empty and the selection is on first position,
complete against a search engine."
  (cond
    ;; Complete a search engine name.
    ((and (not (str:emptyp (input-buffer minibuffer)))
          (zerop (suggestion-cursor minibuffer)))
     (let* ((engines (search-engines *browser*))
            (matching-engines
              (remove-if (complement (alex:curry #'str:starts-with-p (input-buffer minibuffer)))
                         engines
                         :key #'shortcut)))
       (match (length matching-engines)
         (1
          (nyxt/minibuffer-mode:kill-whole-line minibuffer)
          (insert minibuffer (str:concat (shortcut (first matching-engines)) " ")))
         (match-count
          (with-result (engine (read-from-minibuffer
                                (make-minibuffer
                                 :input-prompt "Search engine"
                                 :input-buffer (if (zerop match-count) "" (input-buffer minibuffer))
                                 :suggestion-function #'search-engine-suggestion-filter)))
            (when engine
              (nyxt/minibuffer-mode:kill-whole-line minibuffer)
              (insert minibuffer (str:concat (shortcut engine) " "))))))))
    (t
     (nyxt/minibuffer-mode:insert-suggestion minibuffer))))

(define-mode set-url-mode (nyxt/minibuffer-mode:minibuffer-mode)
  "Minibuffer mode for setting the URL of a buffer."
  ((keymap-scheme
    :initform
    (define-scheme "set-url"
      scheme:cua
      (list
       "tab" 'insert-suggestion-or-search-engine)))))

(define-command set-url (&key new-buffer-p prefill-current-url-p)
  "Set the URL for the current buffer, completing with history."
  (let ((history (minibuffer-set-url-history *browser*)))
    (when history
      (containers:insert-item history (url (current-buffer))))
    (with-result (url (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt (format nil "Open URL in ~A buffer"
                                              (if new-buffer-p
                                                  "new"
                                                  "current"))
                        :input-buffer (if prefill-current-url-p
                                          (object-string (url (current-buffer))) "")
                        :default-modes '(set-url-mode minibuffer-mode)
                        :suggestion-function (history-suggestion-filter
                                              :prefix-urls (list (object-string
                                                                  (url (current-buffer)))))
                        :history history
                        :must-match-p nil)))
      (when (typep url 'history-entry)
        ;; In case read-from-minibuffer returned a string upon
        ;; must-match-p.
        (setf url (url url)))
      (set-url* (object-string url) :buffer (if new-buffer-p
                                                (make-buffer-focus :url nil)
                                                (current-buffer))))))

(define-command set-url-from-current-url ()
  "Set the URL for the current buffer, pre-filling in the current URL."
  (set-url :prefill-current-url-p t))

(define-command set-url-new-buffer ()
  "Prompt for a URL and set it in a new focused buffer."
  (set-url :new-buffer-p t))

(define-command reload-current-buffer (&optional (buffer (current-buffer)))
  "Reload of BUFFER or current buffer if unspecified."
  (set-url* (object-string (url buffer)) :buffer buffer))

(define-command reload-buffer ()
  "Reload queried buffer(s)."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Reload buffer(s)"
                          :multi-selection-p t
                          :suggestion-function (buffer-suggestion-filter))))
    (mapcar #'reload-current-buffer buffers)))

(define-command switch-buffer-previous ()
  "Switch to the previous buffer in the list of buffers.
That is to say, the one with the most recent access time after the current buffer.
The current buffer access time is set to be the last."
  (let* ((buffers (buffer-list :sort-by-time t))
         (last-buffer (alex:last-elt buffers)))
    (when (second buffers)
      (setf (last-access (current-buffer))
            (local-time:timestamp- (last-access last-buffer) 1 :sec))
      (set-current-buffer (second buffers)))))

(define-command switch-buffer-next ()   ; TODO: Rename switch-buffer-oldest
  "Switch to the oldest buffer in the list of buffers."
  (set-current-buffer (alex:last-elt (buffer-list :sort-by-time t))))

(defun active-mode-suggestion-filter (buffers)
  "Return the union of the active modes in BUFFERS."
  (let ((modes (delete-duplicates (mapcar (lambda (m)
                                            (class-name (class-of m)))
                                          (apply #'append (mapcar #'modes buffers))))))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) modes))))

(defun inactive-mode-suggestion-filter (buffers)
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
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) (set-difference all-non-minibuffer-modes common-modes)))))

(define-command disable-mode-for-current-buffer (&key (buffers (list (current-buffer))))
  "Disable queried mode(s)."
  (with-result (modes (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Disable mode(s)"
                        :multi-selection-p t
                        :suggestion-function (active-mode-suggestion-filter buffers))))
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
                          :suggestion-function (buffer-suggestion-filter))))
    (disable-mode-for-current-buffer :buffers buffers)))

(define-command enable-mode-for-current-buffer (&key (buffers (list (current-buffer))))
  "Enable queried mode(s)."
  (with-result (modes (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Enable mode(s)"
                        :multi-selection-p t
                        :suggestion-function (inactive-mode-suggestion-filter buffers))))
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
                          :suggestion-function (buffer-suggestion-filter))))
    (enable-mode-for-current-buffer :buffers buffers)))

(define-command open-inspector ()
  "Open the inspector, a graphical tool to inspect and change the content of the buffer."
  (ffi-inspector-show (current-buffer)))
