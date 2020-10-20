;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;;;; TODO: Remove?

(defun web-buffers ()
  "Return list of web buffers.
I.e. non-special buffers, those with a non-empty URL slot."
  (delete-if (alex:compose #'url-empty-p #'url)
             (buffer-list)))

(defun session-data (&optional (buffer (current-buffer)))
  "Return the data that needs to be serialized.
This data can be used to restore the session later, e.g. when starting a new
instance of Nyxt."
  ;; TODO: What should the session root be?  A hash-table?  A plist?
  ;; For now, it's just a simple list where the first element is the version.
  ;; Maybe not future-proof, but it's unclear that session structure will ever
  ;; change much.
  (list +version+ (get-data (history-path buffer))))

(defmethod store ((profile data-profile) (path session-data-path))
  "Store the current Nyxt session to the BUFFER's `session-path'.
Currently we store the list of current URLs of all buffers."
  ;; TODO: Should we persist keymaps, constructors, etc.?  For instance, should
  ;; we restore the proxy value?  It may be wiser to let the user configure
  ;; allow/deny lists instead.  It's also easier
  (with-data-file (file path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    (log:info "Saving session to ~s." (expand-path path))
    ;; We READ the output of serialize-sexp to make it more human-readable.
    (let ((*package* (find-package :nyxt)))
      ;; We need to make sure current package is :nyxt so that
      ;; symbols are printed with consistent namespaces.
      (format file
              "~s"
              (with-input-from-string (in (with-output-to-string (out)
                                            (s-serialization:serialize-sexp (session-data) out)))
                (read in))))))

(defun make-buffer-from-history (history)
  (let* ((buffer (make-buffer))
         (mode (find-submode buffer 'web-mode))) ; TODO: This assumes BUFFER has web-mode.  What if it doesn't?
    (setf (url buffer)
          (ensure-url (url (htree:data (htree:current history)))))
    (setf (title buffer)
          (title (htree:data (htree:current history))))
    (setf (slot-value buffer 'load-status) :unloaded)
    (setf (nyxt/web-mode:history mode) history)))

(defmethod restore ((profile data-profile) (path session-data-path))
  "Restore the current Nyxt session from the BUFFER's `session-path'."
  (handler-case
      (match (with-data-file (input path
                                    :direction :input
                                    :if-does-not-exist nil)
               (when input
                 ;; We need to make sure current package is :nyxt so that
                 ;; symbols are printed with consistent namespaces.
                 (let ((*package* (find-package :nyxt)))
                   (s-serialization:deserialize-sexp input))))
        ((list version history-tree)
         (let* ((buffer-histories (buffer-local-histories-table history-tree))
                (most-recent-nodes (maphash (alex:rcurry #'sort :test #'local-time:timestamp>
                                                                :key (alex:compose #'last-access
                                                                                   #'htree:data))
                                            buffer-histories)))
           (unless (string= version +version+)
             (log:warn "Session version ~s differs from current version ~s." version +version+))
           (echo "Loading session of ~a URLs from ~s."
                 (hash-table-size buffer-histories)
                 (expand-path path))
           (when history-tree
             (log:info "Restoring ~a."
                       (mapcar (alex:compose #'object-string #'htree:data #'htree:current)
                               history-tree))
             ;; Make the new ones.
             ;; TODO: Replace the loop with a function `make-buffer-from-history' in web-mode?
             (loop for node in most-recent-nodes
                   for buffer = (make-buffer)
                   do (setf (url buffer) (ensure-url (url (htree:data node)))
                            (title buffer) (title (htree:data node))
                            (slot-value buffer 'load-status) :unloaded)
                   do (dolist (history-node (gethash (id (htree:data node)) buffer-histories))
                        (setf (id (htree:data history-node)) (id buffer)))
                      ;; TODO: This (setf ... (history-path ...)) looks wrong.
                   finally (setf (get-data (history-path (current-buffer))) history-tree)))
           ;; TODO: Switch to the last active buffer.  We probably need to serialize *browser*.
           ;; Or else we could include `access-time' in the buffer class.
           )))
    (error (c)
      (log:warn "Failed to restore session from ~a: ~a" (expand-path path) c))))

(defun session-list (&optional (buffer (current-buffer)))
  (mapcar #'pathname-name
          (uiop:directory-files
           (uiop:pathname-directory-pathname
            (expand-path (session-path buffer))))))

(defun session-name-suggestion-filter (minibuffer)
  (fuzzy-match (input-buffer minibuffer) (session-list)))

(define-command store-session-by-name ()
  "Store the session data (i.e. all the opened buffers) in the file named by user input."
  (let ((name (prompt-minibuffer
               :input-prompt "The name to store session with"
               :history (minibuffer-session-restore-history *browser*)
               :suggestion-function #'session-name-suggestion-filter)))
    (when name
      (store (data-profile (current-buffer))
             (make-instance 'session-data-path
                            :dirname (dirname (session-path (current-buffer)))
                            :basename name)))))

(define-command restore-session-by-name ()
  "Restore the session data from the file named by user input."
  (let ((name (prompt-minibuffer
               :input-prompt "The name of the session to restore"
               :history (minibuffer-session-restore-history *browser*)
               :suggestion-function #'session-name-suggestion-filter)))
    (when name
      (restore (data-profile (current-buffer))
               (make-instance 'session-data-path
                              :basename name
                              :dirname (dirname (session-path (current-buffer))))))))

(define-command replace-session-by-name ()
  "Delete all the buffers of the current session and restore the one chosen by user."
  (let ((name (prompt-minibuffer
               :input-prompt "The name of the session to replace the current one"
               :history (minibuffer-session-restore-history *browser*)
               :suggestion-function #'session-name-suggestion-filter)))
    (when name
      (dolist (buffer (buffer-list))
        (buffer-delete buffer))
      (restore (data-profile (current-buffer))
               (make-instance 'session-data-path
                              :basename name
                              :dirname (dirname (session-path (current-buffer))))))))
