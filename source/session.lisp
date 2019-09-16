(in-package :next)

(defun buffer-history (buffer)
  "Return the buffer history of BUFFER."
  (match (find-mode buffer 'document-mode)
    ((guard m m) (next/document-mode:active-history-node m))))

(defun session-data ()
  "Return the data that needs to be serialized.
This data can be used to restore the session later, e.g. when starting a new
instance of Next."
  (delete-if #'null (mapcar #'buffer-history
                            (alexandria:hash-table-values (buffers *interface*)))))

;; TODO: Move session to `remote-interface'?
;; TODO: How can we identify dead buffers?  Maybe with a nil ID?  Or maybe a
;; dead-buffer is just a buffer history.
;; TODO: Use dead buffers for undo.

;; TODO: Include version number in session and warn when not matching.
;; TODO: Make sure URLs are persisted when set from C-l.
(defun store-sexp-session ()
  "Store the current Next session to the last window's `session-path'.
Currently we store the list of current URLs of all buffers."
  ;; TODO: Should we persist keymaps, constructors, etc.?  For instance, should
  ;; we restore the proxy value?  It may be wiser to let the user configure
  ;; whitelitss / blacklists instead.  It's also easier
  (with-open-file (file (session-path (last-active-window *interface*))
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :overwrite)
    (s-serialization:serialize-sexp
     (session-data)
     file)))

(defun restore-sexp-session ()
  "Store the current Next session to the last window `session-path'.
Currently we store the list of current URLs of all buffers."
  (let ((buffer-histories
         (with-open-file (file (session-path (last-active-window *interface*))
                               :direction :input
                               :if-does-not-exist nil)
           (when file
             (s-serialization:deserialize-sexp
              file)))))
    (when buffer-histories
      (log:info "Restoring ~a" (mapcar #'next/document-mode:node-data buffer-histories))
      ;; Delete the old buffers?
      ;; (maphash (lambda (id buffer)
      ;;            (declare (ignore id))
      ;;            (rpc-buffer-delete *interface* buffer))
      ;;          buffers)
      ;; Make the new ones.
      ;; TODO: Add function `make-buffer-from-history' to document-mode?
      (loop for history in buffer-histories
            for buffer = (make-buffer)
            for mode = (find-mode buffer 'document-mode)
            do (set-url (next/document-mode:node-data history) :buffer buffer)
            do (setf (next/document-mode:active-history-node mode) history))
      ;; TODO: Switch to the last active buffer.  We probably need to serialize *interface*.
      ;; Or else we could include `access-time' in the buffer class.
      )))
