;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;;;; TODO: Remove?

(defun session-list (&optional (buffer (current-buffer)))
  (mapcar #'pathname-name
          (uiop:directory-files (dirname (history-path buffer)))))

(defun session-name-suggestion-filter (minibuffer)
  (fuzzy-match (input-buffer minibuffer) (session-list)))

(define-command store-session-by-name ()
  "Store the session data (i.e. all the opened buffers) in the file named by user input."
  (with-data-access (history (history-path (current-buffer)))
    (sera:and-let* ((name (prompt-minibuffer
                           :input-prompt "The name to store session with"
                           :history (minibuffer-session-restore-history *browser*)
                           :suggestion-function #'session-name-suggestion-filter))
                    (path (make-instance 'history-data-path
                                         :dirname (dirname (history-path (current-buffer)))
                                         :basename name)))
      (setf (get-data path) history)
      (store (data-profile (current-buffer)) path)
      (setf (get-data path) nil))))

(define-command restore-session-by-name ()
  "Restore the session data from the file named by user input."
  (with-data-access (history (history-path (current-buffer)))
    (sera:and-let* ((name (prompt-minibuffer
                           :input-prompt "The name of the session to restore"
                           :history (minibuffer-session-restore-history *browser*)
                           :suggestion-function #'session-name-suggestion-filter))
                    (path (make-instance 'history-data-path
                                         :dirname (dirname (history-path (current-buffer)))
                                         :basename name)))
      ;; TODO: Maybe merge the existing history, instead of overwriting it?
      (restore (data-profile (current-buffer)) path)
      ;; TODO: Maybe modify `history-path' of all the buffers instead of polluting history?
      (setf history (get-data path)))))

(define-command replace-session-by-name ()
  "Delete all the buffers of the current session and restore the one chosen by user."
  (with-data-access (history (history-path (current-buffer)))
    (let ((old-buffers (buffer-list)))
      (setf history nil)
      (restore-session-by-name)
      (dolist (buffer old-buffers)
        (buffer-delete buffer)))))
