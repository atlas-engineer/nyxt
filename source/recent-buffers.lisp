;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmethod deleted-buffers ((browser browser))
  (remove-if-not #'dead-buffer-p
                 (cl-containers:container->list (recent-buffers browser))))

(defmethod last-deleted-buffer ((browser browser))
  "Return the last deleted buffer if exists, otherwise NIL."
  (first (deleted-buffers browser)))

(defmethod reopen-dead-buffer ((buffer modable-buffer))
  (when (dead-buffer-p buffer)
    (ffi-buffer-initialize-foreign-object buffer)
    (add-to-recent-buffers buffer)
    (buffers-set (id buffer) buffer)
    (buffer-load (url buffer) :buffer buffer)))

(define-class recent-buffer-source (prompter:source)
  ((prompter:name "Deleted buffers")
   (prompter:enable-marks-p t)
   (prompter:constructor
    (deleted-buffers *browser*))
   (prompter:actions-on-return
    (list
     (lambda-command reopen-dead-buffer-focus (buffer-list)
       "Reopen BUFFER and switch to it."
       (mapc #'reopen-dead-buffer buffer-list)
       (set-current-buffer
        (or (first (prompter:marks (current-source)))
            (current-suggestion-value (current-prompt-buffer)))))
     (lambda-mapped-command reopen-dead-buffer)))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)))

(define-command reopen-buffer ()
  "Reopen queried deleted buffer(s)."
  (prompt :prompt "Reopen buffer(s)" :sources 'recent-buffer-source))

(define-command reopen-last-buffer ()
  "Open a new buffer with the URL of the most recently deleted buffer."
  (alex:if-let ((buffer (last-deleted-buffer *browser*)))
    (progn (reopen-dead-buffer buffer)
           (set-current-buffer buffer))
    (echo "There are no recently-deleted buffers.")))
