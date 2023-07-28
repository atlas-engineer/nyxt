;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'buffer-match-predicate)
(define-generic buffer-match-predicate ((buffer buffer))
  "Return predicate for if the buffer it's called with is the same as BUFFER."
  (lambda (other-buffer)
    (when other-buffer
      (and (quri:uri= (url buffer) (url other-buffer))
           (string= (title buffer) (title other-buffer))))))

(defun reopen-dead-buffer (buffer)
  (containers:delete-item-if (recent-buffers *browser*)
                             (buffer-match-predicate buffer))
  (reload-buffer (resurrect-buffer buffer))
  buffer)

(define-class recent-buffer-source (prompter:source)
  ((prompter:name "Deleted buffers")
   (prompter:enable-marks-p t)
   (prompter:constructor
    (containers:container->list (recent-buffers *browser*)))
   (prompter:actions-on-return
    (list
     (lambda-command reopen-dead-buffer-focus (buffer-list)
       "Reopen BUFFER and switch to it."
       (mapc #'reopen-dead-buffer buffer-list)
       (set-current-buffer (or (first (prompter:marks (current-source)))
                               (current-suggestion-value (current-prompt-buffer)))))
     (lambda-mapped-command reopen-dead-buffer)))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)))

(define-command reopen-buffer ()
  "Reopen queried deleted buffer(s)."
  (prompt :prompt "Reopen buffer(s)" :sources 'recent-buffer-source))

(define-command reopen-last-buffer ()
  "Open a new buffer with the URL of the most recently deleted buffer."
  (if (plusp (containers:size (recent-buffers *browser*)))
      (set-current-buffer
       (reopen-dead-buffer (containers:first-item (recent-buffers *browser*))))
      (echo "There are no recently-deleted buffers.")))
