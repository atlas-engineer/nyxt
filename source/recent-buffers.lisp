;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'buffer-match-predicate)
(defmethod buffer-match-predicate ((buffer buffer))
  (lambda (other-buffer)
    (when other-buffer
      (and (quri:uri= (url buffer) (url other-buffer))
           (string= (title buffer) (title other-buffer))))))

(define-class recent-buffer-source (prompter:source)
  ((prompter:name "Deleted buffers")
   (prompter:must-match-p t)
   (prompter:multi-selection-p t)
   (prompter:initial-suggestions
    (containers:container->list (recent-buffers *browser*)))))

(define-command reopen-buffer ()
  "Reopen queried deleted buffer(s)."
  (let* ((buffers (prompt
                   :prompt "Ropen buffer(s):"
                   :sources (make-instance 'recent-buffer-source)))
         (buffers (uiop:ensure-list buffers)))
    (dolist (buffer buffers)
      (containers:delete-item-if (recent-buffers *browser*)
                                 (buffer-match-predicate buffer))
      (reload-current-buffer (buffer-make *browser* :dead-buffer buffer))
      (when (and (eq buffer (first buffers))
                 (focus-on-reopened-buffer-p *browser*))
        (set-current-buffer buffer)))))

(define-command reopen-last-buffer ()
  "Open a new buffer with the URL of the most recently deleted buffer."
  (if (plusp (containers:size (recent-buffers *browser*)))
      (let ((buffer (buffer-make
                     *browser*
                     :dead-buffer (containers:delete-first (recent-buffers *browser*)))))
        (reload-current-buffer buffer)
        (when (focus-on-reopened-buffer-p *browser*)
          (set-current-buffer buffer)))
      (echo "There are no recently-deleted buffers.")))
