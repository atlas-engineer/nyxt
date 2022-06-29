;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/preview-mode
    (:documentation "Refresh file when changed on disk."))
(in-package :nyxt/preview-mode)

(defun updated-file-p (path-url mode)
  (when (quri:uri-file-p path-url)
    (or (null (last-access mode))
        (local-time:timestamp>
         (local-time:universal-to-timestamp
          (uiop:safe-file-write-date (quri:uri-path path-url)))
         (last-access mode)))))

(define-mode preview-mode (nyxt/process-mode:process-mode)
  "Refreshes the buffer when associated local file is changed."
  ((visible-in-status-p nil)
   (rememberable-p t)
   (nyxt/process-mode:firing-condition #'updated-file-p)
   (nyxt/process-mode:action
    #'(lambda (path-url mode)
        (buffer-load path-url :buffer (buffer mode))
        (setf (last-access mode) (local-time:now))))
   (last-access nil
                :type (or local-time:timestamp null)
                :documentation "The time file was last accessed.")))

(in-package :nyxt)

(define-command preview-file (&optional file (buffer (current-buffer)))
  "Open a file in the current buffer and call `preview-mode' to continuously
watch and refresh it."
  (alex:when-let ((file (or file (prompt :prompt "File to preview"
                                         :input (quri:uri-path (url (current-buffer)))
                                         :sources (list (make-instance 'nyxt/file-manager-mode:file-source))))))
    (buffer-load (quri.uri.file:make-uri-file :path file) :buffer buffer)
    (enable (make-instance 'nyxt/preview-mode:preview-mode :buffer buffer))))
