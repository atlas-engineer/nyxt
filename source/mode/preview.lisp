;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/preview
  (:documentation "Package for `preview-mode', mode to refresh file when changed on disk.

Extends `nyxt/mode/process:process-mode' with custom actions and relies on the
custom `updated-file-p' `nyxt/mode/process:firing-condition'.

See the `preview-mode' documentation for the external user-facing APIs."))
(in-package :nyxt/mode/preview)

(defun updated-file-p (path-url mode)
  (when (quri:uri-file-p path-url)
    (or (null (last-access mode))
        (time:timestamp>
         (time:universal-to-timestamp
          (uiop:safe-file-write-date (quri:uri-path path-url)))
         (last-access mode)))))

(define-mode preview-mode (nyxt/mode/process:process-mode)
  "Refreshes the buffer when associated local file is changed.

See `nyxt/mode/preview' package documentation for implementation details and internal programming APIs."
  ((visible-in-status-p nil)
   (rememberable-p t)
   (nyxt/mode/process:firing-condition #'updated-file-p)
   (nyxt/mode/process:action
    #'(lambda (path-url mode)
        (buffer-load path-url :buffer (buffer mode))
        (setf (last-access mode) (time:now))))
   (last-access nil
                :type (or time:timestamp null)
                :documentation "The time file was last accessed.")))

(in-package :nyxt)

(define-command preview-file (&key (buffer (current-buffer))
                              (file (prompt :prompt "File to preview"
                                            :input (quri:uri-path (url buffer))
                                            :sources 'nyxt/mode/file-manager:file-source)))
  "Open a local FILE in BUFFER and call `preview-mode' to watch and refresh it."
  (buffer-load (quri.uri.file:make-uri-file :path file) :buffer buffer)
  (enable (make-instance 'nyxt/mode/preview:preview-mode :buffer buffer)))
