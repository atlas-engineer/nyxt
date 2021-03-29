;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/preview-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:class-star #:define-class)
  (:documentation "Refresh file when changed on disk."))
(in-package :nyxt/preview-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)
  (trivial-package-local-nicknames:add-package-local-nickname :file-attributes :org.shirakumo.file-attributes))

(defun updated-file-p (path-url mode)
  (and
   (quri:uri-file-p path-url)
   (or (null (last-access mode))
       (local-time:timestamp>
        (local-time:universal-to-timestamp
         (file-attributes:modification-time (quri:uri-path path-url)))
        (last-access mode)))))

(define-mode preview-mode (nyxt/process-mode:process-mode)
  "Refreshes the buffer when associated local file is changed."
  ((firing-condition #'updated-file-p)
   (action
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
  (sera:and-let* ((file (or file (prompt :prompt "File to preview"))))
    (buffer-load (quri.uri.file:make-uri-file :path file) :buffer buffer)
    (nyxt/preview-mode:preview-mode)))
