;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun file-url-p (url &key check-exists-p)
  "Check if a url (string) represents a file, and optionally check if said file
exists."
  ;; check if a string starts with file to avoid excessive processing
  (when (str:starts-with-p "file" url)
    (let ((uri (quri:uri url)))
      (if check-exists-p
          (and (equalp "file" (quri:uri-scheme uri))
               (uiop:probe-file* (quri:uri-path uri)))
          (equalp "file" (quri:uri-scheme uri))))))

(defun read-file-string (url)
  "Read a file from a file:// type URL into a string."
  (uiop:read-file-string (quri:uri-path (quri:uri url))))
