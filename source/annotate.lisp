;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmethod store ((profile data-profile) (path annotations-data-path) &key &allow-other-keys)
  "Store the annotations to the buffer `annotations-path'."
  (with-data-file (file path :direction :output)
    (format file "Hello world!"))
  t)

(defun annotation-add (url &key date title tags)
  (with-data-access (bookmarks (annotations-path (current-buffer)))
    (print "noop")))
