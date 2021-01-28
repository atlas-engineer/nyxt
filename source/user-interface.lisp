;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Implementations of update methods for user-interface library widgets

(in-package :nyxt)

(defmethod user-interface:update ((paragraph user-interface:paragraph))
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer paragraph)
   (ps:ps (let ((element (ps:chain document (get-element-by-id (ps:lisp (user-interface:id paragraph))))))
            (setf (ps:chain element text-content) (ps:lisp (user-interface:text paragraph)))))))
