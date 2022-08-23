;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Implementations of update methods for user-interface library widgets

(in-package :nyxt)

(defmethod user-interface:update ((paragraph user-interface:paragraph))
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer paragraph)
   (ps:ps
     (setf (ps:chain document
                     (query-select (ps:lisp (user-interface:id paragraph)))
                     text-content)
           (ps:lisp (user-interface:text paragraph))))))

(defmethod user-interface:update ((progress-bar user-interface:progress-bar))
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer progress-bar)
   (ps:ps
     (setf (ps:chain document
                     (query-select (ps:lisp (user-interface:id progress-bar)))
                     style
                     width)
           (ps:lisp (format nil "~,1f%" (user-interface:percentage progress-bar)))))))

(defmethod user-interface:update ((button user-interface:button))
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer button)
   (ps:ps
     (setf (ps:chain document
                     (query-select (ps:lisp (user-interface:id button)))
                     text-content)
           (ps:lisp (user-interface:text button)))
     (setf (ps:chain document
                     (query-select (ps:lisp (user-interface:id button)))
                     onclick)
           (ps:lisp (user-interface:action button))))))
