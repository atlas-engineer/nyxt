;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Implementations of update methods for user-interface library widgets

(in-package :nyxt)

(defmethod user-interface:update ((paragraph user-interface:paragraph))
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer paragraph)
   (ps:ps
     (let ((node (ps:chain document
                           (get-element-by-id
                            (ps:lisp (user-interface:id paragraph))))))
       (when node
         (setf (ps:@ node text-content)
               (ps:lisp (user-interface:display-text
                         (user-interface:text paragraph)))))))))

(defmethod user-interface:update ((progress-bar user-interface:progress-bar))
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer progress-bar)
   (ps:ps
     (let ((node (ps:chain document
                           (get-element-by-id
                            (ps:lisp (user-interface:id progress-bar))))))
       (when node
         (setf (ps:chain node style width)
               (ps:lisp (format nil "~,1f%"
                                (user-interface:valid-percentage progress-bar)))))))))

(defun replace-ui-element (element)
  "Replace ELEMENT's DOM node with its current HTML."
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer element)
   (ps:ps
     (let ((node (ps:chain document
                           (get-element-by-id
                            (ps:lisp (user-interface:id element))))))
       (when node
         (setf (ps:@ node |outerHTML|)
               (ps:lisp (spinneret:with-html-string
                          (user-interface:to-html element)))))))))

(defmethod user-interface:update ((button user-interface:button))
  (replace-ui-element button))

(defmethod user-interface:update ((element user-interface:element))
  (replace-ui-element element))

(defmethod user-interface:update ((row user-interface:table-row))
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer row)
   (ps:ps
     (let ((row (ps:chain document
                          (query-selector
                           (+ "[data-id=\""
                              (ps:chain -c-s-s
                                        (escape (ps:lisp (user-interface:id row))))
                              "\"]")))))
       (when row
         (setf (ps:@ row |outerHTML|)
               (ps:lisp (spinneret:with-html-string
                          (user-interface:to-html row)))))))))

(defmethod user-interface:update ((table user-interface:table))
  (ffi-buffer-evaluate-javascript-async
   (user-interface:buffer table)
   (ps:ps
     (let ((element (ps:chain document
                              (get-element-by-id
                               (ps:lisp (user-interface:id table))))))
       (when element
         (setf (ps:@ element class-name)
               (ps:lisp (or (user-interface:html-class table) ""))
               (ps:@ element |innerHTML|)
               (ps:lisp (user-interface:table-inner-html table))))))))
