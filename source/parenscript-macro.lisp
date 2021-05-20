;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/parenscript
  (:nicknames #:nyxt/ps)
  (:use #:common-lisp #:parenscript)
  (:import-from #:serapeum #:export-always))

(in-package :nyxt/parenscript)

(export-always 'qs)
(defpsmacro qs (context selector)
  "Alias of document.querySelector"
  `(chain ,context (query-selector ,selector)))

(export-always 'qsa)
(defpsmacro qsa (context selector)
  "Alias of document.querySelectorAll"
  `(chain ,context (query-selector-all ,selector)))

(export-always 'qs-nyxt-id)
(defpsmacro qs-nyxt-id (context id)
  "document.querySelector tailored for Nyxt IDs."
  `(chain ,context (query-selector (lisp (format nil "[nyxt-identifier=\"~a\"]" ,id)))))

(export-always 'insert-at)
(defpsmacro insert-at (tag input-text)
  "Insert text at a tag."
  `(let ((origin (chain ,tag selection-start))
         (end (chain ,tag selection-end)))
     (setf (chain ,tag value)
           (+ (chain ,tag value (substring 0 origin))
              ,input-text
              (chain ,tag value
                     (substring end
                                (chain ,tag value length)))))
     (if (= origin end)
         (progn
           (setf (chain ,tag selection-start) (+ origin (chain ,input-text length)))
           (setf (chain ,tag selection-end) (chain ,tag selection-start)))
         (progn
           (setf (chain ,tag selection-start) origin)
           (setf (chain ,tag selection-end) (+ origin (chain ,input-text length)))))))

(export-always 'element-drawable-p)
(defpsmacro element-drawable-p (element)
  "Is the element drawable?"
  `(if (or (chain ,element offset-width)
           (chain ,element offset-height)
           (chain ,element (get-client-rects) length))
       t nil))

(export-always 'element-in-view-port-p)
(defpsmacro element-in-view-port-p (element)
  "Is the element in the view port?"
  `(let* ((rect (chain ,element (get-bounding-client-rect))))
     (if (and (>= (chain rect top) 0)
              (>= (chain rect left) 0)
              (<= (chain rect right) (chain window inner-width))
              (<= (chain rect bottom) (chain window inner-height)))
         t nil)))
