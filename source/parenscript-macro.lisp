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
  `(let ((element ,tag)
         (origin (chain element selection-start))
         (end (chain element selection-end))
         (tag-name (chain element tag-name)))
     (cond
      ((or (string= tag-name "INPUT")
           (string= tag-name "TEXTAREA"))
       (setf (chain element value)
             (+ (chain element value (substring 0 origin))
                ,input-text
                (chain element value
                       (substring end (chain element value length)))))
       (if (= origin end)
           (progn
             (setf (chain element selection-start) (+ origin (chain ,input-text length)))
             (setf (chain element selection-end) (chain element selection-start)))
           (progn
             (setf (chain element selection-start) origin)
             (setf (chain element selection-end) (+ origin (chain ,input-text length))))))
      ((chain element is-content-editable)
       ;; TODO: Implement caret movement, as in
       ;; https://stackoverflow.com/questions/6249095/how-to-set-the-caret-cursor-position-in-a-contenteditable-element-div
       (setf (chain element inner-text)
             (+ (chain element inner-text (substring 0 origin))
                ,input-text
                (chain element inner-text
                       (substring end
                                  (chain element inner-text length)))))))))

(export-always 'element-editable-p)
(defpsmacro element-editable-p (element)
  "Is the element editable?"
  `(let ((tag (chain ,element tag-name)))
     (if (or (string= tag "INPUT")
             (string= tag "TEXTAREA")
             (chain ,element is-content-editable))
         t nil)))

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

(export-always 'lisp-eval)
(defpsmacro lisp-eval (form &optional callback)
  "Request the lisp: URL and invoke callback when there's a successful result."
  `(let ((request (fetch (lisp (str:concat
                                "lisp://" (quri:url-encode (write-to-string ,form))))
                         (create :mode "no-cors"))))
     (when ,callback
       (chain request
              (then (lambda (response)
                      (when (@ response ok)
                        (chain response (json)))))
              (then ,callback)))))
