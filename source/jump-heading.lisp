;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-class heading ()
  ((inner-text ""))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "A heading. The inner-text must not be modified, so
  than we can jump to the anchor of the same name."))

(defmethod nyxt:object-string ((heading heading))
  "Cleaned-up text of this heading, to show the user in the minibuffer.
For example, Wikipedia ones end with '[edit]'. We strip what comes after the first bracket."
  (with-slots (inner-text) heading
    (subseq inner-text 0 (position #\[ inner-text))))

(defmethod nyxt:object-display ((heading heading))
  "Same as `nyxt:object-string'."
  (nyxt:object-string heading))

(defun make-headings (list/str)
  "Make a list `heading's from a list of strings."
  (mapcar (lambda (text)
            (make-instance 'heading :inner-text text))
          list/str))

(define-parenscript get-headings ()
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
    (ps:chain |json| (stringify
                        (loop for heading in headings
                           collect (ps:chain heading inner-text))))))

(define-parenscript paren-jump-to-heading (&key heading-inner-text)
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
    (loop for heading in headings do
         (when (equal (ps:lisp heading-inner-text) (ps:chain heading inner-text))
           (ps:chain heading (scroll-into-view t))))))

(define-command jump-to-heading ()
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6."
  (let* ((headings (get-headings))
         (heading (prompt-minibuffer
                   :input-prompt "Jump to heading"
                   :suggestion-function (lambda (minibuffer)
                                          (fuzzy-match
                                           (input-buffer minibuffer)
                                           (make-headings (cl-json:decode-json-from-string headings)))))))
    (paren-jump-to-heading :heading-inner-text (inner-text heading))))
