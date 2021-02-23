;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-class heading ()
  ((inner-text ""))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "A heading. The inner-text must not be modified, so
  than we can jump to the anchor of the same name."))

(defmethod title ((heading heading))
  (subseq (inner-text heading) 0 (position #\[ (inner-text heading))))

(defmethod prompter:object-properties ((heading heading))
  (list :title (title heading)))

(defun get-headings ()
  (pflet ((get-headings ()
           (defun qsa (context selector)
             "Alias of document.querySelectorAll"
             (ps:chain context (query-selector-all selector)))
           (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
             (ps:chain |json| (stringify
                               (loop for heading in headings
                                     collect (ps:chain heading inner-text)))))))
    (mapcar (lambda (i) (make-instance 'heading :inner-text i))
            (cl-json:decode-json-from-string (get-headings)))))

(define-parenscript scroll-page-to-heading (heading)
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
    (loop for heading in headings do
             (when (equal (ps:lisp (inner-text heading))
                          (ps:chain heading inner-text))
               (ps:chain heading (scroll-into-view t))))))

(define-class heading-source (prompter:source)
  ((prompter:name "Headings")
   (prompter:must-match-p t)
   (prompter:follow-p t)
   (prompter:persistent-action #'scroll-page-to-heading)
   (prompter:initial-suggestions (get-headings))
   (prompter:actions '(scroll-page-to-heading))))

(define-command jump-to-heading ()
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6."
  (prompt
   :prompt "Jump to heading:"
   :sources (list (make-instance 'heading-source))))
