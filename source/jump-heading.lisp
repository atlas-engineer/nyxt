;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-class heading ()
  ((inner-text "" :documentation "The inner text of the heading within the document.")
   (element nil :documentation "The header-representing element of `document-model'.")
   (buffer :documentation "The buffer to which this heading belongs."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "A heading. The inner-text must not be modified, so that we
  can jump to the anchor of the same name."))

(defmethod title ((heading heading))
  (subseq (inner-text heading) 0 (position #\[ (inner-text heading))))

(defmethod prompter:object-attributes ((heading heading))
  `(("Title" ,(title heading))))

(defun get-headings (&key (buffer (current-buffer)))
  (with-current-buffer buffer
    (map 'list
         (lambda (e)
           (make-instance 'heading :inner-text (plump:text e) :element e :buffer buffer))
         (clss:select "h1, h2, h3, h4, h5, h6" (document-model (current-mode 'web))))))

(define-parenscript scroll-to-element (&key nyxt-identifier)
  (ps:chain (nyxt/ps:qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier)))
            (scroll-into-view t)))

;; TODO: Make a method on plump:node? Extract to nyxt/dom?
(defun scroll-page-to-heading (heading)
  (set-current-buffer (buffer heading) :focus nil)
  (scroll-to-element :nyxt-identifier (get-nyxt-id (element heading))))

(define-class heading-source (prompter:source)
  ((prompter:name "Headings")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:follow-p t)
   (prompter:follow-mode-functions #'scroll-page-to-heading)
   (prompter:constructor (lambda (source)
                           (get-headings :buffer (buffer source))))
   (prompter:actions (list (make-unmapped-command scroll-page-to-heading)))))

(define-command jump-to-heading (&key (buffer (current-buffer)))
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6."
  (prompt
   :prompt "Jump to heading:"
   :sources (list (make-instance 'heading-source
                                 :buffer buffer))))

(define-command jump-to-heading-buffers ()
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6 across a set
of buffers."
  (let ((buffers (prompt
                  :prompt "Select headings from buffers:"
                  :sources (make-instance 'user-buffer-source
                                          :multi-selection-p t
                                          :actions nil))))
    (prompt
     :prompt "Jump to heading:"
     :sources (loop for buffer in buffers
                    collect (make-instance
                             'heading-source
                             :name (format nil "Headings: ~a" (title buffer))
                             :buffer buffer)))))

