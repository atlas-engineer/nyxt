;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-class heading ()
  ((inner-text "" :documentation "The inner text of the heading within the document.")
   (buffer :documentation "The buffer to which this heading belongs."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "A heading. The inner-text must not be modified, so that we
  can jump to the anchor of the same name."))

(defmethod title ((heading heading))
  (subseq (inner-text heading) 0 (position #\[ (inner-text heading))))

(defmethod prompter:object-attributes ((heading heading))
  `(("Title" ,(title heading))))

(defun get-headings (&key (buffer (current-buffer)))
  (pflet ((get-headings ()
           (defun qsa (context selector)
             "Alias of document.querySelectorAll"
             (ps:chain context (query-selector-all selector)))
           (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
             (ps:chain |json| (stringify
                               (loop for heading in headings
                                     collect (ps:chain heading inner-text)))))))
    (with-current-buffer buffer
      (mapcar (lambda (i) (make-instance 'heading
                                         :inner-text i
                                         :buffer buffer))
              (cl-json:decode-json-from-string (get-headings))))))

(defun scroll-page-to-heading (heading)
  (pflet ((scroll-page-to-heading (heading)
            (defun qsa (context selector)
              (ps:chain context (query-selector-all selector)))
            (let ((headings (qsa document "h1, h2, h3, h4, h5, h6")))
              (loop for heading in headings do
                       (when (equal (ps:lisp (inner-text heading))
                                    (ps:chain heading inner-text))
                         (ps:chain heading (scroll-into-view t)))))))
    (set-current-buffer (buffer heading) :focus nil)
    (scroll-page-to-heading heading)))

(define-class heading-source (prompter:source)
  ((prompter:name "Headings")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:must-match-p t)
   (prompter:follow-p t)
   (prompter:persistent-action #'scroll-page-to-heading)
   (prompter:constructor (lambda (source)
                           (get-headings :buffer (buffer source))))
   (prompter:actions (list (make-unmapped-command scroll-page-to-heading)))))

(define-command jump-to-heading (&key (buffer (current-buffer)))
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6."
  (prompt
   :prompt "Jump to heading:"
   :sources (list (make-instance 'heading-source
                                 :buffer buffer))))

(define-command jump-to-heading-across-buffers ()
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6 across a set
of buffers."
  (let ((buffers (prompt
                  :prompt "Select headings from buffers:"
                  :sources (make-instance 'buffer-source
                                          :multi-selection-p t
                                          :actions nil))))
    (prompt
     :prompt "Jump to heading:"
     :sources (loop for buffer in buffers
                    collect (make-instance
                             'heading-source
                             :name (format nil "Headings: ~a" (title buffer))
                             :buffer buffer)))))

