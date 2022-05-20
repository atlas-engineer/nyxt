;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-class heading ()
  ((inner-text "" :documentation "The inner text of the heading within the document.")
   (element nil :documentation "The header-representing element of `document-model'.")
   (buffer :documentation "The buffer to which this heading belongs.")
   (keywords :documentation "Keywords associated with this heading.")
   (scroll-position :documentation "The scroll position of the heading."))
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A heading. The inner-text must not be modified, so that we
  can jump to the anchor of the same name."))

(defmethod title ((heading heading))
  (subseq (inner-text heading) 0 (position #\[ (inner-text heading))))

(defmethod prompter:object-attributes ((heading heading))
  `(("Title" ,(format nil "~a ~a"
                      (make-string (typecase (element heading)
                                     (nyxt/dom:h1-element 1)
                                     (nyxt/dom:h2-element 2)
                                     (nyxt/dom:h3-element 3)
                                     (nyxt/dom:h4-element 4)
                                     (nyxt/dom:h5-element 5)
                                     (nyxt/dom:h6-element 6)
                                     (t 0))
                                   :initial-element #\*)
                      (title heading)))
    ("Keywords" ,(format nil "~:{~a~^ ~}" (keywords heading)))))

(defun get-headings (&key (buffer (current-buffer)))
  (pflet ((heading-scroll-position
           (element)
           (ps:chain (nyxt/ps:qs-nyxt-id document (ps:lisp (get-nyxt-id element)))
                     (get-bounding-client-rect) y)))
    (with-current-buffer buffer
      (sort (map 'list
                 (lambda (e)
                   (make-instance 'heading :inner-text (plump:text e)
                                           :element e
                                           :buffer buffer
                                           :keywords (ignore-errors
                                                      (analysis:extract-keywords
                                                       (plump:text (plump:next-element e))))
                                           :scroll-position (heading-scroll-position e)))
                 (clss:select "h1, h2, h3, h4, h5, h6" (document-model buffer)))
            #'< :key (alex:compose #'parse-integer #'get-nyxt-id #'element)))))

(defun current-heading (&optional (buffer (current-buffer)))
  (alex:when-let* ((scroll-position (document-scroll-position buffer))
                   (vertical-scroll-position (second scroll-position))
                   (headings (get-headings :buffer buffer)))
    (first (sort headings
                 (lambda (h1 h2)
                   (< (abs (- (scroll-position h1) vertical-scroll-position))
                      (abs (- (scroll-position h2) vertical-scroll-position))))))))

(define-parenscript scroll-to-element (&key nyxt-identifier)
  (ps:chain (nyxt/ps:qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier)))
            (scroll-into-view t)))

;; TODO: Make a method on plump:node? Extract to nyxt/dom?
(defun scroll-page-to-heading (heading)
  (set-current-buffer (buffer heading) :focus nil)
  (scroll-to-element :nyxt-identifier (get-nyxt-id (element heading))))

(define-command next-heading (&optional (buffer (current-buffer)))
  "Scroll to the next heading of the BUFFER."
  (sera:and-let* ((headings (get-headings :buffer buffer))
                  (current (current-heading buffer)))
    (scroll-page-to-heading (elt headings (1+ (position (element current) headings :key #'element))))))

(define-command previous-heading (&optional (buffer (current-buffer)))
  "Scroll to the previous heading of the BUFFER."
  (sera:and-let* ((headings (get-headings :buffer buffer))
                  (current (current-heading buffer)))
    (scroll-page-to-heading (elt headings (1- (position (element current) headings :key #'element))))))

(define-class heading-source (prompter:source)
  ((prompter:name "Headings")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:selection-actions-enabled-p t)
   (prompter:selection-actions #'scroll-page-to-heading)
   (prompter:constructor (lambda (source)
                           (get-headings :buffer (buffer source))))
   (prompter:return-actions (list (lambda-unmapped-command scroll-page-to-heading)))))

(define-command jump-to-heading (&key (buffer (current-buffer)))
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6."
  (prompt
   :prompt "Jump to heading"
   :sources (list (make-instance 'heading-source
                                 :buffer buffer))))

(define-command jump-to-heading-buffers ()
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6 across a set
of buffers."
  (let ((buffers (prompt
                  :prompt "Select headings from buffers"
                  :sources (make-instance 'buffer-source
                                          :multi-selection-p t
                                          :return-actions nil))))
    (prompt
     :prompt "Jump to heading"
     :sources (loop for buffer in buffers
                    collect (make-instance
                             'heading-source
                             :name (format nil "Headings: ~a" (title buffer))
                             :buffer buffer)))))

(nyxt::define-panel-global headings ()
    (panel-buffer "*Headings panel*")
  "Display a list of heading for jumping."
  (labels ((get-level (heading)
             (ignore-errors (parse-integer (subseq (plump:tag-name (element heading)) 1))))
           (group-headings (headings)
             (loop with min-level = (apply #'min (mapcar #'get-level headings))
                   with current = (list)
                   for heading in headings
                   if (= (get-level heading) min-level)
                     collect (nreverse current) into total
                     and do (setf current (list heading))
                   else
                     do (push heading current)
                   finally (return (delete nil (append total (list (nreverse current)))))))
           (headings->html (groups)
             (spinneret:with-html-string
               (:ul
                (dolist (group groups)
                  (let ((heading (first group)))
                    (:li (:a :onclick
                             (ps:ps (nyxt/ps:lisp-eval
                                     `(progn
                                        (switch-buffer :id ,(id (buffer heading)))
                                        (scroll-to-element :nyxt-identifier
                                                           ,(get-nyxt-id (element heading))))))
                             (title heading)))
                    (when (rest group)
                      (:raw (sera:mapconcat #'headings->html (list (group-headings (rest group))) "")))))))))
    (setf (ffi-window-panel-buffer-width (current-window) panel-buffer) 400)
    (spinneret:with-html-string
      (:h1 "Headings")
      (:raw (headings->html (group-headings (get-headings)))))))
