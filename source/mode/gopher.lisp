;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/gopher-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:serapeum #:-> #:export-always)
  (:documentation "Mode for Gopher page interaction."))
(in-package :nyxt/gopher-mode)
(use-nyxt-package-nicknames)

(defun update (mode)
  (setf (line mode) (cl-gopher:parse-gopher-uri (render-url (url (buffer mode))))
        (contents mode) (cl-gopher:get-line-contents (line mode))))

(define-mode gopher-mode ()
  "Gopher page interaction mode."
  ((rememberable-p nil)
   (line :documentation "The line being opened.")
   (contents :documentation "The contents of the current page.")
   (style (theme:themed-css (nyxt::theme *browser*)
            (pre
             :padding 0
             :margin 0
             :border-radius 0)
            (.button
             :margin "3px")))
   (destructor
    (lambda (mode)
      (hooks:remove-hook
       (pre-request-hook (buffer mode))
       'gopher-mode-disable)))
   (constructor
    (lambda (mode)
      (update mode)
      (hooks:add-hook
       (pre-request-hook (buffer mode))
       (make-handler-resource
        (lambda (request-data)
          (when (nyxt/auto-mode::new-page-request-p request-data)
            (if (string= "gopher" (quri:uri-scheme (url request-data)))
                (update mode)
                (disable-modes '(gopher-mode) (buffer mode))))
          request-data)
        :name 'gopher-mode-disable))))))

(defgeneric line->html (line)
  (:documentation "Transform a gopher line to a reasonable HTML representation."))

(export-always 'render)
(defgeneric render (line &optional mode)
  (:documentation "Produce a page content string/array given LINE.
Second return value should be the MIME-type of the content."))

(defmethod line->html ((line cl-gopher:gopher-line))
  (spinneret:with-html-string
    (:pre (cl-gopher:display-string line))))

(defmethod line->html ((line cl-gopher:info-message))
  (spinneret:with-html-string
    (if (str:emptyp (cl-gopher:display-string line))
        (:br)
        (:pre (cl-gopher:display-string line)))))

(defmethod line->html ((line cl-gopher:submenu))
  (spinneret:with-html-string
    (:a :class "button" :href (cl-gopher:uri-for-gopher-line line)
        (cl-gopher:display-string line))
    (:br)))

(defmethod render ((line cl-gopher:text-file) &optional (mode (current-mode 'gopher)))
  (declare (ignore mode))
  (let ((contents (cl-gopher:get-line-contents line)))
    (values (str:join +newline+ (cl-gopher:lines contents)) "text/plain")))

(defmethod render ((line cl-gopher:submenu) &optional (mode (current-mode 'gopher)))
  (let ((contents (cl-gopher:get-line-contents line)))
    (spinneret:with-html-string
      (:style (style (buffer mode)))
      (:style (style mode))
      (loop for line in (cl-gopher:lines contents)
            collect (:raw (line->html line))))))
