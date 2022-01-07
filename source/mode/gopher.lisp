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
   (search-engines
    '()
    :type list
    :reader nil
    :writer t
    :documentation "A list of Gopher search-engines to use when doing `search-gopher'.
Create those with `make-gopher-search-engine'.")
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

(defmethod search-engines ((mode gopher-mode))
  (mapcar (lambda (engine)
            (typecase engine
              (string (cl-gopher:parse-gopher-uri engine))
              (cl-gopher:search-line engine)))
          (slot-value mode 'search-engines)))

(defgeneric line->html (line)
  (:documentation "Transform a gopher line to a reasonable HTML representation."))

(export-always 'render)
(defgeneric render (line &optional mode)
  (:documentation "Produce a page content string/array given LINE.
Second return value should be the MIME-type of the content."))

(defmethod line->html ((line cl-gopher:gopher-line))
  (spinneret:with-html-string
    (:pre (symbol-name (class-name (class-of line)))
          (cl-gopher:display-string line)
          (cl-gopher:uri-for-gopher-line line))))

(defmethod line->html ((line cl-gopher:info-message))
  (spinneret:with-html-string
    (if (str:emptyp (cl-gopher:display-string line))
        (:br)
        (:pre (cl-gopher:display-string line)))))

(defmethod line->html ((line cl-gopher:submenu))
  (spinneret:with-html-string
    (:a :class "button" :href (cl-gopher:uri-for-gopher-line line)
        (cl-gopher:display-string line))))

(defmethod line->html ((line cl-gopher:gif))
  (spinneret:with-html-string
    (:a :href (cl-gopher:uri-for-gopher-line line)
        (:img :src (cl-gopher:uri-for-gopher-line line)
              :alt (cl-gopher:display-string line)))))

(defmethod line->html ((line cl-gopher:png))
  (spinneret:with-html-string
    (:a :href (cl-gopher:uri-for-gopher-line line)
        (:img :src (cl-gopher:uri-for-gopher-line line)
              :alt (cl-gopher:display-string line)))))

(defmethod line->html ((line cl-gopher:search-line))
  "We use `search-gopher' command and custom URLs to simulate search.
That's why `cl-gopher:search-line' renders to nothing."
  (declare (ignore line))
  "")

(defmethod line->html ((line cl-gopher:text-file))
  (spinneret:with-html-string
    (:a :href (cl-gopher:uri-for-gopher-line line)
        (cl-gopher:display-string line))
    (:br)))

(defmethod line->html ((line cl-gopher:html-file))
  (spinneret:with-html-string
    (:a :href (cl-gopher:uri-for-gopher-line line)
        (cl-gopher:display-string line))
    (:br)))

(defmethod render ((line cl-gopher:gopher-line) &optional (mode (current-mode 'gopher)))
  (let ((contents (cl-gopher:get-line-contents line)))
    (spinneret:with-html-string
      (:style (style (buffer mode)))
      (:style (style mode))
      (loop for line in (cl-gopher:lines contents)
            collect (:raw (line->html line))))))

(defmethod render ((line cl-gopher:html-file) &optional (mode (current-mode 'gopher)))
  (declare (ignore mode))
  (let ((contents (cl-gopher:get-line-contents line)))
    (cl-gopher:content-string contents)))

(defmethod render ((line cl-gopher:text-file) &optional (mode (current-mode 'gopher)))
  (declare (ignore mode))
  (let ((contents (cl-gopher:get-line-contents line)))
    (values (str:join +newline+ (cl-gopher:lines contents)) "text/plain")))

(defmethod render ((line cl-gopher:gif) &optional (mode (current-mode 'gopher)))
  (declare (ignore mode))
  (let ((contents (cl-gopher:get-line-contents line)))
    (values (cl-gopher:content-array contents) "image/gif")))

(defmethod render ((line cl-gopher:png) &optional (mode (current-mode 'gopher)))
  (declare (ignore mode))
  (let ((contents (cl-gopher:get-line-contents line)))
    (values (cl-gopher:content-array contents) "image/png")))

(defmethod prompter:object-attributes ((line cl-gopher:search-line))
  `(("Terms" ,(cl-gopher:terms line))
    ("Name" ,(cl-gopher:display-string line))))

(define-class gopher-search-source (prompter:source)
  ((prompter:name "Term Search")
   (prompter:constructor (let ((mode (current-mode 'gopher)))
                           (union (search-engines mode)
                                  (sera:filter (alex:rcurry #'typep 'cl-gopher:search-line)
                                               (cl-gopher:lines (contents mode)))
                                  :test #'string= :key #'cl-gopher:uri-for-gopher-line)))
   (prompter:multi-selection-p t)
   (prompter:filter-preprocessor
    (lambda (suggestions source input)
      (declare (ignore source))
      (dolist (suggestion suggestions)
        (setf (cl-gopher:terms (prompter:value suggestion)) input))
      suggestions))
   (prompter:actions (list (make-command search-gopher* (lines)
                             (buffer-load (str:concat (cl-gopher:uri-for-gopher-line (first lines))
                                                      "?q=" (cl-gopher:terms (first lines))))
                             (dolist (line (rest lines))
                               (make-buffer
                                :url (str:concat (cl-gopher:uri-for-gopher-line (first lines))
                                                 "?q=" (cl-gopher:terms (first lines)))
                                :parent-buffer (current-buffer))))
                           (make-command save-search-engine (lines)
                             (nyxt::configure-slot
                              'search-engines 'gopher-mode
                              :value `(append %slot-default%
                                              ,@(mapcar (lambda (line)
                                                          (make-gopher-search-engine
                                                           (cl-gopher:uri-for-gopher-line line)
                                                           (cl-gopher:display-string line)))
                                                        lines))))))))

(export-always 'make-gopher-search-engine)
(defun make-gopher-search-engine (url name)
  (let ((url (quri:uri url)))
    (make-instance 'cl-gopher:search-line
                   :hostname (quri:uri-host url)
                   :port (or (quri:uri-port url) 70)
                   :selector (or (quri:uri-path url) "/")
                   :display-string name)))

(define-command search-gopher ()
  "Prompt for terms and search those in current page and saved search engines."
  (prompt :prompt "Search Gopher for"
          :sources (list (make-instance 'gopher-search-source))))
