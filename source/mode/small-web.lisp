;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/small-web-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:serapeum #:-> #:export-always)
  (:documentation "Mode for Gopher/Gemini page interaction."))
(in-package :nyxt/small-web-mode)
(use-nyxt-package-nicknames)

(defun update (mode)
  (let ((url (url (buffer mode))))
    (run-thread "small-web-mode update thread"
      (setf (url mode) url
            (model mode) (str:string-case (quri:uri-scheme url)
                           ("gopher" (cl-gopher:get-line-contents
                                      (cl-gopher:parse-gopher-uri (render-url url))))
                           ("gemini" (multiple-value-bind (status meta body)
                                         (gemini:request url)
                                       (if (and (eq :success status)
                                                (str:starts-with-p "text/gemini" meta)
                                                (stringp body))
                                           (gemtext:parse-string body)
                                           body))))))))

(define-mode small-web-mode ()
  "Gopher/Gemini page interaction mode.

Renders gopher elements (provided by `cl-gopher') to human-readable HTML.

The default style is rendering info messages to <pre> text, inlining
images/sounds and showing everything else as buttons.

The rendering of pages is done via the `render' method, while rendering of
separate lines constituting a page is done in `line->html'. If you're
unsatisfied with how pages are rendered, override either of the two.

For example, if you want to render images as links instead of inline image
loading, you'd need to override `line->html' in the following way:

\(defun image->link (line)
  (spinneret:with-html-string
    (:a :href (cl-gopher:uri-for-gopher-line line)
        (cl-gopher:display-string line))))

\(defmethod line->html ((line cl-gopher:image)) (image->link line))
\(defmethod line->html ((line cl-gopher:gif)) (image->html line))
\(defmethod line->html ((line cl-gopher:png)) (image->link line))

Gemini support is a bit more chaotic, but you can override `line->html' for
`phos/gemtext' elements too."
  ((rememberable-p nil)
   (url :documentation "The URL being opened.")
   (model :documentation "The contents of the current page.")
   (redirections nil :documentation "The list of redirection Gemini URLs.")
   (allowed-redirections-count
    5
    :documentation "The number of redirections that Gemini resources are allowed to make.")
   (search-engines
    '()
    :type list
    :reader nil
    :writer t
    :documentation "A list of Gopher search-engines to use when doing `search-gopher'.
Create those with `make-gopher-search-engine'.")
   (style (theme:themed-css (nyxt::theme *browser*)
            (body
             :background-color theme:background)
            (pre
             :background-color theme:quaternary
             :padding "2px"
             :margin "0"
             :border-radius 0)
            (.button
             :margin "0 3px 3px 0"
             :font-size "15px")
            (.search
             :background-color theme:accent)
            (.error
             :background-color theme:accent
             :color theme:background
             :padding "1em 0")))
   (destructor
    (lambda (mode)
      (hooks:remove-hook
       (pre-request-hook (buffer mode))
       'small-web-mode-disable)))
   (constructor
    (lambda (mode)
      (update mode)
      (hooks:add-hook
       (pre-request-hook (buffer mode))
       (make-handler-resource
        (lambda (request-data)
          (when (nyxt/auto-mode::new-page-request-p request-data)
            (if (str:s-member '("gopher" "gemini")
                              (quri:uri-scheme (url request-data)))
                (update mode)
                (disable-modes '(small-web-mode) (buffer mode))))
          request-data)
        :name 'small-web-mode-disable))))))

(defmethod search-engines ((mode small-web-mode))
  (mapcar (lambda (engine)
            (typecase engine
              (string (cl-gopher:parse-gopher-uri engine))
              (cl-gopher:search-line engine)))
          (slot-value mode 'search-engines)))

(defmethod prompter:object-attributes ((line cl-gopher:search-line))
  `(("Terms" ,(or (cl-gopher:terms line) ""))
    ("Name" ,(cl-gopher:display-string line))))

(define-class gopher-search-source (prompter:source)
  ((prompter:name "Term Search")
   (prompter:constructor (let ((mode (current-mode 'small-web)))
                           (union (search-engines mode)
                                  (sera:filter (alex:rcurry #'typep 'cl-gopher:search-line)
                                               (cl-gopher:lines (model mode)))
                                  :test #'string= :key #'cl-gopher:uri-for-gopher-line)))
   (prompter:multi-selection-p t)
   (prompter:filter-preprocessor
    (lambda (suggestions source input)
      (declare (ignore source))
      (mapcar (lambda (suggestion)
                (let ((value (cl-gopher:copy-gopher-line (prompter:value suggestion))))
                  (setf (cl-gopher:terms value) input)
                  (prompter:make-suggestion value)))
              suggestions)))
   (prompter:actions (list (make-command search-gopher* (lines)
                             (buffer-load (cl-gopher:uri-for-gopher-line (first lines)))
                             (dolist (line (rest lines))
                               (make-buffer
                                :url (cl-gopher:uri-for-gopher-line line)
                                :parent-buffer (current-buffer))))
                           (make-command search-gopher-new-buffer* (lines)
                             (dolist (line lines)
                               (make-buffer
                                :url (cl-gopher:uri-for-gopher-line line)
                                :parent-buffer (current-buffer))))
                           (make-command save-search-engine* (lines)
                             (nyxt::configure-slot
                              'search-engines 'small-web-mode
                              :value `(append %slot-default%
                                              (list
                                               ,@(mapcar (lambda (line)
                                                           ;; FIXME: Maybe save the query, actually?
                                                           (setf (cl-gopher:terms line) "")
                                                           `(make-gopher-search-engine
                                                             ,(cl-gopher:uri-for-gopher-line line)
                                                             ,(cl-gopher:display-string line)))
                                                         lines)))))))))

(export-always 'make-gopher-search-engine)
(defun make-gopher-search-engine (url name)
  (let ((line (cl-gopher:parse-gopher-uri url)))
    (setf (cl-gopher:display-string line) name)
    line))

(define-command search-gopher ()
  "Prompt for terms and search those in current page and saved search engines."
  (prompt :prompt "Search Gopher for"
          :sources (list (make-instance 'gopher-search-source))))

;;; Gopher rendering.

(export-always 'line->html)
(defgeneric line->html (line)
  (:documentation "Transform a gopher line to a reasonable HTML representation."))

(export-always 'gopher-render)
(defgeneric gopher-render (line &optional mode)
  (:documentation "Produce a Gopher page content string/array given LINE.
Second return value should be the MIME-type of the content."))

(defmethod line->html ((line cl-gopher:gopher-line))
  (spinneret:with-html-string
    (:pre "[" (symbol-name (class-name (class-of line))) "] "
          (cl-gopher:display-string line)
          " (" (cl-gopher:uri-for-gopher-line line) ")")))

(defmethod line->html ((line cl-gopher:error-code))
  (spinneret:with-html-string
    (:pre :class "error"
          "Error: " (cl-gopher:display-string line))))

(defmethod line->html ((line cl-gopher:info-message))
  (spinneret:with-html-string
    (if (or (str:emptyp (cl-gopher:display-string line))
            (every #'sera:whitespacep (cl-gopher:display-string line)))
        (:br)
        (:pre (cl-gopher:display-string line)))))

(defmethod line->html ((line cl-gopher:submenu))
  (spinneret:with-html-string
    (:a :class "button" :href (cl-gopher:uri-for-gopher-line line)
        (cl-gopher:display-string line))))

(defun image->html (line)
  (spinneret:with-html-string
    (:a :href (cl-gopher:uri-for-gopher-line line)
        (:img :src (cl-gopher:uri-for-gopher-line line)
              :alt (cl-gopher:display-string line)))))

(defmethod line->html ((line cl-gopher:image)) (image->html line))
(defmethod line->html ((line cl-gopher:gif)) (image->html line))
(defmethod line->html ((line cl-gopher:png)) (image->html line))

(defmethod line->html ((line cl-gopher:sound-file))
  (spinneret:with-html-string
    (:audio :src (cl-gopher:uri-for-gopher-line line)
            :controls t
            (cl-gopher:display-string line))))

(defmethod line->html ((line cl-gopher:search-line))
  (spinneret:with-html-string
    (:button :class "button search"
             :onclick (ps:ps (nyxt/ps:lisp-eval `(nyxt/small-web-mode:search-gopher)))
             (:b "[SEARCH] ") (cl-gopher:display-string line))))

(defmethod line->html ((line cl-gopher:html-file))
  (spinneret:with-html-string
    (:a :class "button"
        :href (when (str:starts-with-p "URL:" (cl-gopher:selector line))
                (sera:slice (cl-gopher:selector line) 4))
        (cl-gopher:display-string line))
    (:br)))

(defmethod line->html ((line cl-gopher:text-file))
  (spinneret:with-html-string
    (:a :class "button"
       :href (cl-gopher:uri-for-gopher-line line)
       (cl-gopher:display-string line))
    (:br)))

(defun file-link->html (line)
  (spinneret:with-html-string
    (:a :class "button"
        :style (format nil "background-color: ~a" (theme:primary-color (theme *browser*)))
        :href (cl-gopher:uri-for-gopher-line line)
        (:b "[FILE] ") (cl-gopher:display-string line))
    (:br)))

(defmethod line->html ((line cl-gopher:binary-file)) (file-link->html line))
(defmethod line->html ((line cl-gopher:binhex-file)) (file-link->html line))
(defmethod line->html ((line cl-gopher:dos-file)) (file-link->html line))
(defmethod line->html ((line cl-gopher:uuencoded-file)) (file-link->html line))
(defmethod line->html ((line cl-gopher:unknown)) (file-link->html line))

(defmethod gopher-render ((line cl-gopher:gopher-line) &optional (mode (current-mode 'small-web)))
  (let ((contents (cl-gopher:get-line-contents line))
        (spinneret:*html-style* :tree))
    (spinneret:with-html-string
      (:style (style (buffer mode)))
      (:style (style mode))
      (loop for line in (cl-gopher:lines contents)
            collect (:raw (line->html line))))))

(defmethod gopher-render ((line cl-gopher:html-file) &optional (mode (current-mode 'small-web)))
  (declare (ignore mode))
  (let ((contents (cl-gopher:get-line-contents line)))
    (cl-gopher:content-string contents)))

(defmethod gopher-render ((line cl-gopher:text-file) &optional (mode (current-mode 'small-web)))
  (declare (ignore mode))
  (let ((contents (cl-gopher:get-line-contents line)))
    (values (str:join +newline+ (cl-gopher:lines contents)) "text/plain")))

(defun render-binary-content (line &optional mime)
  (let* ((url (quri:uri (cl-gopher:uri-for-gopher-line line)))
         (file (pathname (quri:uri-path url)))
         (mime (or mime (mimes:mime file)))
         (contents (cl-gopher:get-line-contents line)))
    (values (cl-gopher:content-array contents) mime)))

(defmethod gopher-render ((line cl-gopher:image) &optional (mode (current-mode 'small-web)))
  (declare (ignore mode))
  (render-binary-content line))

(defmethod gopher-render ((line cl-gopher:binary-file) &optional (mode (current-mode 'small-web)))
  (declare (ignore mode))
  (render-binary-content line))

(defmethod gopher-render ((line cl-gopher:binhex-file) &optional (mode (current-mode 'small-web)))
  (declare (ignore mode))
  (render-binary-content line))

(defmethod gopher-render ((line cl-gopher:dos-file) &optional (mode (current-mode 'small-web)))
  (declare (ignore mode))
  (render-binary-content line))

(defmethod gopher-render ((line cl-gopher:uuencoded-file) &optional (mode (current-mode 'small-web)))
  (declare (ignore mode))
  (render-binary-content line))

(defmethod gopher-render ((line cl-gopher:gif) &optional (mode (current-mode 'small-web)))
  (declare (ignore mode))
  (render-binary-content line "image/gif"))

(defmethod gopher-render ((line cl-gopher:png) &optional (mode (current-mode 'small-web)))
  (declare (ignore mode))
  (render-binary-content line "image/png"))

;;; Gemini rendering.

(defmethod line->html ((element gemtext:element))
  (spinneret:with-html-string
    (:pre (gemtext:text element))))

(defmethod line->html ((element gemtext:paragraph))
  (spinneret:with-html-string
    (:p (gemtext:text element))))

(defmethod line->html ((element gemtext:title))
  (spinneret:with-html-string
    (case (gemtext:level element)
      (1 (:h1 (gemtext:text element)))
      (2 (:h2 (gemtext:text element)))
      (3 (:h3 (gemtext:text element))))))

;; TODO: We used to build <ul>-lists out of those. Should we?
(defmethod line->html ((element gemtext:item))
  (spinneret:with-html-string
    (:li (gemtext:text element))))

(defmethod line->html ((element gemtext:link))
  (spinneret:with-html-string
    (let* ((url (render-url (gemtext:url element)))
           (path (quri:uri-path (gemtext:url element)))
           (mime (unless (uiop:emptyp path)
                   (mimes:mime-lookup path)))
           (text (cond
                   ((not (uiop:emptyp (gemtext:text element)))
                    (gemtext:text element))
                   ((not (uiop:emptyp url))
                    url)
                   (t "[LINK]"))))
      (cond
        ((str:starts-with-p "image/" mime)
         (:a :href url (:img :src url :alt text)))
        ((str:starts-with-p "audio/" mime)
         (:audio :src url :controls t text))
        ((str:starts-with-p "video/" mime)
         (:video :src url :controls t))
        (t (:a :class "button" :href url text))))
    (:br)))
