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
       (make-instance
        'hooks:handler
        :fn (lambda (request-data)
              (when (nyxt/auto-mode::new-page-request-p request-data)
                (if (str:s-member '("gopher" "gemini")
                                  (quri:uri-scheme (url request-data)))
                    (update mode)
                    (disable-modes '(small-web-mode) (buffer mode))))
              request-data)
        :name 'small-web-mode-disable))))))

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
    (:a :class "button search"
        :href (cl-gopher:uri-for-gopher-line line)
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

;; TODO: :display-isolated-p?
(define-internal-scheme ("gopher")
  (let* ((line (cl-gopher:parse-gopher-uri %url%)))
    ;; FIXME: This better become a default auto-mode rule.
    (enable-modes '(small-web-mode) %buffer%)
    (if (and (typep line 'cl-gopher:search-line)
             (uiop:emptyp (cl-gopher:terms line)))
        (progn (setf (cl-gopher:terms line)
                     (prompt1
                       :prompt (format nil "Search query for ~a" %url%)
                       :sources (list (make-instance 'prompter:raw-source))))
               (buffer-load (cl-gopher:uri-for-gopher-line line) :buffer %buffer%))
        (nyxt/small-web-mode:gopher-render line))))

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

;; TODO: :secure-p t?
(define-internal-scheme ("gemini")
  (block process-gemini-scheme
    (flet ((make-gemini-error-page (title text)
             (spinneret:with-html-string
               (:h1 title)
               (:pre text))))
      (sera:mvlet* ((status meta body (handler-case
                                          (gemini:request %url%)
                                        (gemini::malformed-response (e)
                                          (return-from process-gemini-scheme
                                            (make-gemini-error-page
                                             "Malformed response"
                                             (format nil "The response for the URL you're requesting (~s) is malformed.

~a"
                                                     %url% e)))))))
        ;; FIXME: This better become a default auto-mode rule.
        (enable-modes '(small-web-mode) %buffer%)
        (unless (member status '(:redirect :permanent-redirect))
          (setf (nyxt/small-web-mode:redirections (current-mode 'small-web)) nil))
        (case status
          ((:input :sensitive-input)
           (let ((text (quri:url-encode
                        (handler-case
                            (prompt1 :prompt meta
                              :sources (list (make-instance 'prompter:raw-source))
                              :invisible-input-p (eq status :sensitive-input))
                          (nyxt::nyxt-prompt-buffer-canceled () "")))))
             (buffer-load (str:concat %url% "?" text) :buffer %buffer%)))
          (:success
           (if (str:starts-with-p "text/gemini" meta)
               (let ((mode (current-mode 'small-web))
                     (elements (phos/gemtext:parse-string body))
                     (spinneret::*html-style* :tree))
                 (spinneret:with-html-string
                   (:style (style %buffer%))
                   (:style (style mode))
                   (loop for element in elements
                         collect (:raw (nyxt/small-web-mode:line->html element)))))
               (values body meta)))
          ((:redirect :permanent-redirect)
           (push %url% (nyxt/small-web-mode:redirections (current-mode 'small-web)))
           (if (< (length (nyxt/small-web-mode:redirections (current-mode 'small-web)))
                  (nyxt/small-web-mode:allowed-redirections-count (current-mode 'small-web)))
               (buffer-load (quri:merge-uris (quri:uri meta) (quri:uri %url%)) :buffer %buffer%)
               (make-gemini-error-page
                "Error"
                (format nil "The server has caused too much (~a+) redirections.~& ~a~{ -> ~a~}"
                        (nyxt/small-web-mode:allowed-redirections-count (current-mode 'small-web))
                        (alex:lastcar (nyxt/small-web-mode:redirections (current-mode 'small-web)))
                        (butlast (nyxt/small-web-mode:redirections (current-mode 'small-web)))))))
          ((:temporary-failure :server-unavailable :cgi-error :proxy-error
            :permanent-failure :not-found :gone :proxy-request-refused :bad-request)
           (make-gemini-error-page "Error" meta))
          (:slow-down
           (make-gemini-error-page
            "Slow down error"
            (format nil "Try reloading the page in ~a seconds." meta)))
          ((:client-certificate-required :certificate-not-authorised :certificate-not-valid)
           (make-gemini-error-page "Certificate error" meta)))))))
