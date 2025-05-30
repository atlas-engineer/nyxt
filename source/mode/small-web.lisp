;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/small-web
  (:documentation "Package for `small-web-mode', which powers Gopher/Gemini page interaction."))
(in-package :nyxt/mode/small-web)

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
\(defmethod line->html ((line cl-gopher:gif)) (image->link line))
\(defmethod line->html ((line cl-gopher:png)) (image->link line))

Gemini support is a bit more brittle, but you can override `line->html' for
`phos/gemtext' elements too."
  ((visible-in-status-p nil)
   (url :documentation "The URL being opened.")
   (model :documentation "The contents of the current page.")
   (redirections nil :documentation "The list of redirection Gemini URLs.")
   (max-redirections
    5
    :documentation "The maximum number of times a redirection is attempted.")
   (style (theme:themed-css (nyxt::theme *browser*)
            `(body
              :background-color ,theme:background-color)
            `(pre
              :background-color ,theme:secondary-color
              :padding "2px"
              :margin "0"
              :border-radius 0)
            '(.button
              :margin "0 3px 3px 0"
              :font-size "15px")
            `(.search
              :background-color ,theme:action-color
              :color ,theme:on-action-color)
            `(.error
              :background-color ,theme:warning-color
              :color ,theme:on-warning-color
              :padding "1em 0"))))
  (:toggler-command-p nil))

;;; Gopher rendering.

(defmethod cl-gopher:display-string :around ((line cl-gopher:gopher-line))
  (cl-ppcre:regex-replace-all "\\e\\[[\\d;]*[A-Za-z]"
                              (slot-value line 'cl-gopher:display-string)
                              ""))

(export-always 'line->html)
(defgeneric line->html (line)
  (:documentation "Transform a Gopher or Gemini line to a reasonable HTML representation."))

(export-always 'gopher-render)
(defgeneric gopher-render (line)
  (:documentation "Produce a Gopher page content string/array given LINE.
Second return value should be the MIME-type of the content.

Implies that `small-web-mode' is enabled."))

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
  (let ((line (cl-gopher:display-string line)))
    (spinneret:with-html-string
      (if (str:blankp line)
          (:br)
          (:pre line)))))

(defmethod line->html ((line cl-gopher:submenu))
  (spinneret:with-html-string
    (:a :class "button" :href (cl-gopher:uri-for-gopher-line line)
        (cl-gopher:display-string line))))

(defun image->html (line)
  (let ((uri (cl-gopher:uri-for-gopher-line line)))
    (spinneret:with-html-string
      (:a :href uri
          (:img :src uri
                :alt (cl-gopher:display-string line))))))

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
  (let ((selector (cl-gopher:selector line)))
    (spinneret:with-html-string
      (:a :class "button"
          :href (if (str:starts-with-p "URL:" selector)
                    (sera:slice selector 4)
                    selector)
          (cl-gopher:display-string line))
      (:br))))

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

(defmethod gopher-render ((line cl-gopher:gopher-line))
  (when-let ((contents (cl-gopher:get-line-contents line))
             (spinneret:*html-style* :tree)
             (mode (find-submode 'small-web-mode)))
    (setf (model mode) contents)
    (values (spinneret:with-html-string
              (:nstyle (style (current-buffer)))
              (:nstyle (style mode))
              (loop for line in (cl-gopher:lines contents)
                    collect (:raw (line->html line))))
            "text/html;charset=utf8")))

(defmethod gopher-render ((line cl-gopher:html-file))
  (let ((contents (cl-gopher:get-line-contents line)))
    (values (cl-gopher:content-string contents) "text/html;charset=utf8")))

(defmethod gopher-render ((line cl-gopher:text-file))
  (let ((contents (cl-gopher:get-line-contents line)))
    ;; TODO: Guess encoding?
    (values (str:join +newline+ (cl-gopher:lines contents)) "text/plain;charset=utf8")))

(defun render-binary-content (line &optional mime)
  (let* ((url (quri:uri (cl-gopher:uri-for-gopher-line line)))
         (file (pathname (quri:uri-path url)))
         (mime (or mime (mimes:mime file)))
         (contents (cl-gopher:get-line-contents line)))
    (values (cl-gopher:content-array contents) mime)))

(defmethod gopher-render ((line cl-gopher:image)) (render-binary-content line))
(defmethod gopher-render ((line cl-gopher:binary-file)) (render-binary-content line))
(defmethod gopher-render ((line cl-gopher:binhex-file)) (render-binary-content line))
(defmethod gopher-render ((line cl-gopher:dos-file)) (render-binary-content line))
(defmethod gopher-render ((line cl-gopher:uuencoded-file)) (render-binary-content line))
(defmethod gopher-render ((line cl-gopher:gif)) (render-binary-content line "image/gif"))
(defmethod gopher-render ((line cl-gopher:png)) (render-binary-content line "image/png"))

(define-internal-scheme "gopher"
    (lambda (url)
      (handler-case
          (let ((line (if (uiop:emptyp (quri:uri-path (quri:uri url)))
                          (ffi-buffer-load (current-buffer) (str:concat url "/"))
                          (cl-gopher:parse-gopher-uri url))))
            (enable-modes :modes '(small-web-mode))
            (if (and (typep line 'cl-gopher:search-line)
                     (uiop:emptyp (cl-gopher:terms line)))
                (progn (setf (cl-gopher:terms line)
                             (prompt1 :prompt (format nil "Search query for ~a" url)
                                      :sources 'prompter:raw-source))
                       (ffi-buffer-load (current-buffer) (cl-gopher:uri-for-gopher-line line)))
                (with-current-buffer (current-buffer) (gopher-render line))))
        (cl-gopher:bad-submenu-error ()
          (error-help (format nil "Malformed line at ~s" url)
                      (format nil "One of the lines on this page has an improper format.
Please report this to the server admin.")))
        (cl-gopher:bad-uri-error ()
          (error-help (format nil "Malformed URL: ~s" url)
                      (format nil "The URL you inputted most probably has a typo in it.
Please, check URL correctness and try again.")))
        (condition (condition)
          (error-help "Unknown error"
                      (format nil "Original text of ~a:~%~a" (type-of condition) condition))))))

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

(export-always 'gemtext-render)
(defun gemtext-render (gemtext &optional (buffer (current-buffer)))
  "Renders the Gemtext (Gemini markup format) to HTML.

Implies that `small-web-mode' is enabled."
  (let ((mode (find-submode 'small-web-mode buffer))
        (elements (phos/gemtext:parse-string gemtext))
        (spinneret::*html-style* :tree))
    (setf (model mode) elements)
    (values (spinneret:with-html-string
              (:nstyle (style buffer))
              (when mode
                (:nstyle (style mode)))
              (loop for element in elements
                    collect (:raw (nyxt/mode/small-web:line->html element))))
            "text/html;charset=utf8")))

(define-internal-scheme "gemini"
    (lambda (url)
      (handler-case
          (sera:mvlet* ((status meta body (gemini:request url)))
            (enable-modes :modes '(small-web-mode))
            (unless (member status '(:redirect :permanent-redirect))
                (setf (nyxt/mode/small-web:redirections (find-submode 'small-web-mode)) nil))
              (case status
                ((:input :sensitive-input)
                 (let ((text (quri:url-encode
                              (handler-case
                                  (prompt1 :prompt meta
                                           :sources 'prompter:raw-source
                                           :height :fit-to-prompt
                                           :invisible-input-p (eq status :sensitive-input))
                                (prompt-buffer-canceled () "")))))
                   (ffi-buffer-load (current-buffer) (str:concat url "?" text))
                   nil))
                (:success
                 (if (str:starts-with-p "text/gemini" meta)
                     (gemtext-render body)
                     (values body meta)))
                ((:redirect :permanent-redirect)
                 (push url (nyxt/mode/small-web:redirections (find-submode 'small-web-mode)))
                 (if (< (length (nyxt/mode/small-web:redirections (find-submode 'small-web-mode)))
                        (nyxt/mode/small-web:max-redirections (find-submode 'small-web-mode)))
                     (progn (ffi-buffer-load (current-buffer) (quri:merge-uris (quri:uri meta) (quri:uri url))) nil)
                     (error-help
                      "Error"
                      (format nil "The server has caused too many (~a+) redirections.~& ~a~{ -> ~a~}"
                              (nyxt/mode/small-web:max-redirections (find-submode 'small-web-mode))
                              (alex:lastcar (nyxt/mode/small-web:redirections (find-submode 'small-web-mode)))
                              (butlast (nyxt/mode/small-web:redirections (find-submode 'small-web-mode)))))))
                ((:temporary-failure :server-unavailable :cgi-error :proxy-error
                  :permanent-failure :not-found :gone :proxy-request-refused :bad-request)
                 (error-help "Error" meta))
                (:slow-down
                 (error-help
                  "Slow down error"
                  (format nil "Try reloading the page in ~a seconds." meta)))
                ((:client-certificate-required :certificate-not-authorised :certificate-not-valid)
                 (error-help "Certificate error" meta))))
        (gemini::malformed-response (e)
          (error-help
           "Malformed response"
           (format nil "The response for the URL you're requesting (~s) is malformed:~2%~a" url e)))
        (condition (condition)
          (error-help "Unknown error"
                      (format nil "Original text of ~a:~%~a" (type-of condition) condition))))))
