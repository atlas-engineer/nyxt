;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class panel-buffer (input-buffer modable-buffer document-buffer network-buffer)
  ((width 256 :documentation "The width in pixels.")
   (style (theme:themed-css (theme *browser*)
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "400" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Regular.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "400" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Italic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "100" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Thin.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "100" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ThinItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "200" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraLight.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "200" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraLightItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "300" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Light.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "300" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-LightItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "500" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Medium.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "500" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-MediumItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "600" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-SemiBold.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "600" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-SemiBoldItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "700" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Bold.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "700" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-BoldItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "800" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraBold.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "800" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-ExtraBoldItalic.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "normal" :font-weight "900" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-Black.woff") "format('woff')")
            `(:font-face :font-family "public sans" :font-style "italic" :font-weight "900" :src ,(format nil "url('nyxt-resource:~a')" "PublicSans-BlackItalic.woff") "format('woff')")
            `(:font-face :font-family "dejavu sans mono" :src ,(format nil "url('nyxt-resource:~a')" "DejaVuSansMono.ttf") "format('ttf')")
            `(body
              :background-color ,theme:background-alt
              :color ,theme:on-background-alt
              :font-family ,theme:font-family
              :margin "0"
              :padding "10px"
              :padding-top "24px"
              :border-style "solid"
              :border-width "0px 1px"
              :border-color ,theme:secondary)
            `("h1,h2,h3,h4,h5,h6"
              :font-family ,theme:font-family
              :font-weight 500)
            `(a
              :color ,theme:primary)
            `("details summary"
              :margin-left "inherit"
              :margin-bottom "8px"
              :cursor "pointer")
            `("summary::-webkit-details-marker"
              :padding-bottom "4px")
            '("details > summary"
              :list-style-type "none")
            '("details > summary::-webkit-details-marker"
              :display "none")
            '("details > summary::before"
              :font-weight "bold"
              :content "+"
              :margin-right "5px"
              :display "inline-block")
            '("details[open] > summary::before"
              :content "−")
            `(pre
              :font-family ,theme:monospace-font-family
              :font-size "0.9rem")
            `(code
              :font-family ,theme:monospace-font-family
              :font-size "0.9rem")
            `(dt
              :font-weight bold)
            `(dd
              :margin-inline-start 1em
              :font-size xx-small)
            `((:and a :hover)
              :cursor "pointer"
              :text-decoration "underline")
            `((:and a :active)
              :opacity 0.6)
            `("#close"
              :position "fixed"
              :top "4px"
              :right "4px"
              :line-height "12px")
            `(button
              :background "transparent"
              :max-width "100%"
              :color "inherit"
              :border "none"
              :padding 0
              :font "inherit"
              :outline "inherit")
            `(.button
              :background-color ,theme:primary
              :color ,theme:on-primary
              :display "inline-block"
              :text-decoration "none"
              :border-radius "2px"
              :padding "6px"
              :margin "2px")
	        `(.action
              :color ,theme:action)
	        `(.button.action
              :background-color ,theme:action
              :color ,theme:on-action
              :border-color ,theme:action+)
            `(.warning
              :color ,theme:warning)
	        `(.button.warning
	          :background-color ,theme:warning
	          :color ,theme:on-warning
	          :border-color ,theme:warning+)
            `(.success
              :color ,theme:success)
	        `(.button.success
              :background-color ,theme:success
              :color ,theme:on-success
              :border-color ,theme:success+)
            `(.highlight
              :color ,theme:highlight)
	        `(.button.highlight
	          :background-color ,theme:highlight
	          :color ,theme:on-highlight
	          :border-color ,theme:highlight+)
            `((:and .button :hover)
              :cursor "pointer"
              :opacity 0.8)
            `((:and .button (:or :visited :active))
              :color ,theme:background)
            `("a:visited"
              :color ,theme:secondary)
            `(".progress-bar-container"
              :border-radius "3px"
              :height "20px"
              :width "100%")
            `(".progress-bar-base"
              :border-radius "3px"
              :background-color ,theme:secondary
              :height "100%")
            `(".progress-bar-fill"
              :border-radius "3px"
              :background-color ,theme:primary
              :height "100%"))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Panel buffer (also known as sidebar): small view on the side of the screen.

Panels (pages openable in panel buffer with respective commands) are defined
with `define-panel-command' and `define-panel-command-global'.

Also see `panel-page'."))

;; TODO: Quite some code could be factored with `internal-page'.

(define-class panel-buffer-source (prompter:source)
  ((prompter:name "Panel buffers")
   (window :accessor window :initarg :window)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:enable-marks-p t)
   (prompter:constructor (lambda (source)
                           (panel-buffers (window source))))))

(define-command-global delete-panel-buffer (&key (window (current-window))
                                            (panels (prompt
                                                     :prompt "Delete a panel buffer"
                                                     :sources (make-instance 'panel-buffer-source
                                                                             :window window))))
  "Prompt for panel buffer(s) to be deleted.
When provided, PANELS are deleted instead."
  (mapc (curry #'window-delete-panel-buffer window) (uiop:ensure-list panels)))

(define-command-global delete-all-panel-buffers (&key (window (current-window)))
  "Delete all the open panel buffers in WINDOW."
  (delete-panel-buffer :panels (panel-buffers window)))

(define-class panel-page (internal-page)
  ((side
    :left
    :type (member :left :right)
    :documentation "The side of the window where the panel is displayed."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Internal page for `panel-buffers'.
The main difference from `internal-page' is that panel command toggles the
panel."))

(defun find-panel-buffer (name)
  "Return first panel buffer which URL is a NAME `panel-page'."
  (find name (panel-buffers (current-window))
        :key (alex:compose #'internal-page-name #'url)))

(defmethod set-internal-page-method ((page panel-page) form)
  (when form
    (let* ((arglist (second form))
           (keywords (nth-value 3 (alex:parse-ordinary-lambda-list arglist)))
           (body (cddr form))
           (documentation (nth-value 2 (alex:parse-body body :documentation t))))
      (closer-mop:ensure-method
       page
       `(lambda (,@arglist)
          ,@(when documentation (list documentation))
          (declare (ignorable ,@(mappend #'cdar keywords)))
          (alex:if-let ((panel-buffer (find-panel-buffer (name ,page))))
            (window-delete-panel-buffer (current-window) panel-buffer)
            (window-add-panel-buffer
             (current-window)
             (buffer-load (nyxt-url (name ,page) ,@(mappend #'first keywords))
                          :buffer (make-instance 'panel-buffer))
             (side ,page))))))))

;; FIXME: Better way to compose HTML wrappers?
(defmethod (setf form) :after (lambda-expression (page panel-page))
  (declare (ignore lambda-expression))
  (let ((original-form (slot-value page 'form)))
    (setf (slot-value page 'form)
          (lambda (&rest args)
            (destructuring-bind (contents &optional (type "text/html;charset=utf8") (status 200)
                                            headers reason)
                (multiple-value-list (apply original-form args))
              (when (str:starts-with-p "text/html" type)
                (setf contents
                      (spinneret:with-html-string
                        (:raw contents)
                        (let ((buffer (find-panel-buffer (name page))))
                          (:button.button
                           :id "close"
                           :title "Close this panel buffer"
                           :onclick (ps:ps (nyxt/ps:lisp-eval
                                            (:title "panel close button"
                                             :buffer buffer)
                                            (window-delete-panel-buffer
                                             (current-window) buffer)))
                           "×")))))
              (values contents type status headers reason))))))

;; TODO: Add define-panel?

(export-always 'define-panel-command)
(defmacro define-panel-command (name (&rest arglist)
                                (buffer-var title &optional (side :left))
                                &body body)
  "Define a panel buffer and:
- A local command called NAME, creating this panel-buffer or closing it if it's shown already.
- A nyxt:NAME URL for the content of this panel buffer.

Should end with a form returning HTML as a string.

BUFFER-VAR is the variable the created panel will be bound to in the BODY. SIDE
is either :LEFT (default) or :RIGHT.

ARGLIST is arguments for the command and for the underlying page-generating
function. Any argument from it is safe to use in the body of this macro.
Beware: the ARGLIST should have nothing but keyword arguments because it's
mapped to query parameters."
  (multiple-value-bind (stripped-body declarations documentation)
      (alex:parse-body body :documentation t)
    `(progn
       (export-always ',name (symbol-package ',name))
       (sera:lret ((gf (defgeneric ,name (,@(generalize-lambda-list arglist))
                         ,@(when documentation
                             `((:documentation ,documentation)))
                         (:generic-function-class panel-page))))
         (let ((wrapped-body '(lambda (,@arglist)
                               ,@(when documentation (list documentation))
                               ,@declarations
                               (let ((,buffer-var (find-panel-buffer ',name)))
                                 (declare (ignorable ,buffer-var))
                                 ,@stripped-body))))
           (set-internal-page-method gf wrapped-body)
           (setf (slot-value #',name 'visibility) :mode)
           (setf (slot-value #',name 'dynamic-title)
                 ,(if (stringp title)
                      title
                      (let ((keywords (nth-value 3 (alex:parse-ordinary-lambda-list arglist))))
                        `(lambda (,@arglist)
                           (declare (ignorable ,@(mappend #'cdar keywords)))
                           ,title))))
           (setf (slot-value #',name 'side) ,side)
           (setf (form gf) wrapped-body))))))

(export-always 'define-panel-command-global)
(defmacro define-panel-command-global (name (&rest arglist)
                                       (buffer-var title &optional (side :left))
                                       &body body)
  "Define a panel buffer with a global command showing it.

See `define-panel-command' for the description of the arguments."
  `(prog1 (define-panel-command ,name (,@arglist) (,buffer-var ,title ,side) ,@body)
     (setf (slot-value #',name 'visibility) :global)))
