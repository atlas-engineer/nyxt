;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class panel-buffer (web-buffer)
  ((window
    nil
    :type (maybe window)
    :documentation "The window this panel is attached to.")
   (width
    320
    :type integer
    :export nil
    :documentation "Width in pixels while the panel is shown.")
   (side
    :left
    :type (member :left :right)
    :documentation "Which edge of the page the panel occupies.")
   (visible-p
    nil
    :type boolean
    :documentation "Whether the panel is shown."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Buffer fixed to the left or right of the page.
A window shows any number of them. Each is as tall as the page and shows one thing."))

(defmethod include-in-buffer-list-p ((buffer panel-buffer))
  "A panel is not one of the window's tabs."
  nil)

(defmethod (setf width) :around (value (panel panel-buffer))
  (call-next-method (clamp-panel-width panel value) panel))

(defmethod (setf width) :after (value (panel panel-buffer))
  (declare (ignore value))
  (when (and (visible-p panel) (window panel))
    (apply-panel-geometry panel)))

(defmethod on-signal-notify-title ((buffer panel-buffer) title)
  "Keep the title given to BUFFER.
A page loaded in the panel does not replace it."
  (declare (ignore title))
  (when (str:emptyp (title buffer))
    (setf (title buffer) (ffi-buffer-title buffer)))
  (dolist (mode (enabled-modes buffer))
    (on-signal-notify-title mode (title buffer)))
  (title buffer))

(-> clamp-panel-width (panel-buffer integer) integer)
(defun clamp-panel-width (panel width)
  "Return WIDTH limited to the range the panel can occupy."
  (let* ((window-width (or (ignore-errors
                            (and (window panel)
                                 (plusp (ffi-width (window panel)))
                                 (ffi-width (window panel))))
                           800))
         (ceiling (max 160 (min 640 (floor window-width 2)))))
    (max 160 (min width ceiling))))

(-> apply-panel-geometry (panel-buffer) t)
(defun apply-panel-geometry (panel)
  "Place PANEL on its window at its current side and width."
  (when (window panel)
    (if (visible-p panel)
        (ffi-add-panel-buffer (window panel) panel (side panel) (width panel))
        (ffi-remove-panel-buffer (window panel) panel))
    (apply-page-container (window panel)))
  panel)

(-> focused-panel (window) (maybe panel-buffer))
(defun focused-panel (window)
  "Return the panel in WINDOW with keyboard focus, or the one most recently added."
  (let ((keyboard (keyboard-buffer window)))
    (if (and (panel-buffer-p keyboard)
             (find keyboard (panel-buffers window)))
        keyboard
        (first (last (panel-buffers window))))))

(export-always 'add-panel-buffer)
(-> add-panel-buffer (window panel-buffer) (values panel-buffer &optional))
(defun add-panel-buffer (window panel)
  "Show PANEL beside WINDOW's page."
  (setf (window panel) window)
  (unless (find panel (panel-buffers window))
    (setf (panel-buffers window) (append (panel-buffers window) (list panel))))
  (setf (visible-p panel) t)
  (apply-panel-geometry panel)
  panel)

(export-always 'open-url-from-panel)
(-> open-url-from-panel (panel-buffer t) t)
(defun open-url-from-panel (panel url)
  "Load URL in the page beside PANEL and focus that page."
  (when-let* ((window (window panel))
              (page (buffer window)))
    (unless (or (eq page panel)
                (panel-buffer-p page)
                (dead-buffer-p page))
      (ffi-buffer-load page (url url))
      (ffi-focus-buffer page)))
  nil)

(export-always 'define-buffer-panel)
(defmacro define-buffer-panel (name initargs &body body)
  "Define a `panel-buffer' subclass NAME and the commands that open it.
INITARGS are passed to `make-instance'. `:side' is set by the command.
The first string in BODY documents the class.
The commands are `add-NAME-left' and `add-NAME-right'. Each takes WINDOW.
BODY runs with PANEL bound to the new panel and WINDOW bound to that argument.

Example:

\(define-buffer-panel buffer-listing-panel (:title \"Buffers\")
  \"Panel that lists the open buffers.\"
  (ffi-buffer-load panel (quri:uri (nyxt-url 'list-buffers))))"
  (unless (and (symbolp name) (not (keywordp name)))
    (error "Panel name must be a symbol, got ~s." name))
  (unless (and (listp initargs) (evenp (length initargs)))
    (error "Initargs for panel ~a must be a plist, got ~s." name initargs))
  (multiple-value-bind (forms declarations documentation)
      (alex:parse-body body :documentation t)
    (unless documentation
      (error "Panel ~a requires documentation." name))
    (flet ((command-name (suffix)
             (without-package-locks
               (intern (str:concat "ADD-" (symbol-name name) suffix)
                       (symbol-package name))))
           (command-documentation (side)
             (let* ((base (string-right-trim ". " documentation))
                    (base (if (uiop:string-prefix-p "Panel" base)
                              (str:concat "Add a panel"
                                          (subseq base (length "Panel")))
                              base)))
               (format nil "~a on the ~a of WINDOW."
                       base
                       (string-downcase (symbol-name side)))))
           (symbol-from-caller (string)
             (without-package-locks
               (or (find-symbol string *package*)
                   (loop for package in (package-use-list *package*)
                         do (multiple-value-bind (symbol status)
                                (find-symbol string package)
                              (when (eq status :external)
                                (return symbol))))
                   (intern string *package*)))))
      (alex:with-gensyms (side)
        (let ((left (command-name "-LEFT"))
              (right (command-name "-RIGHT"))
              (helper (without-package-locks
                       (intern (str:concat "%ADD-" (symbol-name name))
                               (symbol-package name))))
              (panel-var (symbol-from-caller "PANEL"))
              (window-var (symbol-from-caller "WINDOW")))
          `(progn
             (eval-always
               (define-class ,name (panel-buffer)
                 ()
                 (:export-class-name-p t)
                 (:export-accessor-names-p t)
                 (:export-predicate-name-p t)
                 (:metaclass user-class)
                 (:documentation ,documentation)))
             (-> ,helper (window (member :left :right)) (values ,name &optional))
             (defun ,helper (,window-var ,side)
               ,(format nil "Open a `~(~a~)' on SIDE of WINDOW." name)
               (let ((,panel-var (add-panel-buffer ,window-var
                                                   (make-instance ',name
                                                                  :side ,side
                                                                  ,@initargs))))
                 ,@declarations
                 ,@forms
                 ,panel-var))
             (define-command-global ,left ((,window-var window))
               ,(command-documentation :left)
               (,helper ,window-var :left))
             (define-command-global ,right ((,window-var window))
               ,(command-documentation :right)
               (,helper ,window-var :right))))))))

(define-mode panel-mode ()
  "Base mode for `panel-buffer'.
Bind panel resizing, closing, reloading, and command execution without the
navigation bindings of `base-mode'."
  ((visible-in-status-p nil)
   (width-step
    32
    :type integer
    :documentation "Pixels added or removed by `widen-panel' and `narrow-panel'.")
   (keyscheme-map
    (define-keyscheme-map "panel-mode" ()
      keyscheme:default
      (list
       "+" 'widen-panel
       "hyphen" 'narrow-panel
       "C-r" 'reload-current-buffer
       "M-o" 'toggle-prompt-buffer-focus
       "f1 b" 'describe-bindings
       "C-space" 'execute-command)
      keyscheme:cua
      (list
       "f5" 'reload-current-buffer
       "C-w" 'delete-panel-buffer)
      #+darwin keyscheme:cua
      #+darwin
      (list
       "S-r" 'reload-current-buffer
       "S-w" 'delete-panel-buffer)
      keyscheme:emacs
      (list
       "C-x o" 'toggle-prompt-buffer-focus
       "C-x C-k" 'delete-panel-buffer
       "C-h b" 'describe-bindings
       "M-x" 'execute-command)
      keyscheme:vi-normal
      (list
       "R" 'reload-current-buffer
       "D" 'delete-panel-buffer
       ":" 'execute-command))))
  (:toggler-command-p nil))

(defparameter %panel-excluded-modes
  '(:base-mode
    :document-mode
    :hint-mode
    :search-buffer-mode
    :spell-check-mode
    :autofill-mode
    :history-mode
    :bookmark-mode
    :annotate-mode
    :password-mode
    :authenticator-mode)
  "Modes inherited from `web-buffer' that `panel-buffer' omits by default.")

(defmethod default-modes append ((buffer panel-buffer))
  '(panel-mode))

(defmethod default-modes :around ((buffer panel-buffer))
  (let ((excluded (delete nil (mapcar (rcurry #'sym:resolve-symbol :mode)
                                      %panel-excluded-modes))))
    (remove-if (rcurry #'member excluded) (call-next-method))))

(-> panel-width-step (panel-buffer) integer)
(defun panel-width-step (panel)
  "Return the number of pixels PANEL's `panel-mode' resizes by."
  (alex:if-let ((mode (find-if #'panel-mode-p (enabled-modes panel))))
    (width-step mode)
    32))

(define-command widen-panel ((panel panel-buffer))
  "Widen PANEL by one step."
  (incf (width panel) (panel-width-step panel)))

(define-command narrow-panel ((panel panel-buffer))
  "Narrow PANEL by one step."
  (decf (width panel) (panel-width-step panel)))

(-> panel-resize-callback (panel-buffer (member :widen :narrow)) string)
(defun panel-resize-callback (panel direction)
  "Return a live `lisp' URL that resizes PANEL in DIRECTION."
  (let ((name (if (eq direction :widen) "widen-panel" "narrow-panel")))
    (quri:render-uri
     (lisp-url :id name
               :buffer panel
               :title name
               :callback (lambda ()
                           (if (eq direction :widen)
                               (widen-panel panel)
                               (narrow-panel panel)))))))

(-> panel-close-callback (panel-buffer) string)
(defun panel-close-callback (panel)
  "Return a live `lisp' URL that closes PANEL."
  (quri:render-uri
   (lisp-url :id "close-panel"
             :buffer panel
             :title "close-panel"
             :callback (lambda ()
                         (delete-panel-buffer panel)))))

(-> panel-chrome-css (theme:theme) string)
(defun panel-chrome-css (theme)
  "Return the stylesheet for a panel's header and buttons."
  (theme:themed-css theme
    '(html
      :background-color "transparent"
      :height "100%")
    `(body
      :box-sizing "border-box"
      :background-color ,theme:surface-color
      :margin "0"
      :min-height "100vh")
    `("#nyxt-panel-header"
      :position "fixed"
      :top "4px"
      :left "0"
      :right "0"
      :z-index "6"
      :box-sizing "border-box"
      :display "flex"
      :align-items "flex-start"
      :justify-content "space-between"
      :gap "7px"
      :padding "24px" "24px" "0"
      :background-color "transparent"
      :color ,(theme:on-background-color theme)
      :font-family ,(format nil "~s" (theme:font-family theme)))
    '("#nyxt-panel-title"
      :min-width "0"
      :margin-top "-3px"
      :overflow "hidden"
      :white-space "nowrap"
      :text-overflow "ellipsis"
      :font-size "17px"
      :font-weight "600"
      :line-height "24px")
    '("#nyxt-panel-actions"
      :display "flex"
      :flex "none"
      :align-items "center"
      :gap "2px"
      :margin "-6px" "0" "0" "0")
    `(".nyxt-panel-button"
      :flex "none"
      :box-sizing "border-box"
      :appearance "none"
      :display "inline-flex"
      :align-items "center"
      :justify-content "center"
      :width "28px"
      :height "28px"
      :margin "0"
      :padding "0"
      :border "none"
      :border-radius "14px"
      :background-color "transparent"
      :color ,(theme:on-background-color theme)
      :font-size "17px"
      :font-weight "400"
      :line-height "17px"
      :cursor "pointer")
    `(".nyxt-panel-button:hover"
      :background-color ,(theme:background-color- theme))
    `(".nyxt-panel-button:focus-visible"
      :outline "2px" "solid" ,(theme:action-color theme)
      :outline-offset "2px")))

(-> panel-content-css (theme:theme) string)
(defun panel-content-css (theme)
  "Return the stylesheet for the page a panel shows.
Row names are 13px, as on the buffer list, over an 11px address. A section
heading matches the panel title at 17px."
  (theme:themed-css theme
    '("ul.list a span:first-of-type"
      :font-size "13px"
      :line-height "24px")
    `("ul.list a span:last-of-type"
      :font-size "11px"
      :line-height "17px"
      :color ,theme:primary-color)
    '("h2"
      :font-size "17px"
      :font-weight "600"
      :line-height "24px")
    `("p.empty"
      :margin "0"
      :font-size "17px"
      :line-height "24px"
      :color ,theme:primary-color)))

(defmethod style :around ((panel panel-buffer))
  (uiop:strcat (call-next-method)
               (if (and *browser* (theme *browser*))
                   (uiop:strcat (panel-chrome-css (theme *browser*))
                                (panel-content-css (theme *browser*)))
                   "")))

(-> panel-chrome-script (panel-buffer string string string theme:theme) string)
(defun panel-chrome-script (panel widen-url narrow-url close-url theme)
  "Return JavaScript that installs PANEL's title and buttons."
  (ps:ps
    ((lambda ()
       (let ((widen-url (ps:lisp widen-url))
             (narrow-url (ps:lisp narrow-url))
             (close-url (ps:lisp close-url))
             (heading (ps:lisp (or (title panel) ""))))
         (flet ((control (parent id label name url)
                  (let ((button (ps:chain document (create-element "button"))))
                    (setf (ps:@ button id) id
                          (ps:@ button class-name) "nyxt-panel-button"
                          (ps:@ button type) "button"
                          (ps:@ button text-content) label)
                    (ps:chain button (set-attribute "aria-label" name))
                    (ps:chain button
                              (add-event-listener
                               "click"
                               (lambda (event)
                                 (ps:chain event (prevent-default))
                                 (ps:chain event (stop-propagation))
                                 (fetch url (ps:create :mode "no-cors")))))
                    (ps:chain parent (append-child button)))))
           (let ((old (ps:chain document (get-element-by-id "nyxt-panel-chrome"))))
             (when old
               (ps:chain old (remove))))
           (let ((chrome (ps:chain document (create-element "div")))
                 (style (ps:chain document (create-element "style")))
                 (header (ps:chain document (create-element "div")))
                 (label (ps:chain document (create-element "div")))
                 (actions (ps:chain document (create-element "div"))))
             (setf (ps:@ chrome id) "nyxt-panel-chrome"
                   (ps:@ style text-content) (ps:lisp (panel-chrome-css theme))
                   (ps:@ header id) "nyxt-panel-header"
                   (ps:@ label id) "nyxt-panel-title"
                   (ps:@ label text-content) heading
                   (ps:@ actions id) "nyxt-panel-actions")
             (control actions "nyxt-panel-widen-button" "+" "Widen panel" widen-url)
             (control actions "nyxt-panel-narrow-button" "−" "Narrow panel" narrow-url)
             (control actions "nyxt-panel-close-button" "×" "Close panel" close-url)
             (ps:chain header (append-child label))
             (ps:chain header (append-child actions))
             (ps:chain chrome (append-child style))
             (ps:chain chrome (append-child header))
             (let ((root (or (ps:@ document body)
                             (ps:@ document document-element))))
               (when (and root (ps:@ root dataset nyxt-panel-offset))
                 (setf (ps:@ root style padding-top) "")
                 (delete (ps:@ root dataset nyxt-panel-offset)))
               (when (and root (not (ps:@ root dataset nyxt-panel-pad)))
                 (setf (ps:@ root dataset nyxt-panel-pad)
                       (*string (or (parse-float
                                     (ps:@ (ps:chain window (get-computed-style root))
                                           padding-top))
                                    0))))
               (ps:chain root (append-child chrome))
               (when root
                 (let ((base (or (parse-float (ps:@ root dataset nyxt-panel-pad)) 0)))
                   (setf (ps:@ root style padding-top)
                         (+ (max base (+ (ps:@ header offset-height) 24)) "px"))))))))))))

(export-always '*push-occluded-page-elements-p*)
(defvar *push-occluded-page-elements-p* t
  "When non-nil, push page layout and scrollbars clear of floating panels and bars.")

(-> window-safe-area-insets (window) (values integer integer integer integer &optional))
(defun window-safe-area-insets (window)
  "Return the top, left, bottom, and right safe-area insets for WINDOW's active page."
  (if (or (null window)
          (not *push-occluded-page-elements-p*)
          (page-fullscreen-p window))
      (values 0 0 0 0)
      (let ((left 0)
            (right 0)
            (bottom (+ (or (and (status-buffer window)
                                (ffi-height (status-buffer window)))
                           0)
                       (or (and (message-buffer window)
                                (ffi-height (message-buffer window)))
                           0)
                       (or (alex:when-let ((prompt (first (active-prompt-buffers window))))
                             (ffi-height prompt))
                           0))))
        (dolist (panel (panel-buffers window))
          (when (and (panel-buffer-p panel)
                     (visible-p panel)
                     (not (dead-buffer-p panel)))
            (ecase (side panel)
              (:left (incf left (max 0 (width panel))))
              (:right (incf right (max 0 (width panel)))))))
        (values 12
                (if (plusp left) left 12)
                (max 0 bottom)
                (if (plusp right) right 12)))))

(-> page-adjacent-sides (window) (values boolean boolean &optional))
(defun page-adjacent-sides (window)
  "Return whether WINDOW has a visible panel on the left and on the right."
  (let ((left nil)
        (right nil))
    (dolist (panel (panel-buffers window))
      (when (and (panel-buffer-p panel) (visible-p panel))
        (case (side panel)
          (:left (setf left t))
          (:right (setf right t)))))
    (values left right)))

(-> apply-panel-containers (window) t)
(defun apply-panel-containers (window)
  "Inset, round, and blur WINDOW's visible side panels."
  (when window
    (let ((page-fullscreen-p (page-fullscreen-p window))
          (left-seen nil)
          (right-seen nil))
      (dolist (panel (panel-buffers window))
        (when (and (panel-buffer-p panel)
                   (visible-p panel)
                   (not (dead-buffer-p panel)))
          (let ((first-on-side-p (ecase (side panel)
                                   (:left (prog1 (not left-seen) (setf left-seen t)))
                                   (:right (prog1 (not right-seen) (setf right-seen t))))))
            (ffi-buffer-set-background-color panel "transparent")
            (if page-fullscreen-p
                (progn
                  (ffi-buffer-set-corner-radius panel 0)
                  (ffi-buffer-set-background-blur panel 0)
                  (ffi-buffer-set-insets panel :top 0 :left 0 :bottom 0 :right 0))
                (progn
                  (ecase (side panel)
                    (:left
                     (ffi-buffer-set-insets panel
                                            :top 12
                                            :left (if first-on-side-p 12 0)
                                            :bottom 0
                                            :right 12))
                    (:right
                     (ffi-buffer-set-insets panel
                                            :top 12
                                            :left 12
                                            :bottom 0
                                            :right (if first-on-side-p 12 0))))
                  (ffi-buffer-set-corner-radius panel 6)
                  (ffi-buffer-set-background-blur panel 20))))))))
  nil)

(-> apply-status-container (window) t)
(defun apply-status-container (window)
  "Inset, round, and blur WINDOW's status and message bars."
  (when-let ((status (and window (status-buffer window))))
    (ffi-buffer-set-background-color status "transparent")
    (if (page-fullscreen-p window)
        (progn
          (ffi-buffer-set-corner-radius status 0)
          (ffi-buffer-set-background-blur status 0)
          (ffi-buffer-set-insets status :top 0 :left 0 :bottom 0 :right 0))
        (progn
          (ffi-buffer-set-insets status :top 12 :left 12 :bottom 5 :right 12)
          (ffi-buffer-set-corner-radius status 6)
          (ffi-buffer-set-background-blur status 20))))
  (when-let ((message (and window (message-buffer window))))
    (ffi-buffer-set-background-color message "transparent")
    (ffi-buffer-set-corner-radius message 0)
    (ffi-buffer-set-background-blur message 0)
    (if (page-fullscreen-p window)
        (ffi-buffer-set-insets message :top 0 :left 0 :bottom 0 :right 0)
        (ffi-buffer-set-insets message :top 0 :left 12 :bottom 0 :right 12)))
  nil)

(-> apply-prompt-container (window) t)
(defun apply-prompt-container (window)
  "Inset, round, and blur WINDOW's active prompt."
  (when-let ((prompt (first (and window (active-prompt-buffers window)))))
    (ffi-buffer-set-background-color prompt "transparent")
    (if (page-fullscreen-p window)
        (progn
          (ffi-buffer-set-corner-radius prompt 0)
          (ffi-buffer-set-background-blur prompt 0)
          (ffi-buffer-set-insets prompt :top 0 :left 0 :bottom 0 :right 0))
        (progn
          (ffi-buffer-set-insets prompt :top 12 :left 12 :bottom 0 :right 12)
          (ffi-buffer-set-corner-radius prompt 6)
          (ffi-buffer-set-background-blur prompt 20))))
  nil)

(-> apply-page-container (window) t)
(defun apply-page-container (window)
  "Configure WINDOW's full-window page view, frosted overlay views, and safe-area insets."
  (when window
    (apply-panel-containers window)
    (apply-status-container window)
    (apply-prompt-container window)
    (let* ((fullscreen-p (page-fullscreen-p window))
           (theme (and *browser* (theme *browser*)))
           (dark-p (and theme (theme:dark-p theme)))
           (tint-color (or (and theme
                                (if dark-p
                                    (or (theme:background-color theme)
                                        (theme:background-color- theme))
                                    (or (theme:background-color+ theme)
                                        (theme:background-color theme))))
                           "#FFFFFF"))
           (tint-alpha (if fullscreen-p 0.0 (if dark-p 0.84 0.82))))
      (ffi-window-set-viewport-frame window
                                     :enabled-p t
                                     :top (if fullscreen-p 0 12)
                                     :left (if fullscreen-p 0 12)
                                     :bottom 0
                                     :right (if fullscreen-p 0 12)
                                     :corner-radius (if fullscreen-p 0 6)
                                     :blur-radius (if fullscreen-p 0 26)
                                     :tint-color tint-color
                                     :tint-alpha tint-alpha
                                     :border-alpha 0.0))
    (when-let ((page (buffer window)))
      (when (and (web-buffer-p page)
                 (not (panel-buffer-p page))
                 (not (dead-buffer-p page)))
        (ffi-buffer-set-insets page :top 0 :left 0 :bottom 0 :right 0)
        (ffi-buffer-set-corner-radius page 0)
        (ffi-buffer-set-background-blur page 0)
        (when (and *browser* (theme *browser*))
          (ffi-buffer-set-background-color page (theme:background-color (theme *browser*))))
        (multiple-value-bind (top left bottom right)
            (window-safe-area-insets window)
          (ffi-buffer-set-safe-area-insets page
                                           :top top
                                           :left left
                                           :bottom bottom
                                           :right right)))))
  nil)

(export-always 'enable-panel-buffers)
(-> enable-panel-buffers (window) t)
(defun enable-panel-buffers (window)
  "Show WINDOW's visible side panels."
  (when window
    (dolist (panel (panel-buffers window))
      (when (and (panel-buffer-p panel)
                 (visible-p panel)
                 (not (dead-buffer-p panel)))
        (ffi-add-panel-buffer window panel (side panel) (width panel))))
    (apply-page-container window))
  nil)

(export-always 'disable-panel-buffers)
(-> disable-panel-buffers (window) t)
(defun disable-panel-buffers (window)
  "Hide WINDOW's side panels without clearing their `visible-p' state."
  (when window
    (dolist (panel (panel-buffers window))
      (when (and (panel-buffer-p panel)
                 (visible-p panel)
                 (not (dead-buffer-p panel)))
        (ffi-remove-panel-buffer window panel))))
  nil)

(export-always 'enter-page-fullscreen)
(-> enter-page-fullscreen (window &key (:user-event-p boolean)) t)
(defun enter-page-fullscreen (window &key (user-event-p t))
  "Hide WINDOW's chrome, remove container rounding, and fullscreen WINDOW for page content."
  (when (and window (not (page-fullscreen-p window)))
    (setf (fullscreen-before-page-p window) (fullscreen-p window)
          (page-fullscreen-p window) t)
    (disable-message-buffer window)
    (disable-status-buffer window)
    (disable-panel-buffers window)
    (ffi-window-fullscreen window :user-event-p user-event-p))
  nil)

(export-always 'leave-page-fullscreen)
(-> leave-page-fullscreen (window &key (:user-event-p boolean) (:unfullscreen-window-p boolean)) t)
(defun leave-page-fullscreen (window &key (user-event-p t) (unfullscreen-window-p t))
  "Restore WINDOW's chrome and container rounding after page content leaves fullscreen."
  (when (and window (page-fullscreen-p window))
    (let ((keep-fullscreen-p (fullscreen-before-page-p window)))
      (setf (fullscreen-before-page-p window) nil
            (page-fullscreen-p window) nil)
      (when (and unfullscreen-window-p (not keep-fullscreen-p))
        (ffi-window-unfullscreen window :user-event-p user-event-p))
      (enable-message-buffer window)
      (enable-status-buffer window)
      (enable-panel-buffers window)))
  nil)

(defmethod (setf page-fullscreen-p) :after (value (window window))
  "Update container insets and corner rounding when page fullscreen state changes."
  (declare (ignore value))
  (apply-page-container window))

(defmethod ffi-window-unfullscreen :after ((window window) &key (user-event-p t) &allow-other-keys)
  (when (and user-event-p (page-fullscreen-p window))
    (leave-page-fullscreen window :unfullscreen-window-p nil)))

(defmethod (setf buffer) :before ((buffer buffer) (window window))
  (when (and (page-fullscreen-p window)
             (not (panel-buffer-p buffer))
             (not (eq buffer (buffer window))))
    (when-let ((previous (buffer window)))
      (ffi-buffer-evaluate-javascript-async
       previous
       (ps:ps
         (when (ps:@ document fullscreen-element)
           (ps:chain document (exit-fullscreen))))))
    (leave-page-fullscreen window)))

(defmethod (setf visible-p) :after (value (panel panel-buffer))
  "The page margin follows the panels beside it."
  (declare (ignore value))
  (when (window panel)
    (apply-page-container (window panel))))

(defmethod (setf side) :after (value (panel panel-buffer))
  "The page margin follows the side the panel moves to."
  (declare (ignore value))
  (when (window panel)
    (apply-page-container (window panel))))

(defmethod (setf buffer) :after ((buffer web-buffer) (window window))
  "The panels float on the page that just became active.
Specialized on `web-buffer' so the status bar's `buffer' handler, which
uses the unspecialized method, does not replace it."
  (unless (panel-buffer-p buffer)
    (apply-page-container window)))

(-> register-panel-callbacks (panel-buffer) (values string string string &optional))
(defun register-panel-callbacks (panel)
  "Register PANEL's resize and close callbacks and return their URLs."
  (values (panel-resize-callback panel :widen)
          (panel-resize-callback panel :narrow)
          (panel-close-callback panel)))

(-> install-panel-controls (panel-buffer) t)
(defun install-panel-controls (panel)
  "Add the title and buttons on PANEL."
  (multiple-value-bind (widen-url narrow-url close-url)
      (register-panel-callbacks panel)
    (ffi-buffer-evaluate-javascript-async
     panel
     (panel-chrome-script panel
                          widen-url
                          narrow-url
                          close-url
                          (theme *browser*))))
  (when (window panel)
    (apply-page-container (window panel))))

(defmethod ffi-buffer-load :after ((buffer panel-buffer) url)
  (declare (ignore url))
  (unless (dead-buffer-p buffer)
    (register-panel-callbacks buffer)))

(defmethod ffi-buffer-reload :after ((buffer panel-buffer))
  (unless (dead-buffer-p buffer)
    (register-panel-callbacks buffer)))

(defmethod on-signal-load-finished :after ((buffer panel-buffer) url title)
  (declare (ignore url title))
  (install-panel-controls buffer))

(defmethod (setf title) :after (value (panel panel-buffer))
  (declare (ignore value))
  (when (and (visible-p panel) (window panel))
    (install-panel-controls panel)))

(defmethod ffi-buffer-delete :before ((buffer panel-buffer))
  "Drop BUFFER's redraw handlers before its view is destroyed."
  (drop-buffer-setf-handlers buffer)
  (when (slot-boundp buffer 'lisp-url-callbacks)
    (clrhash (lisp-url-callbacks buffer)))
  (dolist (window (window-list))
    (setf (panel-buffers window) (remove buffer (panel-buffers window))))
  (setf (visible-p buffer) nil)
  (when (window buffer)
    (apply-panel-geometry buffer))
  (buffer-delete (id buffer)))

(define-command delete-panel-buffer ((panel panel-buffer))
  "Remove PANEL from its window."
  (let ((window (window panel)))
    (ignore-errors (ffi-buffer-delete panel))
    (buffer-delete (id panel))
    (when (and window (buffer window))
      (ffi-focus-buffer (buffer window))))
  nil)
