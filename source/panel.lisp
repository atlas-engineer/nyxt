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
    (on-signal-notify-title mode (url buffer)))
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

(-> focused-panel ((maybe window)) (maybe panel-buffer))
(defun focused-panel (window)
  "Return the panel in WINDOW with keyboard focus, or the one most recently added."
  (when window
    (let ((keyboard (keyboard-buffer window)))
      (if (and (panel-buffer-p keyboard)
               (find keyboard (panel-buffers window)))
          keyboard
          (first (last (panel-buffers window)))))))

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
             (define-command-global ,left (,window-var)
               ,(command-documentation :left)
               (,helper ,window-var :left))
             (define-command-global ,right (,window-var)
               ,(command-documentation :right)
               (,helper ,window-var :right))))))))

(define-mode panel-mode ()
  "Resize the panel this mode belongs to.
`+' widens it and `hyphen' narrows it."
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
       "hyphen" 'narrow-panel))))
  (:toggler-command-p nil))

(defmethod default-modes append ((buffer panel-buffer))
  '(panel-mode))

(-> panel-width-step (panel-buffer) integer)
(defun panel-width-step (panel)
  "Return the number of pixels PANEL's `panel-mode' resizes by."
  (alex:if-let ((mode (find-if #'panel-mode-p (enabled-modes panel))))
    (width-step mode)
    32))

(define-command widen-panel (panel)
  "Widen PANEL by one step."
  (when (panel-buffer-p panel)
    (incf (width panel) (panel-width-step panel))))

(define-command narrow-panel (panel)
  "Narrow PANEL by one step."
  (when (panel-buffer-p panel)
    (decf (width panel) (panel-width-step panel))))

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
    `(html
      :background-color ,theme:background-color
      :height "100%")
    `(body
      :box-sizing "border-box"
      :background-color ,theme:surface-color
      :backdrop-filter "saturate(180%) blur(20px)"
      :-webkit-backdrop-filter "saturate(180%) blur(20px)"
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

(-> panel-backdrop-color (window) string)
(defun panel-backdrop-color (window)
  "Return the color behind WINDOW's page, or the theme background.
The color is the one stored when the page loaded. A page that has not
been read yet uses the theme."
  (let ((page (and window (buffer window))))
    (or (and page
             (web-buffer-p page)
             (not (panel-buffer-p page))
             (background-color page))
        (theme:background-color (theme *browser*)))))

(-> apply-panel-backdrop (buffer string) t)
(defun apply-panel-backdrop (panel color)
  "Paint COLOR behind PANEL's rounded surface."
  (ffi-buffer-set-background-color panel color)
  (ffi-buffer-evaluate-javascript-async
   panel
   (ps:ps
     (setf (ps:@ document document-element style background-color)
           (ps:lisp color))))
  nil)

(-> backdrop-buffer-p (buffer) boolean)
(defun backdrop-buffer-p (buffer)
  "Whether BUFFER can take the color behind the page.
The status bar, message bar, and prompt are not in the main buffer table, so
`dead-buffer-p' does not describe them."
  (and buffer
       (or (auxiliary-buffer-p buffer)
           (not (dead-buffer-p buffer)))))

(-> paint-panel-backdrops (window) t)
(defun paint-panel-backdrops (window)
  "Give WINDOW's panels and bars the background of its active page."
  (when window
    (let ((color (panel-backdrop-color window)))
      (dolist (buffer (append (panel-buffers window)
                              (list (status-buffer window)
                                    (message-buffer window))
                              (active-prompt-buffers window)))
        (when (backdrop-buffer-p buffer)
          (apply-panel-backdrop buffer color)))))
  nil)

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
  "Inset and round WINDOW's visible side panels."
  (when window
    (let ((page-fullscreen-p (page-fullscreen-p window))
          (left-seen nil)
          (right-seen nil)
          (color (panel-backdrop-color window)))
      (dolist (panel (panel-buffers window))
        (when (and (panel-buffer-p panel)
                   (visible-p panel)
                   (not (dead-buffer-p panel)))
          (let ((first-on-side-p (ecase (side panel)
                                   (:left (prog1 (not left-seen) (setf left-seen t)))
                                   (:right (prog1 (not right-seen) (setf right-seen t))))))
            (if page-fullscreen-p
                (progn
                  (ffi-buffer-set-corner-radius panel 0)
                  (ffi-buffer-set-insets panel :top 0 :left 0 :bottom 0 :right 0))
                (progn
                  (ffi-buffer-set-insets panel
                                         :top 12
                                         :left (if first-on-side-p 8 0)
                                         :bottom 8
                                         :right 8)
                  (ffi-buffer-set-corner-radius panel 6)))
            (when color
              (ffi-buffer-set-background-color panel color)))))))
  nil)

(-> apply-status-container (window) t)
(defun apply-status-container (window)
  "Inset and round WINDOW's status bar."
  (when-let ((status (and window (status-buffer window))))
    (if (page-fullscreen-p window)
        (progn
          (ffi-buffer-set-corner-radius status 0)
          (ffi-buffer-set-insets status :top 0 :left 0 :bottom 0 :right 0))
        (progn
          (ffi-buffer-set-insets status :top 8 :left 8 :bottom 8 :right 8)
          (ffi-buffer-set-corner-radius status 6)))
    (when-let ((color (panel-backdrop-color window)))
      (ffi-buffer-set-background-color status color)))
  nil)

(-> apply-prompt-container (window) t)
(defun apply-prompt-container (window)
  "Inset and round WINDOW's active prompt."
  (when-let ((prompt (first (and window (active-prompt-buffers window)))))
    (if (page-fullscreen-p window)
        (progn
          (ffi-buffer-set-corner-radius prompt 0)
          (ffi-buffer-set-insets prompt :top 0 :left 0 :bottom 0 :right 0))
        (progn
          (ffi-buffer-set-insets prompt :top 8 :left 8 :bottom 8 :right 8)
          (ffi-buffer-set-corner-radius prompt 6)))
    (when-let ((color (panel-backdrop-color window)))
      (ffi-buffer-set-background-color prompt color)))
  nil)

(-> apply-page-container (window) t)
(defun apply-page-container (window)
  "Inset WINDOW's page, panels, status bar, and prompt so the page background shows around them.
The top inset of the page and panels is 12px and the other edges are 8px.
A side that already has an adjacent panel is not inset again.
When the active page in WINDOW is fullscreen, insets and corner rounding are disabled."
  (when window
    (apply-panel-containers window)
    (apply-status-container window)
    (apply-prompt-container window)
    (when-let ((page (buffer window)))
      (when (and (web-buffer-p page)
                 (not (panel-buffer-p page))
                 (not (dead-buffer-p page)))
        (if (page-fullscreen-p window)
            (progn
              (ffi-buffer-set-corner-radius page 0)
              (ffi-buffer-set-insets page :top 0 :left 0 :bottom 0 :right 0))
            (progn
              (multiple-value-bind (left right) (page-adjacent-sides window)
                (ffi-buffer-set-insets page
                                       :top 12
                                       :left (if left 0 8)
                                       :bottom 8
                                       :right (if right 0 8)))
              (ffi-buffer-set-corner-radius page 6)))
        (when-let ((color (panel-backdrop-color window)))
          (ffi-buffer-set-background-color page color)))))
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

(defmethod (setf background-color) :after (color (buffer web-buffer))
  "Panels and bars follow the page they float on."
  (declare (ignore color))
  (dolist (window (window-list))
    (when (eq buffer (buffer window))
      (paint-panel-backdrops window)
      (apply-page-container window))))

(-> refresh-panel-backdrop (window) t)
(defun refresh-panel-backdrop (window)
  "Read the active page's background again.
Switching to a page that is already open does not load it again, so the color
stored at load time may be stale."
  (when-let ((page (and window (buffer window))))
    (when (and (web-buffer-p page) (not (panel-buffer-p page)))
      (run-thread "page-background"
        (update-page-background page))))
  nil)

(defmethod (setf buffer) :after ((buffer web-buffer) (window window))
  "The panels float on the page that just became active.
Specialized on `web-buffer' so the status bar's `buffer' handler, which
uses the unspecialized method, does not replace it."
  (unless (panel-buffer-p buffer)
    (paint-panel-backdrops window)
    (refresh-panel-backdrop window)
    (apply-page-container window)))

(-> install-panel-controls (panel-buffer) t)
(defun install-panel-controls (panel)
  "Add the title and buttons on PANEL."
  (ffi-buffer-evaluate-javascript-async
   panel
   (panel-chrome-script panel
                        (panel-resize-callback panel :widen)
                        (panel-resize-callback panel :narrow)
                        (panel-close-callback panel)
                        (theme *browser*)))
  (when (window panel)
    (paint-panel-backdrops (window panel))
    (apply-page-container (window panel))))

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

(define-command delete-panel-buffer (panel)
  "Remove PANEL from its window."
  (when (panel-buffer-p panel)
    (ignore-errors (ffi-buffer-delete panel))
    (buffer-delete (id panel)))
  nil)
