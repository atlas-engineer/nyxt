;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; Moved here so all the `nyxt-packages' are defined by the moment it's set.
(setf sym:*default-packages* (append '(:nyxt-user) (nyxt-packages)))

(defmacro command-docstring-first-sentence (fn &key (sentence-case-p nil))
  "Print FN first docstring sentence in HTML."
  `(if (fboundp ,fn)
       (spinneret:with-html
         (:span
          (or ,(if sentence-case-p
                   `(sera:ensure-suffix (str:sentence-case (first (ppcre:split "\\.\\s" (documentation ,fn 'function)))) ".")
                   `(sera:ensure-suffix (first (ppcre:split "\\.\\s" (documentation ,fn 'function))) "."))
              (error "Undocumented function ~a." ,fn))))
       (error "~a is not a function." ,fn)))

(defmacro command-information (fn)
  "Print FN keybinding and first docstring sentence in HTML."
  `(spinneret:with-html (:li (:nxref :command ,fn) ": " (command-docstring-first-sentence ,fn))))

(defun list-command-information (fns)
  "Print information over a list of commands in HTML."
  (dolist (i fns)
    (command-information i)))

(defun configure-slot (slot class &key
                                    (type (getf (mopu:slot-properties (find-class class) slot)
                                                :type))
                                    (sources 'prompter:raw-source)
                                    (input "")
                                    (postprocess #'read-from-string))
  "Set value of CLASS' SLOT in `*auto-config-file*'.
Prompt for a new value with prompt SOURCES and type-check
 it against the SLOT's TYPE, if any. CLASS is a class symbol."
  (sera:nlet lp ()
    (let* ((input (prompt
                   :prompt (format nil "Configure slot value ~a" slot)
                   :input input
                   :sources sources))
           (input (if (serapeum:single input)
                      (funcall postprocess (first input))
                      (funcall postprocess input))))
      (cond
        ((and type (not (typep input type)))
         (echo-warning "Type mismatch for ~a: got ~a, expected ~a."
                       slot (type-of input) type)
         (lp))
        (t
         (auto-configure :class-name class :slot slot :slot-value input)
         (echo "Update slot ~s to ~s. You might need to restart to experience the change." slot input))))))

(define-internal-page-command-global common-settings (&key (section 'keybindings))
    (buffer "*Settings*" 'nyxt/mode/help:help-mode)
  "Display an interface to tweak frequently sought-after user options.
The changes are saved to `*auto-config-file*', and persist from one Nyxt session
to the next."
  ;; SECTION stays so a link from the old tabbed page still opens.
  (declare (ignorable section))
  ;; Title and lead are one group, 17px apart. Section titles are 24px, a
  ;; step above the 17px setting names. 55px above a later title opens the
  ;; group; 7px under the title keeps it with its settings. The first title
  ;; sits 17px under the lead. A row is 55px: a name over an 11px detail,
  ;; control on the right.
  (let ((homepage (default-new-buffer-url *browser*)))
    (spinneret:with-html-string
      (:nstyle
        `(body
          :padding "37px"
          :color-scheme ,(if (theme:dark-p theme:theme) "dark" "light"))
        '("h1"
          :margin-bottom "17px")
        `("p.lead"
          :margin "0"
          :font-size "17px"
          :line-height "24px"
          :color ,theme:primary-color)
        '(.page
          :max-width "800px")
        '("table.settings"
          :width "100%"
          :margin "0"
          :border "none"
          :border-radius "0"
          :border-collapse "separate"
          :border-spacing "0"
          :table-layout "auto")
        `("table.settings th, table.settings td"
          :border "none"
          :border-radius "0"
          :background-color "transparent"
          :color ,theme:on-background-color
          :font-size "17px"
          :font-weight "400"
          :line-height "24px"
          :text-align "left"
          :vertical-align "middle"
          :padding "0")
        '("table.settings td.setting"
          :height "55px"
          :padding "7px 17px 7px 0"
          :max-width "0"
          :width "100%"
          :overflow "hidden"
          :vertical-align "top")
        '("table.settings td.setting span"
          :display "block"
          :overflow "hidden"
          :text-overflow "ellipsis"
          :white-space "nowrap")
        '("table.settings td.setting span.name"
          :height "24px"
          :font-size "17px"
          :line-height "24px")
        `("table.settings td.setting span.detail"
          :height "17px"
          :font-size "11px"
          :line-height "17px"
          :color ,theme:primary-color)
        '("table.settings td.control"
          :width "1%"
          :height "55px"
          :padding "0"
          :text-align "right"
          :white-space "nowrap"
          :overflow "visible")
        `("table.settings tbody td"
          :box-shadow ,(format nil "inset 0 -1px 0 ~a" theme:background-color-))
        '("table.settings tbody tr:last-child td"
          :box-shadow "none")
        '("table.settings tbody tr:has(+ tr.group) td"
          :box-shadow "none")
        `("table.settings tr.group th"
          :height "auto"
          :padding "55px 0 7px"
          :font-size "24px"
          :font-weight "600"
          :line-height "37px"
          :color ,theme:on-background-color
          :background-color "transparent"
          :box-shadow "none"
          :overflow "visible"
          :white-space "normal")
        '("table.settings tbody tr.group:first-of-type th"
          :padding-top "17px")
        `("table.settings .radio-div"
          :display "inline-flex"
          :align-items "center"
          :height "28px"
          :margin "0"
          :padding "2px"
          :border-radius "7px"
          :background-color ,theme:background-color-
          :vertical-align "middle")
        `("table.settings .radio-label"
          :position "relative"
          :display "inline-flex"
          :align-items "center"
          :height "24px"
          :margin "0"
          :padding "0 11px"
          :border-radius "5px"
          :font-size "17px"
          :font-weight "400"
          :line-height "24px"
          :color ,theme:on-background-color
          :cursor "pointer"
          :user-select "none")
        '("table.settings .radio-input"
          :position "absolute"
          :width "1px"
          :height "1px"
          :margin "0"
          :opacity "0")
        `("table.settings .radio-label:has(.radio-input:checked)"
          :background-color ,theme:background-color+
          :font-weight "600")
        `("table.settings .radio-label:has(.radio-input:focus-visible)"
          :outline "2px solid"
          :outline-color ,theme:action-color
          :outline-offset "1px")
        `("table.settings button.button"
          :appearance "none"
          :display "inline-flex"
          :align-items "center"
          :justify-content "center"
          :height "28px"
          :margin "0"
          :padding "0 11px"
          :border "none"
          :border-radius "7px"
          :background-color ,theme:background-color+
          :box-shadow ,(format nil "inset 0 0 0 1px ~a" theme:background-color-)
          :color ,theme:on-background-color
          :font-size "17px"
          :font-weight "400"
          :line-height "24px"
          :cursor "pointer")
        `("table.settings select.button"
          :appearance "none"
          :display "inline-block"
          :height "28px"
          :margin "0"
          :padding "0 28px 0 11px"
          :border "none"
          :border-radius "7px"
          :background-color ,theme:background-color+
          :background-image ,(concatenate
                              'string
                              "linear-gradient(45deg, transparent 50%, currentColor 50%), "
                              "linear-gradient(135deg, currentColor 50%, transparent 50%)")
          :background-position "calc(100% - 16px) center, calc(100% - 11px) center"
          :background-size "5px 5px, 5px 5px"
          :background-repeat "no-repeat"
          :box-shadow ,(format nil "inset 0 0 0 1px ~a" theme:background-color-)
          :color ,theme:on-background-color
          :font-size "17px"
          :font-weight "600"
          :line-height "24px"
          :text-align "left"
          :cursor "pointer"
          :vertical-align "middle")
        `("table.settings button.button:hover, table.settings select.button:hover"
          :background-color ,theme:background-color-)
        `("table.settings button.button:focus-visible, table.settings select.button:focus-visible"
          :outline "2px solid"
          :outline-color ,theme:action-color
          :outline-offset "2px"))
      (:div :class "page"
            (:h1 "Settings")
            (:p :class "lead" "These settings apply on restart.")
            (:table :class "settings"
                    (:tbody
                     (:tr :class "group"
                          (:th :colspan 2 :scope "rowgroup" "Keyboard"))
                     (:tr
                      (:td :class "setting"
                           (:span :class "name" "Keybindings")
                           (:span :class "detail" "The keys new buffers start with."))
                      (:td :class "control"
                           (:nradio
                            :name "keyscheme"
                            :checked (cond ((find "nyxt/mode/vi" (default-modes (current-buffer))
                                                  :key #'uiop:symbol-package-name
                                                  :test #'string-equal)
                                            'vi)
                                           ((find "nyxt/mode/emacs" (default-modes (current-buffer))
                                                  :key #'uiop:symbol-package-name
                                                  :test #'string-equal)
                                            'emacs)
                                           (t 'cua))
                            :buffer buffer
                            '(cua "CUA"
                              (nyxt::auto-configure
                               :form '(define-configuration (input-buffer)
                                       ((default-modes (remove-if (lambda (m)
                                                                    (find (symbol-name m)
                                                                          '("EMACS-MODE"
                                                                            "VI-NORMAL-MODE"
                                                                            "VI-INSERT-MODE")
                                                                          :test #'string=))
                                                        %slot-value%))))))
                            '(emacs "Emacs"
                              (nyxt::auto-configure
                               :form '(define-configuration (input-buffer)
                                       ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode
                                                                %slot-value%))))))
                            '(vi "vi"
                              (progn
                                (nyxt::auto-configure
                                 :form '(define-configuration (input-buffer)
                                         ((default-modes (pushnew 'nyxt/mode/vi:vi-normal-mode
                                                                  %slot-value%)))))
                                (nyxt::auto-configure
                                 :form '(define-configuration (prompt-buffer)
                                         ((default-modes (pushnew 'nyxt/mode/vi:vi-insert-mode
                                                                  %slot-value%))))))))))
                     (:tr :class "group"
                          (:th :colspan 2 :scope "rowgroup" "Appearance"))
                     (:tr
                      (:td :class "setting"
                           (:span :class "name" "Interface")
                           (:span :class "detail" "Nyxt's own pages and prompts."))
                      (:td :class "control"
                           (:nradio
                            :name "theme"
                            :checked (if (eq (theme *browser*) theme:+light-theme+)
                                         'theme:+light-theme+
                                         'theme:+dark-theme+)
                            :buffer buffer
                            '(theme:+light-theme+ "Light"
                              (nyxt::auto-configure
                               :form '(define-configuration browser
                                       ((theme theme:+light-theme+)))))
                            '(theme:+dark-theme+ "Dark"
                              (nyxt::auto-configure
                               :form '(define-configuration browser
                                       ((theme theme:+dark-theme+))))))))
                     (:tr
                      (:td :class "setting"
                           (:span :class "name" "Webpages")
                           (:span :class "detail" "Pages you visit."))
                      (:td :class "control"
                           (:nradio
                            :name "darken"
                            :checked (if (find 'nyxt/mode/style:dark-mode
                                               (default-modes (current-buffer)))
                                         'dark
                                         'auto)
                            :buffer buffer
                            '(auto "Default"
                              (nyxt::auto-configure
                               :form '(define-configuration (web-buffer)
                                       ((default-modes (remove-if (lambda (m)
                                                                    (string= (symbol-name m)
                                                                             "DARK-MODE"))
                                                        %slot-value%))))))
                            '(dark "Darkened"
                              (nyxt::auto-configure
                               :form '(define-configuration (web-buffer)
                                       ((default-modes (pushnew 'nyxt/mode/style:dark-mode
                                                                %slot-value%)))))))))
                     (:tr
                      (:td :class "setting"
                           (:span :class "name" "Zoom")
                           (:span :class "detail" "Used when a site has no saved zoom."))
                      (:td :class "control"
                           (:nselect
                            :id "default-zoom-ratio"
                            :buffer buffer
                            :default (format nil "~d%"
                                             (round (* 100 (zoom-ratio-default (current-buffer)))))
                            (loop for number in '(30 50 67 80 90 100
                                                  110 120 133 150 170
                                                  200 240 300 400 500)
                                  collect `((,number ,(format nil "~a%" number))
                                            (nyxt::auto-configure
                                             :class-name 'document-buffer
                                             :slot 'zoom-ratio-default
                                             :slot-value ,(/ number 100.0)))))))
                     (:tr :class "group"
                          (:th :colspan 2 :scope "rowgroup" "Buffers"))
                     (:tr
                      (:td :class "setting"
                           (:span :class "name" "Homepage")
                           (:span :class "detail"
                                  :title (render-url homepage)
                                  (if (quri:uri= (url homepage) (url (nyxt-url 'new)))
                                      "Nyxt's home screen"
                                      (render-url homepage))))
                      (:td :class "control"
                           (:nbutton
                            :text "Set default URL"
                            :title "Choose the address a new buffer opens."
                            :buffer buffer
                            '(nyxt::configure-slot 'default-new-buffer-url 'browser
                              :sources (list
                                        (make-instance
                                         'prompter:raw-source
                                         :name "New URL")
                                        (make-instance
                                         'global-history-source
                                         :enable-marks-p nil
                                         :actions-on-return #'identity))
                              :input (render-url (default-new-buffer-url *browser*))
                              :postprocess (lambda (url-or-history-entry)
                                             (render-url (url url-or-history-entry)))))))
                     (:tr
                      (:td :class "setting"
                           (:span :class "name" "Modes")
                           (:span :class "detail" "Started on every new buffer."))
                      (:td :class "control"
                           (:nbutton
                            :text "Set default modes"
                            :title "Choose the modes a new buffer starts with."
                            :buffer buffer
                            '(nyxt::configure-slot 'default-modes 'buffer
                              :sources (make-instance
                                        'mode-source
                                        :marks (default-modes (current-buffer)))
                              :postprocess (lambda (modes)
                                             `(quote ,modes))
                              :type 'cons))))))))))

(defun tls-help (buffer url)
  "Helper function invoked upon TLS certificate errors."
  (setf (status buffer) :failed)
  (html-set
   (spinneret:with-html-string
     (:h1 (format nil "TLS Certificate Error: ~a" (render-url url)))
     (:p "The address you are trying to visit has an invalid
certificate. By default Nyxt refuses to establish a secure connection
to a host with an erroneous certificate (e.g. self-signed ones). This
could mean that the address you are attempting the access is
compromised.")
     (:p "If you trust the address nonetheless, you can add an exception
for the current hostname with the "
         (:code "add-domain-to-certificate-exceptions")
         " command.  The "
         (:code "certificate-exception-mode")
         " must be active for the current buffer (which is the
default).")
     (:p "To persist hostname exceptions in your initialization
file, see the "
         (:code "add-domain-to-certificate-exceptions")
         " documentation."))
   buffer))

(define-command nyxt-version ()
  "Display the version of Nyxt in the `message-buffer'.
The value is saved to clipboard."
  (copy-to-clipboard +version+)
  (echo "Version ~a" +version+))

(define-internal-page-command-global new ()
    (buffer "*New buffer*")
  "Display a page suitable as `default-new-buffer-url'."
  ;; The logo and the two actions are one group, locked to the same height.
  ;; The page list starts one step (37px) below that group. Content and the
  ;; corner credit share that same 37px inset.
  (labels ((command-row (heading detail description command)
             "Present COMMAND as a name plus its binding."
             (spinneret:with-html
               (:li
                 (:a :href "#"
                     :title description
                     :onclick (ps:ps
                                (ps:chain event (prevent-default))
                                (nyxt/ps:lisp-eval
                                 (:title heading
                                  :buffer buffer)
                                 (funcall command))
                                nil)
                     (:span heading)
                     (:span detail)))))
           (page-row (heading detail description url)
             "Present URL as a name plus a short description."
             (spinneret:with-html
               (:li
                 (:a :href url
                     :title description
                     (:span heading)
                     (:span detail))))))
    (spinneret:with-html-string
      (:nstyle
        '(body
          :min-height "100vh"
          :padding "37px"
          :padding-bottom "112px")
        `(.start
          :display "flex"
          :align-items "center"
          :margin "0 0 37px")
        `(.logo
          :color ,(if (theme:dark-p theme:theme)
                      theme:action-color
                      theme:on-background-color)
          :width "100px"
          :height "100px"
          :flex "none"
          :margin-right "17px")
        '(".logo svg"
          :display "block"
          :width "100px"
          :height "100px"
          :border-radius "7px")
        '(.actions
          :display "flex"
          :flex-direction "column"
          :align-items "flex-start")
        '("div.actions > div"
          :display "flex"
          :align-items "center")
        '("div.actions > div:first-child"
          :margin-bottom "17px")
        `("button.set-url, button.execute-command"
          :appearance "none"
          :display "inline-flex"
          :align-items "center"
          :justify-content "center"
          :box-sizing "border-box"
          :min-width "180px"
          :height "40px"
          :margin "0"
          :padding "0 17px"
          :border "none"
          :border-radius "7px"
          :font-size "17px"
          :font-weight "400"
          :line-height "24px"
          :cursor "pointer")
        `("button.set-url"
          :background-color ,theme:action-color
          :color ,theme:on-primary-color)
        `("button.execute-command"
          :background-color ,theme:primary-color
          :color ,theme:on-primary-color)
        '("button.set-url:hover, button.execute-command:hover"
          :opacity "0.8")
        `(.binding
          :margin-left "12px"
          :font-weight "600"
          :color ,theme:secondary-color)
        `(.copyright
          :position "fixed"
          :z-index "1"
          :right "37px"
          :bottom "37px"
          :margin "0"
          :text-align "right"
          :font-size "17px"
          :line-height "24px"
          :color ,theme:on-background-color)
        `(.program-name
          :color ,theme:action-color
          :font-size "24px"
          :font-weight "600"
          :line-height "24px"))
      (:div :class "start"
            (:div :class "logo" (:raw (glyph-logo *browser*)))
            (:div :class "actions"
                  (:div
                   (:nbutton :class "set-url"
                             :text "Set-URL"
                             :title "Open an address."
                             :buffer buffer
                             '(set-url))
                   (:span :class "binding"
                          (format nil "(~a)" (binding-keys 'set-url))))
                  (:div
                   (:nbutton :class "execute-command"
                             :text "Execute-Command"
                             :title "Run a command by name."
                             :buffer buffer
                             '(execute-command))
                   (:span :class "binding"
                          (format nil "(~a)" (binding-keys 'execute-command))))))
      (:ul :class "list"
           (page-row "Bookmarks"
                     "Saved pages"
                     "View all bookmarks."
                     (nyxt-url 'nyxt/mode/bookmark:list-bookmarks))
           (page-row "History"
                     "Pages you have visited"
                     "List history."
                     (nyxt-url 'nyxt/mode/history:list-history))
           (page-row "Buffers"
                     "Open pages"
                     "List all buffers."
                     (nyxt-url 'nyxt/mode/buffer-listing:list-buffers))
           (page-row "Downloads"
                     "Files from this session"
                     "View downloads for the current session."
                     (nyxt-url 'nyxt/mode/download:list-downloads))
           (page-row "Annotations"
                     "Notes on pages"
                     "Show annotations for all pages."
                     (nyxt-url 'nyxt/mode/annotate:show-annotations))
           (page-row "Manual"
                     "Documentation"
                     "Detailed Nyxt documentation, including configuration guides."
                     (nyxt-url 'manual))
           (page-row "Settings"
                     "Shortcuts, homepage, and zoom"
                     "Set keyboard shortcuts, the homepage URL, or zoom."
                     (nyxt-url 'common-settings))
           (page-row "Keyboard shortcuts"
                     "Bindings for this buffer"
                     "List all keyboard shortcuts for the current buffer."
                     (nyxt-url 'describe-bindings))
           (when (appimage-p)
             (command-row "Install desktop shortcut"
                          nil
                          "Install a desktop entry so Nyxt can be started from your launcher."
                          #'add-desktop-entry)))
      (:p :class "copyright"
          (:span :class "program-name" "Nyxt")
          (format nil " ~a (~a)" +version+ (name *renderer*))
          (:br)
          (format nil "Atlas Engineer, 2018-~a"
                  (time:timestamp-year (time:now)))))))

(define-internal-page-command-global manual ()
    (buffer "*Manual*" 'nyxt/mode/help:help-mode)
  "Display Nyxt manual."
  (spinneret:with-html-string
    (:nstyle
      '(body :max-width "80ch")
      ;; Headings and links share the theme gray. One step darker than
      ;; `primary-color' so the long pages stay readable.
      `("h2, h3, h4, h5, h6, a, .link"
        :color ,theme:primary-color-))
    (:h1 "Nyxt Manual")
    (:ntoc
      (tutorial-content)
      (manual-content))))

(define-internal-page-command-global tutorial ()
    (buffer "*Tutorial*" 'nyxt/mode/help:help-mode)
  "Display Nyxt tutorial."
  (spinneret:with-html-string
    (:nstyle
      '(body :max-width "80ch")
      ;; Headings and links share the theme gray. One step darker than
      ;; `primary-color' so the long pages stay readable.
      `("h2, h3, h4, h5, h6, a, .link"
        :color ,theme:primary-color-))
    (:h1 "Nyxt tutorial")
    (:p "The following tutorial introduces core concepts and
basic usage.  For more details, especially regarding configuration, see
the " (:code (:a.link :href (nyxt-url 'manual) "manual")) ".")
    (tutorial-content)))

(define-internal-page-command-global show-system-information ()
    (buffer "*System information*")
  "Display information about the currently running Nyxt system.

It is of particular interest when reporting bugs.  The content is saved to
clipboard."
  (let* ((*print-length* nil)
         (nyxt-information (system-information)))
    (prog1
        (spinneret:with-html-string
          (:h1 "System information")
          (:pre nyxt-information))
      (copy-to-clipboard nyxt-information)
      (log:info nyxt-information)
      (echo "System information saved to clipboard."))))

(define-command-global report-bug ()
  "Report a bug on Nyxt's issue tracker."
  (make-buffer-focus :url (quri:uri "https://github.com/atlas-engineer/nyxt/issues/new?&template=bug_report.md")))
