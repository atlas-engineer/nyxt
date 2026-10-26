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
  (spinneret:with-html-string
    (:nstyle
      (theme:themed-css (theme *browser*)
        '("body,h3"
          :margin 0)
        '(".radio-div,.checkbox-div"
          :margin-top "1em")
        '(".radio-label,.checkbox-input"
          :display block
          :padding-bottom "0.5em")
        '(".radio-input,.checkbox-input"
          :display inline-block
          :margin-right "0.5em"
          :margin-left "3em")
        '("select.button,.button"
          :display block
          :margin "1em 0 0.5em 2.5em")
        '(.section
          :margin 0
          :padding "2em 0 0.5em 1.5em")
        '(.row
          :display "flex"
          :margin-top "1em")
        `(.tabs
          :flex "0 0 180px"
          :background-color ,theme:background-color+)
        `(.content
          :flex "85%"
          :background-color ,theme:background-color-)
        '(.left
          :flex "0 0 256px")
        `(.right
          :color ,theme:primary-color
          :padding-top "1em"
          :padding-left "4em"
          :max-width "50ch")
        '(p
          :margin 0
          :padding-bottom "0.5em")
        `(.tab-button
          :display "block"
          :text-decoration "none"
          :background-color ,theme:background-color+
          :color ,theme:action-color-
          :padding "1.5em 1em"
          :width "100%"
          :border "none"
          :outline "none"
          :text-align "left"
          :cursor "pointer")
        `((:and .tab-button :hover)
          :background-color ,theme:background-color-)
        `(.tab-button.active
          :background-color ,theme:background-color-
          :color ,theme:on-background-color)))
    (:div.row
     :style "min-height: 100%; margin: 0"
     (:div.tabs
      (:a.tab-button
       :class (when (equal section 'keybindings) "active")
       :href (nyxt-url 'common-settings :section 'keybindings)
       "Keyboard shortcuts")
      (:a.tab-button
       :class (when (equal section 'theme-and-style) "active")
       :href (nyxt-url 'common-settings :section 'theme-and-style)
       "Theme & Style")
      (:a.tab-button
       :class (when (equal section 'buffer-defaults) "active")
       :href (nyxt-url 'common-settings :section 'buffer-defaults)
       "Buffer settings"))
     (:div.content
      (:div.section
       (:h2 "Settings applied on restart."))
      (case section
        (keybindings
         (:div.section
          (:h3 "Keybindings")
          (:div.row
           (:div.left
            (:nradio
              :name "keyscheme"
              :vertical t
              :checked (cond ((find "nyxt/mode/vi" (default-modes (current-buffer))
                                    :key #'uiop:symbol-package-name :test #'string-equal)
                              'vi)
                             ((find "nyxt/mode/emacs" (default-modes (current-buffer))
                                    :key #'uiop:symbol-package-name :test #'string-equal)
                              'emacs)
                             (t 'cua))
              :buffer buffer
              '(cua "CUA (default)"
                (nyxt::auto-configure
                 :form '(define-configuration (input-buffer)
                         ((default-modes (remove-if (lambda (m)
                                                      (find (symbol-name m)
                                                            '("EMACS-MODE" "VI-NORMAL-MODE" "VI-INSERT-MODE")
                                                            :test #'string=))
                                          %slot-value%))))))
              '(emacs "Emacs"
                (nyxt::auto-configure
                 :form '(define-configuration (input-buffer)
                         ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))))
              '(vi "vi"
                (progn
                  (nyxt::auto-configure
                   :form '(define-configuration (input-buffer)
                           ((default-modes (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%)))))
                  (nyxt::auto-configure
                   :form '(define-configuration (prompt-buffer)
                           ((default-modes (pushnew 'nyxt/mode/vi:vi-insert-mode %slot-value%)))))))))
           (:div.right
            (:p "Set the default keybindings (requires restarting Nyxt).")
            (:p "Keybindings can also be enabled on a per-buffer basis by
invoking the " (:nxref :command 'toggle-modes) "command.")))))
        (theme-and-style
         (:div.section
          (:h3 "Browser interface")
          (:div.row
           (:div.left
            (:nradio
              :name "theme"
              :checked (if (eq (theme *browser*) theme:+light-theme+)
                           'theme:+light-theme+
                           'theme:+dark-theme+)
              :vertical t
              :buffer buffer
              '(theme:+light-theme+ "Light theme"
                (nyxt::auto-configure :form '(define-configuration browser
                                              ((theme theme:+light-theme+)))))
              '(theme:+dark-theme+ "Dark theme"
                (nyxt::auto-configure :form '(define-configuration browser
                                              ((theme theme:+dark-theme+)))))))))
         (:div.section
          (:h3 "Webpage theme")
          (:div.row
           (:div.left
            (:nradio
              :name "darken"
              :checked (if (find 'nyxt/mode/style:dark-mode (default-modes (current-buffer)))
                           'dark
                           'auto)
              :vertical t
              :buffer buffer
              '(auto "Default"
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (remove-if (lambda (m)
                                                      (string= (symbol-name m) "DARK-MODE"))
                                          %slot-value%))))))
              '(dark "Darkened"
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (pushnew 'nyxt/mode/style:dark-mode %slot-value%))))))))))
         (:div.section
          (:h3 "Default zoom")
          (:div.row
           (:div.left
            (:nselect
              :id "default-zoom-ratio"
              :default (format nil "~a%" (* 100 (zoom-ratio-default (current-buffer))))
              (loop for number in '(30 50 67 80 90 100
                                    110 120 133 150 170
                                    200 240 300 400 500)
                    collect `((,number ,(format nil "~a%" number))
                              (nyxt::auto-configure
                               :class-name 'document-buffer
                               :slot 'zoom-ratio-default
                               :slot-value ,(/ number 100.0))))))
           (:div.right
            (:p "Nyxt remembers the zoom for each site and uses this ratio when a site has none.")))))
        (buffer-defaults
         (:div.section
          (:h3 "Homepage")
          (:div.row
           (:div.left
            (:nbutton :text "Set default URL"
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
                               (render-url (url url-or-history-entry))))))
           (:div.right
            (:p "By default, it is set to Nyxt's home screen."))))
         (:div.section
          (:h3 "Modes")
          (:div.row
           (:div.left
            (:nbutton :text "Set default modes"
              '(nyxt::configure-slot 'default-modes 'buffer
                :sources (make-instance
                          'mode-source
                          :marks (default-modes (current-buffer)))
                :postprocess (lambda (modes)
                               `(quote ,modes))
                :type 'cons)))
           (:div.right
            (:p "Specify default modes for new buffers.")
            (:p "Modes can also be set interactively by command "
                (:nxref :command 'toggle-modes)
                " ,or by specific mode togglers such as "
                (:nxref :command 'nyxt/mode/no-sound:no-sound-mode) "."))))))))))

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
    (:nstyle '(body :max-width "80ch"))
    (:ntoc
      (tutorial-content)
      (manual-content))))

(define-internal-page-command-global tutorial ()
    (buffer "*Tutorial*" 'nyxt/mode/help:help-mode)
  "Display Nyxt tutorial."
  (spinneret:with-html-string
    (:nstyle '(body :max-width "80ch"))
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

(define-internal-page-command-global list-extensions
    (&key (endpoint "https://nyxt-browser.com/api/extensions"))
    (buffer "*Nyxt extensions*" 'nyxt/mode/help:help-mode)
  "List available extensions for Nyxt.
The `:download-link' is overloaded as a reference URL for external extensions as
they are not served on the Nyxt website."
  (flet ((extension->html (extension)
           (spinneret:with-html
             (:dl
              (:dt "Name")
              (:dd (assoc-value extension :name))
              (:dt "Description")
              (:dd (assoc-value extension :description))
              (:dt "Link")
              (let ((link (if (assoc-value extension :internal-p)
                              (format nil "https://nyxt-browser.com~a"
                                      (assoc-value extension :link))
                              (assoc-value extension :download-link))))
                (:dd (:a :href link link)))))))
    (spinneret:with-html-string
      (:h1 "Nyxt extensions")
      (loop for extension in (cl-json:decode-json-from-string (dex:get endpoint))
            collect (:div (extension->html extension)
                          (:hr))))))
