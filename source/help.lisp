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
        `("body,h3"
          :margin 0)
        `(".radio-div,.checkbox-div"
          :margin-top "1em")
        `(".radio-label,.checkbox-input"
          :display block
          :padding-bottom "0.5em")
        `(".radio-input,.checkbox-input"
          :display inline-block
          :margin-right "0.5em"
          :margin-left "3em")
        `("select.button,.button"
          :display block
          :margin "1em 0 0.5em 2.5em")
        `(.section
          :margin 0
          :padding "2em 0 0.5em 1.5em")
        `(.row
          :display "flex"
          :margin-top "1em")
        `(.tabs
          :flex "0 0 180px"
          :background-color ,theme:background+)
        `(.content
          :flex "85%"
          :background-color ,theme:background-)
        `(.left
          :flex "0 0 256px")
        `(.right
          :color ,theme:primary
          :padding-top "1em"
          :padding-left "4em"
          :max-width "50ch")
        `(p
          :margin 0
          :padding-bottom "0.5em")
        `(.tab-button
          :display "block"
          :text-decoration "none"
          :background-color ,theme:background+
          :color ,theme:action-
          :padding "1.5em 1em"
          :width "100%"
          :border "none"
          :outline "none"
          :text-align "left"
          :cursor "pointer")
        `((:and .tab-button :hover)
          :background-color ,theme:background-)
        `(.tab-button.active
          :background-color ,theme:background-
          :color ,theme:text-)))
    (:div.row
     :style "min-height: 100%; margin: 0"
     (:div.tabs
      (:a.tab-button
       :class (when (equal section 'keybindings) "active")
       :href (nyxt-url 'common-settings :section 'keybindings)
       "Keybindings")
      (:a.tab-button
       :class (when (equal section 'theme-and-style) "active")
       :href (nyxt-url 'common-settings :section 'theme-and-style)
       "Theme & Style")
      (:a.tab-button
       :class (when (equal section 'buffer-defaults) "active")
       :href (nyxt-url 'common-settings :section 'buffer-defaults)
       "Buffer settings")
      (:a.tab-button
       :class (when (equal section 'privacy-and-security) "active")
       :href (nyxt-url 'common-settings :section 'privacy-and-security)
       "Privacy & Security")
      (:a.tab-button
       :class (when (equal section 'text-and-code) "active")
       :href (nyxt-url 'common-settings :section 'text-and-code)
       "Text Editing"))
     (:div.content
      (case section
        (keybindings
         (:div.section
          (:h3 "Keybinding mode")
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
                (nyxt::auto-configure
                 :form '(define-configuration (input-buffer)
                         ((default-modes (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))))))
           (:div.right
            (:p "For individual buffers - keyschemes can be toggled with the (toggle-modes) command.")
            (:p "For a persistent keyscheme each time you start Nyxt - make a selection here.")))))
        (theme-and-style
         (:div.section
          (:h3 "Browser interface")
          (:div.row
           (:div.left
            (:nradio
              :name "theme"
              :checked (if (equal (theme:background-color (theme *browser*))
                                  (theme:background-color theme::+light-theme+))
                           'theme::+light-theme+
                           'theme::+dark-theme+)
              :vertical t
              :buffer buffer
              '(theme::+light-theme+ "Light theme"
                (nyxt::auto-configure :form '(define-configuration browser
                                              ((theme theme::+light-theme+)))))
              '(theme::+dark-theme+ "Dark theme"
                (nyxt::auto-configure :form '(define-configuration browser
                                              ((theme theme::+dark-theme+)))))))
           (:div.right
            (:p "Themes for the browser interface, panel buffers, and internal Nyxt pages like manual, bindings, and common settings."))))
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
              '(auto "Default Web"
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (remove-if (lambda (m)
                                                      (string= (symbol-name m) "DARK-MODE"))
                                          %slot-value%))))))
              '(dark "Darkened Web"
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (pushnew 'nyxt/mode/style:dark-mode %slot-value%))))))))
           (:div.right
            (:p "Select Default Web to view webpages as they are designed.")
            (:p "Select Dark Web to have Nyxt darken any webpage you visit."))))
         (:div.section
          (:h3 "Default zoom ratio")
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
            (:p "Incremental changes advised.")))))
        (buffer-defaults
         (:div.section
          (:h3 "Homepage")
          (:div.row
           (:div.left
            (:nbutton :text "Set default new buffer URL"
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
            (:p "Choose your homepage. By default your homepage is set to the internal Nyxt page (nyxt:new)."))))
         (:div.section
          (:h3 "Session")
          (:div.row
           (:div.left
            (:ncheckbox
              :name "restore-session"
              :checked (restore-session-on-startup-p *browser*)
              :buffer buffer
              '((restore-session-on-startup-p "Restore session on startup")
                (nyxt::auto-configure
                 :class-name 'browser
                 :slot 'restore-session-on-startup-p
                 :slot-value t)
                (nyxt::auto-configure
                 :class-name 'browser
                 :slot 'restore-session-on-startup-p
                 :slot-value nil))))
           (:div.right
            (:p "Choose whether to restore buffers from previous sessions."))))
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
            (:p "Specify default modes for all new buffers.")
            (:p "To set modes for individual buffers, use the (toggle-modes) command, or a specific command like (toggle-no-script-mode).")))))
        (privacy-and-security
         (:div.section
          (:h3 "Privacy & Security Modes")
          (:div.row
           (:div.left
            (:ncheckbox
              :name "blocker-mode"
              :checked (when (find 'nyxt/mode/blocker:blocker-mode (default-modes (current-buffer))) t)
              :buffer buffer
              '((blocker-mode "Blocker mode")
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%)))))
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (remove-if (lambda (m)
                                                      (string= (symbol-name m) "BLOCKER-MODE"))
                                          %slot-value%)))))))
            (:ncheckbox
              :name "no-script-mode"
              :checked (when (find 'nyxt/mode/no-script:no-script-mode (default-modes (current-buffer))) t)
              :buffer buffer
              '((no-script-mode "No-Script mode")
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (pushnew 'nyxt/mode/no-script:no-script-mode %slot-value%)))))
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (remove-if (lambda (m)
                                                      (string= (symbol-name m) "NO-SCRIPT-MODE"))
                                          %slot-value%)))))))
            (:ncheckbox
              :name "reduce-tracking-mode"
              :checked (when (find 'nyxt/mode/reduce-tracking:reduce-tracking-mode (default-modes (current-buffer))) t)
              :buffer buffer
              '((reduce-tracking-mode "Reduce-Tracking mode")
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (pushnew 'nyxt/mode/reduce-tracking:reduce-tracking-mode %slot-value%)))))
                (nyxt::auto-configure
                 :form '(define-configuration (web-buffer)
                         ((default-modes (remove-if (lambda (m)
                                                      (string= (symbol-name m) "REDUCE-TRACKING-MODE"))
                                          %slot-value%))))))))
           (:div.right
            (:p "Select commonly used security modes here to add them to your default modes for new buffers."))))
         (:div.section
          (:h3 "Cookie policy")
          (:div.row
           (:div.left
            (:nradio
              :name "default-cookie-policy"
              :vertical t
              :checked (default-cookie-policy *browser*)
              :buffer buffer
              '(:no-third-party "No third party"
                (nyxt::auto-configure
                 :class-name 'browser
                 :slot 'default-cookie-policy
                 :slot-value :no-third-party))
              '(:accept "Always accept"
                (nyxt::auto-configure
                 :class-name 'browser
                 :slot 'default-cookie-policy
                 :slot-value :accept))
              '(:never "Never accept"
                (nyxt::auto-configure
                 :class-name 'browser
                 :slot 'default-cookie-policy
                 :slot-value :never))))
           (:div.right
            (:p "To accept cookies for current website only, choose \"No third party\".")
            (:p "To always accept cookies, choose \"Always accept\".")
            (:p "To never accept cookies, choose \"Never accept\".")))))
        (text-and-code
         (:div.section
          (:h3 "Edit user files")
          (:div.row
           (:div.left
            (:nbutton :text "Use internal editor"
              '(nyxt/mode/editor::edit-user-file))
            (:nbutton :text "Use external editor"
              '(nyxt::edit-user-file-with-external-editor)))
           (:div.right
            (:p "To use the external editor, you need to set the VISUAL or EDITOR environment variables on your system.")
            (:p "Otherwise, you can use the basic internal text editor."))))))))))

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
  (trivial-clipboard:text +version+)
  (echo "Version ~a" +version+))

(define-internal-page-command-global new ()
    (buffer "*New buffer*")
  "Display a page suitable as `default-new-buffer-url'."
  (spinneret:with-html-string
    (:nstyle
      `(body
        :min-height "100vh"
        :padding "0"
        :margin "0")
      `(nav
        :text-align "center")
      `(.container
        :padding-top "32px"
        :display "flex"
        :flex-direction "row"
        :justify-content "center")
      `(.button
        :background-color ,theme:secondary
        :border-color ,theme:secondary
        :color ,theme:on-secondary
        :min-width "144px")
      `(.copyright
        :position "absolute"
        :bottom "12px"
        :right "48px")
      `(.program-name
        :color ,theme:action
        :font-size "24px"
        :font-weight "bold")
      `(.main
        :margin-top "35vh"
        :display "flex"
        :flex-direction "row")
      `(.logo
        :color ,(if (theme:dark-p theme:theme) theme:action theme:on-background)
        :width "100px"
        :height "100px"
        :padding-top "3px"
        :margin-right "12px")
      `(.set-url
        :min-width "180px"
        :height "40px"
        :line-height "30px"
        :color ,theme:on-action
        :background-color ,theme:action
        :border "none"
        :border-width "2px"
        :border-radius "3px"
        :margin-bottom "17px")
      `(.execute-command
        :min-width "180px"
        :line-height "12px"
        :height "40px"
        :border "none"
        :background-color ,theme:primary
        :border-color ,theme:primary
        :color ,theme:on-primary)
      `(.binding
        :margin-left "12px"
        :font-weight "bold"
        :color ,theme:secondary))
    (:div
     :class "container"
     (:main
      (:nav
       (:nbutton :text "Quick-Start"
         '(quick-start))
       (:a :class "button" :href (nyxt-url 'describe-bindings)
           :title "List all bindings for the current buffer."
           "Describe-Bindings")
       (:a :class "button" :href (nyxt-url 'manual)
           :title "Full documentation about Nyxt, how it works and how to configure it."
           "Manual")
       (:a :class "button" :href (nyxt-url 'common-settings)
           :title "Switch between Emacs/vi/CUA key bindings, set home page URL, and zoom level."
           "Settings"))
      (:div :class "main"
            (:div :class "logo" (:raw (glyph-logo *browser*)))
            (:div
             (:div (:nbutton :class "set-url" :text "Set-URL"
                     '(set-url :prefill-current-url-p nil))
                   (:span :class "binding"
                          (format nil "(~a)" (nyxt::binding-keys 'set-url))))
             (:div (:nbutton :class "execute-command" :text "Execute-Command"
                     '(execute-command))
                   (:span :class "binding"
                          (format nil "(~a)" (nyxt::binding-keys 'execute-command)))))))
     (:p :class "copyright"
         (:span :class "program-name" "Nyxt")
         (format nil " ~a (~a)" +version+ (name *renderer*))
         (:br)
         (format nil "Atlas Engineer, 2018-~a" (time:timestamp-year (time:now)))))))

(sera:eval-always ; To satisfy `fboundp' of `manual' at compile-time (e.g. CCL).
  (define-internal-page-command-global manual ()
      (buffer "*Manual*" 'nyxt/mode/help:help-mode)
    "Display Nyxt manual."
    (spinneret:with-html-string
      (:nstyle '(body :max-width "80ch"))
      (:raw (manual-content)))))

(define-internal-page-command-global tutorial ()
    (buffer "*Tutorial*" 'nyxt/mode/help:help-mode)
  "Display Nyxt tutorial."
  (spinneret:with-html-string
    (:nstyle '(body :max-width "80ch"))
    (:h1 "Nyxt tutorial")
    (:p "The following tutorial introduces core concepts and
basic usage.  For more details, especially regarding configuration, see
the " (:code (:a.link :href (nyxt-url 'manual) "manual")) ".")
    (:raw (tutorial-content))))

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
