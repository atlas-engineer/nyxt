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
                                    (sources 'prompter:raw-source))
  "Set value of CLASS' SLOT in `*auto-config-file*'.
Prompt for a new value with prompt SOURCES and type-check
 it against the SLOT's TYPE, if any. CLASS is a class symbol."
  (sera:nlet lp ()
    (let ((input (read-from-string
                  (prompt1
                   :prompt (format nil "Configure slot value ~a" slot)
                   :sources sources))))
      (cond
        ((and type (not (typep input type)))
         (echo-warning "Type mismatch for ~a: got ~a, expected ~a."
                       slot (type-of input) type)
         (lp))
        (t
         (auto-configure :class-name class :slot slot :slot-value input)
         (echo "Update slot ~s to ~s. You might need to restart to experience the change." slot input))))))

(define-internal-page-command-global common-settings ()
    (buffer "*Settings*" 'nyxt/mode/help:help-mode)
  "Display an interface to tweak frequently sought-after user options.
The changes are saved to `*auto-config-file*', and persist from one Nyxt session
to the next."
  (spinneret:with-html-string
    (:nstyle
      `(.radio-div
        :margin-top "1em")
      `(".radio-label,.checkbox-input"
        :display block
        :padding-bottom "0.5em")
      `(".radio-input,.checkbox-input"
        :display inline-block
        :margin-right "0.5em"
        :margin-left "3em")
      `(.button
        :display block
        :margin "1.5em 0 0.5em 3em")
      `(.section-div
        :margin 0
        :padding "0.5em 0 0.5em 1.5em")
      `(.title-div
        :margin 0
        :padding "0.5em 0 0.5em 1.5em")
      `("h1,h2,h5"
        :margin 0))
    (:div
     :class "title-div"
     (:h1 "Common Settings")
     (:p "Adjustments made here will persist across Nyxt sessions."))
    (:div
     :class "section-div"
     (:h2 "Keybinding scheme")
     (:nradio
       :name "keyscheme"
       :vertical t
       :buffer buffer
       '(cua "CUA (default)"
         (nyxt::auto-configure
          :form '(define-configuration (web-buffer prompt-buffer
                                        panel-buffer nyxt/mode/editor:editor-buffer)
                  ((default-modes (remove-if (lambda (m)
                                               (find (symbol-name m)
                                                     '("EMACS-MODE" "VI-NORMAL-MODE" "VI-INSERT-MODE")))
                                   %slot-value%))))))
       '(emacs "Emacs"
         (nyxt::auto-configure
          :form '(define-configuration (web-buffer prompt-buffer
                                        panel-buffer nyxt/mode/editor:editor-buffer)
                  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))))
       '(vi "VI"
         (nyxt::auto-configure
          :form '(define-configuration (web-buffer prompt-buffer
                                        panel-buffer nyxt/mode/editor:editor-buffer)
                  ((default-modes (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))))))
    (:div
     :class "section-div"
     (:h2 "Theme & style")
     (:div
      :class "section-div"
      (:h5 "Browser interface")
      (:nradio
        :name "theme"
        :vertical t
        :buffer buffer
        '(theme::+light-theme+ "Light theme"
          (nyxt::auto-configure :form '(define-configuration browser
                                        ((theme theme::+light-theme+)))))
        '(theme::+light-theme+ "Dark theme"
          (nyxt::auto-configure :form '(define-configuration browser
                                        ((theme theme::+dark-theme+)))))))
     (:div
      :class "section-div"
      (:h5 "Webpage theme")
        (:nradio
          :name "darken"
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
      (:div
       :class "section-div"
        (:h5 "Default zoom ratio")
        (:nselect
          :id "default-zoom-ratio"
          :default "100%"
          (loop for number in '(30 50 67 80 90 100
                                110 120 133 150 170
                                200 240 300 400 500)
                collect `((,number ,(format nil "~a%" number))
                          (nyxt::auto-configure
                           :class-name 'document-buffer
                           :slot 'zoom-ratio-default
                           :slot-value ,(/ number 100.0)))))))
    (:div
     :class "section-div"
     (:h2 "New Buffer Settings")
     (:div
      :class "section-div"
      (:h5 "Homepage")
      (:nbutton :text "Set default new buffer URL"
       '(nyxt::configure-slot 'default-new-buffer-url 'browser :type 'string)))
     (:div
      :class "section-div"
      (:h5 "Session")
      (:ncheckbox
        :name "restore-session"
        :buffer buffer
        '((restore-session-on-startup-p "Restore session on startup")
          (nyxt::auto-configure
           :class-name 'browser
           :slot 'restore-session-on-startup-p
           :slot-value t)
          (nyxt::auto-configure
           :class-name 'browser
           :slot 'restore-session-on-startup-p
           :slot-value nil)))))
    (:h2 "Miscellaneous")
    (:ul
     (:nbutton :text "Set default new buffer URL"
       '(nyxt::configure-slot 'default-new-buffer-url 'browser :type 'string))
     (:nbutton :text "Set default zoom ratio"
       '(nyxt::configure-slot 'zoom-ratio-default 'document-buffer))
     (:p "On some systems, compositing can cause issues with rendering. If
you are experiencing blank web-views, you can try to disable compositing. After
disabling compositing, you will need to restart Nyxt.")
     (:nbutton :text "Disable compositing"
       '(nyxt::auto-configure
         :form '(setf (uiop:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1")))

     (:label
      "Edit user configuration and other files in external text editor."
      (:nbutton :text "Edit user files"
        '(nyxt::edit-user-file-with-external-editor))))))

(define-command print-bindings ()
  "Display all known bindings for the current buffer."
  (nyxt::html-set-style (theme:themed-css (theme *browser*)
                          `(h3
                            :font-size "10px"
                            :font-family ,theme:font-family
                            :font-weight 500)
                          `(tr
                            :font-size "7px")
                          `(div
                            :display inline-block))
                        (describe-bindings))
  (nyxt/mode/document:print-buffer))

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
        :color ,theme:on-background
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

(define-internal-page-command-global dashboard ()
    (buffer "*Dashboard*")
  "Display a dashboard featuring bookmarks, recent URLs and other useful actions."
  (flet ((list-bookmarks (&key (limit 50) (separator " → "))
           (spinneret:with-html-string
             (let ((mode (make-instance 'nyxt/mode/bookmark:bookmark-mode)))
               (alex:if-let ((bookmarks (files:content (nyxt/mode/bookmark:bookmarks-file mode))))
                 (dolist (bookmark (sera:take limit (the list (sort-by-time bookmarks :key #'nyxt/mode/bookmark:date))))
                   (:li (title bookmark) separator
                        (:a :href (render-url (url bookmark))
                            (render-url (url bookmark)))))
                 (:p (format nil "No bookmarks in ~s." (files:expand (nyxt/mode/bookmark:bookmarks-file mode)))))))))
    (let ((dashboard-style (theme:themed-css (theme *browser*)
                             `(body
                               :background-color ,theme:background
                               :color ,theme:on-background
                               :margin-top 0
                               :margin-bottom 0)
                             `("#title"
                               :font-size "400%")
                             `("#subtitle"
                               :color ,theme:secondary)
                             `(.section
                               :border-style "solid none none none"
                               :border-color ,theme:secondary
                               :margin-top "10px"
                               :overflow "scroll"
                               :min-height "150px")
                             `("h3"
                               :color ,theme:secondary)
                             `("ul"
                               :list-style-type "circle"))))
      (spinneret:with-html-string
        (:nstyle dashboard-style)
        (:div
         (:h1 :id "title" "Nyxt " (:span :id "subtitle" "browser ☺"))
         (:h3 (time:format-timestring nil (time:now) :format time:+rfc-1123-format+))
         (:nbutton :text "🗁 Restore Session"
           '(nyxt::restore-history-by-name))
         (:a :class "button" :href (nyxt-url 'manual) "🕮 Manual")
         (:nbutton
           :text "≡ Execute Command"
           '(nyxt::execute-command))
         (:a :class "button" :href "https://nyxt.atlas.engineer/download" "⇡ Update"))
        (:h3 (:b "Recent URLs"))
        (:ul (:raw (history-html-list :limit 50)))
        (:h3 (:b "Recent bookmarks"))
        (:ul (:raw (list-bookmarks :limit 50)))))))
