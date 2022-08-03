;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmacro command-markup (fn &key (modes nil explicit-modes-p))
  "Print FN in HTML followed by its keybinding in parentheses."
  `(let ((spinneret:*suppress-inserted-spaces* t))
     (spinneret:with-html
       (:span (:nxref :command ,fn)
              " ("
              (:code (apply #'binding-keys ,fn (if ,explicit-modes-p
                                                   (list :modes ,modes)
                                                   '())))
              ")"))))

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
  `(spinneret:with-html (:li (command-markup ,fn) ": " (command-docstring-first-sentence ,fn))))

(defun list-command-information (fns)
  "Print information over a list of commands in HTML."
  (dolist (i fns)
    (command-information i)))

(defun configure-slot (slot class &key
                                    (type (getf (mopu:slot-properties (find-class class) slot)
                                                :type)))
  "Set value of CLASS' SLOT in `*auto-config-file*'.
CLASS is a class symbol."
  (sera:nlet lp ()
    (let ((input (read-from-string
                  (prompt1
                   :prompt (format nil "Configure slot value ~a" slot)
                   :sources 'prompter:raw-source))))
      (cond
        ((and type (not (typep input type)))
         (echo-warning "Type mismatch for ~a: got ~a, expected ~a."
                       slot (type-of input) type)
         (lp))
        (t
         (auto-configure :class-name class :slot slot :slot-value input)
         (echo "Update slot ~s to ~s. You might need to restart to experience the change." slot input))))))

(define-internal-page-command-global common-settings ()
    (buffer "*Settings*" 'nyxt/help-mode:help-mode)
  "Configure a set of frequently used settings."
  (spinneret:with-html-string
    (:h1 "Common Settings")
    (:p "Set the values for frequently configured settings. "
        "Changes only apply to newly created buffers.")
    (:h2 "Keybinding style")
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  (:title "set-cua-scheme")
                                  (nyxt::auto-configure
                                   :class-name 'input-buffer
                                   :form '(disable-modes
                                           :modes '(nyxt/emacs-mode:emacs-mode)
                                           :buffers input-buffer
                                           :bypass-auto-rules-p t))
                                  (nyxt::auto-configure
                                   :class-name 'input-buffer
                                   :form '(disable-modes
                                           :modes '(nyxt/vi-mode:vi-normal-mode)
                                           :buffers input-buffer
                                           :bypass-auto-rules-p t))))
                 "Use default (CUA)"))
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  (:title "set-emacs-scheme")
                                  (nyxt::auto-configure
                                   :class-name 'input-buffer
                                   :form '(disable-modes
                                           :modes '(nyxt/vi-mode:vi-normal-mode)
                                           :buffers input-buffer
                                           :bypass-auto-rules-p t))
                                  (nyxt::auto-configure
                                   :class-name 'input-buffer
                                   :form '(enable-modes
                                           :modes '(nyxt/emacs-mode:emacs-mode)
                                           :buffers input-buffer
                                           :bypass-auto-rules-p t))))
                 "Use Emacs"))
    (:p (:button :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  (:title "set-vi-scheme")
                                  (nyxt::auto-configure
                                   :class-name 'input-buffer
                                   :form '(disable-modes
                                           :modes '(nyxt/emacs-mode:emacs-mode)
                                           :buffers input-buffer
                                           :bypass-auto-rules-p t))
                                  (nyxt::auto-configure
                                   :class-name 'input-buffer
                                   :form '(enable-modes
                                           :modes '(nyxt/vi-mode:vi-normal-mode)
                                           :buffers input-buffer
                                           :bypass-auto-rules-p t))))
                 "Use vi"))
    (flet ((generate-colors (theme-symbol text)
             (spinneret:with-html-string
               (:p (:button :class "button"
                            :style (format nil "background-color: ~a; color: ~a"
                                           (theme:accent-color (symbol-value theme-symbol))
                                           (theme:on-accent-color (symbol-value theme-symbol)))
                            :onclick (ps:ps (nyxt/ps:lisp-eval
                                             (:title "set-theme")
                                             (nyxt::auto-configure
                                              :class-name 'browser
                                              :slot 'theme
                                              :slot-value theme-symbol)))
                            text))
               (:p "Colors:")
               (:dl
                (loop for (name color text-color) in '(("Background" theme:background-color theme:on-background-color)
                                                       ("Accent" theme:accent-color theme:on-accent-color)
                                                       ("Primary" theme:primary-color theme:on-primary-color)
                                                       ("Secondary" theme:secondary-color theme:on-secondary-color))
                      collect (:dt name ": ")
                      collect (:dd (:span :style (format nil "background-color: ~a; color: ~a; border-radius: 0.2em"
                                                         (slot-value (symbol-value theme-symbol) color)
                                                         (slot-value (symbol-value theme-symbol) text-color))
                                          (slot-value (symbol-value theme-symbol) color))))))))
      (:h2 "Theme style")
      (:ul
       (:li (:raw (generate-colors 'theme::+light-theme+ "Use default (Light theme)")))
       (:li (:raw (generate-colors 'theme::+dark-theme+ "Use Dark theme")))))
    (:h2 "Miscellaneous")
    (:ul
     (:li (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    (:title "default-new-buffer-url")
                                    (nyxt::configure-slot 'default-new-buffer-url 'browser :type 'string)))
                   "Set default new buffer URL"))
     (:li (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    (:title "set-zoom-ration")
                                    (nyxt::configure-slot 'current-zoom-ratio 'document-buffer)))
                   "Set default zoom ratio"))
     (:li (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    (:title "disable-compositing")
                                    (nyxt::auto-configure
                                     :form '(setf (uiop:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1"))))
                   "Disable compositing")
          (:p "On some systems, compositing can cause issues with rendering. If
you are experiencing blank web-views, you can try to disable compositing. After
disabling compositing, you will need to restart Nyxt."))

     (:li (:button :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    (:title "edit-user-file")
                                    '(nyxt::edit-user-file-with-external-editor)))
                   "Edit user files")
          (:p "Edit user configuration and other files in external text editor.")))))

(define-command print-bindings-cheatsheet ()
  "Print a buffer listing all known bindings for the current buffer."
  (nyxt::html-set-style (theme:themed-css (theme *browser*)
                          (h3
                           :font-size "10px"
                           :font-family theme:font-family
                           :font-weight 500)
                          (tr
                           :font-size "7px")
                          (div
                           :display inline-block))
                        (describe-bindings))
  (nyxt/document-mode:print-buffer))

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
  "Version number of this version of Nyxt.
The version number is saved to clipboard."
  (trivial-clipboard:text +version+)
  (echo "Version ~a" +version+))

(define-internal-page-command-global new ()
    (buffer "*New buffer*")
  "Open up a buffer with useful links suitable for `default-new-buffer-url'."
  (spinneret:with-html-string
    (:style (:raw (theme:themed-css (theme *browser*)
                    (body
                     :min-height "100vh")
                    (nav
                     :text-align "center"
                     :top 0)
                    (details
                     :display "inline"
                     :margin "1em")
                    (h1
                     :font-size "5em"
                     :margin "0.1em")
                    (main
                     :padding "10%"
                     :text-align "center"
                     :display "flex"
                     :flex-direction "column"
                     :justify-content "center")
                    (.centered
                     :text-align "center")
                    (.button
                     :min-width "100px")
                    (.container
                     :min-height "100%")
                    (.copyright
                     :position "absolute"
                     :bottom "1em"
                     :right "1em"))))
    (:div
     :class "container"
     (:nav
      :class "centered"
      (:a :class "button" :href (nyxt-url 'tutorial)
          :title "An introduction to Nyxt core concepts."
          "Tutorial")
      (:a :class "button" :href (nyxt-url 'manual)
          :title "Full documentation about Nyxt, how it works and how to configure it."
          "Manual")
      (:a :class "button" :href (nyxt-url 'changelog)
          :title "Information about changes between Nyxt versions."
          "Change Log")
      (:a :class "button" :href (nyxt-url 'describe-bindings)
          :title "List all bindings for the current buffer."
          "List bindings")
      (:a :class "button" :href (nyxt-url 'common-settings)
          :title "Switch between Emacs/vi/CUA key bindings, set home page URL, and zoom level."
          "⚙ Settings")
      (:details
       (:summary :class "button" "Other useful links")
       (:a :class "button" :href "https://github.com/atlas-engineer/nyxt/"
           :title "Your contribution will be much appreciated :)"
           "Source Code")
       (:a :class "button" :href "https://www.youtube.com/channel/UC11mZYESUvaKFTaa3zZWHMQ"
           :title "A channel with tips and tricks of Nyxt by one of the developers."
           "Nyxt Academy")
       (:a :class "button" :href "https://nyxt.atlas.engineer/articles"
           :title "Learn more about why's and how's behind Nyxt features."
           "Articles")
       (:a :class "button" :href "https://nyxt.atlas.engineer/applications"
           :title "Check out the applications built on top of Nyxt!"
           "Applications")
       (:a :class "button" :href "https://store.nyxt.atlas.engineer/"
           :title "Buy Nyxt merchandise and support the development!"
           "Store")
       (:a :class "button" :href "https://github.com/atlas-engineer/nyxt/blob/master/documents/README.org"
           :title "Helpful tips for Nyxt hacking and contributing."
           "Developer Manual")
       (:a :class "button" :href "https://discourse.atlas.engineer/"
           :title "A forum for questions and ideas on Nyxt."
           "Forum")
       (:a :class "button" :href "https://kiwiirc.com/nextclient/irc.libera.chat/nyxt"
           :title "Chat with developers and other Nyxt users."
           "Chat")))
     (:main
      (:h1 :class "accent" "Nyxt")
      (:i "The Internet on your terms.")
      (:p (:button :class "button"
                   :type "submit"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    (:title "search")
                                    (set-url :prefill-current-url-p nil)))
                   "Start searching!")))
     (:p :class "copyright"
         (format nil "Nyxt/~a ~a" +renderer+ +version+)
         (:br)
         (format nil "Atlas Engineer LLC, 2018-~a" (time:timestamp-year (time:now)))))))

(sera:eval-always ; To satisfy `fboundp' of `manual' at compile-time (e.g. CCL).
  (define-internal-page-command-global manual ()
      (buffer "*Manual*" 'nyxt/help-mode:help-mode)
    "Show the manual."
    (spinneret:with-html-string
      (:style (cl-css:css '(("body"
                             :max-width "80ch"))))
      (:raw (manual-content)))))

(define-internal-page-command-global tutorial ()
    (buffer "*Tutorial*" 'nyxt/help-mode:help-mode)
  "Show the tutorial."
  (spinneret:with-html-string
    (:style (cl-css:css '(("body"
                           :max-width "80ch"))))
    (:h1 "Nyxt tutorial")
    (:p "The following tutorial introduces core concepts and
basic usage.  For more details, especially regarding configuration, see
the "
        (:code (command-markup 'manual)) ".")
    (:raw (tutorial-content))))

(define-internal-page-command-global show-system-information ()
    (buffer "*System information*")
  "Show buffer with Lisp version, Lisp features, OS kernel, etc.
System information is also saved to clipboard."
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
  "Print a dashboard."
  (flet ((list-bookmarks (&key (limit 50) (separator " → "))
           (spinneret:with-html-string
             (let ((mode (make-instance 'nyxt/bookmark-mode:bookmark-mode)))
               (alex:if-let ((bookmarks (files:content (nyxt/bookmark-mode:bookmarks-file mode))))
                 (dolist (bookmark (sera:take limit (the list (sort-by-time bookmarks :key #'nyxt/bookmark-mode:date))))
                   (:li (title bookmark) separator
                        (:a :href (render-url (url bookmark))
                            (render-url (url bookmark)))))
                 (:p (format nil "No bookmarks in ~s." (files:expand (nyxt/bookmark-mode:bookmarks-file mode)))))))))
    (let ((dashboard-style (theme:themed-css (theme *browser*)
                             (body
                              :background-color theme:background
                              :color theme:on-background
                              :margin-top 0
                              :margin-bottom 0)
                             ("#title"
                              :font-size "400%")
                             ("#subtitle"
                              :color theme:secondary)
                             (.section
                              :border-style "solid none none none"
                              :border-color theme:secondary
                              :margin-top "10px"
                              :overflow "scroll"
                              :min-height "150px")
                             ("h3"
                              :color theme:secondary)
                             ("ul"
                              :list-style-type "circle"))))
      (spinneret:with-html-string
        (:style dashboard-style)
        (:div
         (:h1 :id "title" "Nyxt " (:span :id "subtitle" "browser ☺"))
         (:h3 (time:format-timestring nil (time:now) :format time:+rfc-1123-format+))
         (:button :class "button" :onclick (ps:ps (nyxt/ps:lisp-eval
                                                   (:title "restore-session")
                                                   (nyxt::restore-history-by-name)))
                  "🗁 Restore Session")
         (:a :class "button" :href (nyxt-url 'manual) "🕮 Manual")
         (:button :class "button"
                  :onclick (ps:ps (nyxt/ps:lisp-eval
                                   (:title "execute-command")
                                   (nyxt::execute-command)))
                  "≡ Execute Command")
         (:a :class "button" :href "https://nyxt.atlas.engineer/download" "⇡ Update"))
        (:h3 (:b "Recent URLs"))
        (:ul (:raw (history-html-list :limit 50)))
        (:h3 (:b "Recent bookmarks"))
        (:ul (:raw (list-bookmarks :limit 50)))))))
