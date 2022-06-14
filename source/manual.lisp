;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun manual-content ()
  (str:concat
   (spinneret:with-html-string
     (:h1 "Nyxt manual")
     (:p "This manual first includes the tutorial, then covers the configuration
of Nyxt."))
   (tutorial-content)
   (config-content)))

(defun config-content ()
  (spinneret:with-html-string
    (:h2 "Configuration")
    (:p "Nyxt is written in the Common Lisp programming language which offers a
great perk: everything in the browser can be customized by the user, even while
it's running!")
    (:p "To get started with Common Lisp, we recommend checking out
    our web page: "
        (:a :href "https://nyxt.atlas.engineer/learn-lisp" "Learn Lisp")
        ". It contains numerous pointers to other resources, including
        free books both for beginners and seasoned programmers.")
    (:p "Nyxt provides a mechanism for new users unfamiliar with Lisp to
customize Nyxt. Start by invoking the commands " (command-markup 'describe-class) "
or " (command-markup 'describe-slot) ".  You can press the button marked 'Configure' to
change the value of a setting. The settings will be applied immediately and
saved for future sessions. Please note that these settings will not alter
existing object instances.")
    (:p "Settings created by Nyxt are stored in "
        (:code (files:expand *auto-config-file*)) ".")
    (:p "Any settings can be overridden manually by "
        (:code (files:expand *config-file*)) ".")
    (:p "The following section assumes knowledge of basic Common Lisp or a
similar programming language.")
    (:p "The user needs to manually create the Nyxt configuration file, and the parent folders if necessary."
        (when (and (current-buffer)     ; In case manual is dumped.
                   (not (files:nil-pathname-p (files:expand *config-file*))))
          (:p
           "You can also press the button below to create said file, if it's not
created yet."
           (let ((config-file-path (files:expand *config-file*)))
             (:p (:a :class "button"
                     :href (ps:ps (nyxt/ps:lisp-eval
                                   (:title "maybe-create-config-file")
                                   (if (uiop:file-exists-p config-file-path)
                                       (echo "Configuration file exists")
                                       (progn (ensure-directories-exist config-file-path)
                                              (ensure-file-exists config-file-path)
                                              (echo "Configuration file created at ~s."
                                                    config-file-path)))))
                     "Create configuration file"))))))

    (:p "Example:")
    (:pre (:code "
\(define-configuration buffer
  ((default-modes (append '(no-script-mode) %slot-default%))))"))
    (:p "The above turns on the 'no-script-mode' (disables JavaScript) by default for
every buffer.")
    (:p "The " (:code "define-configuration") " macro can be used to customize
the slots of classes like the browser, buffers, windows, etc.  Refer to the
class and slot documentation for the individual details.")
    (:p "To find out about all modes known to Nyxt,
run " (command-markup 'describe-command) " and type 'mode'.")

    (:h3 "Slot configuration")
    (:p "Slots store values that can be either accessed (get) or changed
(set). Setting new values for slots allows many possibilities of customization.
For instance, keyboard layouts vary across the world. The slot "
        (:code "hints-alphabet")
        " has the default value of "
        (:code "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        ". If the user has an American keyboard, they can do:")
    (:ol
     (:li "Execute command " (command-markup 'describe-slot) ";")
     (:li "Type " (:code 'hints-alphabet)";")
     (:li "Select " (:code "hints-alphabet") " (" (:code "hint-mode") " class option);")
     (:li "Press the button " (:code "Configure") ", and;")
     (:li "Insert the string \"asfdghjkl\"") ".")
    (:p "This will make link-hinting more comfortable for this user. In
addition, other similar approaches of customization can be applied to slots
such as " (:code "spell-check-language") ", which can be expanded to do the
spelling-check of other languages besides English.")
    (:h3 "Different types of buffers")
    (:p "There are multiple buffer classes, such as `document-buffer' (for
structured documents) and `input-buffer' (for buffers that can receive user
input).  A `web-buffer' class is used for web pages, `prompt-buffer' for, well,
the prompt buffer.  Some buffer classes may inherit from multiple other classes.
For instance `web-buffer' and `prompt-buffer' both inherit from
`input-buffer'.")
    (:p "You can configure one of the parent `buffer' classes slots and the new
values will automatically cascade down as a new default for all child classes-
unless this slot is specialized by these child classes.
For instance if you configure the `override-map' slot in `input-buffer', both
`panel-buffer' and `web-buffer' classes will inherit from the new value.")

    (:h3 "Keybinding configuration")
    (:p "Nyxt supports multiple " (:i "bindings schemes") " such as CUA (the
    default), Emacs or vi.  Changing scheme is as simple as running the
    corresponding mode, e.g. "
        (:code "emacs-mode") ".  To make the change persistent across sessions,
add the following to your configuration:")
    (:ul
     (:li "vi bindings:"
          (:pre (:code "
\(define-configuration buffer
  ((default-modes (append '(vi-normal-mode) %slot-default%))))")))
     (:li "Emacs bindings:"
          (:pre (:code "
\(define-configuration buffer
  ((default-modes (append '(emacs-mode) %slot-default%))))"))))
    (:p "You can create new scheme names with " (:code "keymap:make-scheme-name")
        ".  Also see the " (:code "scheme-name") " class and the "
        (:code "define-scheme") " macro.")
    (:p "To extend the bindings of a specific mode, you can extend the mode with "
        (:code "define-configuration") " and extend its binding scheme with "
        (:code "define-scheme") ". For example:")
    (:pre (:code "
\(define-configuration base-mode
  ((keymap-scheme
    (define-scheme (:name-prefix \"my-base\" :import %slot-default%)
      scheme:vi-normal
      (list \"g b\" (make-command switch-buffer* ()
                    (switch-buffer :current-is-last-p t)))))))"))
    (:p "The " (:code "override-map") " is a keymap that has priority over
all other keymaps.  By default, it has few bindings like the one
for " (command-markup 'execute-command) ".  You can use it to set keys globally:")
    (:pre (:code "
\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"override-map\")))
                   (define-key map
                     \"M-x\" 'execute-command
                     \"C-space\" 'nothing)))))"))
    (:p "The " (:code "nothing") " command is useful to override bindings to do
nothing. Note that it's possible to bind any command, including those of
disabled modes that are not listed in " (command-markup 'execute-command) ".")
    (:p "In addition, a more flexible approach is to create your own mode with
your custom keybindings.  When this mode is added first to the buffer mode list,
its keybindings have priorities over the other modes.
Note that this kind of global keymaps also have priority over regular character
insertion, so you should probably not bind anything without modifiers in such a
keymap.")
    (:pre (:code "
\(defvar *my-keymap* (make-keymap \"my-map\"))
\(define-key *my-keymap*
  \"C-f\" 'nyxt/history-mode:history-forwards
  \"C-b\" 'nyxt/history-mode:history-backwards)

\(define-mode my-mode ()
  \"Dummy mode for the custom key bindings in `*my-keymap*'.\"
  ((keymap-scheme (keymap:make-scheme
                   scheme:cua *my-keymap*
                   scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*))))

\(define-configuration (buffer web-buffer)
  ((default-modes (append '(my-mode) %slot-default%))))"))

    (:p "Bindings are subject to various translations as per "
        (:code "keymap:*translator*") ". "
        "By default if it fails to find a binding it tries again with inverted
shifts.  For instance if " (:code "C-x C-F") " fails to match anything " (:code "C-x C-f")
        " is tried."
        "See the default value of " (:code "keymap:*translator*") " to learn how to
         custsomize it or set it to " (:code "nil") " to disable all forms of
         translation.")

    (:h3 "Search engines")
    (:p "See the " (:code "search-engines") " buffer slot documentation.
Bookmarks can also be used as search engines, see the corresponding section.")
    (:p "Nyxt comes with some default search engines for "
        (:code (format nil "~{~a~^, ~}"
                       (mapcar (lambda (engine)
                                 (quri:uri-host (quri:uri (getf engine :search-url))))
                               (rest (getf (mopu:slot-properties 'context-buffer 'search-engines)
                                           :initform)))))
        ". "
        "The following example shows one way to add new search engines.")
    (:pre (:code "
\(defvar *my-search-engines*
  (list
   '(\"python3\" \"https://docs.python.org/3/search.html?q=~a\" \"https://docs.python.org/3\")\
   '(\"doi\" \"https://dx.doi.org/~a\" \"https://dx.doi.org/\")\)
  \"List of search engines.\")

(define-configuration buffer
  ((search-engines (append (mapcar (lambda (engine) (apply 'make-search-engine engine))
                                   *my-search-engines*)
                           %slot-default%))))"))
    (:p "Note that the last search engine is the default one. For example, in
order to make python3 the default, the above code can be slightly modified as
follows.")
    (:pre (:code "
\(defvar *my-search-engines*
  (list
   '(\"doi\" \"https://dx.doi.org/~a\" \"https://dx.doi.org/\")
   '(\"python3\" \"https://docs.python.org/3/search.html?q=~a\" \"https://docs.python.org/3\")))

(define-configuration buffer
  ((search-engines (append %slot-default%
                           (mapcar (lambda (engine) (apply 'make-search-engine engine))
                                   *my-search-engines*)))))"))

    (:h3 "URL-dispatchers")
    (:p "You can configure which actions to take depending on the URL to be
loaded.  For instance, you can configure which Torrent program to start to load
magnet links.  See the" (:code "url-dispatching-handler") " function
documentation.")

    (:h3 "Downloads")
    (:p "See the " (command-markup 'nyxt/download-mode:list-downloads) " command and the "
        (:code "download-path") " buffer slot documentation.")

    (:h3 "Proxy and Tor")
    (:p "See the " (:code "proxy-mode") " documentation.")

    (:h3 "Blocker mode")
    (:p "This mode blocks access to websites related to especific hosts. To see
all hosts being blocked, execute command " (:code "describe-variable") ", choose variable "
(:code "NYXT/BLOCKER-MODE:*DEFAULT-HOSTLIST*") ", and read data on "
(:code "nyxt/blocker-mode:url-body") " slot." " To customize host blocking, read the "
(:code "blocker-mode") " documentation.")

    (:h3 "Custom commands")
    (:p "Creating your own invocable commands is similar to creating a Common
Lisp function, except the form is " (:code "define-command") " instead of "
(:code "defun") ". If you want this command to be invocable outside of
        the context of a mode, use " (:code "define-command-global") ".")
    (:p "Example:")
    (:pre (:code
           "(define-command-global bookmark-url ()
  \"Query the user which URL to bookmark.\"
  (let ((url (prompt
              :prompt \"Bookmark URL\"
              :sources (make-instance 'prompter:raw-source))))
    (bookmark-add url)))"))
    (:p "See the " (:code "prompt-buffer") " class documentation for how to write
write custom prompt-buffers.")

    (:h3 "Hooks")
    (:p "Hooks provide a powerful mechanism to tweak the behaviour of various
events that occur in the context of windows, buffers, modes, etc.")
    (:p "A hook holds a list of " (:i "handlers") ".  Handlers are named and
typed functions.  Each hook has a dedicated handler constructor.")
    (:p
     "Hooks can be 'run', that is, their handlers are run according to
the " (:code "combination") " slot of the hook.  This combination is a function
of the handlers.  Depending on the combination, a hook can run the handlers
either in parallel, or in order until one fails, or even " (:i "compose")
     " them (pass the result of one as the input of the next).  The handler types
specify which input and output values are expected.")
    (:p "Many hooks are executed at different points in Nyxt, among others:
")
    (:ul
     (:li "Global hooks, such as " (:code "*after-init-hook*") ".")
     (:li "Window- or buffer-related hooks.")
     (:li "Commands 'before' and 'after' hooks.")
     (:li "Modes 'enable' and 'disable' hooks."))
    (:p "For instance, if you want to force 'old.reddit.com' over 'www.reddit.com', you
can set a hook like the following in your configuration file:")
    (:pre (:code "
\(defun old-reddit-handler (request-data)
  (let ((url (url request-data)))
    (setf (url request-data)
          (if (search \"reddit.com\" (quri:uri-host url))
              (progn
                (setf (quri:uri-host url) \"old.reddit.com\")
                (log:info \"Switching to old Reddit: ~s\" (render-url url))
                url)
              url)))
  request-data)

\(define-configuration web-buffer
  ((request-resource-hook
    (hooks:add-hook %slot-default% 'old-reddit-handler))))"))
    (:p "(See " (:code "url-dispatching-handler")
        " for a simpler way to achieve the same result.)")
    (:p "Or, if you want to set multiple handlers at once,")
    (:pre (:code "
\(define-configuration web-buffer
  ((request-resource-hook
    (reduce #'hooks:add-hook
            '(old-reddit-handler auto-proxy-handler)
            :initial-value %slot-default%))))"))
    (:p "Some hooks like the above example expect a return value, so it's
important to make sure we return " (:code "request-data") " here.  See the
documentation of the respective hooks for more details.")

    (:h3 "Data paths and data profiles")
    (:p "Nyxt provides a uniform configuration interface for all data files
persisted to disk (bookmarks, cookies, etc.).  To each file corresponds
a " (:code "nyxt-file") " object. An " (:code "nyxt-profile") " is a
customizable object that helps define general rules for data storage.  Both
nyxt-file and nyxt-profile compose, so it's possible to define general rules
for all files (even for those not known in advance) while it's also
possible to specialize some data given an nyxt-profile.")
    (:p "The profile can be set from command line and from the
configuration file."
        "You can list all known profiles (including the user-defined
profiles) with the " (:code "--list-profiles") " command-line option.")
    (:p "The nyxt-files can be passed a hint from the "
        (:code "--with-file") " command line option, but each nyxt-file and
profile rules are free to ignore it.")
    (:p "When a path ends with the " (:code ".gpg") " extension, by default your
GnuPG key is used to decrypt and encrypt the file transparently.  Refer to the
GnuPG documentation for how to set it up.")
    (:p "Note that the socket and the initialization nyxt-paths cannot be set in
your configuration (the socket is used before the initialization file is
loaded).  Instead you can specify these paths from their respective command-line
option.  You can instantiate a unique, separate Nyxt instance when you provide a
new socket path.  This is particularly useful in combination with profiles,
say to develop Nyxt or extensions.")
    (:p "Example to create a development profile that stores all data in "
        (:code "/tmp/nyxt") " and stores bookmark in an encrypted file:")
    (:pre (:code "
\(define-class dev-profile (nyxt-profile)
   ((files:name :initform \"nyxt-dev\"))
   (:documentation \"Development profile.\"))


\(defmethod files:resolve ((profile dev-profile) (path nyxt-file))
  \"Expand all data paths inside a temporary directory.\"
  (serapeum:path-join (files:expand (make-instance 'nyxt-temporary-directory))
                      (uiop:relativize-pathname-directory (call-next-method))))

\(defmethod nyxt:resolve ((profile dev-profile) (file history-file))
  \"Persist history to default location.\"
  (files:resolve *global-profile* file))

;; Make new profile the default:
\(define-configuration buffer
  ((profile (make-instance (or (find-profile-class (getf *options* :profile)) 'dev-profile)))))"))
    (:p "Then you can start a separate instance of Nyxt using this profile
with " (:code "nyxt --profile dev --socket /tmp/nyxt.socket") ".")

    (:h3 "Password management")
    (:p "Nyxt provides a uniform interface to some password managers including "
        (:a :href "https://keepassxc.org/" "KeepassXC")
        " and " (:a :href "https://www.passwordstore.org/" "Password Store") ". "
        "The supported installed password manager is automatically detected."
        "See the " (:code "password-interface") " buffer slot for customization.")
    (:p "You may use the " (:code "define-configuration") " macro with
any of the password interfaces to configure them. Please make sure to
use the package prefixed class name/slot designators within
the " (:code "define-configuration") " macro.")
    (:ul
     (:li (command-markup 'nyxt/password-mode:save-new-password) ": Query for name and new password to persist in the database.")
     (:li (command-markup 'nyxt/password-mode:copy-password) ": " (command-docstring-first-sentence 'nyxt/password-mode:copy-password)))

    (:h3 "Appearance")
    (:p "Much of the visual style can be configured by the user.  Search the
class slots for 'style'.  To customize the status buffer, see
the " (:code "status-buffer") " window slot.")

    (:h3 "Advanced configuration")
    (:p "While " (:code "define-configuration") " is convenient, it is mostly
restricted to class slot configuration.  If you want to do anything else on
class instantiation, you'll have to specialize the
lower-level " (:code "customize-instance") " generic function.  Example:"
(:pre (:code "
\(defmethod customize-instance ((buffer buffer) &key)
  (echo \"Buffer ~a created.\" buffer))")))
    (:p "All classes with metaclass " (:code "user-class") " call "
        (:code "customize-instance") " on instantiation,
after " (:code "initialize-instance :after") ".  The primary method is reserved
to the user, however the " (:code ":after") " method is reserved to the Nyxt
core to finalize the instance.")

    (:h3 "Scripting")
    (:p "You can evaluate code from the command line with "
        (:code "--eval") " and " (:code "--load") ".  From a shell:")
    (:pre (:code "$ nyxt --no-config --eval '+version+' \
  --load my-lib.lisp --eval '(format t \"Hello ~a!~&\" (my-lib:my-world))'"))
    (:p "You can evaluate multiple --eval and --load in a row, they are
executed in the order they appear.")
    (:p "You can also evaluate a Lisp file from the Nyxt interface with
the " (command-markup 'load-file) " command.  For
convenience, " (command-markup 'load-config-file) " (re)loads your initialization file.")
    (:p "You can even make scripts.  Here is an example foo.lisp:")
    (:pre (:code "#!/bin/sh
#|
exec nyxt --script \"$0\"
|#

;; Your code follows:
\(format t \"~a~&\" +version+)"))
    (:p "--eval and --load can be commanded to operate over an
existing instance instead of a separate instance that exits immediately.")
    (:p "The `remote-execution-p' slot of the `browser' class of the remote
instance must be non-nil.")
    (:p "To let know a private instance of Nyxt to load a foo.lisp script and run its
`foo' function:")
    (:pre (:code "nyxt --profile nosave --remote --load foo.lisp --eval '(foo)'"))

    (:h2 "Extensions")
    (:p "To install an extension, copy inside the "
        (:code "*extensions-path*") " (default to "
        (:code "~/.local/share/nyxt/extensions")").")
    (:p "Extensions are regular Common Lisp systems.")
    (:p "A catalogue of extensions is available in the "
        (:code "document/EXTENSIONS.org") " file in the source repository.")

    (:h2 "Troubleshooting")
    (:h3 "Playing videos")
    (:p "Nyxt delegates video support to third-party plugins.")
    (:p "When using the WebKitGTK backends, GStreamer and its plugins are
leveraged.  Depending on the video, you will need to install some of the
following packages:")
    (:ul
     (:li "gst-libav")
     (:li "gst-plugins-bad")
     (:li "gst-plugins-base")
     (:li "gst-plugins-good")
     (:li "gst-plugins-ugly"))
    (:p "On Debian-based systems, you might be looking for (adapt the version numbers):")
    (:ul
     (:li "libgstreamer1.0-0")
     (:li "gir1.2-gst-plugins-base-1.0"))
    (:p "For systems from the Fedora family:")
    (:ul
     (:li "gstreamer1-devel")
     (:li "gstreamer1-plugins-base-devel"))
    (:p "After the desired plugins have been installed, clear the GStreamer cache at "
        (:code "~/.cache/gstreamer-1.0") " and restart Nyxt.")
    (:h3 "Website crashes")
    (:p "If some websites systematically crash, try to install all the required Gstreamer plugins
as mentioned in the 'Playing videos' section.")
    (:h3 "Input method support (CJK, etc.)")
    (:p "Depending on your setup, you might have to set some environment
variables or run some commands before starting Nyxt, for instance")
    (:pre (:code "
GTK_IM_MODULE=xim
XMODIFIERS=@im=ibus
ibus --daemonize --replace --xim"))
    (:p "You can persist this change by saving the commands in
your " (:code ".xprofile") " or similar.")
    (:h3 "Font size on HiDPI displays")
    (:p "On HiDPI displays, the font size used for displaying web and Nyxt's
prompt-buffer content might be too tiny.")
    (:p "To fix this issue when using the WebKitGTK render, export the following
environment variable before starting Nyxt:")
    (:pre (:code "
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5
nyxt
"))
    (:h3 "StumpWM mouse scroll")
    (:p "If the mouse scroll does not work for you, see the "
        (:a
         :href "https://github.com/stumpwm/stumpwm/wiki/FAQ#my-mouse-wheel-doesnt-work-with-gtk3-applications-add-the-following-to"
         "StumpWM FAQ")
        " for a fix.")
    (:h3 "Blank WebKit web-views")
    (:p "If you are experiencing problems with blank web-views on some sites you
    can try to disable compositing. To disable compositing from your
    initialization file, you can do the following: ")
    (:pre (:code "(setf (uiop:getenv \"WEBKIT_DISABLE_COMPOSITING_MODE\") \"1\")"))))
