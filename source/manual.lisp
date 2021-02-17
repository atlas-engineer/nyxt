;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun manual-content ()
  (str:concat
   (markup:markup
    (:h1 "Nyxt manual")
    (:p "This manual first includes the tutorial, then covers the configuration
of Nyxt."))
   (tutorial-content)
   (markup:markup
    (:h2 "Configuration")
    (:p "Nyxt is written in the Common Lisp programming language which offers a
great perk: everything in the browser can be customized by the user, even while
it's running!")
    (:p "To get started with Common Lisp, we recommend checking out
    our web page: "
        (:a :href "https://nyxt.atlas.engineer/learn-lisp" "Learn Lisp")
        ". It contains numerous pointers to other resources, including
        free books both for beginners and seasoned programmers.")
    (:p "Nyxt provides a mechanism for new users unfamiliar with Lisp
to customize Nyxt. Start by invoking the
commands " (:code "describe-class") " or " (:code "describe-slot") ".
You can press the button marked 'Configure' to change the value of a
setting. The settings will be applied immediately and saved for future
sessions.")
    (:p "Settings created by Nyxt are stored in "
        (:code (expand-path *auto-config-file-path*)) ".")
    (:p "Any settings can be overridden manually by "
        (:code (expand-path *init-file-path*)) ".")
    (:p "The following section assumes that you know some basic Common Lisp or a
similar programming language.")
    (:p "Nyxt configuration can be persisted in the user
file " (:code (expand-path *init-file-path*)) " (create the parent folders if
necessary).")
    (:p "Example:")
    (:pre (:code "
\(define-configuration buffer
  ((default-modes (append '(noscript-mode) %slot-default))))"))
    (:p "The above turns on the 'noscript-mode' (disables JavaScript) by default for
every buffer.")
    (:p "The " (:code "define-configuration") " macro can be used to customize
the slots of the classes like the browser, buffers, windows, etc.  Refer to the
class and slot documentation for the individual details.")
    (:p "To find out about all modes known to Nyxt,
run " (:code "describe-command") " and type 'mode' to list them all.")

    (:h3 "Web buffers and internal buffers")
    (:p "`internal-buffer' is used for Nyxt-specific, internal pages such as the
tutorial and the description pages.  `web-buffer' is used for web pages.  Both
the `web-buffer' and the `internal-buffer' classes inherit from the `buffer'
class.")
    (:p "You can configure a `buffer' slot and it will automatically become the
new default for both the `internal-buffer' and `web-buffer' classes unless this
slot is specialized by these child classes.  For example, since the
`default-modes' slot is specialized by the `web-buffer' class, if you want to
include `my-mode' you'll need to configure both the `buffer' and the
`web-buffer' classes.")

    (:h3 "Keybinding configuration")
    (:p "Nyxt supports multiple " (:i "bindings schemes") " such as CUA (the default), Emacs or VI.  Changing scheme is as simple as running the corresponding mode, e.g. "
        (:code "emacs-mode") ".  To make the change persistent across sessions,
add the following to your configuration:")
    (:ul
     (:li "VI bindings:"
      (:pre (:code "
\(define-configuration (buffer web-buffer)
  ((default-modes (append '(vi-normal-mode) %slot-default))))")))
     (:li "Emacs bindings:"
      (:pre (:code "
\(define-configuration (buffer web-buffer)
  ((default-modes (append '(emacs-mode) %slot-default))))"))))
    (:p "You can create new scheme names with " (:code "keymap:make-scheme-name")
        ".  See also the " (:code "scheme-name") " class and the "
        (:code "define-scheme") " macro.")
    (:p "The " (:code "override-map") " is a keymap which has priority over
everything.  By default, it has only very few bindings like the one
for " (:code "execute-command") ".  You can use it to set keys globally:")
    (:pre (:code "
\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"override-map\")))
                   (define-key map
                     \"M-x\" 'execute-command)))))"))
    (:p "A more flexible way is to create your own mode with your custom
keybindings.  When this mode is added first to the buffer mode list, its keys
have priorities over the other modes key bindings.")
    (:pre (:code "
\(defvar *my-keymap* (make-keymap \"my-map\"))
\(define-key *my-keymap*
  \"C-f\" 'nyxt/web-mode:history-forwards
  \"C-b\" 'nyxt/web-mode:history-backwards)

\(define-mode my-mode ()
  \"Dummy mode for the custom key bindings in `*my-keymap*'.\"
  ((keymap-scheme (keymap:make-scheme
                   scheme:cua *my-keymap*
                   scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*))))

\(define-configuration (buffer web-buffer)
  ((default-modes (append '(my-mode) %slot-default))))"))

    (:h3 "Search engines")
    (:p "See the " (:code "search-engines") " buffer slot documentation.
Bookmarks can also be used as search engines, see the corresponding section.")

    (:h3 "URL-dispatchers")
    (:p "You can configure which actions to take depending on the URL to be
loaded.  For instance, you can configure which Torrent program to start to load
magnet links.  See the" (:code "url-dispatching-handler") " function
documentation.")

    (:h3 "Downloads")
    (:p "See the " (:code "list-downloads") " command and the "
        (:code "download-path") " browser slot documentation.")

    (:h3 "Proxy and Tor")
    (:p "See the " (:code "proxy-mode") " documentation.")

    (:h3 "Custom commands")
    (:p "Creating your own invokable commands is similar to creating a Common
Lisp function, except the form is " (:code "define-command") " instead of "
        (:code "defun") ".")
    (:p "Example:")
    (:pre (:code
           "(define-command bookmark-url ()
  \"Allow the user to bookmark a URL via minibuffer input.\"
  (let ((url (prompt-minibuffer
              :input-prompt \"Bookmark URL\")))
    (bookmark-add url)))"))
    (:p "See the " (:code "minibuffer") " class documentation for how to write
write custom minibuffers.")

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
                (log:info \"Switching to old Reddit: ~s\" (object-display url))
                url)
              url)))
  request-data)

\(define-configuration web-buffer
  ((request-resource-hook
    (add-hook %slot-default (make-handler-resource #'old-reddit-handler)))))"))
    (:p "(See " (:code "url-dispatching-handler")
        " for a simpler way to achieve the same result.)")
    (:p "Or, if you want to set multiple handlers at once,")
    (:pre (:code "
\(define-configuration web-buffer
  ((request-resource-hook
    (reduce #'hooks:add-hook
            (mapcar #'make-handler-resource (list #'old-reddit-handler
                                                  #'my-other-handler))
            :initial-value %slot-default))))"))
    (:p "Some hooks like the above example expect a return value, so it's
important to make sure we return " (:code "request-data") " here.  See the
documentation of the respective hooks for more details.")

    (:h3 "Startup behavior")
    (:p "See the " (:code "startup-function") " browser slot.")

    (:h3 "Data paths and data profiles")
    (:p "Nyxt provides a uniform configuration interface for all data files
persisted to disk (bookmarks, cookies, etc.).  To each file corresponds
a " (:code "data-path") " object. A " (:code "data-profile") " is a unique but
customizable object that helps define general rules for data storage.  Both
data-paths and data-profiles compose, so it's possible to define general rules
for all data-paths (even for those not known in advance) while it's also
possible to specialize some data-paths given a data-profile.")
    (:p "The data-profile can be set from command line and from the configuration file.")
    (:p "The data-paths can be passed a hint from the "
        (:code "--with-path") " command line option, but each data-path and
data-profile rules are free to ignore it. The " (:code "expand-default-path") "
helper function uses the --with-path value first, then fallback to a default.
See its documentation for more details.")
    (:p "When the data path ends with the " (:code ".gpg") " extension, your
GnuPG key is used to decrypt and encrypt the file transparently.  Refer to the
GnuPG documentation for how to set it up.")
    (:p "Example to create a development data-profile that stores all data in "
        (:code "/tmp/nyxt") " and stores bookmark in an encrypted file:")
    (:pre (:code "
\(define-class dev-data-profile (data-profile)
   ((name :initform \"dev\"))
   (:export-class-name-p t)
   (:export-accessor-names-p t)
   (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
   (:documentation \"Development profile.\"))

\(defmethod nyxt:expand-data-path ((profile dev-data-profile) (path data-path))
  \"Persist data to /tmp/nyxt/.\"
  (expand-default-path (make-instance (class-name (class-of path))
                                      :basename (basename path)
                                      :dirname \"/tmp/nyxt/\")))

\(defmethod nyxt:expand-data-path ((profile dev-data-profile) (path session-data-path))
  \"Persist session to default location.\"
  (expand-data-path *global-data-profile* path))

;; Make new profile the default:
\(define-configuration buffer
  ((data-profile (make-instance (or (find-data-profile (getf *options* :data-profile))
                                    'dev-data-profile)))
   (bookmarks-path (make-instance 'bookmarks-data-path
                                  :basename \"~/personal/bookmarks/bookmarks.lisp.gpg\"))))"))
    (:p "Then you can start an instance of Nyxt using this profile
with " (:code "nyxt --data-profile dev") ".")

    (:h3 "Password management")
    (:p "Nyxt provides a uniform interface to some password managers including "
        (:a :href "https://keepassxc.org/" "KeepassXC")
        " and " (:a :href "https://www.passwordstore.org/" "Password Store") ". "
        "The installed password manager is automatically
detected. Use " (:code "make-password-interface") " to automatically
return the first password interface with a non-nil executable
path (e.g. the executable was found on your system).")
    (:p "See the " (:code "password:*interfaces*") " for the list of registered
password manager interfaces.")
    (:p "You may use the " (:code "define-configuration") " macro with
any of the password interfaces to configure them. Please make sure to
use the package prefixed class name/slot designators within
the " (:code "define-configuration") " macro.")
    (:ul
     (:li (command-markup 'save-new-password) ": Query for name and new password to persist in the database.")
     (:li (command-markup 'copy-password) ": Copy selected password to the clipboard."))

    (:h3 "Appearance")
    (:p "Much of the visual style can be configured by the user.  Search the
class slots for 'style'.  To customize the status area, see
the " (:code "status-formatter") " window slot.")

    (:h3 "Scripting")
    (:p "You can evaluate code from the command line with "
        (:code "--eval") " and " (:code "--load") ".  From a shell:")
    (:pre (:code "$ nyxt --no-init --eval '+version+' \
  --load my-lib.lisp --eval '(format t \"Hello ~a!~&\" (my-lib:my-world))'"))
    (:p "You can evaluate multiple --eval and --load in a row, they are
executed in the order they appear.")
    (:p "You can also evaluate a Lisp file from the Nyxt interface with
the " (:code "load-file") " command.  For
convenience, " (:code "load-init-file") " (re)loads your initialization file.")
    (:p "You can even make scripts.  Here is an example foo.lisp:")
    (:pre (:code "#!nyxt --script
\(format t \"~a~&\" +version+)"))
    (:p "--eval and --load can be commanded to operate over an
existing instance instead of a separate instance that exits immediately.")
    (:p "The `remote-execution-p' slot of the `browser' class of the remote
instance must be non-nil.")
    (:p "To let know a private instance of Nyxt to load a foo.lisp script and run its
`foo' function:")
    (:pre (:code "nyxt --data-profile nosave --remote --load foo.lisp --eval '(foo)'"))

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
     (:li "gstreamer1-plugins-base"))
    (:p "After the desired plugins have been installed, clear the GStreamer cache at "
        (:code "~/.cache/gstreamer-1.0") " and restart Nyxt.")
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
minibuffer content might be too tiny.")
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
        " for a fix."))))
