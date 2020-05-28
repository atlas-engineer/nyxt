(in-package :next)

(defun tutorial-content ()
  (markup:markup
   (:h1 "Next tutorial")
   (:p "By reading this tutorial you'll learn:")
   (:ul
    (:li "Core concepts")
    (:li "Basic keybindings")
    (:li "How to use the help system to learn more about Next"))

   (:h2 "Core Concepts")
   (:h3 "Keybindings and Commands")
   (:p "Commands in Next are invoked by pressing specific keys or from
the " (:code "execute-command") " menu (" (:code (binding-keys 'execute-command))
") which will prompt you for a list of commands which you can then select
from.")
   (:p "In Next, keybindings are represented as in 'control-space' or
equivalently 'C-space'. In this example, 'C' is a shortcut for the modifier
'control', and 'space' represents the character ' '.  A modifier is a key that
does nothing on its own, its purpose is to alter the meaning of a
non-modifier key when pressed together.  Therefore, to input the 'C-x'
keybinding you would keep 'control' pressed and then hit 'x'.  Multiple
key presses can be chained: in 'C-x C-M-left', you would have to press 'C-x',
let go of all keys, and then press 'control', 'meta' and 'left'.")
   (:p "Modifier keys legend:")
   (:ul
    (:li (:code "control") " (" (:code "C") "): Control key")
    (:li (:code "super") " (" (:code "S") "): Windows key, Command key")
    (:li (:code "meta") " (" (:code "M") "): Alt key, Option key")
    (:li (:code "shift") " (" (:code "s") "): Shift key"))

   (:h3 "Buffers")
   (:p "Next uses the concept of buffers instead of the more limited \"tabs\"
used by many applications. Unlike tabs, the buffer display scales, that is to
say, it's just as convenient to navigate 2 or 100 buffers.  The buffer states
are fully separated, for instance two buffers can have different set of
keybindings.")
   (:h3 "Modes")
   (:p "Each buffer has its own set of modes. A mode is a set of functions,
hooks, keybindings and other facilities that modify the behavior of a buffer.
For example, 'blocker-mode' can be used for domain-based adblocking while
noscript-mode disables JavaScript.")
   (:p "Each mode has an associated " (:i "mode toggler") " which is a command
of the same name the toggles the mode for the current buffer.")
   (:h3 "Minibuffer")
   (:p "The minibuffer is a menu that will appear when a command requests user
input. For example, when invoking the " (:code "set-url") " command, you must
supply the URL you would like to navigate to. The minibuffer can provide
suggestions.  The list of suggestions will automatically narrow down to those
matching your input as you type.")
   (:ul
    (:li (command-markup 'return-input
                         :modes (list (make-instance 'minibuffer-mode)))
         ": Validate the selected suggestion(s) or the current input if there is
no suggestion.")
    (:li (command-markup 'return-immediate
                         :modes (list (make-instance 'minibuffer-mode)))
         ": Validate the current input, ignoring any suggestion."))
   (:p " Some commands support multiple selections, for
instance " (:code "delete-buffer") " can delete all selected buffers at once.
When the input is changed and the candidates are re-filtered, the selection is
not altered even if the marked elements don't show.")
   (:p "When at least one candidate is marked, only the marked candidates are processed
upon return.  The candidate under the cursor is not processed if not marked.")
   (:ul
    (:li (command-markup 'minibuffer-toggle-mark
                         :modes (list (make-instance 'minibuffer-mode)))
         ": Select or deselect the current suggestion.")
    (:li (command-markup 'minibuffer-mark-all
                         :modes (list (make-instance 'minibuffer-mode)))
         ": Select all currently-displayed suggestions.")
    (:li (command-markup 'minibuffer-unmark-all
                         :modes (list (make-instance 'minibuffer-mode)))
         ": Deselect all currently-displayed suggestions."))
   (:h3 "Message Area")
   (:p "The message area represents a space (typically at the bottom of a
window) where Next outputs messages back to you. To view the history of all
messages, invoke the command " (:code "messages") ".")
   (:h3 "Status Area")
   (:p "The status area is where information about the state of that buffer is
printed. By default this includes the active modes, the URL, and the title of
the current buffer.")

   (:h2 "Basic controls")
   (:h3 "Moving within a buffer")
   (:p "To move within a buffer, several commands are provided:")
   (:ul
    (:li (command-markup 'scroll-down) ": Move down.")
    (:li (command-markup 'scroll-up) ": Move up.")
    (:li (command-markup 'scroll-to-bottom) ": Jump to bottom of page.")
    (:li (command-markup 'scroll-to-top) ": Jump to top of page."))
   (:h3 "Setting the URL")
   (:p "When ambiguous URLs are inputted, Next will attempt the best guess it
can. If the you do not supply a protocol in a URL, HTTPS will be assumed. To
visit a site supporting only the less secure HTTP, you must explicitly type the
full URL including the 'http://' prefix.")
   (:ul
    (:li (command-markup 'set-url) ": Set URL of current buffer.")
    (:li (command-markup 'set-url-new-buffer) ": Open a new buffer and set its URL.")
    (:li (command-markup 'make-buffer-focus) ": Make a new empty buffer."))
   (:h3 "Switching buffers")
   (:ul
    (:li (command-markup 'switch-buffer) ": Switch buffer using fuzzy completion
to quickly find whatever buffer you are looking for.")
    (:li (command-markup 'switch-buffer-next) ": Go to next buffer.")
    (:li (command-markup 'switch-buffer-previous) ": Go to previous buffer."))
   (:h3 "Link navigation")
   (:p "Link-hinting allows you to visit URLs on a page without using the mouse.
Invoke one of the commands below: several hints will appear on screen and all
links on the page will be listed in the minibuffer.  You can select the hints
by matching against the hint, the URL or the title.")
   (:ul
    (:li (command-markup 'follow-hint) ": Go to link in current buffer.")
    (:li (command-markup 'follow-hint-new-buffer-focus) ": Create new buffer with link, focus on new buffer.")
    (:li (command-markup 'follow-hint-new-buffer) ": Create new buffer with link, keep focus on current buffer."))
   (:h3 "Using the buffer history")
   (:p "History is represented as a tree that you can traverse: when you go back
in history, then follow a new URL, it effectively creates a new branch without
deleting the old path. The tree makes sure you never lose track of where you've
been.")
   (:ul
    (:li (command-markup 'next/web-mode:history-forwards) ": History forwards.")
    (:li (command-markup 'next/web-mode:history-backwards) ": History backwards.")
    (:li (command-markup 'next/web-mode:history-forwards-query) ": History forwards query to any following location on the branch.")
    (:li (command-markup 'next/web-mode:history-backwards-query) ": History backwards query to any previous location.")
    (:li (command-markup 'next/web-mode:history-forwards-all-query) ": History forwards query to any following location on all branches.")
    (:li (command-markup 'next/web-mode:history-all-query) ": History all query, jump to any history entry."))
   (:p "You can also view a full tree of the history for a given buffer by
invoking the command 'buffer-history-tree'.")
   (:h3 "Searching")
   (:p "Next can search a single buffer or multiple buffers at the same time.")
   (:p "You can view candidates for search results in the minibuffer in one
place rather than having to jump around on a buffer (or multiple buffers).")
   (:ul
    (:li (command-markup 'search-buffer) ": Search buffer.")
    (:li (command-markup 'search-buffers) ": Search multiple buffers."))
   (:h3 "Miscellaneous")
   (:p (command-markup 'quit) ": Close all Next windows and quit.")

   (:h2 "The Next Help System")
   (:p "Next provides introspective and help capabilities.  All commands,
classes, slots, variables, functions and bindings can be inspected for
definition and documentation.")
   (:ul
    (:li (command-markup 'help) ": Open up a small help buffer.")
    (:li (command-markup 'tutorial) ": Open up this tutorial.")
    (:li (command-markup 'describe-key) ": Lets you to input a key binding and
see what command it is bound to.")
    (:li (command-markup 'describe-bindings) ": View all of your currently set
bindings in the current buffer.")
    (:li (command-markup 'describe-command) ": Find out about a particular
command (including showing its source).")
    (:li (command-markup 'describe-function) ": Find out about a particular
function.")
    (:li (command-markup 'describe-variable) ": View the value and documentation
of a variable.")
    (:li (command-markup 'describe-class) ": Lookup a class documentation and all its slots.")
    (:li (command-markup 'describe-slot) ": Lookup a class slot value and documentation."))

   (:h2 "Configuration")
   (:p "Next is written in the Common Lisp programming language which offers a
great perk: everything in the browser can be customized by the user, even while
it's running!")
   (:p "The following section assumes that you know some basic Common Lisp or a
similar programming language.")
   (:p "Next configuration can be persisted in the user
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
   (:p "To find out about all modes known to Next,
run " (:code "describe-command") " and type 'mode' to list them all.")

   (:h3 "Keybinding configuration")
   (:p "Next supports multiple " (:i "bindings schemes") " such as CUA (the default), Emacs or VI.  Changing scheme is as simple as running the corresponding mode, e.g. "
       (:code "emacs-mode") ".  To make the change persistent across sessions,
add the following to you configuration (for VI bindings):")
(:pre (:code "
\(define-configuration buffer
  ((default-modes (append '(vi-normal-mode) %slot-default))))"))
   (:p "The " (:code "override-map") " is a keymap which has priority over
everything.  By default, it has only very few bindings like the one
for " (:code "execute-command") ".  You can use it to set keys globally:")
   (:pre (:code "
\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"override-map\")))
                   (define-key map
                     \"M-x\" 'execute-command)))))"))
   (:p "A more flexibly way is to create your own mode with you custom
keybindings.  When this mode is added first to the buffer mode list, its keys
have priorities over the other modes key bindings.")
   (:pre (:code "
\(defvar *my-keymap* (make-keymap \"my-map\"))
\(define-key *my-keymap*
  \"C-f\" 'history-forwards
  \"C-b\" 'history-backwards)

\(define-mode my-mode ()
  \"Dummy mode for the custom key bindings in `*my-keymap*'.\"
  ((keymap-scheme :initform (keymap:make-scheme
                             scheme:cua *my-keymap*
                             scheme:emacs *my-keymap*
                             scheme:vi-normal *my-keymap*))))

\(define-configuration buffer
  ((default-modes (append '(my-mode) %slot-default))))"))

   (:h3 "Custom commands")
   (:p "Creating your own invokable commands is similar to creating a Common
Lisp function except the form is " (:code "define-command") " instead of "
(:code "defun") ".")
   (:p "Example:")
   (:pre (:code
          "(define-command bookmark-url ()
  \"Allow the user to bookmark a URL via minibuffer input.\"
  (with-result (url (read-from-minibuffer
                     (make-minibuffer
                      :input-prompt \"Bookmark URL\")))
    (bookmark-add url)))"))

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
   (:p "Many hooks are executed at different points in Next, among others:
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
  (let* ((url (url request-data))
         (uri (quri:uri url)))
    (setf (url request-data)
          (if (search \"reddit.com\" (quri:uri-host uri))
              (progn
                (setf (quri:uri-host uri) \"old.reddit.com\")
                (let ((new-url (quri:render-uri uri)))
                  (log:info \"Switching to old Reddit: ~a\" new-url)
                  new-url))
              url)))
  request-data)

\(define-configuration buffer
  ((request-resource-hook
    (add-hook %slot-default (make-handler-resource #'old-reddit-handler)))))"))
   (:p "Or, if you want to set multiple handlers at once,")
   (:pre (:code "
\(define-configuration buffer
  ((request-resource-hook
    (reduce #'hooks:add-hook
            (mapcar #'make-handler-resource (list #'old-reddit-handler
                                                  #'my-other-handler))
            :initial-value %slot-default))))"))
   (:p "Some hooks like the above example expect a return value, so it's
important to make sure we return " (:code "request-data") " here.  See the
documentation of the respective hooks for more details.")

   (:h3 "Data paths and data profiles")
   (:p "Next provides a uniform configuration interface for all data files
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
   (:p "Example to create a development data-profile that stores all data in "
       (:code "/tmp/next") ":")
   (:pre (:code "
\(defvar +dev-data-profile+ (make-instance 'data-profile :name \"dev\")
  \"Development profile.\")

\(defmethod next:expand-data-path ((profile (eql +dev-data-profile+)) (path data-path))
  \"Persist data to /tmp/next/.\"
  (expand-default-path (make-instance (class-name (class-of path))
                                      :basename (basename path)
                                      :dirname \"/tmp/next/\")))

\(defmethod next:expand-data-path ((profile (eql +dev-data-profile+)) (path session-data-path))
  \"Persist session to default location.\"
  (expand-data-path +default-data-profile+ path))

;; Make new profile the default:
\(define-configuration browser
  ((data-profile (or (find-data-profile (getf *options* :data-profile))
                     +dev-data-profile+))))"))

   (:h2 "Troubleshooting")
   (:h3 "Playing videos")
   (:p "Next delegates video support to third party plugins.")
   (:p "When using the WebKitGTK backends, GStreamer and its plugins are
leveraged.  Depending on the video you will need to install some of the
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
       (:code "~/.cache/gstreamer-1.0") " and restart Next.")))
