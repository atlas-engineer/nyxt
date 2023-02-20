;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'manual-content)
(defun manual-content ()
  (str:concat
   (spinneret:with-html-string
     (:h1 "Nyxt manual")
     (:p "This manual first includes the tutorial, then covers the configuration
of Nyxt."))
   (tutorial-content)
   (manual-sections)))

(defun manual-sections ()
  (spinneret:with-html-string
    (let ((auto-config-file (namestring (files:expand *auto-config-file*)))
          (config-file (namestring (files:expand *config-file*)))
          (rules-file (namestring (files:expand (make-instance 'auto-rules-file)))))
      (:nsection :title "Configuration"
        (:p "Nyxt is written in the Common Lisp programming language which offers a
great perk: everything in the browser can be customized by the user, even while
it's running!")
        (:p "To get started with Common Lisp, we recommend checking out
    our web page: "
            (:a :href "https://nyxt.atlas.engineer/learn-lisp" "Learn Lisp")
            ". It contains numerous pointers to other resources, including
        free books both for beginners and seasoned programmers.")
        (:p "Nyxt provides a mechanism for new users unfamiliar with Lisp to
customize Nyxt. Start by invoking the commands " (:nxref :command 'describe-class) "
or " (:nxref :command 'describe-slot) ".  You can press the button marked 'Configure' to
change the value of a setting. The settings will be applied immediately and
saved for future sessions. Please note that these settings will not alter
existing object instances.")
        (unless (str:empty? auto-config-file)
          (:p "Settings created by Nyxt are stored in " (:code auto-config-file) "."))
        (unless (str:empty? config-file)
          (:p "Any settings can be overridden manually by " (:code config-file) "."))
        (:p "The following section assumes knowledge of basic Common Lisp or a
similar programming language.")
        (:p "The user needs to manually create the Nyxt configuration file, and the parent folders if necessary."
            (when (and (current-buffer) ; In case manual is dumped.
                       (not (files:nil-pathname-p config-file))
                       (not (uiop:file-exists-p config-file)))
              (:p "You can also press the button below to create it."
                  (:p (:a :class "button"
                          :onclick (ps:ps
                                     (nyxt/ps:lisp-eval
                                      (:title "create-config-file")
                                      (ensure-directories-exist config-file)
                                      (ensure-file-exists config-file)
                                      (echo "Configuration file created at ~s." config-file)))
                          "Create configuration file")))))
        (:p "Example:")
        (:ncode
          (define-configuration web-buffer
            ((default-modes (pushnew 'nyxt/no-script-mode:no-script-mode %slot-value%))))))
      (:p "The above turns on the 'no-script-mode' (disables JavaScript) by default for
every buffer.")
      (:p "The " (:nxref :macro 'define-configuration) " macro can be used to customize
the slots of classes like the browser, buffers, windows, etc.  Refer to the
class and slot documentation for the individual details.")
      (:p "To find out about all modes known to Nyxt,
run " (:nxref :command 'describe-command) " and type 'mode'.")

      (:nsection :title "Slot configuration"
        (:p "Slots store values that can be either accessed (get) or changed
(set). Setting new values for slots allows many possibilities of customization.
For instance, keyboard layouts vary across the world. The slot "
            (:nxref :slot 'nyxt/hint-mode:hints-alphabet :class-name 'nyxt/hint-mode:hint-mode)
            " has the default value of "
            (:code "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            ". If the user has an American keyboard, they can do:")
        (:ol
         (:li "Execute command " (:nxref :command 'describe-slot) ";")
         (:li "Type " (:code "hints-alphabet")";")
         (:li "Select " (:code "hints-alphabet") " (" (:code "hint-mode") " class option);")
         (:li "Press the button " (:code "Configure") ", and;")
         (:li "Insert the string \"asfdghjkl\"") ".")
        (:p "This will make link-hinting more comfortable for this user. In
addition, other similar approaches of customization can be applied to slots
such as " (:nxref :slot 'nyxt/spell-check-mode:spell-check-language :class-name 'nyxt/spell-check-mode:spell-check-mode)
", which can be expanded to do the spelling-check of other languages besides English."))

      (:nsection :title "Different types of buffers"
        (:p "There are multiple buffer classes, such as "
            (:nxref :class-name 'document-buffer) " (for structured documents) and "
            (:nxref :class-name 'input-buffer) " (for buffers that can receive user input).  A "
            (:nxref :class-name 'web-buffer) " class is used for web pages, " (:nxref :class-name 'prompt-buffer)
            " for, well, the prompt buffer.  Some buffer classes may inherit from multiple other classes.
For instance " (:nxref :class-name 'web-buffer) " and " (:nxref :class-name 'prompt-buffer)
            " both inherit from" (:nxref :class-name 'input-buffer) ".")
        (:p "You can configure one of the parent " (:nxref :class-name 'buffer) " classes slots and the new
values will automatically cascade down as a new default for all child classes-
unless this slot is specialized by these child classes.
For instance if you configure the " (:nxref :slot 'override-map :class-name 'input-buffer)
" slot in " (:nxref :class-name 'input-buffer) ", both " (:nxref :class-name 'panel-buffer) " and "
(:nxref :class-name 'web-buffer) " classes will inherit from the new value."))

      (:nsection :title "Keybinding configuration"
        (:p "Nyxt supports multiple " (:i "bindings schemes") " such as CUA (the
    default), Emacs or vi.  Changing scheme is as simple as setting the
    corresponding mode as default, e.g. "
            (:nxref :class-name 'nyxt/emacs-mode:emacs-mode) ".  To make the change persistent across sessions,
add the following to your configuration:")
        (:ul
         (:li "vi bindings:"
              (:ncode
                (define-configuration buffer
                  ((default-modes (pushnew 'nyxt/vi-mode:vi-normal-mode %slot-value%))))))
         (:li "Emacs bindings:"
              (:ncode
                (define-configuration buffer
                  ((default-modes (pushnew 'nyxt/emacs-mode:emacs-mode %slot-value%)))))))
        (:p "You can create new scheme names with " (:nxref :function 'nkeymaps:make-keyscheme)
            ".  Also see the "
            (:nxref :function 'keymaps:define-keyscheme-map "define-keyscheme-map macro") ".")
        (:p "To extend the bindings of a specific mode, you can configure the mode with "
            (:nxref :macro 'define-configuration) " and extend its "
            (:nxref :slot 'keyscheme-map :class-name 'mode) " with "
            (:nxref :function 'keymaps:define-keyscheme-map) ". For example:")
        (:ncode
          (define-configuration base-mode
            "Note the :import part of the define-keyscheme-map.
It re-uses the other keymap (in this case, the one that was slot value before
the configuration) and merely adds/modifies it."
            ((keyscheme-map
              (define-keyscheme-map
                  "my-base" (list :import %slot-value%)
                keyscheme:vi-normal
                (list "g b" (lambda-command switch-buffer* ()
                              (switch-buffer :current-is-last-p t))))))))
        (:p "The " (:nxref :slot 'override-map :class-name 'input-buffer) " is a keymap that has priority over
all other keymaps.  By default, it has few bindings like the one
for " (:nxref :command 'execute-command) ".  You can use it to set keys globally:")
        (:ncode
          (define-configuration input-buffer
            ((override-map (let ((map (make-keymap "override-map")))
                             (define-key map
                               "M-x" 'execute-command
                               "C-space" 'nothing))))))
        (:p "The " (:nxref  :command 'nothing) " command is useful to override bindings to do
nothing. Note that it's possible to bind any command, including those of
disabled modes that are not listed in " (:nxref :command 'execute-command)
". Binding to " (:nxref :command 'nothing)
" and binding to NIL means different things (see the documentation of "
(:nxref :function 'keymaps:define-key) " for details):")
        (:dl
         (:dt (:nxref  :command 'nothing))
         (:dd "Binds the key to a command that does nothing. Still discovers the key and
recognizes it as pressed.")
         (:dt "NIL")
         (:dd "Un-binds the key, removing all the bindings that it had in a given
mode/keyscheme-map. If you press the un-bound key, the bindings that used to be
there will not be found anymore, and the key will be forwarded to the renderer.")
         (:dt "Any other symbol/command")
         (:dd "Replaces the command that was there before, with the new one. When the key is
pressed, the new command will fire instead of the old one."))
        (:p "In addition, a more flexible approach is to create your own mode with
your custom keybindings.  When this mode is added first to the buffer mode list,
its keybindings have priorities over the other modes.
Note that this kind of global keymaps also have priority over regular character
insertion, so you should probably not bind anything without modifiers in such a
keymap.")
        (:ncode
          (defvar *my-keymap* (make-keymap "my-map"))
          (define-key *my-keymap*
            "C-f" 'nyxt/history-mode:history-forwards
            "C-b" 'nyxt/history-mode:history-backwards)

          (define-mode my-mode ()
            "Dummy mode for the custom key bindings in `*my-keymap*'."
            ((keyscheme-map (keymaps:make-keyscheme-map
                             nyxt/keyscheme:cua *my-keymap*
                             nyxt/keyscheme:emacs *my-keymap*
                             nyxt/keyscheme:vi-normal *my-keymap*))))

          (define-configuration web-buffer
            "Enable this mode by default."
            ((default-modes (pushnew 'my-mode %slot-value%)))))
        (:p "Bindings are subject to various translations as per "
            (:nxref :variable 'nkeymaps:*translator*) ". "
            "By default if it fails to find a binding it tries again with inverted
shifts.  For instance if " (:code "C-x C-F") " fails to match anything " (:code "C-x C-f")
            " is tried."
            "See the default value of " (:nxref :variable 'nkeymaps:*translator*) " to learn how to
         customize it or set it to " (:code "nil") " to disable all forms of
         translation."))

      (:nsection :title "Search engines"
        (:p "See the " (:nxref :slot 'search-engines :class-name 'context-buffer) " buffer slot
documentation.  Bookmarks can also be used as search engines, see the
corresponding section.")
        (:p "Nyxt comes with default search engines for "
            (:code (format nil "~{~a~^, ~}"
                           (mapcar (lambda (engine)
                                     (quri:uri-host (quri:uri (getf engine :search-url))))
                                   (rest (getf (mopu:slot-properties 'context-buffer 'search-engines)
                                               :initform)))))
            ". "
            "The following example shows one way to add new search engines.")
        (:ncode
          '(defvar *my-search-engines*
            (list
             '("google" "https://google.com/search?q=~a" "https://google.com")
             '("python3" "https://docs.python.org/3/search.html?q=~a" "https://docs.python.org/3")
             '("doi" "https://dx.doi.org/~a" "https://dx.doi.org/"))
            "List of search engines.")

          '(define-configuration context-buffer
            "Go through the search engines above and `make-search-engine' out of them."
            ((search-engines
              (append (mapcar (lambda (engine) (apply 'make-search-engine engine))
                       *my-search-engines*)
               %slot-default%)))))
        (:p "Note that the last search engine is the default one. For example, in
order to make python3 the default, the above code can be slightly modified as
follows.")
        (:ncode
          (defvar *my-search-engines*
            (list
             '("google" "https://google.com/search?q=~a" "https://google.com")
             '("doi" "https://dx.doi.org/~a" "https://dx.doi.org/")
             '("python3" "https://docs.python.org/3/search.html?q=~a" "https://docs.python.org/3"))
            "List of search engines.")

          (define-configuration context-buffer
            "Go through the search engines above and `make-search-engine' out of them."
            ((search-engines (append %slot-default%
                                     (mapcar (lambda (engine) (apply 'make-search-engine engine))
                                             *my-search-engines*))))))
        (:p "If you don't want to use " (:nxref :function 'make-search-engine)
            " and want to try building the engines yourself, you can always make new "
            (:nxref :class-name 'search-engine) " and add it to "
            (:nxref :class-name 'context-buffer :slot 'search-engines) " list:")
        (:ncode
          (define-configuration context-buffer
            "Add a single search engine manually."
            ((search-engines (pushnew (make-instance 'search-engine
                                                     :name "Reddit"
                                                     :shortcut "r"
                                                     :search-url "https://reddit.com/search/?q=~a"
                                                     :fallback-url "https://reddit.com")
                                      %slot-value%))))))

      (:nsection :title "History"
        (:p "Nyxt history model is a tree whose nodes are URLs. It branches out through all
the buffers. If you create a new buffer (via " (:nxref :command 'nyxt/hint-mode:follow-hint-new-buffer)
" or " (:nxref :command 'make-buffer) "), it becomes a new history branch originating
from the branch of the previous buffer.")
        (:p "History can be navigated with the arrow keys in the status buffer, or with
commands like " (:nxref :command 'nyxt/history-mode:history-backwards) " and "
            (:nxref :command 'nyxt/history-mode:history-forwards)
            " (which the arrows are bound to).")
        (:p "If the beyond-buffer-boundaries behavior sounds like too much to you, or you
prefer the behavior of Nyxt 2, where the history was still a tree, but was not
spilling across the buffers, then configure "
            (:nxref :slot 'nyxt/history-mode:conservative-history-movement-p
              :class-name 'nyxt/history-mode:history-mode)
            " to be T. This would make all buffers to have their own history, not connected
to the other buffers at all. All the history commands (like "
            (:nxref :command 'nyxt/history-mode:history-backwards) " and "
            (:nxref :command 'nyxt/history-mode:history-forwards)
            ") will only work inside the buffer history then.")
        (:p "Nyxt supra-buffer history has benefits, though: it optimizes browsing patterns
into more intuitive and productive structures. One particular pattern Nyxt
history optimizes is hub-and-spoke search, where you keep returning to a certain
hub to start your search/navigation from a familiar point. You can enable the
optimization (merely going back in history to the hub page, instead of creating
a new history node) for this strategy by configuring "
            (:nxref :slot 'nyxt/history-mode:backtrack-to-hubs-p
              :class-name 'nyxt/history-mode:history-mode)
            " to T.")
        (:p "Another useful side to Nyxt tree-like history are braching-aware history
commands, like "
            (:nxref :command 'nyxt/history-mode:history-forwards-query)
            ", allowing one to choose which branch of history they are going to visit, if
there are several. If there's only one branch, then this command behaves much
like regular " (:nxref :command 'nyxt/history-mode:history-forwards) ".")
        (:p "There are commands that allow to move across all the history before or after
the current node:")
        (:ul (list-command-information '(nyxt/history-mode:history-backwards-query
                                         nyxt/history-mode:history-forwards-all-query
                                         nyxt/history-mode:history-all-query)))
        (:p "If you need to know more: most of the optimizations and data structures are
in " (:nxref :package :history-tree) " library, while most of the Nyxt-specific interface is in "
(:nxref :package :nyxt/history-tree-mode) "."))

      (:nsection :title "Downloads"
        (:p "See the " (:nxref :command 'nyxt/download-mode:list-downloads) " command and the "
            (:nxref :slot 'download-path :class-name 'buffer) " buffer slot documentation."))

      (:nsection :title "Proxy and Tor"
        (:p "See the " (:nxref :class-name 'nyxt/proxy-mode:proxy-mode) " documentation."))

      (:nsection :title "Blocker mode"
        (:p "This mode blocks access to websites related to specific hosts. To see
all hosts being blocked, execute command " (:code "describe-variable") ", choose variable "
(:code "NYXT/BLOCKER-MODE:*DEFAULT-HOSTLIST*") ", and read data on "
(:code "nyxt/blocker-mode:url-body") " slot." " To customize host blocking, read the "
(:nxref :class-name 'nyxt/blocker-mode:blocker-mode) " documentation."))

      (:nsection :title "URL-dispatchers"
        (:p "You can configure which actions to take depending on the URL to be
loaded.  For instance, you can configure which Torrent program to start to load
magnet links.  See the " (:nxref :function 'url-dispatching-handler) " function
documentation."))

      (:nsection :title "Auto rules"
        (:p "Auto-rules toggle modes when the URL satisfies the given
conditions. URL-dispatchers can also be used for this, but it is
simpler to use an auto-rule. Given that Nyxt's functionality is
mode-based, the consequences are far reaching.")
        (:p "These can be used in the following ways:")
        (:ul
         (:li "Manually, by calling:")
         (:dl
          (:dt (:nxref :command 'save-non-default-modes-for-future-visits))
          (:dd "which saves \"unusual\" modes - non-default modes that were
toggled exclusively for a given URL.")
          (:dt (:nxref :command 'save-exact-modes-for-future-visits))
          (:dd "which saves the exact list of enabled modes for a given
          URL."))
         (:li "Automatically, by setting "
              (:nxref :slot 'prompt-on-mode-toggle-p :class-name 'modable-buffer)
              " to non-nil (refer to the "
              (:a :href "#configuration" "configuration section") " for help)."))
        (:p "All rules are stored at " (:code rules-file) ", which " (:u "is
meant to be human-readable and human-writable")". You can find instructions at the top of it.
The gist is that rules are mere Lisp lists which start with a condition that checks the
 URL. When conditions are met, modes are toggled. Besides user-defined conditions, the
following are often useful: "
(:ul
 (:li (:nxref :function 'match-domain))
 (:li (:nxref :function 'match-host))
 (:li (:nxref :function 'match-url))
 (:li (:nxref :function 'match-regex))
 (:li (:nxref :function 'match-scheme))))
        (:p "By default, "
            (:nxref :slot 'apply-all-matching-auto-rules-p :class-name 'modable-buffer)
            " is nil meaning that only the most specific rules are honored.")
        (:p "Auto-rules can also be defined for custom use-cases via "
            (:nxref :function 'define-auto-rule) " and un-defined with "
            (:nxref :function 'undefine-auto-rule) "."))

      (:nsection
        :title "Custom commands"
        :open-p nil
        (:p "Creating your own invocable commands is similar to creating a Common
Lisp function, except the form is " (:code "define-command") " instead of "
(:code "defun") ". If you want this command to be invocable outside of
        the context of a mode, use " (:code "define-command-global") ".")
        (:p "Example:")
        (:ncode
          (define-command-global my-bookmark-url ()
            "Query which URL to bookmark."
            (let ((url (prompt
                        :prompt "Bookmark URL"
                        :sources 'prompter:raw-source)))
              (nyxt/bookmark-mode:bookmark-add url))))
        (:p "See the " (:nxref :class-name 'prompt-buffer) " class documentation for how
to write custom prompt buffers.")
        (:p "You can also create your own context menu entries binding those to Lisp commands, using "
            (:nxref :function 'ffi-add-context-menu-command) " function. You can bind the "
            (:code "bookmark-url") " like this:")
        (:ncode
          (ffi-add-context-menu-command 'my-bookmark-url "Bookmark URL"))
        (:p "Currently, context menu commands don't have access to the renderer objects (and
shouldn't hope to). Commands you bind to context menu actions should deduce most
of the information from their surroundings, using JavaScript and Lisp functions
Nyxt provides. For example, one can use the "
            (:nxref :slot 'url-at-point :class-name 'buffer)
            " to get thep URL currently under pointer.")
        (:p "With this, one can improve the bookmarking using "
            (:nxref :slot 'url-at-point :class-name 'buffer) ":")
        (:ncode
          (ffi-add-context-menu-command
           (lambda ()
             (nyxt/bookmark-mode:bookmark-add (url-at-point (current-buffer))))
           "Bookmark Link")))

      (:nsection :title "Custom URL schemes"
        (:p "If there's a scheme that Nyxt doesn't support, but you want it to, you can
always define the handler for this scheme so that it's Nyxt-openable.")
        (:p "As a totally hypothetical example, you can define a nonsense scheme "
            (:code "bleep") " to generate a page with random text:")
        (:ncode
          '(define-internal-scheme "bleep"
            (lambda (url buffer)
              (values
               (spinneret:with-html-string
                 (:h1 "Bleep bloop?")
                 (:p (loop repeat (parse-integer (quri:uri-path (url url)) :junk-allowed t)
                           collect (:li (elt '("bleep" "bloop") (random 2))))))
               "text/html;charset=utf8"))
            :local-p t))
        (:p "What this piece of code does is")
        (:ul
         (:li "Define a new scheme.")
         (:li "Make a handler for it that takes the URL (as a string) and a buffer it's being
opened in.")
         (:li "Read the path (the part after the bleep:) of the URL and interpret it as a number.")
         (:ul
          (:li "(Note that you need to wrap the URL into a " (:nxref :function 'url)
               " call so that it turns into a " (:nxref :class-name 'quri:uri)
               " for the convenience of path (and other elements) fetching.)"))
         (:li "Generate a random list of \"bleep\" and \"bloop\".")
         (:li "Return it as a " (:code "text/html") " content."))
        (:p "The next time you run Nyxt and open " (:code "bleep:20")
            ", you'll see a list of twenty bleeps and bloops.")
        (:p "Internal schemes can return any type of content (both strings and arrays of
bytes are recognized), and they are capable of being "
            (:nxref :class-name 'scheme :slot 'cors-enabled-p "CORS-enabled")
            ", " (:nxref :class-name 'scheme :slot 'local-p "protected")
            ", and are in general capable of whatever the renderer-provided schemes do.")

        (:nsection :title "nyxt: URLs and internal pages"
          (:p "You can create pages out of Lisp commands, and make arbitrary computations for
the content of those. More so: these pages can invoke Lisp commands on demand,
be it on button click or on some page event. The macros and functions to look at are:")
          (:ul
           (:li (:nxref :macro 'define-internal-page) " to create new pages.")
           (:li (:nxref :function 'buffer-load-internal-page-focus)
                " to either get or create the buffer for the page.")
           (:li (:nxref :function 'nyxt-url) " to reference the internal pages by their name.")
           (:li (:nxref :macro 'define-internal-page-command)
                " to generate a mode-specific command loading the internal page.")
           (:li (:nxref :macro 'define-internal-page-command-global)
                " to generate a global command loading the internal page."))
          (:p "Using the facilities Nyxt provides, you can make a random number generator
page:")
          (:ncode
            '(define-internal-page-command-global random-number (&key (max 1000000))
              (buffer "*Random*")
              "Generates a random number on every reload."
              (spinneret:with-html-string
                (:h1 (princ-to-string (random max)))
                (:button.button
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  (:title "re-load/re-generate the random number")
                                  (reload-buffer buffer)))
                 :title "Re-generate the random number again"
                 "New number"))))
          (:p "Several things to notice here:")
          (:ul
           (:li "Internal page command is much like a regular command in being a Lisp function
that you can call either from the REPL or from the " (:nxref :command 'execute-command) " menu.")
           (:ul
            (:li "With one important restriction: internal page commands should only have keyword
arguments. Other argument types are not supported. This is to make them
invocable through the URL they are assigned. For example, when you invoke the "
                 (:code "random-number") " command you've written, you'll see the "
                 (:code "nyxt:nyxt-user:random-number?max=%1B1000000")
                 " URL in the status buffer. The keyword argument is being seamlessly translated
into a URL query parameter.")
            (:li "There's yet another important restriction: the values you provide to the
internal page command should be serializable to URLs. Which restricts the
arguments to numbers, symbols, and strings, for instance."))
           (:li "Those commands should return the content of the page in their body, like
internal schemes do.")
           (:li "If you want to return HTML, then " (:nxref :macro 'spinneret:with-html-string)
                " is your best friend, but no one restricts you from producing HTML in any other
way, including simply writing it by hand ;)")
           (:li (:code "nyxt/ps:lisp-eval")
                " is a Parenscript macro to request Nyxt to run arbitrary code. The signature is: "
                (:code "((&key (buffer '(nyxt:current-buffer)) title callback) &body form)")
                ". You can bind it to a " (:code "<button>") "'s " (:code "onClick")
                " event, for example."))
          (:p "If you're making an extension, you might find other macros more useful. "
              (:nxref :macro 'define-internal-page-command)
              ", for example, defines a command to only be visible when in the corresponding mode
is enabled. Useful to separate the context-specific commands from the
universally useful (" (:code "-global")
              ") ones. If there's a page that you'd rather not have a command for, you can
still define it as:")
          (:ncode
            '(define-internal-page not-a-command ()
              (:title "*Hello*" :page-mode 'base-mode)
              "Hello there!"))
          (:p " and use as:")
          (:ncode
            (buffer-load-internal-page-focus 'not-a-command))
          (:p "See the slots and documentation of " (:nxref :class-name 'internal-page)
              " to understand what you can pass to "
              (:nxref :macro 'define-internal-page) ".")))

      (:nsection :title "Hooks"
        (:p "Hooks provide a powerful mechanism to tweak the behavior of various
events that occur in the context of windows, buffers, modes, etc.")
        (:p "A hook holds a list of " (:i "handlers") ".  Handlers are named and
typed functions.  Each hook has a dedicated handler constructor.")
        (:p
         "Hooks can be 'run', that is, their handlers are run according to
the " (:nxref :slot 'nhooks:combination :class-name 'nhooks:hook) " slot of the hook.  This combination is a function
of the handlers.  Depending on the combination, a hook can run the handlers
either in parallel, or in order until one fails, or even " (:i "compose")
         " them (pass the result of one as the input of the next).  The handler types
specify which input and output values are expected.")
        (:p "To add or delete a hook, you only need to know a couple of functions:"
            (:ul
             (:li (:nxref :class-name 'nhooks:handler) " a class to wrap hook handlers in.")
             (:li (:nxref :function 'nhooks:add-hook) " (also known as "
                  (:code "hooks:add-hook")
                  ") allows you to add a handler to a hook,for it to be invoked when the hook fires.")
             (:li (:code "nhooks:on") " (also available as " (:code "hooks:on")
                  ") as a shorthand for the " (:code "nhooks:add-hook") ".")
             (:li (:nxref :function 'nhooks:remove-hook) " (also available as "
                  (:code "hooks:remove-hook") ") that removes the handler from a certain hook.")
             (:li (:code "nhooks:once-on") " (also available as " (:code "hooks:once-on")
                  ") as a one-shot version of " (:code "nhooks:on")
                  " that removes the handler right after it's completed.")))
        (:p "Many hooks are executed at different points in Nyxt, among others:")
        (:ul
         (:li "Global hooks, such as " (:nxref :slot 'after-init-hook :class-name 'browser)
              " or " (:nxref :slot 'after-startup-hook :class-name 'browser) ".")
         (:li "Window- or buffer-related hooks.")
         (:ul
          (:li (:nxref :slot 'window-make-hook :class-name 'window) " for when a new window is created.")
          (:li (:nxref :slot 'window-delete-hook :class-name 'window) " for when a window is deleted.")
          (:li (:nxref :slot 'window-set-buffer-hook :class-name 'window)
               " for when the " (:nxref :function 'current-buffer) " changes in the window.")
          (:li (:nxref :slot 'buffer-load-hook :class-name 'network-buffer)
               " for when there's a new page loading in the buffer.")
          (:li (:nxref :slot 'buffer-loaded-hook :class-name 'network-buffer)
               " for when this page is mostly done loading (some scripts/image/styles may not
be fully loaded yet, so you may need to wait a bit after it fires.)")
          (:li (:nxref :slot 'request-resource-hook :class-name 'network-buffer)
               " for when a new request happens. Allows redirecting and blocking requests, and
is a good place to do something conditioned on the links being loaded.")
          (:li (:nxref :slot 'prompt-buffer-ready-hook :class-name 'prompt-buffer)
               " fires when the prompt buffer is ready for user input. You may need to call "
               (:nxref :function 'prompter:all-ready-p)
               " on the prompt to ensure all the sources it contains are ready too, and then
you can safely set new inputs and select the necessary suggestions."))
         (:li "Commands :before and :after methods.")
         (:ul
          (:li "Try, for example, "
               (:code "(defmethod set-url :after (&key (prefill-current-url-p t)) ...)")
               " to do something after the set-url finishes executing."))
         (:li "Modes 'enable' and 'disable' methods and their :before, :after, and :around methods.")
         (:li "Mode-specific hooks, like " (:nxref :slot 'nyxt/download-mode:before-download-hook
                                             :class-name 'nyxt/download-mode:download-mode)
              " and " (:nxref :slot 'nyxt/download-mode:after-download-hook
                        :class-name 'nyxt/download-mode:download-mode)
              " for " (:nxref :class-name 'nyxt/download-mode:download) "."))
        (:p "For instance, if you want to force 'old.reddit.com' over 'www.reddit.com', you
can set a hook like the following in your configuration file:")
        (:ncode
          (defun old-reddit-handler (request-data)
            (let ((url (url request-data)))
              (setf (url request-data)
                    (if (search "reddit.com" (quri:uri-host url))
                        (progn
                          (setf (quri:uri-host url) "old.reddit.com")
                          (log:info "Switching to old Reddit: ~s" (render-url url))
                          url)
                        url)))
            request-data)
          (define-configuration web-buffer
            ((request-resource-hook
              (hooks:add-hook %slot-default% 'old-reddit-handler)))))
        (:p "(See " (:nxref :function 'url-dispatching-handler)
            " for a simpler way to achieve the same result.)")
        (:p "Or, if you want to set multiple handlers at once,")
        (:ncode
          (define-configuration web-buffer
            ((request-resource-hook
              (reduce #'hooks:add-hook
                      '(old-reddit-handler auto-proxy-handler)
                      :initial-value %slot-default%)))))
        (:p "Some hooks like the above example expect a return value, so it's
important to make sure we return " (:nxref :class-name 'request-data) " here.  See the
documentation of the respective hooks for more details."))

      (:nsection :title "Data paths and data profiles"
        (:p "Nyxt provides a uniform configuration interface for all data files
persisted to disk (bookmarks, cookies, etc.).  To each file corresponds
a " (:nxref :class-name 'nyxt-file) " object. An " (:nxref :class-name 'nyxt-profile) " is a
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
        (:ncode
          (define-class dev-profile (nyxt-profile)
            ((files:name :initform "nyxt-dev"))
            (:documentation "Development profile."))
          (defmethod files:resolve ((profile dev-profile) (path nyxt-file))
            "Expand all data paths inside a temporary directory."
            (serapeum:path-join (files:expand (make-instance 'nyxt-temporary-directory))
                                (uiop:relativize-pathname-directory (call-next-method))))
          (defmethod files:resolve ((profile dev-profile) (file history-file))
            "Persist history to default location."
            (files:resolve (global-profile) file))
          (define-configuration web-buffer
            "Make new profile the default."
            ((profile (make-instance (or (find-profile-class (getf *options* :profile)) 'dev-profile))))))
        (:p "Then you can start a separate instance of Nyxt using this profile
with " (:code "nyxt --profile dev --socket /tmp/nyxt.socket") "."))

      (:nsection :title "Password management"
        (:p "Nyxt provides a uniform interface to some password managers including "
            (:a :href "https://keepassxc.org/" "KeepassXC")
            " and " (:a :href "https://www.passwordstore.org/" "Password Store") ". "
            "The supported installed password manager is automatically detected."
            "See the " (:code "password-interface") " buffer slot for customization.")
        (:p "You may use the " (:nxref :macro 'define-configuration) " macro with
any of the password interfaces to configure them. Please make sure to
use the package prefixed class name/slot designators within
the " (:nxref :macro 'define-configuration) ".")
        (:ul
         (:li (:nxref :command 'nyxt/password-mode:save-new-password) ": Query for name and new password to persist in the database.")
         (:li (:nxref :command 'nyxt/password-mode:copy-password) ": " (command-docstring-first-sentence 'nyxt/password-mode:copy-password)))

        (:nsection :title "KeePassXC support"
          (:p "The interface for KeePassXC should cover most use-cases for KeePassXC, as it
supports password database locking with")
          (:ul
           (:li (:nxref :slot 'password:master-password :class-name 'password:keepassxc-interface) ",")
           (:li (:nxref :slot 'password:key-file :class-name 'password:keepassxc-interface) ",")
           (:li "and " (:nxref :slot 'password:yubikey-slot :class-name 'password:keepassxc-interface)))
          (:p "To configure KeePassXC interface, you might need to add something like this
snippet to your config:")
          (:ncode
            ;; FIXME: Why does `define-configuration' not work for password
            ;; interfaces? Something's fishy with user classes...
            (defmethod initialize-instance :after ((interface password:keepassxc-interface) &key &allow-other-keys)
              "It's obviously not recommended to set master password here,
as your config is likely unencrypted and can reveal your password to someone
peeking at the screen."
              (setf (password:password-file interface) "/path/to/your/passwords.kdbx"
                    (password:key-file interface) "/path/to/your/keyfile"
                    (password:yubikey-slot interface) "1:1111"))
            (define-configuration nyxt/password-mode:password-mode
              ((nyxt/password-mode:password-interface (make-instance 'password:keepassxc-interface))))
            (define-configuration buffer
              ((default-modes (append (list 'nyxt/password-mode:password-mode) %slot-value%)))))))

      (:nsection :title "Appearance"
        (:p "Much of the visual style can be configured by the user. You can use the
facilities provided by " (:nxref :package :theme) " and "
(:nxref :slot 'nyxt:theme :class-name 'nyxt:browser "browser theme slot")
". For example, to set a theme to a midnight-like one, you can add this snippet
to your configuration file:")
        (:ncode
          (define-configuration browser
            ((theme (make-instance 'theme:theme
                                   :dark-p t
                                   :background-color "black"
                                   :on-background-color "#808080"
                                   :accent-color "#37a8e4"
                                   :on-accent-color "black"
                                   :primary-color "gray"
                                   :on-primary-color "white"
                                   :secondary-color "darkgray"
                                   :on-secondary-color "black")
                    :doc "You can omit the colors you like in default theme, and they will stay as they were."))))
        (:p "This, on the next restart of Nyxt, will repaint all the interface elements into
a dark-ish theme.")
        (:p "As an alternative to the all-encompassing themes, you can alter the style of
every individual class controlling Nyxt interface elements. All such classes have a "
            (:nxref :function 'nyxt:style)
            " slot that you can configure with your own CSS like this:")
        (:ncode
          (define-configuration nyxt/style-mode:dark-mode
            ((style
              (theme:themed-css (theme *browser*)
                `(*
                  :background-color ,theme:background "!important"
                  :background-image none "!important"
                  :color "red" "!important")
                `(a
                  :background-color ,theme:background "!important"
                  :background-image none "!important"
                  :color "#AAAAAA" "!important"))))))
        (:p "This snippet alters the " (:nxref :slot 'style :class-name 'nyxt/style-mode:dark-mode)
            " of Nyxt dark mode to have a more theme-compliant colors, using the "
            (:code "theme:themed-css")
            " macro (making all the theme colors you've configured earlier available as
variables like " (:code "theme:on-primary") ".)")

        (:nsection :title "Status buffer appearance"
          (:p "You can customize the layout and styling of " (:nxref :class-name 'status-buffer)
              " using the methods it uses for layout. These methods are: ")
          (:dl
           (:dt (:nxref :function 'nyxt:format-status))
           (:dd "General layout of the status buffer, including the parts it consists of.")
           (:dt (:nxref :function 'nyxt::format-status-buttons))
           (:dd "The (\"Back\", \"Forward\", \"Reload\") buttons section.")
           (:dt (:nxref :function 'nyxt::format-status-url))
           (:dd "The current URL display section.")
           (:dt (:nxref :function 'nyxt::format-status-tabs))
           (:dd "Tab listing.")
           (:dt (:nxref :function 'nyxt::format-status-modes))
           (:dd "List of modes."))
          (:p "To complement the layout produced by these " (:code "format-*")
              " functions, you might need to add more rules or replace the "
              (:nxref :slot 'style :class-name 'status-buffer "style of status buffer") ".")))

      (:nsection :title "Scripting"
        (:p "You can evaluate code from the command line with "
            (:code "--eval") " and " (:code "--load") ".  From a shell:")
        (:pre (:code "$ nyxt --no-config --eval '+version+' \
  --load my-lib.lisp --eval '(format t \"Hello ~a!~&\" (my-lib:my-world))'"))
        (:p "You can evaluate multiple --eval and --load in a row, they are
executed in the order they appear.")
        (:p "You can also evaluate a Lisp file from the Nyxt interface with
the " (:nxref :command 'load-file) " command.  For
convenience, " (:nxref :command 'load-config-file) " (re)loads your initialization file.")
        (:p "You can even make scripts.  Here is an example foo.lisp:")
        (:pre (:code "#!/bin/sh
#|
exec nyxt --script \"$0\"
|#

;; Your code follows:
\(format t \"~a~&\" +version+)"))
        (:p "--eval and --load can be commanded to operate over an
existing instance instead of a separate instance that exits immediately.")
        (:p "The" (:nxref :slot 'remote-execution-p :class-name 'browser)
            " of the remote instance must be non-nil:")
        (:ncode
          (define-configuration browser
            ((remote-execution-p t))))
        (:p "To let know a private instance of Nyxt to load a foo.lisp script and run its"
            (:code "foo") "function:")
        (:pre (:code "nyxt --profile nosave --remote --load foo.lisp --eval '(foo)' --quit"))
        (:p "Note that " (:code "--quit")
            "at the end of each Nyxt CLI call here. If you don't provide " (:code "--quit")
            " when dealing with a remote instance, it will go into a REPL mode, allowing an
immediate communication with an instance:")
        (:pre (:code "nyxt --remote
(echo \"~s\" (+ 1 2)) ;; Shows '3' in the message area of remote Nyxt")))

      (:nsection :title "User scripts"
        (:p "User scripts are a conventional and lightweight way to run arbitrary JavaScript
code on some set of pages/conditions. While not as powerful as either
WebExtensions on Lisp-native extensions to Nyxt, those hook into the tenderer
inner working and allow you to change the page and JavaScript objects associated
to it.")
        (:p "As an example, you can remove navbars from all the pages you visit with this
small configuration snippet (note that you'd need to have "
            (:nxref :class-name 'nyxt/user-script-mode:user-script-mode)
            " in your " (:nxref :function 'default-modes "buffer default-modes") " ):")
        (:ncode
          (define-configuration web-buffer
            "Enable user-script-mode, if you didn't already."
            ((default-modes (pushnew 'nyxt/user-script-mode:user-script-mode %slot-value%))))

          (define-configuration nyxt/user-script-mode:user-script-mode
            ((nyxt/user-script-mode:user-scripts
              (list
               (make-instance 'nyxt/user-script-mode:user-script
                              :code "// ==UserScript==
                              // @name          No navbars!
                              // @description	A simple script to remove navbars
                              // @run-at        document-end
                              // @include       http://*/*
                              // @include       https://*/*
                              // @noframes
                              // ==/UserScript==

                              var elem = document.querySelector(\"header\") || document.querySelector(\"nav\");
                              if (elem) {
                              elem.parentNode.removeChild(elem);
                              }"))
              :doc "Alternatively, save the code to some file and use
:base-path #p\"/path/to/our/file.user.js\".
Or fetch a remote script with
url (quri:uri \"https://example.com/script.user.js\")"))))
        (:p (:a :href "https://wiki.greasespot.net/Metadata_Block" "Greasemonkey documentation")
            " lists all the possible properties that a user script might have. To Nyxt
implementation, only those are meaningful:")
        (:dl
         (:dt "@include and " (:nxref :function 'nyxt/user-script-mode:include))
         (:dd "Sets the URL pattern to enable this script for. Follows the pattern "
              (:code "scheme://host/path")
              ", where scheme is either a literal scheme or and asterisk (matching any
scheme), and host and path are any valid characters plus asterisks (matching any
set of characters) anywhere.")
         (:dt "@match")
         (:dd "Same as @include.")
         (:dt "@exclude and " (:nxref :function 'nyxt/user-script-mode:exclude))
         (:dd "Similar to @include, but rather disables the script for the matching pages.")
         (:dt "@noframes and " (:nxref :function 'nyxt/user-script-mode:all-frames-p))
         (:dd "When present, disables the script for all the frames but toplevel ones. When
absent, injects the script everywhere. The Lisp-side"
              (:nxref :function 'nyxt/user-script-mode:all-frames-p) "works in an opposite way.")
         (:dt "@run-at and " (:nxref :function 'nyxt/user-script-mode:run-at))
         (:dd "When to run a script. Allowed values: document-start, document-end,
document-idle (in Nyxt implementation, same as document-end).")
         (:dt "@require")
         (:dd "Allows including arbitrary JS files hosted on the Internet or loaded from the
same place as the script itself. Neat for including some JS libraries, like jQuery.")))

      (:nsection :title "Headless mode"
        (:p "Similarly to Nyxt's scripting functionality, headless mode runs without a
graphical user interface. Possible use-cases for this mode are web scraping,
automations and web page analysis.")
        (:p "To enable headless mode, simply start Nyxt with the "
            (:code "--headless")
            " CLI flag and provide a script file to serve as the configuration file:")
        (:pre (:code "nyxt --headless --config /path/to/your/headless-config.lisp"))
        (:p "Note that you pass it a " (:i "configuration file")
            "—headless mode is only different from the regular Nyxt functions in that it has
no GUI, and is all the same otherwise, contrary to all the seeming similarities
to the " (:code "--script") " flag usage.")
        (:p "The example below showcases frequent idioms that are found in the
mode's configuration file:")
        (:pre (:code "#!/bin/sh
#|
exec nyxt --headless --no-auto-config --profile nosave --config \"$0\"
|#

;; Disable session restoration to speed up startup and get more reproducible behavior.
\(define-configuration browser
  ((restore-session-on-startup-p nil)))

;; Load the URL of Nyxt repository by default in all new buffers.
;; Alternatively, call `buffer-load' in `after-startup-hook'.
\(define-configuration browser
  ((default-new-buffer-url (quri:uri \"https://github.com/atlas-engineer/nyxt\"))))

\(hooks:on (after-startup-hook *browser*) (browser)
  ;; Once the page's done loading, do your thing.
  (hooks:once-on (buffer-loaded-hook (current-buffer)) (buffer)
    ;; It's sometimes necessary to sleep, as `buffer-loaded-hook' fires when the
    ;; page is loaded, which does not mean that all the resources and scripts
    ;; are done loading yet. Give it some time there.
    (sleep 0.5)
    ;; All the Nyxt reporting happens in headless mode, so you may want to log
    ;; it with `echo' and `echo-warning'.
    (echo \"Nyxt GitHub repo open.\")
    ;; Updating the `document-model' so that it includes the most relevant
    ;; information about the page.
    (nyxt:update-document-model)
    ;; Click the star button.
    (nyxt/dom:click-element
     (elt (clss:select \"[aria-label=\\\"Star this repository\\\"]\"
                       (document-model buffer))
                       0))
    (echo \"Clicked the star.\")
    ;; It's good tone to `nyxt:quit' after you're done, but if you use nyxt
    ;; --no-socket, you don't have to. Just be ready for some RAM eating :)
    (nyxt:quit)))"))
        (:p "The contents of headless-config.lisp feature configuration forms that
make Nyxt perform some actions to the opened pages and/or on certain
hooks. Things you'd most probably want to put there are: ")
        (:ul
         (:li "Hook bindings, using the " (:nxref :package 'nhooks)
              " library and hooks provided by Nyxt.")
         (:li "Operations on the page. Check the " (:nxref :package 'nyxt/dom) "
          library and the " (:nxref :function 'document-model) " method.")
         (:ul
          (:li "The " (:nxref :function 'document-model)
               " method has a reasonably fresh copy of the page DOM (Document Object Model,
reflecting the dynamic structure of the page). It is a " (:nxref :package 'plump "Plump")
               " DOM, which means that all " (:nxref :package 'plump "Plump") " (and "
               (:nxref :package 'clss "CLSS") ") functions can be used on it.")
          (:li (:nxref :function 'update-document-model)
               " is a function to force DOM re-parsing for the cases when you consider the
current " (:nxref :function 'document-model) " too outdated.")
          (:li (:nxref :function 'clss:select)
               " is a CLSS function to find elements using CSS selectors (a terse
           notation for web page element description).")
          ;; FIXME: Make a nxref once we update CLSS submodule.
          (:li (:code"clss:ordered-select") " is the same as "
               (:nxref :function 'clss:select)
               ", except it guarantees that all the elements are returned in a
           depth-first traversal order.")
          (:li (:nxref :function 'nyxt/dom:click-element)
               " to programmatically click a certain element (including the ones returned by "
               (:nxref :function 'clss:select) ".)")
          (:li (:nxref :function 'nyxt/dom:focus-select-element) " to focus an input
           field, for example.")
          (:li (:nxref :function 'nyxt/dom:check-element) " to check a checkbox or a radio button.")
          (:li (:nxref :function 'nyxt/dom:select-option-element) " to select an option from the "
               (:code "<select>") " element options.")))
        (:p "Additionally, headless mode gracefully interacts with other CLI toggles the
Nyxt has:")
        (:ul
         (:li (:code "--headless") " itself! Notice that you can debug your script
by omitting this CLI flag.  When you're confident enough about it, put it back
in. A good debugging tip, isn't it?")
         (:li (:code "--no-socket")
              " flag allows starting as many Nyxt instances as your machine can
handle. Useful to parallelize computations.")
         (:li (:code "--profile nosave")
              " to not pollute your history and cache with the script-accessed pages.")))

      (:nsection :title "Built-in REPL"
        (:p "Nyxt has a built-in REPL, available with "
            (:nxref :command 'nyxt/repl-mode:repl) " command."
            "The REPL can be used to try out some code snippets for automation or quickly
make some Lisp calculations. All the packages Nyxt depends on are available in
REPL with convenient nicknames, and all the code is evaluated in "
            (:nxref :package :nyxt-user) " package.")
        (:p "Once the REPL is open, there's only one input cell visible. This cell, always
present at the bottom of the screen, adds new cells to the multi-pane interface
of Nyxt REPL. You can type in " (:code "(print \"Hello, Nyxt!\")")
" and press C-return to evaluate the cell. A new cell will appear at the top of the buffer, with
input area containing familiar code, with some " (:code "v332 = \"Hello, Nyxt!\"")
" variable assignment, and with a verbatim text outputted by your code:")
        (:pre (:code "Hello, Nyxt!"))
        (:p "This cell-based code evaluation is the basis of the Nyxt REPL. For more features, see "
            (:nxref :package :nyxt/repl-mode "REPL mode documentation") "."))

      (:nsection :title "Advanced configuration"
        (:p "While " (:nxref :macro 'define-configuration) " is convenient, it is mostly
restricted to class slot configuration.  If you want to do anything else on
class instantiation, you'll have to specialize the
lower-level " (:nxref :function 'customize-instance)
" generic function.  Example:")
        (:ncode
          (defmethod customize-instance ((buffer buffer) &key)
            (echo "Buffer ~a created." buffer)))
        (:p "All classes with metaclass " (:nxref :class-name 'user-class) " call "
            (:nxref :function 'customize-instance) " on instantiation,
after " (:nxref :function 'initialize-instance)(:code " :after") ".  The primary method is reserved
to the user, however the " (:code ":after") " method is reserved to the Nyxt
core to finalize the instance."))

      (:nsection :title "Extensions"
        (:p "To install an extension, copy inside the "
            (:nxref :variable '*extensions-directory*) " (default to "
            (:code "~/.local/share/nyxt/extensions")").")
        (:p "Extensions are regular Common Lisp systems.")
        (:p "A catalog of extensions is available in the "
            (:code "document/EXTENSIONS.org") " file in the source repository."))

      (:nsection :title "Troubleshooting"

        (:nsection :title "Debugging and reporting errors"
          (:p "If you experience hangs or errors you can reproduce, you can use the "
              (:nxref :command 'nyxt:toggle-debug-on-error)
              " command to enable Nyxt-native debugger and see the reasons of these. Based on
this information, you can report a bug using " (:nxref :command 'nyxt:report-bug) ".")
          (:p "You can also try to start the browser with the " (:code "--failsafe")
              " command line option and see if you can reproduce your issue then.  If not,
then the issue is most likely due to your configuration, an extension, or some
corrupt data file like the history.")
          (:p "Note that often errors, hangs, and crashes happen on the side of renderer and
thus are not visible to the Nyxt-native debugger and fixable on the side of
Nyxt. See below."))

        (:nsection :title "Playing videos"
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
              (:code "~/.cache/gstreamer-1.0") " and restart Nyxt."))

        (:nsection :title "Website crashes"
          (:p "If some websites systematically crash, try to install all the required
GStreamer plugins as mentioned in the 'Playing videos' section."))

        (:nsection :title "Input method support (CJK, etc.)"
          (:p "Depending on your setup, you might have to set some environment variables
or run some commands before starting Nyxt, for instance")
          (:pre (:code "GTK_IM_MODULE=xim
XMODIFIERS=@im=ibus
ibus --daemonize --replace --xim"))
          (:p "You can persist this change by saving the commands in
your " (:code ".xprofile") " or similar."))

        (:nsection :title "Font size on HiDPI displays"
          (:p "On HiDPI displays, the font size used for displaying web and Nyxt's
prompt buffer content might be too tiny.")
          (:p "To fix this issue when using the WebKitGTK render, export the
following environment variable before starting Nyxt:")
          (:pre (:code "export GDK_SCALE=2
nyxt
"))
          (:p "If that doesn't look satisfactory, try exporting the following environment variables before starting Nyxt:")
          (:pre (:code "export GDK_SCALE=2
export GDK_DPI_SCALE=0.5
nyxt
")))

        (:nsection :title "StumpWM mouse scroll"
          (:p "If the mouse scroll does not work for you, see the "
              (:a
               :href "https://github.com/stumpwm/stumpwm/wiki/FAQ#my-mouse-wheel-doesnt-work-with-gtk3-applications-add-the-following-to"
               "StumpWM FAQ")
              " for a fix."))

        (:nsection :title "Blank WebKit web-views"
          (:p "If you are experiencing problems with blank web-views on some sites you
    can try to disable compositing. To disable compositing from your
    initialization file, you can do the following: ")
          (:ncode
            (setf (uiop:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1")))))))
