;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defparameter +changelog+ (make-hash-table :test #'equal)
  "A Hash table of Key = version, and Value = the information about that
particular revision.")

(defmacro define-version (version-string &body body)
  `(setf (gethash ,version-string +changelog+)
         (spinneret:with-html-string (:div (:h2 ,version-string) ,@body))))

(define-command-global changelog ()
  "Show the changelog."
  (with-current-html-buffer (buffer "*Changelog*" 'base-mode)
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 "Change Log")
      (:p "Nyxt tries to use semantic versioning.")
      (:p "Given a version number MAJOR.MINOR.PATCH, we increment:")
      (:ul
       (:li "MAJOR when we make incompatible API changes.")
       (:li "MINOR when we add functionality in a backwards-compatible manner.")
       (:li "PATCH when we make backwards-compatible bug fixes."))
      (loop for version in (alex:hash-table-values +changelog+)
            collect (:raw version)))))

(define-version "2.0.0"
  (:ul
   (:li "Search engine autocompletion. See the search engine suggestions in
        the prompt buffer as you type. By default, support for DuckDuckGo and
        Wikipedia is included.")
   (:li (:code "set-url") " and related commands now allow for multiple selection.")
   (:li "On downloads, display the number of downloaded bytes.")
   (:li "Buffer default modes can now be customized via
        the " (:code "default-modes") " generic function. This function uses
        the " (:code "append") " method
        combination." (:code "define-configuration") " now supports customizing
        these methods just as for slots.")
   (:li "Commands defined with " (:code "define-command") " are no longer
        listed in " (:code "execute-command") "
        use " (:code "define-command-global") " to define global commands that
        are always listed.")
   (:li "New " (:code "define-command-global") "function. Use this to
        define globally visible commands.")
   (:li "Removed the " (:code "notify") " function, also removing the
        dependency on notify-osd. This function was not generic enough and only
        used in one place. Notifications will be reintrocued with a more generic
        interface.")
   (:li "Remove " (:code "set-url-from-bookmark-new-buffer") " It is
        redundant with set-url and set-url-from-bookmark.")
   (:li "New system-information command line option.")
   (:li "Removed Common Lisp file-attributes dependency."))
  (:h3 "Bug fixes")
  (:ul
   (:li "Fixed crash when right-clicking on some elements, like the Slack
        composition area.")
   (:li "Fixed crashes on click and scroll in prompt buffer.")
   (:li "Removed duplicates from prompt buffer history.")
   (:li "Fixed some commands, like" (:code "describe-variable") " and " (:code "history-tree."))
   (:li "Fixed " (:code "url-dispatching-handler") " when handler returns NIL.")
   (:li "Fixed loading of local HTML files when passed as command line argument.")))

(define-version "2.1.0"
  (:ul
   (:li (:code "expedition-mode") ". Expedition mode stores a set of links
        that you can then traverse forwards and backwards as if you are on an
        expedition through the Internet!")
   (:li (:code "select-frame") ". Select frame allows the user to
        drag-select multiple elements on screen. For example, the user can
        drag-select to open up all of the links of a search result.")
   (:li (:code "process-mode") ". Process mode enables you to automate the
        repetition of tedious tasks. For example, you can have process mode
        refresh a page whenever you save a file. This could be useful for
        previewing LaTeX documents, or working on websites!")
   (:li (:code "cruise-control-mode") ".  Cruise control mode allows you to
        scroll down the page at a fixed speed. This mode is particularly well
        suited for reading long articles.")
   (:li "Replace 'loading' text with animated spinner.")
   (:li "vi mode automatically switches to vi-insert-mode when logical.")
   (:li "The prompt buffer is used to interface with the user instead of
        GTK dialogs. This behavior can be controlled via
        the " (:code "native-dialogs") (:code "browser") "slot.")
   (:li "add raw-source
        to " (:code "add-domain-to-certificate-exceptions") " to allow for
        arbitrary URLs."))
  (:h3 "Bug fixes")
  (:ul
   (:li "Fix broken status buffer button to list-buffers.")
   (:li "Make bookmarklets callable from " (:code "execute-command") ".")
   (:li "Quit gracefully when " (:code "C-c") " is pressed in the parent shell.")
   (:li "Fix search engine queries with " (:code "&") " and " (:code "%") " characters.")
   (:li "Make " (:code "search-buffer-source") " customizable
        with " (:code "define-configuration") ". Thanks to @mariari.")
   (:li "Support command-line arguments in " (:code "external-editor-program") ".")
   (:li "Fix " (:code "proxy-mode") " configuration. Thanks to @Zwo1in.")
   (:li "Fix history corruptions when an error occurred while saving the history to disk.")
   (:li "Fix " (:code "history-all-query") ". Thanks to @franburstall.")
   (:li "Fix " (:code "search-buffer") "when input contains regexp-special characters.")))

(define-version "2.1.1"
  (:ul
   (:li (:code "reopen-buffer") " restores the scroll position.")
   (:li "New " (:code "copy-username") " command for password managers."))
  (:h3 "Bug fixes")
  (:ul
   (:li "Fix history double-restore which led to many crashes.")
   (:li "Create file and parent directories of configuration files if they
        don't exist.")
   (:li "Fix " (:code "set-url-from-bookmark") " on multi-selection.")
   (:li "Fix " (:code "process-mode") " to not run an action when it is nil.")))

(define-version "2.2.0"
  (:ul
   (:li "New " (:code "changelog") " command.")
   (:li "New " (:code "show-qrcode-of-current-url") " command.  (Thanks to @ag91!)")
   (:li "New " (:code "view-source") " command.")
   (:li "New " (:code "edit-user-file-with-external-editor") " command.")
   (:li "New " (:code "summarize-buffer") " command.")
   (:li "Macro editor: interactively record command macros with " (:code "edit-macro") "."
        " Macros can be saved to the " (:code "auto-config.lisp") " file.")
   (:li "New action for the " (:code "switch-buffer") " command to delete selected buffers.")
   (:li "Support for GTK clipboard.  (Thanks to @shamazmazum!)")
   (:li "Deprecated commands no longer show in the "
        (:code "execute-command") " list.")
   (:li "New " (:code "titler") " window slot to allow customizing the window title.") (:li (:code "jump-to-heading") " now also lists content keywords.")
   (:li "Various manual and tutorial improvements and fixes.  (Thanks to @pdelfino!)")
   (:li "Deprecate " (:code "paste-or-set-url") ".")
   (:li "Replace " (:code "copy-system-information") " with " (:code "show-system-information") ".")
   (:li (:p "Don't forward printable characters in vi-normal-mode.")
        (:p "Concretely, pressing an unbound letter won't insert it in an HTML input."))
   (:li (:p "New VI status and prompt buffer indicator.  (Thanks to @edgar-vincent!)")
        (:p "Both the status area and the prompt buffer now display a colored  indicator for "
            (:code "vi-normal-mode") " and " (:code "vi-insert-mode") "."))
   (:li "New navigation commands: "
        (:code "go-previous") ", "
        (:code "go-next") ", "
        (:code "go-up") ", "
        (:code "go-to-homepage") ".")
   (:li "Password commands set the default input to the current host.")
   (:li "Anonymous commands are now allowed in keybindings (e.g. with "
        (:code "make-command") ").")
   (:li (:code "describe-variable")
        " can now describe compound values recursively, such as lists of variables.")
   (:li (:code "execute-extended-command") " performs type checking and
        prefills default values."))

  (:h3 "Backward-incompatible changes")
  (:ul
   (:li (:p "Updated " (:code "cl-webkit") " to version 3.0.0.")
        (:p (:code "webkit2:webkit-web-view-evaluate-javascript")
            " lambda list changed."))
   (:li (:p "We've introduced an API-breaking change to the " (:code "nyxt/history-tree")
            " library: now all its call points take an owner as parameter."
            "This has allowed us to fix the constant history corruptions.")))
  (:h3 "Element hinting overhaul")
  (:p "It should be now faster, better integrated and more exhaustive (such as drop-down buttons).")
  (:p
   "A new DOM parser has been developed.  As a result, much of the
       former Parenscript code has been replaced by pure Lisp code.")
  (:h3 "New 'panel' buffers.")
  (:p "Panel buffers can be used to display arbitrary information on the
       side of a window.  A few new commands are thus introduced:")
  (:ul
   (:li (:code "show-bookmarks-panel"))
   (:li (:code "show-buffers-panel"))
   (:li (:code "delete-panel-buffer")))
  (:h3 "Prompt buffer")
  (:ul
   (:li "Input area no longer stutters when updating the display.")
   (:li "Key press responsiveness has been dramatically improved.")
   (:li "The prompt is properly refreshed when pasting to it.")
   (:li (:p "Bind " (:code "M-space") " to " (:code "resume-prompt") " to make it more useful.")
        (:p "It's particularly useful to resume a search."))
   (:li (:p "Bind " (:code "C-M-space") " to " (:code "execute-extended-command") ".")
        (:p "It makes it easier for users to execute commands and supply parameters.")))

  (:h3 "Status area")
  (:ul
   (:li "Clicking on modes now describes them.")
   (:li "New '+' button to toggles modes.")
   (:li "Arbitrary HTML is now allowed for mode glyphs."))
  (:h3 "Support for buffer tree relationships")
  (:ul
   (:li (:code "switch-buffer-previous") " and " (:code "switch-buffer-next")
        " browse the buffer tree in a depth-first fashion.")
   (:li "New " (:code "switch-buffer-last")
        " command to easily switch back and forth between the 2 most recent buffers.")
   (:li (:code "list-buffers") " can now display the the buffers as a tree."))
  (:h3 "Platform support")
  (:ul
   (:li "Fix slow buffer and prompt-buffer creation on FreeBSD.  (Thanks to
        @shamazmazum!)"))
  (:h3 "Bug fixes")
  (:ul
   (:li "Various fixes with the password manager commands.")
   (:li "Don't crash when an error occurs in a GTK signal.")
   (:li "Start up errors due to flawed user init files now lead to a restart
    of Nyxt without init file.")
   (:li (:p "Fix endless compilation of =tutorial.lisp= when SBCL dynamic space
    size to low.")
        (:p (:code "cl-markup") " was replaced by " (:code "spinneret") "."))
   (:li "Fix potential dead-lock in " (:code "restore-history-buffers") ".")
   (:li "Fix " (:code "buffer-history-tree") " that showed a truncated tree.")))
