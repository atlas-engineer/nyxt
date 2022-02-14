;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defparameter +changelog+ (make-hash-table :test #'equal)
  "A hash table of Key = version, and Value = the information about that
particular revision.")

(defmacro define-version (version-string &body body)
  `(setf (gethash ,version-string +changelog+)
         (spinneret:with-html-string (:div (:h2 ,version-string) ,@body))))

(define-internal-page-command-global changelog ()
    (buffer "*Changelog*" 'base-mode)
  "Show the changelog."
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
          collect (:raw version))))

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

(define-version "2.2.1"
  (:ul
   (:li "Re-added the inferred-keyword source to the bookmark prompt buffer.")
   (:li "Numerous documentation fixes and updates.")
   (:li (:code "define-scheme") "syntax has been extended to allow importing
   other schemes.  See the manual for an example.")
   (:li "Arbitrary HTML is now allowed in mode formatting.")
   (:li "*Bookmarks* buffer is automatically updated when deleting entries.  (Thanks to @shamazmazum!)")
   (:li "Processes can now be stopped in process-mode.")
   (:li "New " (:code "repeat-times") " command.")
   (:li "List methods in class descriptions."))
  (:h3 "Build and compilation changes")
  (:ul
   (:li "Quicklisp is no longer used to fetch dependencies.  Instead, all Lisp
   dependencies are pinned and fetched via Git submodules into the "
   (:code "_build") " subdirectory.  This should improve reproducibility.
   The " (:code "NYXT_QUICKLISP") " environment variable has been replaced by "
   (:code "NYXT_SUBMODULES") "."))
  (:h3 "Platform support")
  (:ul
   (:li "Quicklisp can now be properly loaded when Nyxt was installed via the .deb file."))
  (:h3 "Bug fixes")
  (:ul
   (:li "Properly handle cancelation in yes/no prompt-buffers.")
   (:li "Fix sandboxing.  (Thanks to @tiberious726!)")
   (:li "Fix toggle-mark in visual-mode.  (Thanks to @hendursaga!)")
   (:li "Report load-after-system warnings.  (Thanks to @hendursaga!)")
   (:li "Properly scroll into view when in visual-mode.  (Thanks to @aaron-tan!)")
   (:li "Fix upload of files with wildcard characters in their name.  (Thanks to @shamazmazum!)")))

(define-version "2.2.2"
  (:ul
   (:li "HTTP redirects are no longer stored to history.")
   (:li "Selecting hints in prompt-buffer no longer scrolls the page automatically, press "
        (:code "C-l") " instead."))
  (:h3 "Build and compilation changes")
  (:ul
   (:li "The source tarball now embeds the Git submodules and thus fixes build errors about missing dependencies."))
  (:h3 "Platform support")
  (:ul
   (:li "Fix type errors when building with some unusual compiler. (Thanks to @lpaviets!)"))
  (:h3 "Bug fixes")
  (:ul
   (:li "Fix prompt buffer paging.")
   (:li (:code "switch-buffer") " is no longer triggered when there is no buffer to restore.")
   (:li "On various occasions, KeepassXC entries could be missing from the prompt, this is now fixed.")
   (:li (:code "lisp-repl") " now prints all results properly.")
   (:li "Onion URLs are now supported.  (Thanks to @hendursaga!)")))

(define-version "2.2.3"
  (:ul
   (:li "Speed up most network queries.  This may also prevent some hangs that
used to happen when loading resource-intensive pages.")
   (:li "Speed up " (:code "set-url") " and friends.  (Thanks to @shamazmazum!)"
        (:br)
        "The user input suggestion should now be instantaneously updated in the
        suggestion list.  Search engine completions no longer hold back the
        listing of other suggestions.")
   (:li "Nyxt now prompts for confirmation before deleting a buffer that has
   edited contents.")
   (:li "New common text editing bindings (select-all, undo, redo, cut, etc.).")
   (:li "Display source code in " (:code "describe-function") " whenever possible.")
   (:li "Allow for arbitrary HTML in the prompt buffer (both attributes and the prompt).")
   (:li "Permission requests are now handled (such geolocalization access).")
   (:li "Intelligent Tracking Prevention is no longer systematically enabled. "
        "This should fix some website incompatibilities. "
        "ITP can be selectively enabled with " (:code "reduce-tracking-mode") ".")
   (:li (:code "reduce-tracking-mode") " has a new options, "
        (:code "preferred-user-agent") " which is set to a generic value by default.")
   (:li "The Lisp REPL now highlights the input, displays the input package and
       displays the current package at the prompt.")
   (:li "New " (:code "m l") " VI binding to " (:code "list-bookmarks") "."))

  (:h3 "Bug fixes")
  (:ul
   (:li "Fix crash and hangs with WebKitGTK 2.34."
        (:br) (:b "Warning: ") "Sandboxing is no longer enforced.")
   (:li "Fix hangs in some cases when " (:code "blocker-mode") " hostlists were
out-of-date.")
   (:li "Work around load failures when going backward or forward in history.")
   (:li "Catch more errors in search completion.")))

(define-version "2.2.4"
  (:ul
   (:li "New " (:code "forward-to-renderer") " command.  When a key is bound to
   it, the last keypress is forwarded to the renderer.")
   (:li "New " (:code "reset-asdf-registries") " helper function.  "
        "Call it in your initialization file to re-enable the default ASDF
search paths, "
        "e.g. to find your Common Lisp libraries in ~/common-lisp or your Emacs
SLY install.")
   (:li "Prompt buffer attributes can now be computed asynchronously."
        "This is used for instance to speed up " (:code "switch-buffer") ".")
   (:li "Non-prefixed completion is now enabled for the default search engine."
        "You can turn off this behaviour it with the "
        (:code "search-always-auto-complete-p") " slot option.")
   (:li "New " (:code "delete-command") " command.  (Thanks to @BlueFlo0d!)")
   (:li "More bindings and some minor improvements to " (:code "visual-mode") ".  (Thanks to @hendursaga!)"))

  (:h3 "Platform support")
  (:p "Nyxt is compiled with SBCL set to use a dynamic-space-size of at least 3GiB."
      "This should mitigate memory errors since Nyxt is very memory intensive."
      "You can override this by setting the " (:code "LISP_FLAGS") " variable as
      per the installation instructions.")
  (:h3 "Bug fixes")
  (:ul
   (:li "Fix the frequent dead locks on multi-buffer deletion.")
   (:li "Fix some rare dead locks on page load.")
   (:li "Fix crash on unhandled condition in a prompt buffer thread.")
   (:li "Fix iframe support on redirects (for instance with "
        (:code "blocker-mode") ").")
   (:li "Keymap fixes:"
        (:ul
         (:li "Prefix keymaps can no longer be shadowed.")
         (:li "Remove shadowed bindings from the binding listing.")
         (:li "Don't list shadowed parent keymap bindings.")))
   (:li "Fix " (:code "resume-prompt") " to hide the resumed prompt buffer when done.")
   (:li "Fix freeze due to errors in " (:code "blocker-mode") ".")
   (:li "Fix formatting of web process crash report.")
   (:li "Fix some " (:code "auto-mode") " issues.  (Thanks to @efimerspan!)")))

(define-version "3.0.0"
  (:ul
   (:li "Revamp status area design.")
   (:li "New prompt-buffer fuzzy matching algorithm, hopefully offering more
  relevant results.  (Thanks to @BlueFlo0d!)")
   (:li "Add support for the Gopher and Gemini protocols.")
   (:li "New " (:code "save-input-data") ", " (:code "set-input-data-from-saved")
        " and " (:code "set-input-data-from-saved-domain") " commands to record
and restore input fields.")
   (:li "Internal pages are now using the " (:code "nyxt") " URL scheme.  They support the "
        (:code "lisp") " protocol to allow evaluating arbitrary Lisp, for instance from a button click."
        "Internal pages also have a URL now, which means they have history support.")
   (:li "New " (:code "define-internal-page-command") " helper to define internal pages.")
   (:li "New " (:code "define-internal-scheme") " helper to define custom schemes.")
   (:li (:code "jump-to-heading") " commands now sort and indent the headings in a natural fashion.")
   (:li "New " (:code "match-port") " URL designator predicate for auto-mode rules.")
   (:li "New " (:code "toggle-message-buffer") " and " (:code "toggle-status-buffer") " commands.")
   (:li "New " (:code "bookmark-frequent-visits") " mode.")
   (:li (:code "application-mode") " is now " (:code "passthrough-mode") ".")

   (:h3 "Bindings")
   (:ul
    (:li "Add Emacs/VI text editing bindings in " (:code "prompt-buffer-mode") " and " (:code "lisp-repl") ".")
    (:li "Rebind " (:code "history-forwards") " to " (:code "history-forwards-maybe-query") " in the Emacs and VI schemes.")
    (:li "Rebind " (:code "bookmark-url") " and " (:code "copy-title") " to be more consistent with other bindings."))

   (:h3 "Programming interface")
   (:ul
    (:li "Thread name is now mandatory in " (:code "run-thread") ".")
    (:li "New " (:code "nyxt-unstable") " " (:code "*features*") " when built from source on an untagged commit.  A feature with the commit is also added.")
    (:li "New " (:code "prompt1") " helper.")
    (:li "New " (:code "theme") " library.")
    (:li "Rename buffer slot " (:code "load-status") " to " (:code "status") "."))))
