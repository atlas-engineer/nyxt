;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-command-global changelog ()
  "Show the changelog."
  (with-current-html-buffer (buffer "*Changelog*" 'base-mode)
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 "Change Log")
      (:p "Nyxt uses semantic versionining.")
      (:p "Given a version number MAJOR.MINOR.PATCH, we increment:")
      (:ul
       (:li "MAJOR when we make incompatible API changes.")
       (:li "MINOR when we add functionality in a backwards-compatible manner.")
       (:li "PATCH when we make backwards-compatible bug fixes."))
      (:div
       (:h2 "2.1.1")
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
      (:div
       (:h2 "2.1.0")
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
      (:div
       (:h2 "2.0.0")
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
        (:li "Fixed loading of local HTML files when passed as command line argument."))))))
