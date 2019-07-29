(defpackage :next.tests
  (:use :common-lisp
        :next
        :prove))

(in-package :next.tests)

(plan nil)

(defparameter *candidates* '("link-hints" "active-history-node" "history-backwards"
                             "did-finish-navigation" "history-forwards"
                             "history-forwards-query" "copy-title" "did-commit-navigation"
                             "copy-url" "add-or-traverse-history" "set-url-new-buffer"
                             "noscript-mode" "help" "jump-to-heading" "next-search-hint"
                             "bookmark-current-page" "new-buffer" "command-inspect"
                             "add-search-hints" "kill" "remove-search-hints" "load-file"
                             "keymap" "next-version" "name" "scroll-left" "activate"
                             "scroll-page-down" "scroll-right" "destructor"
                             "scroll-to-bottom" "switch-buffer-next" "command-evaluate"
                             "did-finish-navigation" "bookmark-anchor" "scroll-down"
                             "scroll-up" "vi-button1" "reload-current-buffer"
                             "copy-anchor-url" "bookmark-delete" "go-anchor-new-buffer"
                             "zoom-out-page" "keymap-schemes" "buffer" "new-window"
                             "execute-command" "make-visible-new-buffer" "download-url"
                             "switch-buffer" "application-mode" "delete-buffer"
                             "start-swank" "did-commit-navigation" "delete-window"
                             "bookmark-url" "unzoom-page" "load-init-file"
                             "download-anchor-url" "zoom-in-page" "document-mode"
                             "scroll-to-top" "vi-insert-mode" "help-mode" "vi-normal-mode"
                             "minibuffer-mode" "proxy-mode" "blocker-mode"
                             "delete-current-buffer" "scroll-page-up"
                             "set-url-from-bookmark" "switch-buffer-previous"
                             "download-list" "download-mode" "set-url-current-buffer"
                             "about" "variable-inspect" "go-anchor" "previous-search-hint"
                             "go-anchor-new-buffer-focus")
  "existing next commands.")

(subtest "Fuzzy match"
  (is "help" (first (fuzzy-match "hel"
                                 '("help-mode" "help" "foo-help" "help-foo-bar"))))
  (is "help" (first (fuzzy-match "hel"
                                 *candidates*))
      "match 'help' with real candidates list")
  (is "switch-buffer" (first (fuzzy-match "swit buf"
                                          '("about" "switch-buffer-next" "switch-buffer"
                                            "delete-buffer")))
      "match 'swit buf' (small list)")
  (is "switch-buffer" (first (fuzzy-match "swit buf"
                                          *candidates*))
      "match 'swit buf' with real candidates list")
  (is "switch-buffer" (first (fuzzy-match "buf swit"
                                          '("about" "switch-buffer-next" "switch-buffer"
                                            "delete-buffer")))
      "reverse match 'buf swit' (small list)")
  (is "switch-buffer" (first (fuzzy-match "buf swit"
                                          *candidates*))
      "reverse match 'buf swit' with real candidates list")

  (is "delete-foo" (first (fuzzy-match "de"
                                       '("some-mode" "delete-foo")))
      "candidates beginning with the first word appear first")

  (is "foo-bar" (first (fuzzy-match "foobar"
                                    '("foo-dash-bar" "foo-bar")))
      "search witout a space. All characters count (small list).")
  (is "switch-buffer" (first (fuzzy-match "sbf"
                                    *candidates*))
      "search witout a space. All characters count, real list."))

(finalize)
