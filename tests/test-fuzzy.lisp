(defpackage :next.tests
  (:use :common-lisp
        :next
        :prove))

(in-package :next.tests)

(plan nil)

(defparameter *candidates* '("LINK-HINTS" "ACTIVE-HISTORY-NODE" "HISTORY-BACKWARDS"
                             "DID-FINISH-NAVIGATION" "HISTORY-FORWARDS"
                             "HISTORY-FORWARDS-QUERY" "COPY-TITLE" "DID-COMMIT-NAVIGATION"
                             "COPY-URL" "ADD-OR-TRAVERSE-HISTORY" "SET-URL-NEW-BUFFER"
                             "NOSCRIPT-MODE" "HELP" "JUMP-TO-HEADING" "NEXT-SEARCH-HINT"
                             "BOOKMARK-CURRENT-PAGE" "NEW-BUFFER" "COMMAND-INSPECT"
                             "ADD-SEARCH-HINTS" "KILL" "REMOVE-SEARCH-HINTS" "LOAD-FILE"
                             "KEYMAP" "NEXT-VERSION" "NAME" "SCROLL-LEFT" "ACTIVATE"
                             "SCROLL-PAGE-DOWN" "SCROLL-RIGHT" "DESTRUCTOR"
                             "SCROLL-TO-BOTTOM" "SWITCH-BUFFER-NEXT" "COMMAND-EVALUATE"
                             "DID-FINISH-NAVIGATION" "BOOKMARK-ANCHOR" "SCROLL-DOWN"
                             "SCROLL-UP" "VI-BUTTON1" "RELOAD-CURRENT-BUFFER"
                             "COPY-ANCHOR-URL" "BOOKMARK-DELETE" "GO-ANCHOR-NEW-BUFFER"
                             "ZOOM-OUT-PAGE" "KEYMAP-SCHEMES" "BUFFER" "NEW-WINDOW"
                             "EXECUTE-COMMAND" "MAKE-VISIBLE-NEW-BUFFER" "DOWNLOAD-URL"
                             "SWITCH-BUFFER" "APPLICATION-MODE" "DELETE-BUFFER"
                             "START-SWANK" "DID-COMMIT-NAVIGATION" "DELETE-WINDOW"
                             "BOOKMARK-URL" "UNZOOM-PAGE" "LOAD-INIT-FILE"
                             "DOWNLOAD-ANCHOR-URL" "ZOOM-IN-PAGE" "DOCUMENT-MODE"
                             "SCROLL-TO-TOP" "VI-INSERT-MODE" "HELP-MODE" "VI-NORMAL-MODE"
                             "MINIBUFFER-MODE" "PROXY-MODE" "BLOCKER-MODE"
                             "DELETE-CURRENT-BUFFER" "SCROLL-PAGE-UP"
                             "SET-URL-FROM-BOOKMARK" "SWITCH-BUFFER-PREVIOUS"
                             "DOWNLOAD-LIST" "DOWNLOAD-MODE" "SET-URL-CURRENT-BUFFER"
                             "ABOUT" "VARIABLE-INSPECT" "GO-ANCHOR" "PREVIOUS-SEARCH-HINT"
                             "GO-ANCHOR-NEW-BUFFER-FOCUS")
  "existing next commands.")

(subtest "Fuzzy match"
  (is "help" (first (next::fuzzy-match "hel"
                                       '("help-mode" "help" "foo-help" "help-foo-bar"))))
  (is "HELP" (first (next::fuzzy-match "hel"
                                       *candidates*))
      "match 'help' with real candidates list")
  (is "switch-buffer" (first (next::fuzzy-match "swit buf"
                                                '("about" "switch-buffer-next" "switch-buffer"
                                                  "delete-buffer")))
      "match 'swit buf' (small list)")
  (is "SWITCH-BUFFER" (first (next::fuzzy-match "swit buf"
                                                *candidates*))
      "match 'swit buf' with real candidates list")
  (is "switch-buffer" (first (next::fuzzy-match "buf swit"
                                                '("about" "switch-buffer-next" "switch-buffer"
                                                  "delete-buffer")))
      "reverse match 'buf swit' (small list)")
  (is "SWITCH-BUFFER" (first (next::fuzzy-match "buf swit"
                                                *candidates*))
      "reverse match 'buf swit' with real candidates list")

  (is "delete-foo" (first (next::fuzzy-match "de"
                                             '("some-mode" "delete-foo")))
      "candidates beginning with the first word appear first")

  (is "foo-bar" (first (next::fuzzy-match "foobar"
                                          '("foo-dash-bar" "foo-bar")))
      "search without a space. All characters count (small list).")
  (is "SWITCH-BUFFER" (first (next::fuzzy-match "sbf"
                                                *candidates*))
      "search without a space. All characters count, real list.")
  (is "FOO-BAR" (first (next::fuzzy-match "FOO"
                                          '("foo-dash-bar" "FOO-BAR")))
      "input is uppercase (small list)."))

(subtest "Parse URL"
  (is "https://next.atlas.engineer" (next::parse-url "https://next.atlas.engineer")
      "full URL")
  (is "https://next.atlas.engineer" (next::parse-url "next.atlas.engineer")
      "URL without protocol")
  (is "https://en.wikipedia.org/w/index.php?search=+wikipedia" (next::parse-url "wiki wikipedia")
      "search engine")
  (is "https://duckduckgo.com/?q=next+browser" (next::parse-url "next browser")
      "default search engine")
  (is "https://en.wikipedia.org/w/index.php?search=+wikipedia" (next::parse-url "wiki wikipedia")
      "wiki search engine")
  (is "file:///readme.org" (next::parse-url "file:///readme.org")
      "local file")
  (is "https://duckduckgo.com/?q=foo" (next::parse-url "foo")
      "empty domain")
  (is "https://duckduckgo.com/?q=algo" (next::parse-url "algo")
      "same domain and TLD")
  (is "http://[1:0:0:2::3:0.]/" (first (next::fuzzy-match "[" '("test1"
                                                                "http://[1:0:0:2::3:0.]/"
                                                                "test2")))
      "match regex meta-characters"))

(finalize)
