(defpackage :nyxt.tests
  (:use :common-lisp
        :nyxt
        :prove))

(in-package :nyxt.tests)

(plan nil)

(setf *browser* (make-instance *browser-class*))

(defparameter *suggestions* '("LINK-HINTS" "ACTIVE-HISTORY-NODE" "HISTORY-BACKWARDS"
                             "DID-FINISH-NAVIGATION" "HISTORY-FORWARDS"
                             "HISTORY-FORWARDS-QUERY" "COPY-TITLE" "DID-COMMIT-NAVIGATION"
                             "COPY-URL" "ADD-OR-TRAVERSE-HISTORY" "SET-URL-NEW-BUFFER"
                             "NOSCRIPT-MODE" "HELP" "JUMP-TO-HEADING" "NYXT-SEARCH-HINT"
                             "BOOKMARK-CURRENT-PAGE" "NEW-BUFFER" "COMMAND-INSPECT"
                             "ADD-SEARCH-HINTS" "KILL" "REMOVE-SEARCH-HINTS" "LOAD-FILE"
                             "KEYMAP" "NYXT-VERSION" "NAME" "SCROLL-LEFT" "ACTIVATE"
                             "SCROLL-PAGE-DOWN" "SCROLL-RIGHT" "DESTRUCTOR"
                             "SCROLL-TO-BOTTOM" "SWITCH-BUFFER-NYXT" "COMMAND-EVALUATE"
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
  "Existing Nyxt commands.")

(subtest "Fuzzy match"
  (is (first (nyxt::fuzzy-match "hel"
                                '("help-mode" "help" "foo-help" "help-foo-bar")))
      "help")
  (is (first (nyxt::fuzzy-match "hel"
                                *suggestions*))
      "HELP"
      "match 'help' with real suggestions list")
  (is (first (nyxt::fuzzy-match "swit buf"
                                '("about" "switch-buffer-nyxt" "switch-buffer"
                                  "delete-buffer")))
      "switch-buffer"
      "match 'swit buf' (small list)")
  (is (first (nyxt::fuzzy-match "swit buf"
                                *suggestions*))
      "SWITCH-BUFFER"
      "match 'swit buf' with real suggestions list")
  ;; TODO: Fix reverse fuzzy matching.
  ;; (is (first (nyxt::fuzzy-match "buf swit"
  ;;                               '("about" "switch-buffer-nyxt" "switch-buffer"
  ;;                                 "delete-buffer")))
  ;;     "switch-buffer"
  ;;     "reverse match 'buf swit' (small list)")
  ;; (is (first (nyxt::fuzzy-match "buf swit"
  ;;                               *suggestions*))
  ;;     "SWITCH-BUFFER"
  ;;     "reverse match 'buf swit' with real suggestions list")

  (is (first (nyxt::fuzzy-match "de"
                                '("some-mode" "delete-foo")))
      "delete-foo"
      "suggestions beginning with the first word appear first")

  (is (first (nyxt::fuzzy-match "foobar"
                                '("foo-dash-bar" "foo-bar")))
      "foo-bar"
      "search without a space. All characters count (small list).")
  (is (first (nyxt::fuzzy-match "sbf"
                                *suggestions*))
      "SWITCH-BUFFER"
      "search without a space. All characters count, real list.")
  (is (first (nyxt::fuzzy-match "FOO"
                                '("foo-dash-bar" "FOO-BAR")))
      "FOO-BAR"
      "input is uppercase (small list)."))

(subtest "Parse URL"
  (is (nyxt::parse-url "https://nyxt.atlas.engineer")
      "https://nyxt.atlas.engineer"
      "full URL")
  (is (nyxt::parse-url "nyxt.atlas.engineer")
      "https://nyxt.atlas.engineer"
      "URL without protocol")
  (is (nyxt::parse-url "wiki wikipedia")
      "https://en.wikipedia.org/w/index.php?search=wikipedia"
      "search engine")
  (is (nyxt::parse-url "nyxt browser")
      "https://duckduckgo.com/?q=nyxt+browser"
      "default search engine")
  (is (nyxt::parse-url "wiki wikipedia")
      "https://en.wikipedia.org/w/index.php?search=wikipedia"
      "wiki search engine")
  (is (nyxt::parse-url "file:///readme.org")
      "file:///readme.org"
      "local file")
  (is (nyxt::parse-url "foo")
      "https://duckduckgo.com/?q=foo"
      "empty domain")
  (is (nyxt::parse-url "algo")
      "https://duckduckgo.com/?q=algo"
      "same domain and TLD")
  (is (first (nyxt::fuzzy-match "[" '("test1"
                                      "http://[1:0:0:2::3:0.]/"
                                      "test2")))
      "http://[1:0:0:2::3:0.]/"
      "match regex meta-characters"))

(finalize)
