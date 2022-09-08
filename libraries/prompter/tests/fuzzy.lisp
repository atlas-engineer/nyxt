;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter/tests)

(defparameter *suggestions*
  '("LINK-HINTS" "ACTIVE-HISTORY-NODE" "HISTORY-BACKWARDS"
    "DID-FINISH-NAVIGATION" "HISTORY-FORWARDS"
    "HISTORY-FORWARDS-QUERY" "COPY-TITLE" "DID-COMMIT-NAVIGATION"
    "COPY-URL" "ADD-OR-TRAVERSE-HISTORY" "SET-URL-NEW-BUFFER"
    "NOSCRIPT-MODE" "HELP" "JUMP-TO-HEADING" "NYXT-SEARCH-HINT"
    "BOOKMARK-CURRENT-URL" "NEW-BUFFER" "COMMAND-INSPECT"
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
  "Historical list of Nyxt commands.")

(define-test fuzzy-match ()
  (let ((source (make-instance 'prompter:raw-source)))
    (flet ((match (input list)
             (setf (prompter::current-input-downcase-p source)
                   (str:downcasep input))
             (prompter:value
              (first (sort (mapcar (lambda (suggestion)
                                     (prompter:fuzzy-match suggestion source input))
                                   (prompter::ensure-suggestions-list source list))
                           #'prompter:score>)))))
      (assert-string= "help"
                      (match "hel" '("help-mode" "help" "foo-help" "help-foo-bar")))
      ;; match 'help' with real suggestions list
      (assert-string= "HELP"
                      (match "hel" *suggestions*))
      ;; match 'swit buf' (small list)
      (assert-string= "switch-buffer"
                      (match "swit buf" '("about" "switch-buffer-next" "switch-buffer" "delete-buffer")))
      ;; match 'swit buf' with real suggestions list
      (assert-string= "SWITCH-BUFFER"
                      (match "swit buf" *suggestions*))
      ;; reverse match 'buf swit' (small list)
      (assert-string= "switch-buffer"
                      (match "buf swit" '("about" "switch-buffer-next" "switch-buffer" "delete-buffer")))
      ;; reverse match 'buf swit' with real suggestions list
      (assert-string= "SWITCH-BUFFER"
                      (match "buf swit" *suggestions*))
      ;; suggestions beginning with the first word appear first
      (assert-string= "delete-foo"
                      (match "de" '("some-mode" "delete-foo")))
      ;; search without a space. All characters count (small list).
      (assert-string= "foo-bar"
                      (match "foobar" '("foo-dash-bar" "foo-bar")))
      ;; search without a space. All characters count, real list.
      (assert-string= "SWITCH-BUFFER"
                      (match "sbf" *suggestions*))
      ;; input is uppercase (small list).
      (assert-string= "FOO-BAR"
                      (match "FOO" '("foo-dash-bar" "FOO-BAR")))
      ;; lowercase matches uppercase
      (assert-string= "FOO-BAR"
                      (match "foo" '("zzz" "FOO-BAR")))
      ;; match regex meta-characters
      (assert-string= "http://[1:0:0:2::3:0.]/"
                      (match "[" '("test1" "http://[1:0:0:2::3:0.]/" "test2"))))))
