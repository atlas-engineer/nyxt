;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun tutorial-content ()
  (spinneret:with-html-string
    (:nsection :title "Core concepts"

      (:nsection :title "Keybindings and commands"
        (:p "Commands are invoked by pressing specific keys or from
the " (:nxref :command 'execute-command)
".")
        (:p "Keybindings are represented like this: 'C-x'. In this example, 'C' is a
shortcut for the modifier 'control', and 'x' represents the character 'x'. To
input the 'C-x' keybinding you would keep 'control' pressed and then hit 'x'.
Multiple key presses can be chained: in 'C-x C-s', you would have to press
'C-x', and then press 'C-s'.")
        (:p "Modifier keys legend:")
        (:ul
         (:li (:code "control") " (" (:code "C") "): Control key")
         (:li (:code "super") " (" (:code "S") "): Windows key, Command key")
         (:li (:code "meta") " (" (:code "M") "): Alt key, Option key")
         (:li (:code "shift") " (" (:code "s") "): Shift key"))
        (:p "Modifiers can be remapped, see the " (:code "modifier-translator")
            " slot of the " (:code "gtk-browser") " class."))

      (:nsection :title "Quickstart keys"
        (:ul
         (list-command-information '(set-url reload-current-buffer
                                     set-url-new-buffer
                                     switch-buffer-previous
                                     nyxt/mode/history:history-backwards
                                     nyxt/mode/history:history-forwards
                                     nyxt/mode/hint:follow-hint
                                     nyxt/mode/hint:follow-hint-new-buffer
                                     quit execute-command describe-bindings))))

      (:nsection :title "Buffers"
        (:p "Nyxt uses the concept of buffers instead of tabs. Unlike tabs, buffers
are fully separated, each buffer having its own behavior and settings."))

      (:nsection :title "Modes"
        (:p "Each buffer has its own list of modes, ordered by priority.  A mode is a
set of functions, hooks, keybindings and other facilities that may modify the
behavior of a buffer.  For example, 'blocker-mode' can be used for domain-based
ad-blocking while 'no-script-mode' disables JavaScript.")
        (:p "Each buffer has separate instances of modes, which means that altering
the settings of a mode in a buffer does not impact other buffers.  Mode specific
functions/commands are only available when a mode is enabled for the current
buffer.")
        (:p "Each mode has an associated " (:i "mode toggler") " which is a command
of the same name that toggles the mode for the current buffer."))

      (:nsection :title "Prompt buffer"
        (:p "The prompt buffer is a menu that will appear when a command requests user
input. For example, when invoking the " (:code "set-url") " command, you must
supply the URL you would like to navigate to.  The prompt buffer can provide
suggestions.  The list of suggestions will automatically narrow down to those
matching your input as you type.")
        (:ul
         (:li  (:nxref :command 'nyxt/mode/prompt-buffer:run-action-on-return
                 :mode 'nyxt/mode/prompt-buffer:prompt-buffer-mode)
               ": Validate the selected suggestion(s) or the current input if there is
no suggestion.")
         (:li  (:nxref :command 'nyxt/mode/prompt-buffer:set-action-on-return
                 :mode 'nyxt/mode/prompt-buffer:prompt-buffer-mode)
               ": Query the user for an action to run over the marked suggestion(s)."))
        (:p " Some commands support marks, for
instance " (:code "delete-buffer") " can delete all selected buffers at once.
When the input is changed and the suggestions are re-filtered, the marks are
not altered even if the marked suggestions aren't visible.")
        (:p "When at least one suggestion is marked, only the marked suggestions are processed
upon return.  The suggestion under the cursor is not processed if not marked.")
        (:ul
         (:li  (:nxref :command 'nyxt/mode/prompt-buffer:toggle-mark-forwards
                 :mode 'nyxt/mode/prompt-buffer:prompt-buffer-mode)
               ": Select or deselect the current suggestion.")
         (:li  (:nxref :command 'nyxt/mode/prompt-buffer:mark-all
                 :mode 'nyxt/mode/prompt-buffer:prompt-buffer-mode)
               ": Select all currently-displayed suggestions.")
         (:li  (:nxref :command 'nyxt/mode/prompt-buffer:unmark-all
                 :mode 'nyxt/mode/prompt-buffer:prompt-buffer-mode)
               ": Deselect all currently-displayed suggestions.")
         (:li  (:nxref :command 'nyxt/mode/prompt-buffer:toggle-mark-all
                 :mode 'nyxt/mode/prompt-buffer:prompt-buffer-mode)
               ": Toggle the mark of all currently-displayed suggestions.")
         (:li  (:nxref :command 'nyxt/mode/prompt-buffer:toggle-attributes-display
                 :mode 'nyxt/mode/prompt-buffer:prompt-buffer-mode)
               ": Change which attributes are displayed in the suggestions list.")))

      (:nsection :title "Message area"
        (:p "The message area represents a space (typically at the bottom of a
window) where Nyxt outputs messages back to you. To view the history of all
messages, invoke the command " (:nxref :command 'nyxt/mode/message:list-messages) "."))

      (:nsection :title "Status buffer"
        (:p "The status buffer is where information about the state of that buffer is
printed (typically at the bottom of a window). By default, this includes the
active modes, the URL, and the title of the current buffer.")))

    (:nsection :title "Basic controls"

      (:nsection :title "Moving within a buffer"
        (:p "To move within a buffer, several commands are provided:")
        (:ul
         (list-command-information '(nyxt/mode/document:scroll-down
                                     nyxt/mode/document:scroll-up
                                     nyxt/mode/document:scroll-page-down
                                     nyxt/mode/document:scroll-page-up
                                     nyxt/mode/document:scroll-to-bottom
                                     nyxt/mode/document:scroll-to-top))))

      (:nsection :title "Setting the URL"
        (:p "When ambiguous URLs are inputted, Nyxt will attempt the best guess it
can. If you do not supply a protocol in a URL, HTTPS will be assumed. To
visit a site supporting only the less secure HTTP, you must explicitly type the
full URL including the 'http://' prefix.")
        (:ul
         (list-command-information '(set-url set-url-new-buffer make-buffer-focus))))

      (:nsection :title "Switching buffers"
        (:ul
         (list-command-information '(switch-buffer switch-buffer-next switch-buffer-previous))))

      (:nsection :title "Copy and paste"
        (:p "Unlike other web browsers, Nyxt provides powerful ways of copying
   and pasting content via different commands. Starting from:")
        (:ul
         (list-command-information '(nyxt/mode/document:copy nyxt/mode/document:paste)))
        (:p "Passing through webpage's data:")
        (:ul
         (list-command-information '(copy-url copy-title nyxt/mode/document:copy-placeholder nyxt/mode/hint:copy-hint-url)))
        (:p "Leveraging password managers: ")
        (:ul
         (list-command-information '(nyxt/mode/password:copy-username nyxt/mode/password:copy-password nyxt/mode/password:copy-password-prompt-details)))
        (:p "And more: ")
        (:ul
         (list-command-information '(nyxt/mode/document:paste-from-clipboard-ring show-system-information))))

      (:nsection :title "Link navigation"
        (:p "Link-hinting allows you to visit URLs on a page without using the mouse.
Invoke one of the commands below: several hints will appear on screen and all
links on the page will be listed in the prompt buffer.  You can select the hints
by matching against the hint, the URL or the title.")
        (:ul
         (list-command-information '(nyxt/mode/hint:follow-hint
                                     nyxt/mode/hint:follow-hint-new-buffer-focus
                                     nyxt/mode/hint:follow-hint-new-buffer))))

      (:nsection :title "Using the buffer history"
        (:p "History is represented as a tree that you can traverse: when you go back
in history, then follow a new URL, it effectively creates a new branch without
deleting the old path. The tree makes sure you never lose track of where you've
been.")
        (:ul
         (list-command-information '(nyxt/mode/history:history-forwards
                                     nyxt/mode/history:history-backwards
                                     nyxt/mode/history:history-forwards-query
                                     nyxt/mode/history:history-backwards-query
                                     nyxt/mode/history:history-forwards-all-query
                                     nyxt/mode/history:history-all-query)))
        (:p "You can also view a full tree of the history for a given buffer by
invoking the command 'buffer-history-tree'."))

      (:nsection :title "Incremental Search"
        (:p "Nyxt's search is incremental, i.e. it begins as soon as you type
the first character of the search string.  A single or multiple buffers can be
queried, and all results are displayed in the prompt buffer.")
        (:p "This makes it easy to interact with results found in different URLs
from a unified interface.")
        (:ul
         (list-command-information '(nyxt/mode/search-buffer:search-buffer
                                     nyxt/mode/search-buffer:search-buffers
                                     nyxt/mode/search-buffer:remove-search-marks))))

      (:nsection :title "Bookmarks"
        (:p "The bookmark file "
            (:code (let ((mode (make-instance 'nyxt/mode/bookmark:bookmark-mode)))
                     (files:expand (nyxt/mode/bookmark:bookmarks-file mode))))
            " is made to be human readable and editable.
Bookmarks can have the following settings:")
        (:ul
         (:li (:code ":url") ": The URL of the bookmark.")
         (:li (:code ":title") ": The title of the bookmark.")
         (:li (:code ":tags") ": A list of strings.  Useful to categorize and filter bookmarks."))
        (:p "Bookmark-related commands")
        (:ul
         (list-command-information '(nyxt/mode/bookmark:bookmark-current-url nyxt/mode/bookmark:bookmark-buffer-url
                                     nyxt/mode/bookmark:bookmark-url nyxt/mode/bookmark:bookmark-hint
                                     nyxt/mode/bookmark:set-url-from-bookmark nyxt/mode/bookmark:delete-bookmark
                                     nyxt/mode/bookmark:list-bookmarks
                                     nyxt/mode/bookmark:import-bookmarks-from-html
                                     nyxt/mode/bookmark-frequent-visits:bookmark-frequent-visits-mode))))

      (:nsection :title "Annotations"
        (:p "Annotations can have the following settings:")
        (:ul
         (:li (:nxref :slot 'nyxt/mode/annotate:snippet
                :class-name 'nyxt/mode/annotate:snippet-annotation)
              ": The snippet which was highlighted by the user.")
         (:li (:nxref :slot 'nyxt/mode/annotate::url
                :class-name 'nyxt/mode/annotate:url-annotation)
              ": The URL of the annotation.")
         (:li (:nxref :slot 'nyxt/mode/annotate:page-title
                :class-name 'nyxt/mode/annotate:url-annotation)
              ": The title of the annotation.")
         (:li (:nxref :slot 'nyxt/mode/annotate::data
                :class-name 'nyxt/mode/annotate:annotation)
              ": The comment about the highlighted snippet or
the URL.")
         (:li (:nxref :slot 'nyxt/mode/annotate:tags
                :class-name 'nyxt/mode/annotate:annotation)
              ": A list of strings.  Useful to categorize and filter annotations."))
        (:p "Annotate-related commands")
        (:ul
         (list-command-information '(nyxt/mode/annotate:annotate-current-url nyxt/mode/annotate:annotate-highlighted-text
                                     nyxt/mode/annotate:show-annotation nyxt/mode/annotate:show-annotations nyxt/mode/annotate:show-annotations-for-current-url))))

      (:nsection :title "Passthrough mode"
        (:p "The command " (:code "passthrough-mode") " forwards all keys to the
renderer. For instance, using the default binding of Nyxt (" (:code "web-cua-map") ") the
keybinding " (:code "C-i") " executes " (:code "autofill") ". Suppose
a user is using their email client which also uses " (:code "C-i") " for the italic command. Thus, after
executing " (:code "passthrough-mode") " the " (:code "C-i") " binding is associated
with the webpage's italic command instead of " (:code "autofill") ". Finally, the
user can return to their configuration just by executing " (:code "passthrough-mode") " again."))

      (:nsection :title "Enable, disable, and toggle multiple modes"
        (:p "The command " (:nxref :command 'enable-modes) " allows the user to apply multiple
modes (such as " (:code "nosound-mode") " and " (:code "dark-mode") ") to
multiple buffers at once. Conversely, it is possible to revert this action by
executing " (:nxref :command 'disable-modes) " while choosing exactly the same buffers and
modes previously selected. Finally, " (:code "toggle-mode") " also allows
activation and deactivation of multiple modes, but only for the current
buffer."))

      (:nsection :title "Light navigation"
        (:p "Reduce bandwidth usage via: ")
        (:ul
         (list-command-information '(nyxt/mode/no-image:no-image-mode
                                     nyxt/mode/no-script:no-script-mode
                                     nyxt/mode/no-webgl:no-webgl-mode)))
        (:p "It is possible to enable these three modes at once
   with: " (:code "reduce-bandwidth-mode") "."))

      (:nsection :title "Structural navigation"
        (:p "It is possible to navigate using the structure in between the file: ")
        (:ul
         (list-command-information '(nyxt/mode/document:jump-to-heading
                                     nyxt/mode/document:previous-heading
                                     nyxt/mode/document:next-heading
                                     nyxt/mode/document:jump-to-heading-buffers)))
        (:p "And navigate to interconnected files: ")
        (:ul
         (list-command-information '(nyxt/mode/document:go-next
                                     nyxt/mode/document:go-previous
                                     nyxt/mode/document:go-up
                                     nyxt/mode/document:go-to-homepage))))

      (:nsection :title "Spelling check"
        (:p "Several commands are provided to spell check words. The default is
English but it is possible to change the slot "
            (:nxref :slot 'nyxt/mode/spell-check:spell-check-language :class-name 'nyxt/mode/spell-check:spell-check-mode)
            " for other languages:")
        (:ul
         (list-command-information '(nyxt/mode/spell-check:spell-check-word
                                     nyxt/mode/spell-check:spell-check-word-at-cursor
                                     nyxt/mode/spell-check:spell-check-suggest-word
                                     nyxt/mode/spell-check:spell-check-highlighted-word
                                     nyxt/mode/spell-check:spell-check-list-languages
                                     nyxt/mode/spell-check:spell-check-text-input))))

      (:nsection :title "Visual mode"
        (:p "Select text without a mouse. Nyxt's "
            (:code "visual-mode") " imitates Vim's visual mode (and comes with the
CUA and Emacs-like keybindings out of the box, too). Activate it with the "
            (:nxref :command 'nyxt/mode/visual:visual-mode) " command.")
        (:p "Visual mode provides the following commands: ")
        (:ul
         (:li (:nxref :command 'nyxt/mode/visual:visual-mode
                :mode 'nyxt/mode/visual:visual-mode)
              ": Quit visual mode.")
         (:li (:nxref :command 'nyxt/mode/visual:select-paragraph
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:select-paragraph))
         (:li (:nxref :command 'nyxt/mode/visual:toggle-mark
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:toggle-mark))
         (:li (:nxref :command 'nyxt/mode/visual:forward-char
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:forward-char))
         (:li (:nxref :command 'nyxt/mode/visual:backward-char
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:backward-char))
         (:li (:nxref :command 'nyxt/mode/visual:forward-word
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:forward-word))
         (:li (:nxref :command 'nyxt/mode/visual:backward-word
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:backward-word))
         (:li (:nxref :command 'nyxt/mode/visual:forward-line
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:forward-line))
         (:li (:nxref :command 'nyxt/mode/visual:backward-line
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:backward-line))
         (:li (:nxref :command 'nyxt/mode/visual:beginning-line
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:beginning-line))
         (:li (:nxref :command 'nyxt/mode/visual:end-line
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:end-line))
         (:li (:nxref :command 'nyxt/mode/visual:forward-sentence
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:forward-sentence))
         (:li (:nxref :command 'nyxt/mode/visual:backward-sentence
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:backward-sentence)))
        (:p "Commands designed to ease the use for CUA users (but available to all users): ")
        (:ul
         (:li (:nxref :command 'nyxt/mode/visual:forward-char-with-selection
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:forward-char-with-selection))
         (:li (:nxref :command 'nyxt/mode/visual:backward-char-with-selection
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:backward-char-with-selection))
         (:li (:nxref :command 'nyxt/mode/visual:forward-line-with-selection
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:forward-line-with-selection))
         (:li (:nxref :command 'nyxt/mode/visual:backward-line-with-selection
                :mode 'nyxt/mode/visual:visual-mode)
              ": " (command-docstring-first-sentence 'nyxt/mode/visual:backward-line-with-selection)))
        (:p "A note for " (:code "emacs-mode") " users: unlike in Emacs, in Nyxt the command "
            (:nxref :command 'nyxt/mode/visual:toggle-mark
              :mode 'nyxt/mode/visual:visual-mode)
            " is bound to Shift-space, as C-space is bound to 'execute-command,
overriding any mode keybinding. If you want to toggle mark with C-space,
you'll need to set your own override-map such that C-space is not bound.
An example:")
        (:ncode
          '(define-configuration input-buffer
            ((override-map (let ((map (make-keymap "override-map")))
                             (define-key map
                               "M-x" 'execute-command)))))))

      (:nsection :title "Automation"
        (:p "Nyxt has many facilities for automation. For instance, it is possible to
automate the reading experience:")
        (:ul
         (list-command-information '(nyxt/mode/cruise-control:cruise-control-mode)))
        (:p "Symmetrically, it is possible to automate the filling of forms: ")
        (:ul
         (list-command-information '(nyxt/mode/autofill:autofill
                                     nyxt/mode/bookmarklets::toggle-checkboxes)))
        (:p "In addition, it is possible to automate actions over time: "
            (:ul
             (list-command-information '(nyxt/mode/watch:watch-mode))
             (:li (:nxref :command 'nyxt/mode/repeat:repeat-every) ": "
                  (command-docstring-first-sentence 'nyxt/mode/repeat:repeat-every
                                                    :sentence-case-p t))))
        (:p "Or even automate actions based on conditions: "
            (:ul
             (list-command-information '(nyxt/mode/repeat:repeat-mode
                                         nyxt/mode/preview:preview-mode))))
        (:p "Nyxt also offers a no-code interface to build automation via Common Lisp
macros: ")
        (:ul
         (list-command-information '(nyxt/mode/macro-edit:edit-macro)))
        (:p "Lastly, the  " (:nxref :mode 'nyxt/mode/process:process-mode) " must be highlighted: ")
        (:p (:nxref :mode 'nyxt/mode/process:process-mode) " is actually a building block
for other modes previously mentioned, such as " (:nxref :mode 'nyxt/mode/repeat:repeat-mode) ".
The extension relationship goes further, since
" (:nxref :mode 'nyxt/mode/cruise-control:cruise-control-mode) " is in its turn an
extension and a composition of " (:nxref :mode 'nyxt/mode/repeat:repeat-mode) " and "
(:nxref :command 'nyxt/mode/document:scroll-down) ". Further extensions and compositions can be
creatively tailor-made by users to automate their own use of Nyxt."))

      (:nsection :title "Miscellaneous"
        (:ul
         (list-command-information '(nyxt/mode/document:zoom-page
                                     nyxt/mode/document:unzoom-page
                                     nyxt/mode/document:reset-page-zoom
                                     nyxt/mode/autofill::autofill
                                     nyxt/mode/file-manager:download-open-file
                                     edit-with-external-editor)))))

    (:nsection :title "The Nyxt help system"
      (:p "Nyxt provides introspective and help capabilities.  All commands,
classes, slots, variables, functions and bindings can be inspected for
definition and documentation.")
      (:ul
       (list-command-information '(tutorial describe-key describe-bindings
                                   describe-command describe-function
                                   describe-variable describe-class
                                   describe-slot describe-any)))
      (:p "A good starting point is to study the documentation of the classes "
          (:code "browser") ", " (:code "window") ", " (:code "buffer") " and "
          (:code "prompt-buffer") "."))))

(defun quick-start-exit (panel-buffer)
  "Close tutorial panel buffer and set the main buffer to `default-new-buffer-url'"
  (delete-panel-buffer :panels panel-buffer)
  (alex:if-let ((default-new (find (default-new-buffer-url *browser*) (buffer-list)
                                   :test #'quri:uri= :key #'url)))
    (set-current-buffer default-new)
    (make-buffer-focus)))

(define-panel-command-global quick-start (&key (page 0))
    (panel "*Quick Start*" :left)
  "Display Nyxt quick start tutorial."
  ;; FIXME Maybe add a width parameter to `define-panel-*'?
  (setf (ffi-width panel) 350)
  (spinneret:with-html-string
    (let* ((titles '("Quick Start" "Buffers" "Commands"
                     "Basic Navigation" "Modes" "Well Done"))
           (len-titles (length titles)))
      (case page
        (0
         (:h2 (nth page titles))
         (:p "This " (:b "quick start")
             "presents you the key concepts to be effective at extracting
information from the Internet with Nyxt.")
         (:div :style "height: 20px;")
         (:hr)
         (:div :style "height: 20px;")
         (:p :style "text-align:center;" "Table of Contents")
         (:ol
          (loop for title in (rest titles) and page from 1
                do (:li (:a :href (nyxt-url 'quick-start :page page) title))))
         (:div :style "height: 20px;"))
        (1
         (:h2 (nth page titles))
         (:p "The " (:b "buffer")
             "is the fundamental unit of information in Nyxt.")
         (:p "While " (:b "buffers")
             "are similar to browser tabs, they can be navigated and organized
in more complex ways such as filtering, or grouping by URLs, titles, contexts,
tags, keywords, and bookmarks."))
        (2
         (:h2 (nth page titles))
         (:p "Commands are invocations that trigger certain actions.  For
example, mouse clicks trigger commands.")
         (:p "All commands in Nyxt have a " (:b "name")
             ", and some have an associated key binding (also known as keyboard
shortcuts).")
         (:p "Commands can be called by name from the "
             (:b "Execute-Command Menu") ":")
         (:li (:nxref :command 'nyxt:execute-command) " to open;")
         (:li (:nxref :command 'nyxt/mode/prompt-buffer:quit-prompt-buffer) " to close.")
         (:p "This menu is called the prompt buffer and most of the interaction
between you and Nyxt goes through it.")
         (:hr)
         (:p "Some other commands to try:")
         (:li (:nxref :command 'nyxt:reload-current-buffer) " reloads the main buffer;")
         (:li (:nxref :command 'nyxt:make-buffer-focus) " opens a new one."))
        (3
         (:h2 (nth page titles))
         (:p (:nxref :command 'nyxt:set-url) " invokes the address bar.")
         (:p (:nxref :command 'nyxt:set-url-new-buffer) " as above, but creates a new buffer.")
         (:p (:nxref :command 'nyxt:switch-buffer) " to show a list of buffers.")
         (:p (:nxref :command 'nyxt:delete-current-buffer) " to close a buffer.")
         (:hr)
         (:p "Notice that most of these commands invoke the prompt
buffer. Every time that a command request input from the user, this menu will
appear.")
         (:p "Each command can either be called by name or by its corresponding keyboard
shortcut."))
        (4
         (:h2 (nth page titles))
         (:p "Separate tasks are best handled with separate settings.")
         (:p (:b "Modes") " are toggled [on/off] for different settings in each
         buffer.")
         (:hr)
         (:p "Example:")
         (:p "To enjoy the image-less Web, you can enable "
             (:nxref :mode 'nyxt/mode/no-image:no-image-mode)
             " by invoking the command "
             (:nxref :command 'nyxt/mode/no-image:no-image-mode) ".")
         (:p "Again, you can access this command, and all others via "
             (:nxref :command 'nyxt:execute-command) ".")
         (:hr)
         (:small "A list of all modes (including those currently enabled) is
available by invoking the " (:nxref :command 'nyxt:toggle-modes)
" command."))
        (5
         (:h2 (nth page titles))
         (:p "You are now licensed to build, destroy, and enjoy Nyxt. Where you
go from here is up to you.")
         (:div :style "height: 20px;")
         (:div :style "text-align: center;"
               (:nbutton
                 :title "Visit default new buffer"
                 :text "Start using Nyxt"
                 :buffer panel
                 `(quick-start-exit ,panel))
               (:div :style "height: 20px;")
               (:p (:i "Happy Hacking!"))
               (:div :style "height: 20px;"))
         (:hr)
         (:small "Find further information by issuing commands that start with
describe, such as "
                 (:nxref :command 'describe-bindings) " or "
                 (:nxref :command 'describe-any) ".")))
      (:hr)
      ;; FIXME There must be a cleaner way to do it.
      (:div
       :style "position: fixed; bottom: 4px; width: 80%;  margin: 4px 7%;"
       :class "progress-bar-container"
       (:div :class "progress-bar-base"
             (:div
              :class "progress-bar-fill"
              :style (format nil "width: ~,1f%;" (* 100 (/ (1+ page) len-titles)))))
       (:a.button
        :style "line-height: 12px; text-decoration: none; position: fixed; bottom: 4px; left: 4px;"
        :href (nyxt-url 'quick-start :page (max (1- page) 0))
        "⬅")
       (:a.button
        :style "line-height: 12px; text-decoration: none; position: fixed; bottom: 4px; right: 4px;"
        :href (nyxt-url 'quick-start :page (min (1+ page) (1- len-titles)))
        "➡")))))
