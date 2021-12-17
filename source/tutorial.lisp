;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun tutorial-content ()
  (spinneret:with-html-string
   (:h2 "Core concepts")
   (:h3 "Keybindings and commands")
   (:p "Commands are invoked by pressing specific keys or from
the " (:code "execute-command") " menu (" (:code (binding-keys 'execute-command))
").")
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
       " slot of the " (:code "gtk-browser") " class.")

   (:h3 "Quickstart keys")
   (:ul
    (list-command-information '(set-url reload-current-buffer
                                set-url-new-buffer
                                switch-buffer-previous
                                nyxt/web-mode:history-backwards
                                nyxt/web-mode:history-forwards
                                nyxt/web-mode:follow-hint
                                nyxt/web-mode:follow-hint-new-buffer
                                quit execute-command describe-bindings)))

   (:h3 "Buffers")
   (:p "Nyxt uses the concept of buffers instead of tabs. Unlike tabs, buffers
are fully separated, each buffer having its own behavior and settings.")
   (:h3 "Modes")
   (:p "Each buffer has its own list of modes, ordered by priority.  A mode is a
set of functions, hooks, keybindings and other facilities that may modify the
behavior of a buffer.  For example, 'blocker-mode' can be used for domain-based
adblocking while 'no-script-mode' disables JavaScript.")
   (:p "Each buffer has separate instances of modes, which means that altering
the settings of a mode in a buffer does not impact other buffers.  Mode specific
functions/commands are only available when a mode is enabled for the current
buffer.")
   (:p "Each mode has an associated " (:i "mode toggler") " which is a command
of the same name that toggles the mode for the current buffer.")

   (:h3 "Prompt buffer")
   (:p "The prompt buffer is a menu that will appear when a command requests user
input. For example, when invoking the " (:code "set-url") " command, you must
supply the URL you would like to navigate to.  The prompt buffer can provide
suggestions.  The list of suggestions will automatically narrow down to those
matching your input as you type.")
   (:ul
    (:li  (command-markup 'nyxt/prompt-buffer-mode:return-selection
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Validate the selected suggestion(s) or the current input if there is
no suggestion.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:return-selection-over-action
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Query the user for an action to run over the selected suggestion(s)."))
   (:p " Some commands support multiple selections, for
instance " (:code "delete-buffer") " can delete all selected buffers at once.
When the input is changed and the suggestions are re-filtered, the selection is
not altered even if the marked elements don't show.")
   (:p "When at least one suggestion is marked, only the marked suggestions are processed
upon return.  The suggestion under the cursor is not processed if not marked.")
   (:ul
    (:li  (command-markup 'nyxt/prompt-buffer-mode:toggle-mark
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Select or deselect the current suggestion.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:mark-all
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Select all currently-displayed suggestions.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:unmark-all
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Deselect all currently-displayed suggestions.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:toggle-mark-all
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Toggle the mark of all currently-displayed suggestions.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:toggle-attributes-display
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Change which attributes are displayed in the suggestions list."))

   (:h3 "Message area")
   (:p "The message area represents a space (typically at the bottom of a
window) where Nyxt outputs messages back to you. To view the history of all
messages, invoke the command " (:code "list-messages") ".")

   (:h3 "Status area")
   (:p "The status area is where information about the state of that buffer is
printed (typically at the bottom of a window). By default, this includes the
active modes, the URL, and the title of the current buffer.")

   (:h2 "Basic controls")
   (:h3 "Moving within a buffer")
   (:p "To move within a buffer, several commands are provided:")
   (:ul
    (list-command-information '(nyxt/web-mode:scroll-down
                                nyxt/web-mode:scroll-up
                                nyxt/web-mode:scroll-page-down
                                nyxt/web-mode:scroll-page-up
                                nyxt/web-mode:scroll-to-bottom
                                nyxt/web-mode:scroll-to-top)))
   (:h3 "Setting the URL")
   (:p "When ambiguous URLs are inputted, Nyxt will attempt the best guess it
can. If you do not supply a protocol in a URL, HTTPS will be assumed. To
visit a site supporting only the less secure HTTP, you must explicitly type the
full URL including the 'http://' prefix.")
   (:ul
    (list-command-information '(set-url set-url-new-buffer make-buffer-focus)))
   (:h3 "Switching buffers")
   (:ul
    (list-command-information '(switch-buffer switch-buffer-next switch-buffer-previous)))
   (:h3 "Copy and paste")
   (:p "Unlike other web browsers, Nyxt provides powerful ways of copying
   and pasting content via different commands. Starting from:")
   (:ul
    (list-command-information '(nyxt/web-mode:copy nyxt/web-mode:paste)))
   (:p "Passing through webpage's data:")
   (:ul
    (list-command-information '(copy-url copy-title nyxt/web-mode:copy-placeholder nyxt/web-mode:copy-hint-url)))
   (:p "Leveraging password managers: ")
   (:ul 
    (list-command-information '(copy-username copy-password copy-password-prompt-details)))
   (:p "And more: ")
   (:ul
    (list-command-information '(nyxt/web-mode:paste-from-clipboard-ring show-system-information)))
   (:h3 "Link navigation")
   (:p "Link-hinting allows you to visit URLs on a page without using the mouse.
Invoke one of the commands below: several hints will appear on screen and all
links on the page will be listed in the prompt buffer.  You can select the hints
by matching against the hint, the URL or the title.")
   (:ul
    (list-command-information '(nyxt/web-mode:follow-hint
                                nyxt/web-mode:follow-hint-new-buffer-focus
                                nyxt/web-mode:follow-hint-new-buffer)))
   (:h3 "Using the buffer history")
   (:p "History is represented as a tree that you can traverse: when you go back
in history, then follow a new URL, it effectively creates a new branch without
deleting the old path. The tree makes sure you never lose track of where you've
been.")
   (:ul
    (list-command-information '(nyxt/web-mode:history-forwards
                                nyxt/web-mode:history-backwards
                                nyxt/web-mode:history-forwards-query
                                nyxt/web-mode:history-backwards-query
                                nyxt/web-mode:history-forwards-all-query
                                nyxt/web-mode:history-all-query)))
   (:p "You can also view a full tree of the history for a given buffer by
invoking the command 'buffer-history-tree'.")
   (:h3 "Searching")
   (:p "Nyxt can search a single buffer or multiple buffers at the same time.")
   (:p "You can view suggestions for search results in the prompt buffer in one
place rather than having to jump around in a buffer (or multiple buffers).")
   (:ul
    (list-command-information '(nyxt/web-mode:search-buffer
                                nyxt/web-mode:search-buffers
                                nyxt/web-mode:remove-search-hints)))
   (:h3 "Bookmarks")
   (:p "The bookmark file "
       (:code (let ((buffer (make-instance 'buffer)))
                (expand-path (bookmarks-path buffer))))
       " is made to be human readable and editable.
Bookmarks can have the following settings:")
   (:ul
    (:li (:code ":url") ": The URL of the bookmark.")
    (:li (:code ":title") ": The title of the bookmark.")
    (:li (:code ":tags") ": A list of strings.  Useful to categorize and filter bookmarks."))
   (:p "Bookmark-related commands")
   (:ul
    (list-command-information '(bookmark-current-url bookmark-buffer-url
                                bookmark-url nyxt/web-mode:bookmark-hint
                                set-url-from-bookmark delete-bookmark
                                list-bookmarks)))
   (:h3 "Application mode")
   (:p "The command " (:code "passthrough-mode") " forwards all keys to the
renderer. For instance, using the default binding of Nyxt (" (:code "web-cua-map") ") the
key-binding " (:code "C-i") " executes " (:code "autofill") ". Suppose
a user is using their email client which also uses " (:code "C-i") " for the italic command. Thus, after
executing " (:code "passthrough-mode") " the " (:code "C-i") " binding is associated
with the webpage's italic command instead of " (:code "autofill") ". Finally, the
user can return to their configuration just by executing " (:code "passthrough-mode") " again.")
   (:h3 "Enable, disable, and toggle multiple modes")
   (:p "The command " (:code "enable-mode") " allows the user to apply multiple
modes (such as " (:code "nosound-mode") " and " (:code "dark-mode") ") to
multiple buffers at once. Conversely, it is possible to revert this action by
executing "(:code "disable-mode") " while choosing exactly the same buffers and
modes previously selected. Finally, " (:code "toggle-mode") " also allows
activation and deactivation of multiple modes, but only for the current
buffer.")
   (:h3 "Light navigation")
   (:p "Reduce bandwidth usage via: ")
   (:ul
    (list-command-information '(nyxt/no-image-mode:no-image-mode
                                nyxt/no-script-mode:no-script-mode
                                nyxt/no-webgl-mode:no-webgl-mode)))
   (:p "It is possible to enable these three modes at once
   with: " (:code "reduce-bandwidth-mode") ".")
   (:h3 "Structural navigation")
   (:p "It is possible to navigate using the structure in between the file: ")
   (:ul
    (list-command-information '(nyxt/web-mode:jump-to-heading
                                nyxt/web-mode:jump-to-heading-buffers)))
   (:p "And navigate to interconnected files: ")
   (:ul
    (list-command-information '(nyxt/web-mode:go-next
                                nyxt/web-mode:go-previous
                                nyxt/web-mode:go-up
                                nyxt/web-mode:go-to-homepage)))
   (:h3 "Spelling check")
   (:p "Several commands are provided to spell check words. The default is
English but it is possible to change the slot for other languages:")
   (:ul
    (list-command-information '(nyxt/web-mode:spell-check-word
                                nyxt/web-mode:spell-check-word-at-cursor
                                nyxt/web-mode:spell-check-suggest-word
                                nyxt/web-mode:spell-check-highlighted-word
                                nyxt/web-mode:spell-check-list-languages
                                nyxt/web-mode:spell-check-text-input)))
   (:h3 "Visual mode")
   (:p "Select text without a mouse. Nyxt's "
       (:code "visual-mode") " imitates Vim's visual mode (and comes with the
CUA and Emacs-like keybindings out of the box, too). Activate it with the "
       (command-markup 'nyxt/visual-mode:visual-mode) " command.")
   (:p "Visual mode provides the following commands: ")
   (:ul
    (:li (command-markup 'nyxt/visual-mode:visual-mode
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Quit visual mode.")
    (:li (command-markup 'nyxt/visual-mode:select-paragraph
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:select-paragraph))
    (:li (command-markup 'nyxt/visual-mode:toggle-mark
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:toggle-mark))
    (:li (command-markup 'nyxt/visual-mode:forward-char
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:forward-char))
    (:li (command-markup 'nyxt/visual-mode:backward-char
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:backward-char))
    (:li (command-markup 'nyxt/visual-mode:forward-word
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:forward-word))
    (:li (command-markup 'nyxt/visual-mode:backward-word
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:backward-word))
    (:li (command-markup 'nyxt/visual-mode:forward-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:forward-line))
    (:li (command-markup 'nyxt/visual-mode:backward-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:backward-line))
    (:li (command-markup 'nyxt/visual-mode:beginning-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:beginning-line))
    (:li (command-markup 'nyxt/visual-mode:end-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:end-line))
    (:li (command-markup 'nyxt/visual-mode:forward-sentence
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:forward-sentence))
    (:li (command-markup 'nyxt/visual-mode:backward-sentence
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:backward-sentence)))
   (:p "Commands designed to ease the use for CUA users (but available to all users): ")
   (:ul
    (:li (command-markup 'nyxt/visual-mode:forward-char-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:forward-char-with-selection))
    (:li (command-markup 'nyxt/visual-mode:backward-char-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:backward-char-with-selection))
    (:li (command-markup 'nyxt/visual-mode:forward-line-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:forward-line-with-selection))
    (:li (command-markup 'nyxt/visual-mode:backward-line-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": " (command-docstring-first-sentence 'nyxt/visual-mode:backward-line-with-selection)))
   (:p "A note for " (:code "emacs-mode") " users: unlike in Emacs, in Nyxt the command "
       (command-markup 'nyxt/visual-mode:toggle-mark
                       :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
       " is bound to Shift-space, as C-space is bound to 'execute-command,
overriding any mode keybinding. If you want to toggle mark with C-space,
you'll need to set your own override-map such that C-space is not bound.
An example:")
   (:pre (:code "
\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"override-map\")))
                   (define-key map
                     \"M-x\" 'execute-command)))))"))

   (:h3 "Automation")
   (:p "Nyxt has many facilities for automation. For instance, it is possible to
automate the reading experience:")
   (:ul
    (list-command-information '(nyxt/cruise-control-mode:cruise-control-mode)))
   (:p "Symmetrically, it is possible to automate the filling of forms: ")
   (:ul
    (list-command-information '(autofill
                                nyxt/web-mode::toggle-checkboxes)))
   (:p "In addition, it is possible to automate actions over time: "
   (:ul
    (list-command-information '(nyxt/watch-mode:watch-mode))
    (:li (command-markup 'nyxt/repeat-mode:repeat-every) ": "
         (command-docstring-first-sentence 'nyxt/repeat-mode:repeat-every
                                           :sentence-case-p t))))
   (:p "Or even automate actions based on conditions: "
   (:ul
    (list-command-information '(nyxt/repeat-mode:repeat-mode
                                nyxt/preview-mode:preview-mode))))
   (:p "Nyxt also offers a no-code interface to build automation via Common Lisp
macros: ")
   (:ul
    (list-command-information '(nyxt/macro-edit-mode:edit-macro)))
   (:p "Lastly, the command " (:code 'nyxt/process-mode:process-mode) " must be
highlighted: ")
   (:ul
    (list-command-information '(nyxt/process-mode:process-mode)))
   (:p (:code 'nyxt/process-mode:process-mode) " is actually a building block
for other modes previously mentioned, such as " (:code
'nyxt/repeat-mode:repeat-mode) ". The extension relationship goes further, since
" (:code 'nyxt/cruise-control-mode:cruise-control-mode) " is in its turn an
extension and a composition of " (:code 'nyxt/repeat-mode:repeat-mode) " and "
(:code 'nyxt/web-mode:scroll-down) ". Further extensions and compositions can be
creatively tailor-made by users to automate their own use of Nyxt.")
   (:h3 "Miscellaneous")
   (:ul
    (list-command-information '(nyxt/web-mode:zoom-page
                                nyxt/web-mode:unzoom-page
                                nyxt/web-mode:reset-page-zoom
                                nyxt/web-mode::autofill
                                download-open-file
                                edit-with-external-editor)))

   (:h2 "The Nyxt help system")
   (:p "Nyxt provides introspective and help capabilities.  All commands,
classes, slots, variables, functions and bindings can be inspected for
definition and documentation.")
   (:ul
    (list-command-information '(help tutorial describe-key describe-bindings
                                describe-command describe-function
                                describe-variable describe-class
                                describe-slot describe-any)))
   (:p "A good starting point is to study the documentation of the classes "
       (:code "browser") ", " (:code "window") ", " (:code "buffer") " and "
       (:code "prompt-buffer") ".")))
