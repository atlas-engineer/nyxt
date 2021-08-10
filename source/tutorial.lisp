;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun tutorial-content ()
  (spinneret:with-html-string
   (:h2 "Core concepts")
   (:h3 "Key-bindings and commands")
   (:p "Commands are invoked by pressing specific keys or from
the " (:code "execute-command") " menu (" (:code (binding-keys 'execute-command))
").")
   (:p "Key-sequences used for key-bindings are represented like this: 'C-x'.~%
Here, 'C' is a
shortcut for the modifier 'control', and 'x' is the character 'x'.~%To
input 'C-x', keep 'control' pressed and then hit 'x'.~%
Chain distinct key-sequences, you madlad. With 'C-x C-s', 'C-x', is pressed first,
and then 'C-s'.")
   (:p "Modifier-keys legend:")
   (:ul
    (:li (:code "control") " (" (:code "C") "): Control key")
    (:li (:code "super") " (" (:code "S") "): Super key, Command key")
    (:li (:code "meta") " (" (:code "M") "): Alt key, Option key")
    (:li (:code "shift") " (" (:code "s") "): Shift key"))
   (:p "Modifiers can be remapped. Read the " (:code "modifier-translator")
       " slot of the " (:code "gtk-browser") " class.")

   (:h3 "Quickstart keys")
   (:ul
    (:li (:code (binding-keys 'set-url)) ": Load a URL")
    (:li (:code (binding-keys 'reload-current-buffer)) ": Reload a buffer")
    (:li (:code (binding-keys 'set-url-new-buffer)) ": Load a URL in a new buffer")
    (:li (:code (binding-keys 'switch-buffer-previous)) ", " (:code (binding-keys 'switch-buffer-next)) ": Switch buffer")
    (:li (:code (binding-keys 'nyxt/web-mode:history-backwards)) ": Backwards history")
    (:li (:code (binding-keys 'nyxt/web-mode:history-forwards)) ": Forwards history")
    (:li (:code (binding-keys 'nyxt/web-mode:follow-hint)) ": Follow a link in the current buffer")
    (:li (:code (binding-keys 'nyxt/web-mode:follow-hint-new-buffer)) ": Follow a link in a new buffer")
    (:li (:code (binding-keys 'quit)) ": Quit")
    (:li (:code (binding-keys 'execute-command)) ": Run a command by name")
    (:li (:code (binding-keys 'describe-bindings)) ": List all bindings for the current buffer"))

   (:h3 "Buffers")
   (:p "Buffers are instead of tabs. Unlike tabs, buffers
are fully separated, each having its own behavior and settings.")
   (:h3 "Modes")
   (:p "Each buffer has its own list of modes, ordered by priority.~%A mode is a
set of functions, hooks, keybindings and other facilities that may modify the
behaviour of a buffer.~%For example, 'blocker-mode' can be used for domain-based
ad-blocking while 'noscript-mode' disables JavaScript.")
   (:p "Each buffer has separate instances of modes, which means altering
the settings of a mode in a buffer does not impact other buffers.~%Mode specific
functions/commands are only available when a mode is enabled for the current
buffer.")
   (:p "Each mode has an associated " (:i "mode toggler") " which is a command
of the same name that toggles the mode for the current buffer.")

   (:h3 "Prompt buffer")
   (:p "The prompt buffer is a menu appearing when a command requests user
input.~%Supply a desired URL when using the " (:code "set-url") " command.~%
The prompt buffer can provide suggestions.~%The list of suggestions are narrowed down
to those matching your input (as you type).")
   (:ul
    (:li  (command-markup 'nyxt/prompt-buffer-mode:return-selection
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Validate the selected suggestion(s) or the current input if there is
no suggestion.")
    (:li  (command-markup 'nyxt/prompt-buffer-mode:return-selection-over-action
                          :modes (list (make-instance 'nyxt/prompt-buffer-mode:prompt-buffer-mode)))
          ": Query the user for an action to run over the selected suggestion(s)."))
   (:p " Some commands support multiple selections; " (:code "delete-buffer") " can delete all
selected buffers at once.~%When the input is changed and the suggestions are re-filtered, the
selection is not altered, even if the marked elements don't show.")
   (:p "When at least one suggestion is marked, only the marked suggestions are processed
upon return.~%Unless marked, the suggestion under the cursor is not processed.")
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
          ": Change which attributes are displayed in the suggestion list."))

   (:h3 "Message area")
   (:p "Messages are displayed in a area (typically at the bottom of a
window).~%Get the history of all messages with the " (:code "list-messages") ".
command.")

   (:h3 "Status area")
   (:p "The status area (typically at the bottom of a window) displays buffer state.~%
By default this includes the active modes, URL, and title of the current buffer.")

   (:h2 "Basic controls")
   (:h3 "Moving within a buffer")
   (:p "Commands to navigate buffers:")
   (:ul
    (:li (command-markup 'nyxt/web-mode:scroll-down) ": Move down.")
    (:li (command-markup 'nyxt/web-mode:scroll-up) ": Move up.")
    (:li (command-markup 'nyxt/web-mode:scroll-to-bottom) ": Jump to the bottom of a page.")
    (:li (command-markup 'nyxt/web-mode:scroll-to-top) ": Jump to the top of a page."))
   (:h3 "Setting the URL")
   (:p "Ambiguous URL input is taken at best-guess basis.~%Without a protocol in a URL,
HTTPS is assumed.~%To
visit a site supporting only the less secure HTTP, you must explicitly type the
full URL including the 'http://' prefix.")
   (:ul
    (:li (command-markup 'set-url) ": Sets the URL of the current buffer.")
    (:li (command-markup 'set-url-new-buffer) ": Opens a new buffer and set its URL.")
    (:li (command-markup 'make-buffer-focus) ": Makes a new empty buffer."))
   (:h3 "Switching buffers")
   (:ul
    (:li (command-markup 'switch-buffer) ": Switch buffer using fuzzy completion
to quickly find your desired buffer.")
    (:li (command-markup 'switch-buffer-next) ": Go to the next buffer.")
    (:li (command-markup 'switch-buffer-previous) ": Go to the previous buffer."))
   (:h3 "Copy and paste")
   (:p "Harness powerful ways of copying and pasting content via different commands. Starting from:")
   (:ul
    (:li (command-markup 'nyxt/web-mode:copy) ": " (command-docstring-first-sentence 'nyxt/web-mode:copy))
    (:li (command-markup 'nyxt/web-mode:paste) ": " (command-docstring-first-sentence 'nyxt/web-mode:paste)))
   (:p "Passing through a webpage's data:")
   (:ul
    (:li (command-markup 'copy-url) ": " (command-docstring-first-sentence 'copy-url))
    (:li (command-markup 'copy-title) ": " (command-docstring-first-sentence 'copy-title))
    (:li (command-markup 'nyxt/web-mode:copy-placeholder) ": " (command-docstring-first-sentence 'nyxt/web-mode:copy-placeholder))
    (:li (command-markup 'nyxt/web-mode:copy-hint-url) ": " (command-docstring-first-sentence 'nyxt/web-mode:copy-hint-url)))
   (:p "Leveraging password-managers: ")
   (:ul 
    (:li (command-markup 'copy-username) ": " (command-docstring-first-sentence 'copy-username))
    (:li (command-markup 'copy-password) ": " (command-docstring-first-sentence 'copy-password))
    (:li (command-markup 'copy-password-prompt-details) ": " (command-docstring-first-sentence 'copy-password-prompt-details)))
   (:p "And more: ")
   (:ul
    (:li (command-markup 'nyxt/web-mode:paste-from-clipboard-ring) ": " (command-docstring-first-sentence 'nyxt/web-mode:paste-from-clipboard-ring))
    (:li (command-markup 'copy-system-information) ": " (command-docstring-first-sentence 'copy-system-information)))
   (:h3 "Link navigation")
   (:p "Link-hinting allows visiting URLs on a page without mousing.~%
Invoke one of the commands below: Several hints will appear on screen and all
links on the page will be listed in the prompt buffer.~%Select the hints
by matching against the hint, URL, or title.")
   (:ul
    (:li (command-markup 'nyxt/web-mode:follow-hint) ": Go to link in current buffer.")
    (:li (command-markup 'nyxt/web-mode:follow-hint-new-buffer-focus) ": Create new buffer with link; focus on new buffer.")
    (:li (command-markup 'nyxt/web-mode:follow-hint-new-buffer) ": Create new buffer with link; keep focus on current buffer."))
   (:h3 "Using the buffer history")
   (:p "History is represented as a traversable tree: When you go back
in history, then follow a new URL, it effectively creates a new branch without
deleting the old path.~%The tree ensures you never lose track of where you've
been.")
   (:ul
    (:li (command-markup 'nyxt/web-mode:history-forwards) ": History forwards.")
    (:li (command-markup 'nyxt/web-mode:history-backwards) ": History backwards.")
    (:li (command-markup 'nyxt/web-mode:history-forwards-query) ": History forwards query to any following location on the branch.")
    (:li (command-markup 'nyxt/web-mode:history-backwards-query) ": History backwards query to any previous location.")
    (:li (command-markup 'nyxt/web-mode:history-forwards-all-query) ": History forwards query to any following location on all branches.")
    (:li (command-markup 'nyxt/web-mode:history-all-query) ": History all query, jump to any history entry."))
   (:p "Get a full tree of the history for a given buffer with the 'buffer-history-tree' command.")
   (:h3 "Searching")
   (:p "Search a single buffer or multiple buffers at the same time.")
   (:p "Get suggestions for search results in the prompt buffer in one
place rather than having to jump around in one or more buffers.")
   (:ul
    (:li (command-markup 'nyxt/web-mode:search-buffer) ": Search buffer.")
    (:li (command-markup 'nyxt/web-mode:search-buffers) ": Search multiple buffers.")
    (:li (command-markup 'nyxt/web-mode:remove-search-hints) ": Remove the highlighting around the search hits."))
   (:h3 "Bookmarks")
   (:p "The bookmark file "
       (:code (let ((buffer (make-instance 'buffer)))
                (expand-path (bookmarks-path buffer))))
       " is made to be human-readable and editable.~%
Available bookmark settings:")
   (:ul
    (:li (:code ":url") ": The URL of the bookmark.")
    (:li (:code ":title") ": The title of the bookmark.")
    (:li (:code ":tags") ": A list of strings. Useful to categorize and filter bookmarks."))
   (:p "Bookmark-related commands")
   (:ul
    (:li (command-markup 'bookmark-current-url) ": Bookmark the current page.
Prompt for tags.~%The input defaults to the existing tags: Removing tags 
from the input also removes them from the existing bookmark.")
    (:li (command-markup 'bookmark-buffer-url) ": Same as above but prompt for a buffer first.")
    (:li (command-markup 'bookmark-url) ": Same as above but prompt for a URL first.")
    (:li (command-markup 'nyxt/web-mode:bookmark-hint) ": Same as above but prompt for a hinted URL first.")
    (:li (command-markup 'set-url-from-bookmark) ": Opens a bookmark in current buffer.")
    (:li (command-markup 'delete-bookmark) ": Delete queried bookmarks.")
    (:li (command-markup 'list-bookmarks) ": Display a new buffer containing the
list of all bookmarks."))
   (:h3 "Application mode")
   (:p "The command " (:code "application-mode") " forwards all keys to the
renderer.~%When using the default binding of Nyxt (" (:code "web-cua-map") "), the
key-binding " (:code "C-i") " executes " (:code "autofill") ".~%If using an e-mail client
which also uses " (:code "C-i") " for the italic command, after
executing " (:code "application-mode") " the " (:code "C-i") " binding is associated
with the webpage's italic command instead of " (:code "autofill") ".~%Return to the
configuration by executing " (:code "application-mode") " again.")
   (:h3 "Enable, disable, and toggle multiple modes")
   (:p "The command " (:code "enable-mode") " allows applying multiple
modes (such as " (:code "nosound-mode") " and " (:code "dark-mode") ") to
multiple buffers at once.~%Revertable by executing "(:code "disable-mode") "
while choosing exactly the same buffers and modes previously selected.~%
" (:code "toggle-mode") " also allows activation and deactivation of multiple modes,
but only for the current buffer.")
   (:h3 "Visual mode")
   (:p "Select text without a mouse. Turning on Nyxt's " (:code "visual-mode") "
(with the " (command-markup 'nyxt/visual-mode:visual-mode) " command) imitates that
of Vim's (and comes with the CUA and Emacs-like keybindings out of the box, too).")
   (:p "Available visual-mode commands: ")
   (:ul
    (:li (command-markup 'nyxt/visual-mode:visual-mode
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Quit visual mode.")
    (:li (command-markup 'nyxt/visual-mode:select-paragraph
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Opens a prompt buffer to select a paragraph to set the caret on.")
    (:li (command-markup 'nyxt/visual-mode:toggle-mark
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Toggle text selection.")
    (:li (command-markup 'nyxt/visual-mode:forward-char
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret (or a selection of text) one character forward.")
    (:li (command-markup 'nyxt/visual-mode:backward-char
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret (or a selection of text) one character backward.")
    (:li (command-markup 'nyxt/visual-mode:forward-word
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret (or a selection of text) one word forward.")
    (:li (command-markup 'nyxt/visual-mode:backward-word
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret (or a selection of text) one line backward.")
    (:li (command-markup 'nyxt/visual-mode:forward-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret (or a selection of text) one line forward.")
    (:li (command-markup 'nyxt/visual-mode:backward-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret (or a selection of text) one line backward.")
    (:li (command-markup 'nyxt/visual-mode:beginning-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret to the beginning of the line.")
    (:li (command-markup 'nyxt/visual-mode:end-line
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret to the end of the line.")
    (:li (command-markup 'nyxt/visual-mode:forward-sentence
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret (or a selection of text) one sentence forward.")
    (:li (command-markup 'nyxt/visual-mode:backward-sentence
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Move the caret (or a selection of text) one sentence backward."))
   (:p "Avilable commands for ease of (default) CUA-mode use,
(but available to all): ")
   (:ul
    (:li (command-markup 'nyxt/visual-mode:forward-char-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Turn on the text selection and move caret one character forward.")
    (:li (command-markup 'nyxt/visual-mode:backward-char-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Turn on the text selection and move caret one character backward.")
    (:li (command-markup 'nyxt/visual-mode:forward-line-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Turn on the text selection and move caret one line forward.")
    (:li (command-markup 'nyxt/visual-mode:backward-line-with-selection
                         :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
         ": Turn on the text selection and move the caret one line backward."))
   (:p "If using " (:code "emacs-mode") ", the command "
       (command-markup 'nyxt/visual-mode:toggle-mark
                       :modes (list (make-instance 'nyxt/visual-mode:visual-mode)))
       " is bound to Shift-space (unlike in Emacs), as C-space is bound to
'execute-command, overriding any mode keybinding.~%If you want to toggle mark with
C-space, set your own override-map such that C-space is unbound.
An example:")
   (:pre (:code "
\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"override-map\")))
                   (define-key map
                     \"M-x\" 'execute-command)))))"))

   (:h3 "Automation")
   (:p "Nyxt can do automation, like simplifying the reading experience:")
   (:ul
    (:li (command-markup 'nyxt/cruise-control-mode:cruise-control-mode) ": "
    (command-docstring-first-sentence
    'nyxt/cruise-control-mode:cruise-control-mode)))
   (:p "Symmetrically, forms can be auto-filled: ")
   (:ul
    (:li (command-markup 'autofill) ": " (command-docstring-first-sentence
    'autofill))
    (:li (command-markup 'nyxt/web-mode::toggle-checkboxes) ": "
    (command-docstring-first-sentence 'nyxt/web-mode::toggle-checkboxes)))
   (:p "In addition, actions can be automated over time: "
   (:ul
    (:li (command-markup 'nyxt/watch-mode:watch-mode) ": "
         (command-docstring-first-sentence 'nyxt/watch-mode:watch-mode))
    (:li (command-markup 'nyxt/repeat-mode:repeat-every) ": "
         (command-docstring-first-sentence 'nyxt/repeat-mode:repeat-every))))
   (:p "Or even automate actions based on conditions: "
   (:ul
    (:li (command-markup 'nyxt/repeat-mode:repeat-mode) ": "
         (command-docstring-first-sentence 'nyxt/repeat-mode:repeat-mode))
    (:li (command-markup 'nyxt/preview-mode:preview-mode) ": "
         (command-docstring-first-sentence 'nyxt/preview-mode:preview-mode))))
   (:p "Nyxt also offers a no-code interface to build automation via Common Lisp
macros: ")
   (:ul
    (:li (command-markup 'nyxt/macro-edit-mode:edit-macro) ": "
         (command-docstring-first-sentence
         'nyxt/macro-edit-mode:edit-macro)))
   (:p "Lastly, the command " (:code 'nyxt/process-mode:process-mode) " must be
highlighted: ")
   (:ul
    (:li (command-markup 'nyxt/process-mode:process-mode) ": "
         (command-docstring-first-sentence 'nyxt/process-mode:process-mode)))
   (:p (:code 'nyxt/process-mode:process-mode) " is actually a building block
for other modes previously mentioned, such as " (:code
'nyxt/repeat-mode:repeat-mode) ". The extension relationship goes further, since
" (:code 'nyxt/cruise-control-mode:cruise-control-mode) " is in its turn an
extension and a composition of " (:code 'nyxt/repeat-mode:repeat-mode) " and "
(:code 'nyxt/web-mode:scroll-down) ". Make your own extensions and compositions
to automate your use of Nyxt.")
   (:h3 "Miscellaneous")
   (:ul
    (:li (command-markup 'nyxt/web-mode:zoom-page)
         ", " (command-markup 'nyxt/web-mode:unzoom-page)
         ", " (command-markup 'nyxt/web-mode:reset-page-zoom)
         ": Control the page zoom.")
    (:li (command-markup 'nyxt/web-mode:jump-to-heading) ": Query a heading (a
section) of the current page and jump to it.")
    (:li (command-markup 'nyxt/web-mode::autofill) ": Read the "
         (:code "autofills") " browser slot.")
    (:li (command-markup 'download-open-file)
         ": Opens a file in Nyxt or via other means.  Read " (:code "open-file-function") ".")
    (:li (command-markup 'edit-with-external-editor)
         ": Edit selected HTML input tag in an external editor.")
    (:li (command-markup 'quit) ": Close all Nyxt windows and quit."))

   (:h2 "The Nyxt help system")
   (:p "Nyxt provides introspection, clarity, and help.~%All commands,
classes, slots, variables, functions and bindings can be inspected for
definition and documentation.")
   (:ul
    (:li (command-markup 'help) ": Opens a small help buffer.")
    (:li (command-markup 'tutorial) ": Opens this tutorial.")
    (:li (command-markup 'describe-key) ": Shows what command an inserted key sequence
is bound to.")
    (:li (command-markup 'describe-bindings) ": Displays all currently set
bindings in the current buffer.")
    (:li (command-markup 'describe-command) ": Details a particular
command (including showing its source).")
    (:li (command-markup 'describe-function) ": Details a particular
function.")
    (:li (command-markup 'describe-variable) ": Displays the value of and documentation
for a variable.")
    (:li (command-markup 'describe-class) ": Displays documentation for a class and all its slots.")
    (:li (command-markup 'describe-slot) ": Displays the slot value for a class and its documentation.")
    (:li (command-markup 'describe-any) ": The conflation of all the 'describe' functions."))
   (:p "A good starting point is to study the documentation of the "
       (:code "browser") ", " (:code "window") ", " (:code "buffer") " and "
       (:code "prompt-buffer") " classes.")))
