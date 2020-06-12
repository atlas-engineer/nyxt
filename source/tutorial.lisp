(in-package :next)

(defun tutorial-content ()
  (markup:markup
   (:h2 "Core Concepts")
   (:h3 "Keybindings and Commands")
   (:p "Commands in Next are invoked by pressing specific keys or from
the " (:code "execute-command") " menu (" (:code (binding-keys 'execute-command))
") which will prompt you for a list of commands which you can then select
from.")
   (:p "In Next, keybindings are represented as in 'control-space' or
equivalently 'C-space'. In this example, 'C' is a shortcut for the modifier
'control', and 'space' represents the character ' '.  A modifier is a key that
does nothing on its own, its purpose is to alter the meaning of a
non-modifier key when pressed together.  Therefore, to input the 'C-x'
keybinding you would keep 'control' pressed and then hit 'x'.  Multiple
key presses can be chained: in 'C-x C-M-left', you would have to press 'C-x',
let go of all keys, and then press 'control', 'meta' and 'left'.")
   (:p "Modifier keys legend:")
   (:ul
    (:li (:code "control") " (" (:code "C") "): Control key")
    (:li (:code "super") " (" (:code "S") "): Windows key, Command key")
    (:li (:code "meta") " (" (:code "M") "): Alt key, Option key")
    (:li (:code "shift") " (" (:code "s") "): Shift key"))
   (:p "Modifiers can be remapped, see the `modifier-translator' slot of the
`gtk-browser' class.")

   (:h3 "Buffers")
   (:p "Next uses the concept of buffers instead of the more limited \"tabs\"
used by many applications. Unlike tabs, the buffer display scales, that is to
say, it's just as convenient to navigate 2 or 100 buffers.  The buffer states
are fully separated, for instance two buffers can have different set of
keybindings.")
   (:h3 "Modes")
   (:p "Each buffer has its own list of modes, ordered by priority.  A mode is a
set of functions, hooks, keybindings and other facilities that modify the
behavior of a buffer.  For example, 'blocker-mode' can be used for domain-based
adblocking while 'noscript-mode' disables JavaScript.")
   (:p "Each buffer has separate instance of modes, which means that altering
the settings of a mode in a buffer does not impact the other buffers.  Mode
functions are only available when the mode is enabled for the current buffer.")
   (:p "Each mode has an associated " (:i "mode toggler") " which is a command
of the same name the toggles the mode for the current buffer.")
   (:h3 "Minibuffer")
   (:p "The minibuffer is a menu that will appear when a command requests user
input. For example, when invoking the " (:code "set-url") " command, you must
supply the URL you would like to navigate to. The minibuffer can provide
suggestions.  The list of suggestions will automatically narrow down to those
matching your input as you type.")
   (:ul
    (:li (command-markup 'next/minibuffer-mode:return-input
                         :modes (list (make-instance 'next/minibuffer-mode:minibuffer-mode)))
         ": Validate the selected suggestion(s) or the current input if there is
no suggestion.")
    (:li (command-markup 'next/minibuffer-mode:return-immediate
                         :modes (list (make-instance 'next/minibuffer-mode:minibuffer-mode)))
         ": Validate the current input, ignoring any suggestion."))
   (:p " Some commands support multiple selections, for
instance " (:code "delete-buffer") " can delete all selected buffers at once.
When the input is changed and the candidates are re-filtered, the selection is
not altered even if the marked elements don't show.")
   (:p "When at least one candidate is marked, only the marked candidates are processed
upon return.  The candidate under the cursor is not processed if not marked.")
   (:ul
    (:li (command-markup 'next/minibuffer-mode:minibuffer-toggle-mark
                         :modes (list (make-instance 'next/minibuffer-mode:minibuffer-mode)))
         ": Select or deselect the current suggestion.")
    (:li (command-markup 'next/minibuffer-mode:minibuffer-mark-all
                         :modes (list (make-instance 'next/minibuffer-mode:minibuffer-mode)))
         ": Select all currently-displayed suggestions.")
    (:li (command-markup 'next/minibuffer-mode:minibuffer-unmark-all
                         :modes (list (make-instance 'next/minibuffer-mode:minibuffer-mode)))
         ": Deselect all currently-displayed suggestions."))
   (:h3 "Message Area")
   (:p "The message area represents a space (typically at the bottom of a
window) where Next outputs messages back to you. To view the history of all
messages, invoke the command " (:code "messages") ".")
   (:h3 "Status Area")
   (:p "The status area is where information about the state of that buffer is
printed. By default this includes the active modes, the URL, and the title of
the current buffer.")

   (:h2 "Basic controls")
   (:h3 "Moving within a buffer")
   (:p "To move within a buffer, several commands are provided:")
   (:ul
    (:li (command-markup 'next/web-mode:scroll-down) ": Move down.")
    (:li (command-markup 'next/web-mode:scroll-up) ": Move up.")
    (:li (command-markup 'next/web-mode:scroll-to-bottom) ": Jump to bottom of page.")
    (:li (command-markup 'next/web-mode:scroll-to-top) ": Jump to top of page."))
   (:h3 "Setting the URL")
   (:p "When ambiguous URLs are inputted, Next will attempt the best guess it
can. If the you do not supply a protocol in a URL, HTTPS will be assumed. To
visit a site supporting only the less secure HTTP, you must explicitly type the
full URL including the 'http://' prefix.")
   (:ul
    (:li (command-markup 'set-url) ": Set URL of current buffer.")
    (:li (command-markup 'set-url-new-buffer) ": Open a new buffer and set its URL.")
    (:li (command-markup 'make-buffer-focus) ": Make a new empty buffer."))
   (:h3 "Switching buffers")
   (:ul
    (:li (command-markup 'switch-buffer) ": Switch buffer using fuzzy completion
to quickly find whatever buffer you are looking for.")
    (:li (command-markup 'switch-buffer-next) ": Go to next buffer.")
    (:li (command-markup 'switch-buffer-previous) ": Go to previous buffer."))
   (:h3 "Link navigation")
   (:p "Link-hinting allows you to visit URLs on a page without using the mouse.
Invoke one of the commands below: several hints will appear on screen and all
links on the page will be listed in the minibuffer.  You can select the hints
by matching against the hint, the URL or the title.")
   (:ul
    (:li (command-markup 'next/web-mode:follow-hint) ": Go to link in current buffer.")
    (:li (command-markup 'next/web-mode:follow-hint-new-buffer-focus) ": Create new buffer with link, focus on new buffer.")
    (:li (command-markup 'next/web-mode:follow-hint-new-buffer) ": Create new buffer with link, keep focus on current buffer."))
   (:h3 "Using the buffer history")
   (:p "History is represented as a tree that you can traverse: when you go back
in history, then follow a new URL, it effectively creates a new branch without
deleting the old path. The tree makes sure you never lose track of where you've
been.")
   (:ul
    (:li (command-markup 'next/web-mode:history-forwards) ": History forwards.")
    (:li (command-markup 'next/web-mode:history-backwards) ": History backwards.")
    (:li (command-markup 'next/web-mode:history-forwards-query) ": History forwards query to any following location on the branch.")
    (:li (command-markup 'next/web-mode:history-backwards-query) ": History backwards query to any previous location.")
    (:li (command-markup 'next/web-mode:history-forwards-all-query) ": History forwards query to any following location on all branches.")
    (:li (command-markup 'next/web-mode:history-all-query) ": History all query, jump to any history entry."))
   (:p "You can also view a full tree of the history for a given buffer by
invoking the command 'buffer-history-tree'.")
   (:h3 "Searching")
   (:p "Next can search a single buffer or multiple buffers at the same time.")
   (:p "You can view candidates for search results in the minibuffer in one
place rather than having to jump around on a buffer (or multiple buffers).")
   (:ul
    (:li (command-markup 'next/web-mode:search-buffer) ": Search buffer.")
    (:li (command-markup 'next/web-mode:search-buffers) ": Search multiple buffers.")
    (:li (command-markup 'next/web-mode:remove-search-hints) ": Remove the highlighting around the search hits."))
   (:h3 "Bookmarks")
   (:p "The bookmark file "
       (:code (expand-path (bookmarks-path *browser*)))
       " is made to be human readable and editable.
Bookmarks can have the following settings:")
   (:ul
    (:li (:code ":url") ": The URL of the bookmark.")
    (:li (:code ":title") ": The title of the bookmark.")
    (:li (:code ":tags") ": A list of strings.  Useful to categorize and filter bookmarks.")
    (:li (:code ":shortcut") ": A single-word string.  Directly opens the
bookmark when inputted in one of the 'set-url' commands."))
   (:p "Bookmark-related commands")
   (:ul
    (:li (command-markup 'bookmark-current-page) ": Bookmark current page.
Prompt for tags.  The input defaults to the existing tags: if some tags are
removed from the input, they are also removed from the existing bookmark.")
    (:li (command-markup 'bookmark-page) ": Same as above but prompt for a buffer first.")
    (:li (command-markup 'bookmark-url) ": Same as above but prompt for a URL first.")
    (:li (command-markup 'next/web-mode:bookmark-hint) ": Same as above but prompt for a hinted URL first.")
    (:li (command-markup 'set-url-from-bookmark) ": Open bookmark in current buffer.")
    (:li (command-markup 'set-url-from-bookmark-new-buffer) ": Open bookmark in new buffer.")
    (:li (command-markup 'bookmark-delete) ": Delete queried bookmarks.")
    (:li (command-markup 'show-bookmarks) ": Display a new buffer containing the
list of all bookmarks."))
   (:p "You can filter them with selectors: use '+', '-' or write a compound
query inside parenthesis in which you can use 'and', 'or' and 'not'. Examples:")
   (:ul
    (:li "+lisp -blog ")
    (:li "+blog (or lisp emacs) ")
    (:li "+foo -bar (or (and john doe) (not (and tic tac toe)))"))

   (:h3 "Miscellaneous")
   (:ul
    (:li (command-markup 'next/web-mode:zoom-in-page)
         ", " (command-markup 'next/web-mode:zoom-out-page)
         ", " (command-markup 'next/web-mode:unzoom-page)
         ": Control the page zoom.")
    (:li (command-markup 'next/web-mode:jump-to-heading) ": Query a heading (a
section) of the current page and jump to it.")
    (:li (command-markup 'next/web-mode:autofill) ": See the "
         (:code "autofills") " browser slot.")
    (:li (command-markup 'vcs-clone) ": Clone version control repository
matching current URL.")
    (:li (command-markup 'open-file) ", " (command-markup 'download-open-file)
         ": Open file in Next or externally.  See `*open-file-function*'.")
    (:li (command-markup 'fill-input-from-external-editor)
         ": Edit selected HTML input tag with an external editor.")
    (:li (command-markup 'quit) ": Close all Next windows and quit."))

   (:h2 "The Next Help System")
   (:p "Next provides introspective and help capabilities.  All commands,
classes, slots, variables, functions and bindings can be inspected for
definition and documentation.")
   (:ul
    (:li (command-markup 'help) ": Open up a small help buffer.")
    (:li (command-markup 'tutorial) ": Open up this tutorial.")
    (:li (command-markup 'describe-key) ": Lets you to input a key binding and
see what command it is bound to.")
    (:li (command-markup 'describe-bindings) ": View all of your currently set
bindings in the current buffer.")
    (:li (command-markup 'describe-command) ": Find out about a particular
command (including showing its source).")
    (:li (command-markup 'describe-function) ": Find out about a particular
function.")
    (:li (command-markup 'describe-variable) ": View the value and documentation
of a variable.")
    (:li (command-markup 'describe-class) ": Lookup a class documentation and all its slots.")
    (:li (command-markup 'describe-slot) ": Lookup a class slot value and documentation."))
   (:p "A good starting point is to study the documentation of the classes "
       (:code "browser") ", " (:code "window") ", " (:code "buffer") " and "
       (:code "minibuffer") ".")))
