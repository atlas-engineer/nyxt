(in-package :next)

(defun tutorial-content ()
  (markup:markup
   (:h1 "Next tutorial")
   (:p "By reading this tutorial you'll learn:")
   (:ul
    (:li "Core concepts")
    (:li "Basic keybindings")
    (:li "How to use the help system to learn more about Next"))
   (:h2 "Core Concepts")
   (:h3 "Keybindings and Commands")
   (:p "Commands in Next are invoked by pressing any key or from
the " (:code "execute-command") " menu. Typically, are paired with a modifier
key. A modifier key is a key that does not produce its own output, but rather
modifies the output of whatever key you depress with it. Examples of modifier
keys include: control, option, alt, shift, command, or the windows key.")
   (:p "Modifier keys legend:")
   (:ul
    (:li (:code "control") " (" (:code "C") "): Control key")
    (:li (:code "super") " (" (:code "S") "): Windows key, Command key")
    (:li (:code "meta") " (" (:code "M") "): Alt key, Option key")
    (:li (:code "shift") " (" (:code "s") "): Shift key"))
   (:p "Throughout this tutorial, keybindings will be represented as in
'control-x' or equivalently 'C-x'. In this example, 'C' if a shortcut for the
modifier 'control', and 'x' represents the character 'x'. Therefore, to input
the 'C-x' keybinding you would have to keep Control pressed and they hit 'x'.
Multiple key presses can be chained: in 'C-x M-s', you would have to press
'C-x', let go of all keys, and then press 'M-s'.")
   (:p "You can also invoke commands by name using
the " (:code "execute-command") " command which will prompt you for a list of
commands which you can then select from.")
   (:h3 "Buffers")
   (:p "Next uses the concept of a buffer instead of tabs. Beyond tabs, buffers
provide additional functionality. For example, a buffer can have a unique
keybinding scheme. That is, example Buffer A and Buffer B can have a completely
different set of keybindings.")
   (:h3 "Modes")
   (:p "Buffers have a set of modes. A mode is a set of functions, hooks,
keybindings and/or other facilities that modify the behavior of a buffer.")
   (:h3 "Minibuffer")
   (:p "The minibuffer is a input area where you can provide input to
commands. By default, the minibuffer will appear at the bottom of a window when
you are required to input additional information to a command. For example, when
invoking the 'set-url' command, you must supply the URL you would like to
navigate to. The minibuffer will also frequently provide
completions (suggestions) for you when using it, simply type some characters to
see what the minibuffer suggests.")
   (:h3 "Message Area")
   (:p "The message area represents a space (typically at the bottom of a
window) where Next will output messages back to you. To view the history of all
things printed in the messages area, invoke the command 'messages'")
   (:h3 "Status Area")
   (:p "The status area represents a space below each buffer where information
about the state of that buffer is printed. In a typical installation this
includes the active modes, the URL, and the title of the current buffer.")
   (:h2 "Basic Keybindings")
   (:h3 "Moving within a buffer")
   (:p "To Move within a buffer, several commands are provided:")
   (:ul
    (:li (:code (binding-keys 'scroll-down) " scroll-down") ": Move down")
    (:li (:code (binding-keys 'scroll-up) " scroll-up") ": Move up")
    (:li (:code (binding-keys 'scroll-to-bottom) " scroll-to-bottom") ": Jump to bottom of page")
    (:li (:code (binding-keys 'scroll-to-top) " scroll-to-top") ": Jump to top of page"))
   (:h3 "Setting the URL")
   (:p "When ambiguous URLs are inputted, Next will attempt the best guess it
can. If the you do not supply a protocol in a URL, 'https' will be assumed. To
visit a site supporting only 'http', you must explicitly type the full URL with
'http' included.")
   (:ul
    (:li (:code (binding-keys 'set-url) " set-url") ": Set URL of current buffer")
    (:li (:code (binding-keys 'set-url-new-buffer) " set-url-new-buffer") ": Open a new buffer and set its URL")
    (:li (:code (binding-keys 'make-buffer-focus) " make-buffer-focus") ": Make a new buffer"))
   (:h3 "Switching buffers")
   (:p "You can switch buffers using the 'switch-buffer' command which provides
fuzzy completion. This allows you to quickly find whatever buffer you are
looking for.")
   (:ul
    (:li (:code (binding-keys 'switch-buffer) " switch-buffer") ": Switch buffer")
    (:li (:code (binding-keys 'switch-buffer-next) " switch-buffer-next") ": Go to next buffer")
    (:li (:code (binding-keys 'switch-buffer-previous) " switch-buffer-previous") ": Go to previous buffer"))
   (:h3 "Link Navigation")
   (:p "Next provides a feature called link-hinting to allow you to visit URLs
on a page without using the mouse. To use it, type invoke the 'follow-hint'
command. Several hints will appear on screen and in the minibuffer, these hints
represent elements that you can interact with enter in one of the hint strings
into the minibuffer to invoke it.")
   (:ul
    (:li (:code (binding-keys 'follow-hint) " follow-hint") ": Go to link in current buffer")
    (:li (:code (binding-keys 'follow-hint-new-buffer-focus) " follow-hint-new-buffer-focus") ": Create new buffer with link, focus on new buffer")
    (:li (:code (binding-keys 'follow-hint-new-buffer) " follow-hint-new-buffer") ": Create new buffer with link, keep focus on current buffer"))
   (:h3 "Using the buffer history")
   (:p "History is represented as a tree that you can traverse. More complex
than the 'forwards-backwards' abstraction found in other browsers, the tree
makes sure you never lose track of where you've been.")
   (:ul
    (:li (:code (binding-keys 'next/web-mode:history-forwards) " history-forwards") ": History forwards")
    (:li (:code (binding-keys 'next/web-mode:history-backwards) " history-backwards") ": History backwards")
    (:li (:code (binding-keys 'next/web-mode:history-forwards-query) " history-forwards-query") ": History forwards query, forward to any forward location")
    (:li (:code (binding-keys 'next/web-mode:history-backwards-query) " history-backwards-query") ": History backwards query, backward to any backward location")
    (:li (:code (binding-keys 'next/web-mode:history-all-query) " history-all-query") ": History all query, Jump to any history entry."))
   (:p "You can also view a full tree of the history for a given buffer by
invoking the command 'buffer-history-tree'.")
   (:h3 "Searching")
   (:p "Next can search a single buffer or multiple buffers at the same time.")
   (:p "You can view candidates for search results in the minibuffer in one
place rather than having to jump around on a buffer (or multiple buffers).")
   (:ul
    (:li (:code (binding-keys 'search-buffer) " search-buffer") ": Search buffer")
    (:li (:code (binding-keys 'search-buffers) " search-buffers") ": Search buffers"))
   (:h3 "Exiting Next")
   (:p "To quit Next, enter the command " (:code (binding-keys 'quit)))
   (:h2 "The Next Help System")
   (:p "Next provides introspective and help capabilities. You can view and
modify Next's source code during runtime. A few help commands are described
below:")
   (:ul
    (:li (:code (binding-keys 'help) " help")
         ": The command 'help' opens up a small help buffer.")
    (:li (:code (binding-keys 'tutorial) " tutorial")
         ": The command 'tutorial' opens up this tutorial.")
    (:li (:code (binding-keys 'describe-key) " describe-key")
         ": The command 'describe-key' allows you to input a key binding and see
what command it is bound to.")
    (:li (:code (binding-keys 'describe-slot) " describe-slot")
         ": The command 'describe-slot' allows you to find out the value of a
particular class slot and view its documentation.")
    (:li (:code (binding-keys 'describe-command) " describe-command")
         ": The commmand 'describe-command' allows you to find out about a
particular command (including showing its source).")
    (:li (:code (binding-keys 'describe-bindings) " describe-bindings")
         ": The command 'describe-bindings' to view all of your currently set
bindings in your buffer.")
    (:li (:code (binding-keys 'describe-variable) " describe-variable")
         ": The command 'describe-variable' allows you to view the value and
documentation of a variable."))))
