nEXT Release & Feature Timeline
=======================================================================
Major releases are issued by incrementing the first digit. That is,
release 1.20, and 2.20 are one major release away from each other. Minor
releases are issued by incrementing by 0.01. That is, 1.01, and 1.02 are
two minor releases following major release 1.0.

0.01
------------------------------------------------------------------------
This version describes the minimum usability as a basic browser, with
the following features:

- Implementation of document-mode, the major-mode that all modes extend
- Implementation of html-mode major-mode for html documents
- Ability to set key bindings for major modes
- Ability to browse and change buffers using C-x b

Definitions
------------------------------------------------------------------------
Buffer: All documents are contained in an object type called a
buffer. As an example, a document on the web located at
http://www.url.com can be contained in a buffer with a similar name. A
buffer is composed of all elements (text, bitmaps, etc) necessary to
render a single document.

Mode-map: A keyboard to command mapping function or table.

Minibuffer: A special buffer dedicated to interacting with emacs
commands in progress. This buffer appears at the bottom of the screen
and is collapsed when not in use.

Major-mode: A major mode is defined as the primary mode of interacting
with a particular buffer. A mode defines a set of keybindings, hooks
for actions, and presentation details for a given view. At any given
time, there may only be one major mode for a buffer. All major modes
are composed of entirely lower case alpha with dashes used as a
separator. Every major mode has a keyboard mapping that follows this
pattern: document-mode, will have a mode map called document-mode-map.

Minor-mode: A minor mode is a secondary mode of modifying a buffer's
behavior and content. There can be an infinite amount of minor modes
applied to a given buffer. All minor modes are composed of entirely
lower case alpha with dashes used as a separator.

Major mode: document-mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All major modes inherit from document mode. Document mode provides the
basic framework for mapping global commands and general
behavior.

- Ability to open new buffers with the key-binding C-x C-o

Opening new buffers
Opening of new buffers by invoking C-x C-o will open the mini buffer.
Within the minibuffer, the user will be presented with a prompt in
which they can enter in the url they would like to visit in a new
buffer.


Major mode: html-mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
html-mode will be the basic major mode for opening documents on the
web. html-mode will extend document-mode, and thus will inherit all of
its keybindings. If there is a conflict of keybindings, the lowest
scope keybinding will be prioritized. As a concrete example, all
bindings defined in html-mode will override any defined in
document-mode. In the first release, html-mode will support the
following keybindings and features:

- Ability to open a new html document with the keybinding C-o
- Ability to navigate forward and backward in history with the
  keybinding M-f, and M-b for forward and backward respectively

Opening new pages in the same buffer
Opening of new pages in the same buffer can be invoked by the keybinding
C-o. This keybinding will open up the minibuffer and prompt the user
to enter the url which they would like to visit.

History
Within html-mode the history will be represnted as a tree, with forwards
and backwards navigation creating new nodes and traversals. This will
allow for all points in history to be reachable, and a future expansion
designed to recreate the functionality offered by undo-tree:
https://www.emacswiki.org/emacs/UndoTree

Ability to set Keybindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following syntax should be used to set a keybinding:

(define-key x-mode-map (kbd "C-h") 'function)

    Where x-mode-map is a keymap relating to a mode (major or minor).

    Where 'function is a function that is passed to define-key to
    trigger a function upon a key press.

(kbd "C-h") defines that the keyboard sequence Control + h is
represented. For the keyboard syntax, the following keys are
described:

- s = super key (windows/command key)
- S = shift key
- C = control key
- M = meta key (alt key)

A chain of keybindings may be set in the following manner:

(kbd "C-x C-s") will denote the following key presses, Ctrl + x, followed
by Ctrl + s immediately thereafter.

Ability to browse and change buffers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The user will be able to invoke the keybinding C-x b to bring up
a menu in the minbuffer in which they will be able to select a new buffer
to bring to focus.
