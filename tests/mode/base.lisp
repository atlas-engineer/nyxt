;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-base-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (assert-true (enable-modes* 'nyxt:base-mode buffer))
    (assert-true (disable-modes* 'nyxt:base-mode buffer))))

(define-test focus-buffer-bindings ()
  (assert-eq 'focus-buffer-left
             (mode-keyscheme-binding 'nyxt:base-mode keyscheme:default "shift-left"))
  (assert-eq 'focus-buffer-right
             (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "shift-right"))
  (assert-eq 'focus-buffer-up
             (mode-keyscheme-binding 'nyxt:base-mode keyscheme:emacs "shift-up"))
  (assert-eq 'focus-buffer-down
             (mode-keyscheme-binding 'nyxt:base-mode keyscheme:vi-normal "shift-down"))
  (assert-eq 'focus-buffer-up
             (mode-keyscheme-binding 'nyxt:focus-move-mode keyscheme:cua "C-M-up"))
  (assert-eq 'focus-buffer-down
             (mode-keyscheme-binding 'nyxt:focus-move-mode keyscheme:emacs "shift-down")))

(defun mode-keyscheme-binding (mode-class keyscheme keyspec)
  (let ((mode (make-instance mode-class)))
    (keymaps:lookup-key keyspec
                        (keymaps:get-keymap keyscheme (keyscheme-map mode)))))

(define-test cua-macos-keybindings ()
  (assert-eq 'make-buffer-focus
             (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "C-t"))
  (assert-eq 'delete-current-buffer
             (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "C-w"))
  #-darwin
  (progn
    (assert-false (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-t"))
    (assert-false (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-,")))
  #+darwin
  (progn
    (assert-eq 'make-buffer-focus
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-t"))
    (assert-eq 'reopen-buffer
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-T"))
    (assert-eq 'common-settings
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-,"))
    (assert-eq 'set-url
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-l"))
    (assert-eq 'reload-current-buffer
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-r"))
    (assert-eq 'delete-current-buffer
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-w"))
    (assert-eq 'make-window
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-n"))
    (assert-eq 'delete-current-window
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-W"))
    (assert-eq 'quit
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-q"))
    (assert-eq 'switch-buffer-previous
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-{"))
    (assert-eq 'switch-buffer-next
               (mode-keyscheme-binding 'nyxt:base-mode keyscheme:cua "S-}"))
    (assert-false (mode-keyscheme-binding 'nyxt:base-mode keyscheme:emacs "S-t"))
    (assert-false (mode-keyscheme-binding 'nyxt:base-mode keyscheme:emacs "S-,"))
    (assert-false (mode-keyscheme-binding 'nyxt:base-mode keyscheme:vi-normal "S-t"))
    (assert-false (mode-keyscheme-binding 'nyxt:base-mode keyscheme:vi-normal "S-,"))
    (assert-eq 'nyxt/mode/document:copy
               (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:cua "S-c"))
    (assert-eq 'nyxt/mode/document:paste
               (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:cua "S-v"))
    (assert-eq 'nyxt/mode/document:cut
               (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:cua "S-x"))
    (assert-eq 'nyxt/mode/document:select-all
               (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:cua "S-a"))
    (assert-eq 'nyxt/mode/document:undo
               (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:cua "S-z"))
    (assert-eq 'nyxt/mode/document:redo
               (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:cua "S-Z"))
    (assert-eq 'nyxt/mode/document:zoom-page
               (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:cua "S-+"))
    (assert-eq 'nyxt/mode/document:unzoom-page
               (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:cua "S-hyphen"))
    (assert-eq 'nyxt/mode/document:reset-page-zoom
               (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:cua "S-0"))
    (assert-false (mode-keyscheme-binding 'nyxt/mode/document:document-mode keyscheme:emacs "S-c"))
    (assert-eq 'nyxt/mode/history:history-backwards
               (mode-keyscheme-binding 'nyxt/mode/history:history-mode keyscheme:cua "S-["))
    (assert-eq 'nyxt/mode/history:history-forwards
               (mode-keyscheme-binding 'nyxt/mode/history:history-mode keyscheme:cua "S-]"))
    (assert-eq 'nyxt/mode/history:list-history
               (mode-keyscheme-binding 'nyxt/mode/history:history-mode keyscheme:cua "S-y"))
    (assert-false (mode-keyscheme-binding 'nyxt/mode/history:history-mode keyscheme:emacs "S-["))
    (assert-eq 'nyxt/mode/bookmark:add-bookmark
               (mode-keyscheme-binding 'nyxt/mode/bookmark:bookmark-mode keyscheme:cua "S-d"))
    (assert-eq 'nyxt/mode/bookmark:list-bookmarks
               (mode-keyscheme-binding 'nyxt/mode/bookmark:bookmark-mode keyscheme:cua "S-M-b"))
    (assert-eq 'nyxt/mode/search-buffer:search-buffer
               (mode-keyscheme-binding 'nyxt/mode/search-buffer:search-buffer-mode keyscheme:cua "S-f"))
    (assert-eq 'nyxt/mode/prompt-buffer:paste
               (mode-keyscheme-binding 'nyxt/mode/prompt-buffer:prompt-buffer-mode keyscheme:cua "S-v"))
    (assert-eq 'delete-panel-buffer
               (mode-keyscheme-binding 'nyxt:panel-mode keyscheme:cua "S-w"))))
