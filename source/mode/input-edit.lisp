;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/input-edit
  (:documentation "Mode for editing HTML input areas with convenient keybindings.

In addition to the commands `input-edit-mode' exposes, there are
several internal utilities for general HTML input editing:
- Parenscript functions:
  - `active-input-area-content'.
  - `set-active-input-area-content'.
  - `active-input-area-cursor'.
  - `set-active-input-area-cursor'.
- Macros `with-text-buffer' and `with-input-area'.
- Definition macro `define-input-edit-command'.

Several editing commands are based on `move-n-elements' internal
function."))
(in-package :nyxt/mode/input-edit)

;;;; Commands for navigating/editing input fields on HTML pages.

(define-parenscript active-input-area-content (buffer)
  (ps:chain (nyxt/ps:active-element document) value))

(define-parenscript set-active-input-area-content (buffer content)
  (setf (ps:chain (nyxt/ps:active-element document) value) (ps:lisp content)))

(define-parenscript active-input-area-cursor (buffer)
  (ps:chain (nyxt/ps:active-element document) selection-start))

(define-parenscript set-active-input-area-cursor (buffer selection-start selection-end)
  (ps:chain (nyxt/ps:active-element document)
            (set-selection-range
             (ps:lisp selection-start)
             (ps:lisp selection-end))))

(export-always 'with-text-buffer)
(defmacro with-text-buffer ((buffer-name cursor-name
                             &optional initial-contents
                               initial-cursor-position)
                            &body body)
  "Create a BUFFER-NAME buffer with INITIAL-CONTENTS and CURSOR-NAME at
INITIAL-CURSOR-POSITION.
Run the BODY in the environment with these bound."
  `(let ((,buffer-name (make-instance 'text-buffer:text-buffer))
         (,cursor-name (make-instance 'text-buffer:cursor)))
     (cluffer:attach-cursor ,cursor-name ,buffer-name)
     (when ,initial-contents
       (text-buffer::insert-string ,cursor-name ,initial-contents))
     (when ,initial-cursor-position
       (setf (cluffer:cursor-position ,cursor-name)
             (truncate ,initial-cursor-position)))
     ,@body))

(export-always 'with-input-area)
(defmacro with-input-area ((buffer contents cursor-position) &body body)
  "Bind CONTENTS and CURSOR-POSITION to the ones in BUFFER's focused input field."
  (let ((unprocessed-cursor (gensym)))
    `(let* ((,contents (active-input-area-content ,buffer))
            (,unprocessed-cursor (active-input-area-cursor ,buffer))
            (,cursor-position (when (numberp ,unprocessed-cursor)
                                (truncate ,unprocessed-cursor))))
       (declare (ignorable ,contents ,cursor-position))
       (if ,cursor-position
           ,@body
           (echo-warning "Cannot get cursor. Are you in an input field?")))))

(defun move-n-elements (buffer n)
  (with-input-area (buffer contents cursor-position)
    (let ((new-position (+ cursor-position n)))
      (set-active-input-area-cursor buffer new-position new-position))))

(defun sync-prompt-input-area (buffer)
  (when (prompt-buffer-p buffer)
    (nyxt::update-prompt-input buffer)))

(defmacro define-input-edit-command (name (buffer &rest args) documentation &body body)
  `(define-command ,name (,buffer ,@args)
     ,documentation
     ,@body))

(define-input-edit-command cursor-forwards (buffer)
  "Move cursor forward by one element."
  (move-n-elements buffer 1))

(define-input-edit-command cursor-backwards (buffer)
  "Move cursor backwards by one element."
  (move-n-elements buffer -1))

(define-input-edit-command cursor-forwards-word (buffer)
  "Move cursor forwards a word."
  (with-input-area (buffer contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::move-forward-word cursor
                                      :conservative-word-move
                                      (conservative-word-move buffer))
      (set-active-input-area-cursor buffer
                                    (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-input-edit-command cursor-backwards-word (buffer)
  "Move cursor backwards a word."
  (with-input-area (buffer contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::move-backward-word cursor
                                       :conservative-word-move
                                       (conservative-word-move buffer))
      (set-active-input-area-cursor buffer
                                    (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-input-edit-command delete-forwards (buffer)
  "Delete character after cursor."
  (with-input-area (buffer contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-item-forward cursor)
      (set-active-input-area-content
       buffer
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor buffer
                                    (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor))
      (sync-prompt-input-area buffer))))

(define-input-edit-command delete-backwards (buffer)
  "Delete character before cursor."
  (with-input-area (buffer contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-item-backward cursor)
      (set-active-input-area-content
       buffer
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor buffer
                                    (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor))
      (sync-prompt-input-area buffer))))

(define-input-edit-command delete-backwards-word (buffer)
  "Delete backwards a word."
  (with-input-area (buffer contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-backward-word cursor)
      (set-active-input-area-content
       buffer
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor buffer
                                    (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor))
      (sync-prompt-input-area buffer))))

(define-input-edit-command delete-forwards-word (buffer)
  "Delete forwards a word."
  (with-input-area (buffer contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-forward-word cursor)
      (set-active-input-area-content
       buffer
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor buffer
                                    (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor))
      (sync-prompt-input-area buffer))))

(define-mode input-edit-mode ()
  "Mode for editing input areas in HTML.
Overrides many of the bindings in other modes, so you will have to
disable/enable it as necessary."
  ((keyscheme-map
    (define-keyscheme-map "input-edit-mode" ()
      keyscheme:emacs
      (list
       "C-b" 'cursor-backwards
       "C-f" 'cursor-forwards
       "C-d" 'delete-forwards
       "M-b" 'cursor-backwards-word
       "M-f" 'cursor-forwards-word
       "M-backspace" 'delete-backwards-word
       "C-backspace" 'delete-backwards-word
       "M-d" 'delete-forwards-word
       "C-g" 'input-edit-mode)))))
