;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/input-edit-mode
    (:documentation "Mode for editing HTML input areas."))
(in-package :nyxt/input-edit-mode)

;;;; commands for navigating/editing input fields on HTML pages

(define-parenscript active-input-area-content ()
  (ps:chain document active-element value))

(define-parenscript set-active-input-area-content (content)
  (setf (ps:chain document active-element value) (ps:lisp content)))

(define-parenscript active-input-area-cursor ()
  (ps:chain document active-element selection-start))

(define-parenscript set-active-input-area-cursor (selection-start
                                                  selection-end)
  (ps:chain document active-element (set-selection-range
                                     (ps:lisp selection-start)
                                     (ps:lisp selection-end))))

(export-always 'with-text-buffer)
(defmacro with-text-buffer ((buffer-name cursor-name
                             &optional initial-contents
                                       initial-cursor-position)
                            &body body)
  `(let ((,buffer-name (make-instance 'text-buffer:text-buffer))
         (,cursor-name (make-instance 'text-buffer:cursor)))
     (cluffer:attach-cursor ,cursor-name ,buffer-name)
     (when ,initial-contents
       (text-buffer::insert-string ,cursor-name ,initial-contents))
     (when ,initial-cursor-position
       (setf (cluffer:cursor-position ,cursor-name) (truncate ,initial-cursor-position)))
     ,@body))

(export-always 'with-input-area)
(defmacro with-input-area ((contents cursor-position) &body body)
  (let ((unprocessed-cursor (gensym)))
    `(let* ((,contents (active-input-area-content))
            (,unprocessed-cursor (active-input-area-cursor))
            (,cursor-position (when (numberp ,unprocessed-cursor)
                                (truncate (active-input-area-cursor)))))
       (declare (ignorable ,contents ,cursor-position))
       (if ,cursor-position
           ,@body
           (echo-warning "Cannot get cursor position. Are you in an input field?")))))

(defun move-n-elements (n)
  (with-input-area (contents cursor-position)
    (let ((new-position (+ cursor-position n)))
      (set-active-input-area-cursor new-position
                                    new-position))))

(defmacro define-input-edit-command (name (&rest args) documentation &body body)
  `(define-command ,name (,@args)
     ,documentation
     (with-current-buffer (or (current-prompt-buffer) (current-buffer))
       ,@body)))

(define-input-edit-command cursor-forwards ()
  "Move cursor forward by one element."
  (move-n-elements 1))

(define-input-edit-command cursor-backwards ()
  "Move cursor backwards by one element."
  (move-n-elements -1))

(define-input-edit-command cursor-forwards-word ()
  "Move cursor forwards a word."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::move-forward-word cursor
                                      :conservative-word-move
                                      (conservative-word-move (current-buffer)))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-input-edit-command cursor-backwards-word ()
  "Move cursor backwards a word."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::move-backward-word cursor
                                      :conservative-word-move
                                      (conservative-word-move (current-buffer)))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-input-edit-command delete-forwards ()
  "Delete character after cursor."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-item-forward cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-input-edit-command delete-backwards ()
  "Delete character before cursor."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-item-backward cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-input-edit-command delete-backwards-word ()
  "Delete backwards a word."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-backward-word cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-input-edit-command delete-forwards-word ()
  "Delete forwards a word."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-forward-word cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-mode input-edit-mode ()
  "Mode for editing input areas in HTML. Overrides many of the
bindings in other modes, so you will have to disable/enable it as
necessary."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (keyscheme-map
    (define-keyscheme-map "input-edit-mode" ()
      ;; TODO: Add VI-normal?
      keyscheme:emacs
      (list
       "C-b"         'cursor-backwards
       "C-f"         'cursor-forwards
       "C-d"         'delete-forwards
       "M-b"         'cursor-backwards-word
       "M-f"         'cursor-forwards-word
       "M-backspace" 'delete-backwards-word
       "C-backspace" 'delete-backwards-word
       "M-d"         'delete-forwards-word)))))
