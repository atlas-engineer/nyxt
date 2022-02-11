;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/repl-mode
  (:use #:common-lisp #:nyxt)
  (:import-from #:keymap #:define-scheme)
  (:export :repl-mode))

(in-package :nyxt/repl-mode)

(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "repl"
      scheme:cua
      (list
       "return" 'return-input
       "(" 'paren
       ")" 'closing-paren)
      scheme:emacs
      (list
       "C-f" 'nyxt/input-edit-mode:cursor-forwards
       "C-b" 'nyxt/input-edit-mode:cursor-backwards
       "M-f" 'nyxt/input-edit-mode:cursor-forwards-word
       "M-b" 'nyxt/input-edit-mode:cursor-backwards-word
       "C-d" 'nyxt/input-edit-mode:delete-forwards
       "M-backspace" 'nyxt/input-edit-mode:delete-backwards-word
       "M-d" 'nyxt/input-edit-mode:delete-forwards-word)
      scheme:vi-normal
      (list
       ;; TODO: deleting chars/words
       "l" 'nyxt/input-edit-mode:cursor-forwards
       "h" 'nyxt/input-edit-mode:cursor-backwards
       "w" 'nyxt/input-edit-mode:cursor-forwards-word
       "b" 'nyxt/input-edit-mode:cursor-backwards-word)))
   (style (theme:themed-css (theme *browser*)
            (* :font-family "monospace,monospace")
            (body :margin-right "0")
            ("#container" :display "flex"
                          :flex-flow "column"
                          :height "100%"
                          :color theme:text
                          :background-color theme:background)
            ("#input" :display "grid"
                      :grid-template-columns "auto 1fr"
                      :width "100%"
                      :padding 0
                      :margin 0
                      :background-color theme:tertiary)
            ("#input-buffer" :width "100%"
                             :border "none"
                             :outline "none"
                             :padding "3px"
                             :background-color theme:quaternary
                             :autofocus "true")
            ("#evaluation-history"
             :font-size "12px"
             :flex-grow "1"
             :overflow-y "auto"
             :overflow-x "auto")
            ("#prompt" :padding-right "4px"
                       :padding-left "4px"
                       :line-height "30px"
                       :color theme:background)
            (ul :list-style "none"
                :padding "0"
                :margin "0")
            (li :padding "2px"))
          :documentation "The CSS applied to a REPL when it is set-up.")
   (evaluation-history (list)
                       :documentation "A list of pairs of (INPUT RESULTS).
INPUT is a string and RESULTS is a list of Lisp values.")))

(defun package-short-name (package)
  (first (sort (append (package-nicknames package)
                       (list (package-name package)))
               #'string<)))

(defmethod input ((mode repl-mode))
  (pflet ((input-text ()
           (ps:@ (nyxt/ps:qs document "#input-buffer") value)))
    (with-current-buffer (buffer mode)
      (input-text))))

(defmethod (setf input) (new-text (mode repl-mode))
  (pflet ((set-input-text (text)
           (setf (ps:@ (nyxt/ps:qs document "#input-buffer") value) (ps:lisp text))))
    (with-current-buffer (buffer mode)
      (set-input-text new-text))))

(defmethod cursor ((mode repl-mode))
  (pflet ((selection-start ()
           (ps:chain (nyxt/ps:qs document "#input-buffer") selection-start)))
    (with-current-buffer (buffer mode)
      (let ((cursor (selection-start)))
        (if (numberp cursor)
            cursor
            0)))))

(defmethod (setf cursor) (new-position (mode repl-mode))
  (pflet ((selection-start (position)
           (setf (ps:chain (nyxt/ps:qs document "#input-buffer") selection-start)
                 (setf (ps:chain (nyxt/ps:qs document "#input-buffer") selection-end)
                       (ps:lisp position)))))
    (with-current-buffer (buffer mode)
      (selection-start new-position))))

(define-command return-input (&optional (repl (current-mode 'repl)))
  "Return inputted text."
  (let ((input (input repl)))
    (add-object-to-evaluation-history repl
                                      (list (format nil "~a> ~a"
                                                    (package-short-name *package*)
                                                    input)
                                            (nyxt::evaluate input)))
    (reset-input repl)
    (update-evaluation-history-display repl)))

(define-command reset-input (&optional (repl (current-mode 'repl)))
  "Clear the inputted text."
  (setf (input repl) ""))

(defmethod add-object-to-evaluation-history ((repl repl-mode) item)
  (push item (evaluation-history repl)))

(defmethod update-evaluation-history-display ((repl repl-mode))
  (flet ((generate-evaluation-history-html (repl)
           (spinneret:with-html-string
             (:ul (loop for (input results) in (reverse (evaluation-history repl))
                        collect (:li (:b input)
                                     (loop for result in results
                                           collect (:li (prin1-to-string result)))))))))
    (ffi-buffer-evaluate-javascript-async
     (buffer repl)
     (ps:ps
       (setf (ps:chain document (get-element-by-id "prompt") |innerHTML|)
             (ps:lisp (format nil "~a>" (package-short-name *package*))))
       (setf (ps:chain document (get-element-by-id "evaluation-history") |innerHTML|)
             (ps:lisp (generate-evaluation-history-html repl)))))))

(define-command paren (&optional (repl (current-mode 'repl)))
  ;; FIXME: Not an intuitive behavior? What does Emacs do?
  "Inserts the closing paren after the opening one is inputted."
  (let ((input (input repl))
        (cursor (cursor repl)))
    (setf (input repl) (str:insert "()" cursor input)
          (cursor repl) (1+ cursor))))

(define-command closing-paren (&optional (repl (current-mode 'repl)))
  "Automatically closes all the open parens before the cursor."
  (let* ((input (input repl))
         (cursor (cursor repl))
         (parens-to-complete (- (count #\( input :end cursor)
                                (count #\) input))))
    (setf (input repl)
          (str:insert (make-string parens-to-complete :initial-element #\)) cursor input))))

(define-internal-page-command-global lisp-repl ()
    (repl-buffer "*Lisp REPL*" 'repl-mode)
  "Show Lisp REPL."
  (spinneret:with-html-string
    (:head (:style (style (find-mode repl-buffer 'repl-mode))))
    (:body
     (:div :id "container"
           (:div :id "evaluation-history" "")
           (:div :id "input"
                 (:span :id "prompt"
                        (format nil "~a>" (package-short-name *package*)))
                 (:input :type "text" :id "input-buffer"))))))
