;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/diff-mode
    (:use :common-lisp :nyxt)
  (:documentation "Mode for displaying web-buffer diffs."))
(in-package :nyxt/diff-mode)

;; colours based on the modus-operandi theme by Protesilaos Stavrou, which
;; follows the highest standard on accessibility
(defvar html-diff-style
  "<style>
  ins
  {background-color: #bbeabb;
   text-decoration: none}
  del
  {background-color: #efcbcf;
   text-decoration: none}
</style>
"
  "TODO")

;; this is the right way, but the I can't define a background-colour for the del
;; and ins tag within the nyxt-diff-replace class
;; (defvar html-diff-style
;;   "<style>
;;   .nyxt-diff-insert
;;   {background-color: #bbeabb;
;;    text-decoration: none}
;;   .nyxt-diff-delete
;;   {background-color: #efcbcf;
;;    text-decoration: none}
;;   .nyxt-diff-replace
;;   {background-color: #ecdfbf;
;;    text-decoration: none}
;; </style>"
;;   "TODO")

;; this doesn't contemplate if buffer *diff* already exists
;; add a proper title perhaps?  (*diff* + old buffer name + new buffer name)
(defun make-diff-buffer (html-diff-string html-diff-style)
  "TODO"
  (with-current-html-buffer (buffer "*diff*" 'base-mode)
    ;; FIXME title isn't overridden when the html contains the title tag
    (str:concat html-diff-style
                (if (str:contains? "<title>" html-diff-string)
                    (ppcre:regex-replace "<title>.*</title>" html-diff-string "")
                    html-diff-string))))

(defun diff-buffers ()
  "TODO"
  ;; facilitates the following use case:
  ;; the user wants to make a diff between the current-buffer and last inactive
  ;; buffer
  (let* ((old-html (ffi-buffer-get-document
                    (prompt-minibuffer
                     :input-prompt "Old buffer"
                     :suggestion-function (buffer-suggestion-filter))))
         (new-html (ffi-buffer-get-document
                    (prompt-minibuffer
                     :input-prompt "New buffer"
                     :suggestion-function (buffer-suggestion-filter
                                           :current-is-last-p t))))
         (diff-html (html-diff:html-diff old-html
                                         new-html
                                         :insert-class "nyxt-diff-insert"
                                         :delete-class "nyxt-diff-delete"
                                         :replace-class "nyxt-diff-replace")))
    (make-diff-buffer (princ diff-html) html-diff-style)))

;; (define-mode diff-mode ()
;;   "TODO"
;;   ((new-html :documentation "TODO")
;;    (old-html :documentation "TODO")
;;    (default-display-diff-view :documentation "TODO")
;;    (destructor (lambda (mode) TODO))
;;    (constructor (lambda (mode) TODO))))
