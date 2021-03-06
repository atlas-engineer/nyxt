;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/diff-mode
    (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for displaying web-buffer diffs."))

(in-package :nyxt/diff-mode)

(define-mode diff-mode ()
  "Diff mode is used to view the diffs between two buffers."
  (;; (buffer (make-internal-buffer :title "*diff*"
   ;;                               :modes '(base-mode))
   ;;         :documentation "TODO")
   (old-html :documentation "TODO")
   (new-html :documentation "TODO")
   (diff-html :documentation "TODO")
   (diff-style (cl-css:css
                '((".nyxt-diff-insert"
                   :text-decoration "none"
                   :background-color "#bbeabb")
                  ("ins.nyxt-diff-replace"
                   :text-decoration "none"
                   :background-color "#bbeabb")
                  (".nyxt-diff-delete"
                   :text-decoration "none"
                   :background-color "#efcbcf")
                  ("del.nyxt-diff-replace"
                   :text-decoration "none"
                   :background-color "#efcbcf")))
               :documentation "Colours based on the modus-operandi theme by
Protesilaos Stavrou, which follows the highest standard on accessibility.")
   (keymap-scheme (define-scheme "diff"
                    scheme:cua
                    (list "q" 'delete-current-buffer))
                  :type keymap:scheme)
   (destructor (lambda (mode) (nyxt::buffer-delete (buffer mode))))
   (constructor (lambda (mode)
                  ;; TODO device a smart way to let users choose from buffers
                  ;; and/or files
                  (setf (old-html mode)
                        (ffi-buffer-get-document
                         (prompt-minibuffer
                          :input-prompt "Old buffer"
                          :suggestion-function (buffer-suggestion-filter))))
                  (setf (new-html mode)
                        (ffi-buffer-get-document
                         (prompt-minibuffer
                          :input-prompt "New buffer"
                          :suggestion-function (buffer-suggestion-filter
                                                :current-is-last-p t))))
                  (setf (diff-html mode)
                        (html-diff:html-diff (old-html mode)
                                             (new-html mode)
                                             :insert-class "nyxt-diff-insert"
                                             :delete-class "nyxt-diff-delete"
                                             :replace-class "nyxt-diff-replace"))
                  (nyxt::html-set
                   (str:concat (markup:markup (:style (diff-style mode)))
                               (diff-html mode))
                   (buffer mode))
                  ;; dirty fix to set the title
                  (setf (title (buffer mode)) "diff")
                  (set-current-buffer (buffer mode))))))
