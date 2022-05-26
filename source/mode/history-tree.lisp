;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-and-set-package :nyxt/history-tree-mode
  (:documentation "Mode for history-trees."))

(define-mode history-tree-mode ()
  "Mode for history-tree listing."
  ((rememberable-p nil)
   (style
    (theme:themed-css (theme *browser*)
      (body
       :color theme:text
       :background theme:background
       :line-height "initial")
      (* :margin 0
         :padding 0
         :list-style "none")
      (a
       :color theme:text)
      ("a:hover"
       :color theme:primary)
      (".current-buffer a"
       :color theme:text)
      (".current-buffer a:hover"
       :color theme:primary)
      (".other-buffer a"
       :color theme:primary)
      (".other-buffer a:hover"
       :color theme:secondary)
      (li
       :white-space "nowrap")
      ("ul li"
       :margin-left "15px"
       :position "relative"
       :padding-left "5px")
      ("ul li::before"
       :content "' '"
       :position "absolute"
       :width "1px"
       :background-color theme:text
       :color theme:background
       :top "5px"
       :bottom "-12px"
       :left "-10px")
      ("body > ul > li:first-child::before"
       :top "12px")
      ("ul li:not(:first-child):last-child::before"
       :display "none")
      ("ul li:only-child::before"
       :display "list-item"
       :content "' '"
       :position "absolute"
       :width "1px"
       :background-color theme:text
       :color theme:background
       :top "5px"
       :bottom "7px"
       :height "7px"
       :left "-10px")
      ("ul li::after"
       :content "' '"
       :position "absolute"
       :left "-10px"
       :width "10px"
       :height "1px"
       :background-color theme:text
       :color theme:background
       :top "12px"))))
  (:toggler-command-p nil))
