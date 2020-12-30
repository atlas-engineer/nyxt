(uiop:define-package :nyxt/history-tree-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Mode for history-trees and logs"))
(in-package :nyxt/history-tree-mode)

(define-mode history-tree-mode ()
  "Mode for history-tree listing."
  ((style
    (cl-css:css
     '((body
        :line-height "initial")
       (* :margin 0
          :padding 0
          :list-style "none")
       ("ul li"
        :margin-left "15px"
        :position "relative"
        :padding-left "5px")
       ("ul li::before"
        :content "' '"
        :position "absolute"
        :width "1px"
        :background-color "#000"
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
        :background-color "#000"
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
        :background-color "#000"
        :top "12px"))))))
