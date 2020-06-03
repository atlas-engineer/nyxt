(uiop:define-package :next/repl-mode
    (:use :common-lisp :next)
  (:import-from #:keymap #:define-scheme)
  (:export :repl-mode))
(in-package :next/repl-mode)

(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((keymap-scheme
    :initform
    (define-scheme "repl"
      scheme:cua
      (list
       "C-up" 'scroll-to-top)
      scheme:emacs
      (list
       "M-f" 'history-forwards-query)
      scheme:vi-normal
      (list
       "H" 'history-backwards)))
   (style :accessor style
          :initform (cl-css:css
                     '((* :font-family "monospace,monospace")
                       (body :margin "0"
                             :padding "0 6px")
                       ("#container" :display "flex"
                                     :flex-flow "column"
                                     :height "100%")
                       ("#input" :padding "6px 0"
                                 :border-bottom "solid 1px lightgray")
                       ("#completions" :flex-grow "1"
                                       :overflow-y "auto"
                                       :overflow-x "auto")
                       ("#cursor" :background-color "gray"
                                  :color "white")
                       ("#prompt" :padding-right "4px"
                                  :color "dimgray")
                       (ul :list-style "none"
                           :padding "0"
                           :margin "0")
                       (li :padding "2px")))
          :documentation "The CSS applied to a REPL when it is set-up.")
   (constructor
    :initform
    (lambda (mode)
      (initialize-display mode)))))

(defmethod initialize-display ((repl repl-mode))
  (markup:markup
   (:head (:style (style repl)))
   (:body
    (:div :id "container"
          (:div :id "history" "")
          (:div :id "input" (:span :id "prompt" "") (:span :id "input-buffer" ""))
          (:div :id "completions" ""))))
  (print "hello world"))
