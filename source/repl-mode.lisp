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
       "a" 'self-insert-repl
       "space" 'self-insert-repl)
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
                       ("#evaluation-history" :flex-grow "1"
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
   (input-buffer :accessor input-buffer
                 :initform "Hello World")
   (evaluation-history :accessor evaluation-history
                       :initform (list))
   (constructor
    :initform
    (lambda (mode)
      (initialize-display mode)
      (add-object-to-evaluation-history mode "goldfish")
      (add-object-to-evaluation-history mode "sunfish")
      (update-evaluation-history-display mode)
      (update-input-buffer-display mode)))))

(defmethod initialize-display ((repl repl-mode))
  (let* ((content (markup:markup
                   (:head (:style (style repl)))
                   (:body
                    (:div :id "container"
                          (:div :id "evaluation-history" "")
                          (:div :id "input" (:span :id "prompt" "") (:span :id "input-buffer" ""))
                          (:div :id "completions" "")))))
         (insert-content (ps:ps (ps:chain document
                                          (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript (buffer repl) insert-content)))

(defmethod add-object-to-evaluation-history ((repl repl-mode) item)
  (push item (evaluation-history repl)))

(defmethod update-evaluation-history-display ((repl repl-mode))
  (flet ((generate-evaluation-history-html (repl)
           (markup:markup 
            (:ul (loop for item in (evaluation-history repl)
                       collect (markup:markup
                                (:li item)))))))
    (ffi-buffer-evaluate-javascript 
     (buffer repl)
     (ps:ps (setf (ps:chain document (get-element-by-id "evaluation-history") |innerHTML|)
                  (ps:lisp (generate-evaluation-history-html repl)))))))

(defmethod update-input-buffer-display ((repl repl-mode))
  (flet ((generate-input-buffer-html (repl)
           (markup:markup
            (:span (input-buffer repl)))))
    (ffi-buffer-evaluate-javascript
     (buffer repl)
     (ps:ps (setf (ps:chain document (get-element-by-id "prompt") |innerHTML|)
                  (ps:lisp (generate-input-buffer-html repl)))))))

(defmethod insert ((repl repl-mode) characters)
  (setf (input-buffer repl) (concatenate 'string (input-buffer repl) characters))
  (update-input-buffer-display repl))

(defun self-insert-repl ()
  (next/minibuffer-mode:self-insert
   (find-if (lambda (i) (eq (class-of i)
                            (find-class 'next/repl-mode:repl-mode)))
            (next:modes (next:current-buffer)))))

(in-package :next)

(define-command lisp-repl ()
  "Show Lisp REPL."
  (let* ((repl-buffer (make-buffer :title "*Lisp REPL*" :modes '(base-mode next/repl-mode:repl-mode))))
    (set-current-buffer repl-buffer)
    repl-buffer))
