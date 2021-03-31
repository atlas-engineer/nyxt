;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/repl-mode)

(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((keymap-scheme
    (define-scheme "repl"
      scheme:cua
      (list
       "return" 'return-input)
      scheme:emacs
      (list)))
   (style (cl-css:css
           '((* :font-family "monospace,monospace")
             (body :margin "0"
                   :padding "0 6px")
             ("#container" :display "flex"
                           :flex-flow "column"
                           :height "100%")
             ("#input" :padding "6px 0"
                       :border-top "solid 1px lightgray")
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
   (evaluation-history (list))
   (constructor
    (lambda (mode)
      (initialize-display mode)
      (update-evaluation-history-display mode)))))

(define-command return-input (&optional (repl (current-repl)))
  "Return inputted text."
  (pflet ((input-text ()
           (ps:chain document (get-element-by-id "input-buffer") value)))
    (let ((input (input-text)))
      (add-object-to-evaluation-history repl (format nil "> ~a" input))
      (dolist (result (nyxt::evaluate input))
        (add-object-to-evaluation-history repl result))
      (reset-input repl)
      (update-evaluation-history-display repl))))

(define-command reset-input (&optional (repl (current-repl)))
  "Clear the inputted text."
  (with-current-buffer (buffer repl)
    (pflet ((clear-input-buffer-text ()
           (setf (ps:chain document (get-element-by-id "input-buffer") value) "")))
      (clear-input-buffer-text))))

(defun current-repl ()
  (find-submode (current-buffer) 'repl-mode))

(defmethod active-repl-p ((window nyxt:window))
  (current-repl))

(defmethod initialize-display ((repl repl-mode))
  (let* ((content (markup:markup
                   (:head (:style (style repl)))
                   (:body
                    (:div :id "container"
                          (:div :id "evaluation-history" "")
                          (:div :id "input" (:span :id "prompt" ">") (:input :type "text" :id "input-buffer"))
                          (:div :id "suggestions" "")))))
         (insert-content (ps:ps (ps:chain document
                                          (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript-async (buffer repl) insert-content)))

(defmethod add-object-to-evaluation-history ((repl repl-mode) item)
  (push item (evaluation-history repl)))

(defmethod update-evaluation-history-display ((repl repl-mode))
  (flet ((generate-evaluation-history-html (repl)
           (markup:markup
            (:ul (loop for item in (reverse (evaluation-history repl))
                       collect (markup:markup
                                (:li item)))))))
    (ffi-buffer-evaluate-javascript-async
     (buffer repl)
     (ps:ps (setf (ps:chain document (get-element-by-id "evaluation-history") |innerHTML|)
                  (ps:lisp (generate-evaluation-history-html repl)))))))

(define-command nyxt::lisp-repl ()
  "Show Lisp REPL."
  (let* ((repl-buffer (make-buffer :title "*Lisp REPL*" :modes '(nyxt/repl-mode:repl-mode base-mode))))
    (set-current-buffer repl-buffer)
    repl-buffer))
