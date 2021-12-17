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
       "return" 'return-input)
      scheme:emacs
      (list)))
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
INPUT is a string and RESULTS is a list of Lisp values.")
   (constructor
    (lambda (mode)
      (initialize-display mode)
      (update-evaluation-history-display mode)))))

(defun package-short-name (package)
  (first (sort (append (package-nicknames package)
                       (list (package-name package)))
               #'string<)))

(define-command return-input (&optional (repl (current-mode 'repl)))
  "Return inputted text."
  (pflet ((input-text ()
           (ps:chain document (get-element-by-id "input-buffer") value)))
    (let ((input (input-text)))
      (add-object-to-evaluation-history repl
                                        (list (format nil "~a> ~a"
                                                      (package-short-name *package*)
                                                      input)
                                              (nyxt::evaluate input)))
      (reset-input repl)
      (update-evaluation-history-display repl))))

(define-command reset-input (&optional (repl (current-mode 'repl)))
  "Clear the inputted text."
  (with-current-buffer (buffer repl)
    (pflet ((clear-input-buffer-text ()
           (setf (ps:chain document (get-element-by-id "input-buffer") value) "")))
      (clear-input-buffer-text))))

(defmethod initialize-display ((repl repl-mode))
  (let* ((content (spinneret:with-html-string
                    (:head (:style (style repl)))
                    (:body
                     (:div :id "container"
                           (:div :id "evaluation-history" "")
                           (:div :id "input"
                                 (:span :id "prompt"
                                        (format nil "~a>" (package-short-name *package*)))
                                 (:input :type "text" :id "input-buffer"))))))
         (insert-content (ps:ps (ps:chain document
                                          (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript-async (buffer repl) insert-content)))

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

(define-command-global lisp-repl ()
  "Show Lisp REPL."
  (let* ((repl-buffer (make-internal-buffer
                       :title "*Lisp REPL*"
                       :modes '(nyxt/repl-mode:repl-mode base-mode))))
    (set-current-buffer repl-buffer)
    repl-buffer))
