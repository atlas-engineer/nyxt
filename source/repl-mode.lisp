(in-package :nyxt/repl-mode)

(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((keymap-scheme
    :initform
    (define-scheme "repl"
      scheme:cua
      (list
       "a" 'self-insert-repl
       "hyphen" 'self-insert-repl
       "space" 'self-insert-repl
       "C-f" 'cursor-forwards
       "M-f" 'cursor-forwards-word
       "C-right" 'cursor-forwards-word
       "C-b" 'cursor-backwards
       "M-b" 'cursor-backwards-word
       "C-left" 'cursor-backwards-word
       "M-d" 'delete-forwards-word
       "M-backspace" 'delete-backwards-word
       "right" 'cursor-forwards
       "left" 'cursor-backwards
       "C-d" 'delete-forwards
       "delete" 'delete-forwards
       "backspace" 'delete-backwards
       "C-a" 'cursor-beginning
       "home" 'cursor-beginning
       "C-e" 'cursor-end
       "end" 'cursor-end
       "C-k" 'kill-line
       "return" 'return-input)
      scheme:emacs
      (list)))
   (style :accessor style
          :initform (cl-css:css
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
   (input-buffer :accessor input-buffer
                 :initform (make-instance 'text-buffer:text-buffer)
                 :documentation "Buffer used to capture keyboard input.")
   (input-cursor :accessor input-cursor
                 :initform (make-instance 'text-buffer:cursor)
                 :documentation "Cursor used in conjunction with the input-buffer.")
   (evaluation-history :accessor evaluation-history
                       :initform (list))
   (constructor
    :initform
    (lambda (mode)
      (initialize-display mode)
      (cluffer:attach-cursor (input-cursor mode) (input-buffer mode))
      (update-evaluation-history-display mode)
      (update-input-buffer-display mode)))))

(define-command cursor-forwards (&optional (repl (current-repl)))
  "Move cursor forward by one element."
  (text-buffer::safe-forward (input-cursor repl))
  (update-input-buffer-display repl))

(define-command cursor-backwards (&optional (repl (current-repl)))
  "Move cursor backwards by one element."
  (text-buffer::safe-backward (input-cursor repl))
  (update-input-buffer-display repl))

(define-command delete-forwards (&optional (repl (current-repl)))
  "Delete character after cursor."
  (cluffer:delete-item (input-cursor repl))
  (update-input-buffer-display repl))

(define-command delete-backwards (&optional (repl (current-repl)))
  "Delete character before cursor."
  (text-buffer::delete-item-backward (input-cursor repl))
  (update-input-buffer-display repl))

(define-command cursor-beginning (&optional (repl (current-repl)))
  "Move cursor to the beginning of the buffer."
  (cluffer:beginning-of-line (input-cursor repl))
  (update-input-buffer-display repl))

(define-command cursor-end (&optional (repl (current-repl)))
  "Move cursor to the end of the buffer."
  (cluffer:end-of-line (input-cursor repl))
  (update-input-buffer-display repl))

(define-command cursor-forwards-word (&optional (repl (current-repl)))
  "Move cursor forwards a word."
  (text-buffer::move-forward-word (input-cursor repl))
  (update-input-buffer-display repl))

(define-command cursor-backwards-word (&optional (repl (current-repl)))
  "Move cursor backwards a word."
  (text-buffer::move-backward-word (input-cursor repl))
  (update-input-buffer-display repl))

(define-command delete-backwards-word (&optional (repl (current-repl)))
  "Delete backwards a word."
  (text-buffer::delete-backward-word (input-cursor repl))
  (update-input-buffer-display repl))

(define-command delete-forwards-word (&optional (repl (current-repl)))
  "Delete forwards a word."
  (text-buffer::delete-forward-word (input-cursor repl))
  (update-input-buffer-display repl))

(define-command kill-line (&optional (repl (current-repl)))
  "Delete forwards a word."
  (text-buffer::kill-forward-line (input-cursor repl))
  (update-input-buffer-display repl))

(define-command return-input (&optional (repl (current-repl)))
  "Return inputted text."
  (let ((input (str:replace-all " " " " (text-buffer::string-representation (input-buffer repl)))))
    (add-object-to-evaluation-history repl (format nil "> ~a" input))
    (add-object-to-evaluation-history repl (nyxt::evaluate input))
    (text-buffer::kill-line (input-cursor repl))
    (update-display repl)))

(defun current-repl ()
  (find-if (lambda (i) (eq (class-of i)
                           (find-class 'nyxt/repl-mode:repl-mode)))
           (nyxt:modes (nyxt::current-buffer))))

(defmethod active-repl-p ((window nyxt:window))
  (find-if (lambda (i) (eq (class-of i)
                           (find-class 'nyxt/repl-mode:repl-mode)))
           (nyxt:modes (nyxt::active-buffer window))))

(defmethod initialize-display ((repl repl-mode))
  (let* ((content (markup:markup
                   (:head (:style (style repl)))
                   (:body
                    (:div :id "container"
                          (:div :id "evaluation-history" "")
                          (:div :id "input" (:span :id "prompt" ">") (:span :id "input-buffer" ""))
                          (:div :id "suggestions" "")))))
         (insert-content (ps:ps (ps:chain document
                                          (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript (buffer repl) insert-content)))

(defmethod add-object-to-evaluation-history ((repl repl-mode) item)
  (push item (evaluation-history repl)))

(defmethod update-evaluation-history-display ((repl repl-mode))
  (flet ((generate-evaluation-history-html (repl)
           (markup:markup 
            (:ul (loop for item in (reverse (evaluation-history repl))
                       collect (markup:markup
                                (:li item)))))))
    (ffi-buffer-evaluate-javascript 
     (buffer repl)
     (ps:ps (setf (ps:chain document (get-element-by-id "evaluation-history") |innerHTML|)
                  (ps:lisp (generate-evaluation-history-html repl)))))))

(defmethod update-input-buffer-display ((repl repl-mode))
  (flet ((generate-input-buffer-html (repl)
           (cond ((eql 0 (cluffer:item-count (input-buffer repl)))
                  (markup:markup (:span :id "cursor" (markup:raw "&nbsp;"))))
                 ((eql (cluffer:cursor-position (input-cursor repl)) (cluffer:item-count (input-buffer repl)))
                  (markup:markup (:span (text-buffer::string-representation (input-buffer repl)))
                                 (:span :id "cursor" (markup:raw "&nbsp;"))))
                 (t (markup:markup (:span (subseq (text-buffer::string-representation (input-buffer repl)) 0 (cluffer:cursor-position (input-cursor repl))))
                                   (:span :id "cursor" (subseq (text-buffer::string-representation (input-buffer repl)) (cluffer:cursor-position (input-cursor repl)) (+ 1 (cluffer:cursor-position (input-cursor repl)))))
                                   (:span (subseq (text-buffer::string-representation (input-buffer repl)) (+ 1  (cluffer:cursor-position (input-cursor repl))))))))))
    (ffi-buffer-evaluate-javascript
     (buffer repl)
     (ps:ps (setf (ps:chain document (get-element-by-id "input-buffer") |innerHTML|)
                  (ps:lisp (generate-input-buffer-html repl)))))))

(defmethod update-display ((repl repl-mode))
  (update-evaluation-history-display repl)
  (update-input-buffer-display repl))

(defmethod insert ((repl repl-mode) characters)
  (cluffer:insert-item (input-cursor repl) characters)
  (update-input-buffer-display repl))

(defun self-insert-repl ()
  (nyxt/minibuffer-mode:self-insert (current-repl)))

(in-package :nyxt)

(define-command lisp-repl ()
  "Show Lisp REPL."
  (let* ((repl-buffer (make-buffer :title "*Lisp REPL*" :modes '(nyxt/repl-mode:repl-mode base-mode))))
    (set-current-buffer repl-buffer)
    repl-buffer))
