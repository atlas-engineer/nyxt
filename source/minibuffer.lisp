;;; minibuffer.lisp --- major mode for input

(in-package :next)

(define-mode minibuffer-mode ()
    "Mode for the minibuffer."
    ((name :accessor name :initform "minibuffer")
     (keymap
      :initform
      (let ((map (make-keymap)))
        (define-key (key "HYPHEN") #'self-insert
          (key "SPACE") #'self-insert
          (key "C-f") #'cursor-forwards
          (key "M-f") #'cursor-forwards-word
          (key "C-b") #'cursor-backwards
          (key "M-b") #'cursor-backwards-word
          (key "M-d") #'delete-forwards-word
          (key "M-BACKSPACE") #'delete-backwards-word
          (key "Right") #'cursor-forwards
          (key "Left") #'cursor-backwards
          (key "C-d") #'delete-forwards
          (key "DELETE") #'delete-forwards
          (key "BACKSPACE") #'delete-backwards
          (key "C-a") #'cursor-beginning
          (key "C-e") #'cursor-end
          (key "C-k") #'kill-line
          (key "RETURN") #'return-input
          (key "C-RETURN") #'return-immediate
          (key "C-g") #'cancel-input
          (key "ESCAPE") #'cancel-input
          (key "C-n") #'select-next
          (key "C-p") #'select-previous
          (key "Down") #'select-next
          (key "Up") #'select-previous
          (key "C-v") #'paste
          (key "C-y") #'paste
          :keymap map)
        map))))

(defclass minibuffer (buffer)
  ((mode :accessor mode :initarg :mode :initform 'minibuffer-mode)
   (completion-function :accessor completion-function)
   (callback-function :accessor callback-function)
   (callback-buffer :accessor callback-buffer)
   (setup-function :accessor setup-function)
   (cleanup-function :accessor cleanup-function)
   (empty-complete-immediate :accessor empty-complete-immediate)
   (display-mode :accessor display-mode :initform :nil)
   (input-prompt :accessor input-prompt :initform "Input:")
   (input-buffer :accessor input-buffer :initform "")
   (input-buffer-cursor :accessor input-buffer-cursor :initform 0)
   (completions :accessor completions)
   (completion-cursor :accessor completion-cursor :initform 0)
   (minibuffer-style :accessor minibuffer-style
                     :initform (cl-css:css
                                '((* :font-family "monospace,monospace"
                                     :font-size "14px")
                                  (body :border-top "4px solid dimgray"
                                        :margin "0"
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
                                  (li :padding "2px")
                                  (.selected :background-color "gray"
                                             :color "white")))
                     :documentation "The CSS applied to a minibuffer when it is set-up.")))

(defmethod initialize-instance :after ((minibuffer minibuffer)
                                       &key &allow-other-keys)
  (when (symbolp (mode minibuffer))
    (setf (mode minibuffer) (make-instance (mode minibuffer)))))

(defmethod read-from-minibuffer ((minibuffer minibuffer)
                                 &key callback input-prompt completion-function setup-function
                                   cleanup-function empty-complete-immediate)
  (if input-prompt
      (setf (input-prompt minibuffer) input-prompt)
      (setf (input-prompt minibuffer) "Input:"))
  (setf (display-mode minibuffer) :read)
  (setf (callback-function minibuffer) callback)
  (setf (completion-function minibuffer) completion-function)
  (setf (completions minibuffer) nil)
  (setf (completion-cursor minibuffer) 0)
  (setf (setup-function minibuffer) setup-function)
  (setf (cleanup-function minibuffer) cleanup-function)
  (setf (empty-complete-immediate minibuffer) empty-complete-immediate)
  (setf (callback-buffer minibuffer) (active-buffer *interface*))
  (if setup-function
      (funcall setup-function)
      (setup-default minibuffer))
  (update-display minibuffer)
  (show *interface*))

(define-command return-input (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Return with minibuffer selection."
  (hide *interface*)
  (setf (display-mode minibuffer) :nil)
  (set-active-buffer *interface* (callback-buffer minibuffer))
  (with-slots (callback-function cleanup-function
               empty-complete-immediate completions completion-cursor)
      minibuffer
    (if completions
        (let ((completion (nth completion-cursor completions)))
          (if completion
              ;; if we're able to find a completion
              (funcall callback-function completion)
              ;; if we can't find a completion
              (when empty-complete-immediate
                ;; if we accept immediate output in place of completion
                (return-immediate (mode minibuffer) minibuffer))))
        ;; if there's no completion function
        (return-immediate (mode minibuffer) minibuffer))
    (when cleanup-function
      (funcall cleanup-function))))

(define-command return-immediate (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Return with minibuffer input, ignoring the selection."
  (hide *interface*)
  (setf (display-mode minibuffer) :nil)
  (set-active-buffer *interface* (callback-buffer minibuffer))
  (with-slots (callback-function cleanup-function) minibuffer
    (funcall callback-function (input-buffer minibuffer))
    (when cleanup-function
      (funcall cleanup-function))))

(define-command cancel-input (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Close the minibuffer query without further action."
  (log:debug (callback-buffer minibuffer))
  (setf (display-mode minibuffer) :nil)
  (set-active-buffer *interface* (callback-buffer minibuffer))
  (with-slots (cleanup-function) minibuffer
    (when cleanup-function
      (funcall cleanup-function)))
  (hide *interface*))

(defmethod set-input ((minibuffer minibuffer) input)
  (when input
    (%%minibuffer-evaluate-javascript
     *interface* (%%window-active *interface*)
     (ps:ps (ps:chain document (write (ps:lisp input)))))))

(defmethod erase-document ((minibuffer minibuffer))
  (%%minibuffer-evaluate-javascript
   *interface* (%%window-active *interface*)
   (ps:ps
     (ps:chain document (open))
     (ps:chain document (close)))))

(defmethod setup-default ((minibuffer minibuffer))
  (erase-document minibuffer)
  (setf (input-buffer minibuffer) "")
  (setf (input-buffer-cursor minibuffer) 0)
  (set-input minibuffer
             (cl-markup:markup
              (:head (:style (minibuffer-style minibuffer)))
              (:body
               (:div :id "container"
                     (:div :id "input" (:span :id "prompt" "") (:span :id "input-buffer" ""))
                     (:div :id "completions" ""))))))

(defmethod show ((interface remote-interface))
  (let ((active-window (%%window-active interface)))
    (setf (minibuffer-active active-window) t)
    (%%window-set-minibuffer-height interface
                                    active-window
                                    (minibuffer-open-height active-window))))

(defmethod hide ((interface remote-interface))
  (let ((active-window (%%window-active interface)))
    (setf (minibuffer-active active-window) nil)
    (%%window-set-minibuffer-height *interface*
                                    active-window
                                    (minibuffer-closed-height active-window))))

(defun insert (characters &optional (minibuffer (minibuffer *interface*)))
    (setf (input-buffer minibuffer)
          (cl-strings:insert characters
                             (input-buffer minibuffer)
                             :position (input-buffer-cursor minibuffer)))
    (incf (input-buffer-cursor minibuffer) (length characters))
    (setf (completion-cursor minibuffer) 0)
    (update-display minibuffer))

(define-command self-insert (minibuffer-mode)
  "Insert key-chord-stack in MINIBUFFER."
  (let ((key-string (key-chord-key-string (first (key-chord-stack *interface*))))
        (translation-table '(("HYPHEN" "-")
                             ;; Regular spaces are concatenated into a single
                             ;; one by HTML rendering, so we use a non-breaking
                             ;; space to avoid confusing the user.
                             ("SPACE" " "))))
    (setf key-string (or (cadr (assoc key-string translation-table :test #'string=))
                         key-string))
    (insert key-string)))

(define-command delete-forwards (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Delete character after cursor."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (unless (= input-buffer-cursor (length input-buffer))
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 input-buffer-cursor)
                         (subseq input-buffer
                                 (+ 1 input-buffer-cursor)
                                 (length input-buffer))))))
  (update-display minibuffer))

(define-command delete-backwards (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Delete character before cursor."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (unless (= input-buffer-cursor 0)
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 (- input-buffer-cursor 1))
                         (subseq input-buffer input-buffer-cursor (length input-buffer))))
      (decf input-buffer-cursor)))
  (update-display minibuffer))

(define-command cursor-forwards (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Move cursor forward by one."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (when (< input-buffer-cursor (length input-buffer))
      (incf input-buffer-cursor)))
  (update-display minibuffer))

(define-command cursor-backwards (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Move cursor backwards by one."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (when (> input-buffer-cursor 0)
      (decf input-buffer-cursor)))
  (update-display minibuffer))

(define-command cursor-beginning (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Move cursor to the beginning of the input area."
  (with-slots (input-buffer-cursor) minibuffer
    (setf input-buffer-cursor 0))
  (update-display minibuffer))

(define-command cursor-end (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Move cursor to the end of the input area."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (setf input-buffer-cursor (length input-buffer)))
  (update-display minibuffer))

(defun char-at-cursor (&optional (minibuffer (minibuffer *interface*)))
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (if (< input-buffer-cursor (length input-buffer))
        (char (input-buffer minibuffer) (input-buffer-cursor minibuffer)))))

(define-command cursor-forwards-word (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Move cursor to the end of the word at point."
  (let ((stop-characters '(#\: #\/ #\- #\. #\Space)))
    (with-slots (input-buffer input-buffer-cursor) minibuffer
      (if (intersection stop-characters (list (char-at-cursor minibuffer)))
          (loop while (and
                       (intersection stop-characters (list (char-at-cursor minibuffer)))
                       (< input-buffer-cursor (length input-buffer)))
                do (incf input-buffer-cursor))
          (loop while (and
                       (not (intersection stop-characters (list (char-at-cursor minibuffer))))
                       (< input-buffer-cursor (length input-buffer)))
                do (incf input-buffer-cursor)))))
  (update-display minibuffer)
  (input-buffer-cursor minibuffer))

;; TODO: Re-use cursor-forwards-word
(define-command cursor-backwards-word (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Move cursor to the beginning of the word at point."
  (let ((stop-characters '(#\: #\/ #\- #\. #\Space)))
    (with-slots (input-buffer input-buffer-cursor) minibuffer
      (if (intersection stop-characters (list (char-at-cursor minibuffer)))
          (loop while (and
                       (intersection stop-characters (list (char input-buffer input-buffer-cursor)))
                       (> input-buffer-cursor 0))
                do (decf input-buffer-cursor))
          (loop while (and
                       (not (intersection stop-characters (list (char-at-cursor minibuffer))))
                       (> input-buffer-cursor 0))
                do (decf input-buffer-cursor)))))
  (update-display minibuffer)
  (input-buffer-cursor minibuffer))

(define-command delete-forwards-word (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Delete characters from cursor position until the end of the word at point."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (let* ((current-cursor-position input-buffer-cursor)
           (new-cursor-position (cursor-forwards-word (mode minibuffer) minibuffer))
           (transpose-distance (- new-cursor-position current-cursor-position)))
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 current-cursor-position)
                         (subseq input-buffer new-cursor-position (length input-buffer))))
      (setf input-buffer-cursor (- input-buffer-cursor transpose-distance))))
  (update-display minibuffer))

(define-command delete-backwards-word (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Delete characters from cursor position until the beginning of the word at point."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (let ((current-cursor-position input-buffer-cursor)
          (new-cursor-position (cursor-backwards-word (mode minibuffer) minibuffer)))
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 new-cursor-position)
                         (subseq input-buffer current-cursor-position (length input-buffer))))))
  (update-display minibuffer))

(define-command kill-line (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Delete all characters from cursor position until the end of the line."
    (with-slots (input-buffer input-buffer-cursor) minibuffer
      (setf input-buffer (subseq input-buffer 0 input-buffer-cursor)))
  (update-display minibuffer))

(defun generate-input-html (input-buffer cursor-index)
  (cond ((equal "" input-buffer) (cl-markup:markup (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
        ((eql cursor-index (length input-buffer)) (cl-markup:markup (:span input-buffer)
                                                                    (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
        (t (cl-markup:markup (:span (subseq input-buffer 0 cursor-index))
                             (:span :id "cursor" (subseq input-buffer cursor-index (+ 1 cursor-index)))
                             (:span (subseq input-buffer (+ 1  cursor-index)))))))

(defun generate-completion-html (completions cursor-index)
  (cl-markup:markup (:ul (loop for i from 0 for completion in completions
                               collect
                               (cl-markup:markup
                                (:li :class (when (equal i cursor-index) "selected")
                                     :id (when (equal i cursor-index) "selected")
                                     (object-string completion)))))))

(defmethod update-display ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor completion-function
               completions completion-cursor)
      minibuffer
    (if completion-function
        (setf completions (funcall completion-function input-buffer))
        (setf completions nil))
    (let ((input-text (generate-input-html input-buffer input-buffer-cursor))
          (completion-html (generate-completion-html completions completion-cursor)))
      (%%minibuffer-evaluate-javascript
       *interface* (%%window-active *interface*)
       (ps:ps
         (setf (ps:chain document (get-element-by-id "prompt") |innerHTML|)
               (ps:lisp (input-prompt minibuffer)))
         (setf (ps:chain document (get-element-by-id "input-buffer") |innerHTML|)
               (ps:lisp input-text))
         (setf (ps:chain document (get-element-by-id "completions") |innerHTML|)
               (ps:lisp completion-html)))))))

(define-command select-next (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Select next entry in minibuffer."
  (when (< (completion-cursor minibuffer) (- (length (completions minibuffer)) 1))
    (incf (completion-cursor minibuffer))
    (update-display minibuffer)
    (%%minibuffer-evaluate-javascript
     *interface* (%%window-active *interface*)
     (ps:ps (ps:chain (ps:chain document (get-element-by-id "selected"))
                      (scroll-into-view false))))))

(define-command select-previous (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Select previous entry in minibuffer."
  (when (> (completion-cursor minibuffer) 0)
    (decf (completion-cursor minibuffer))
    (update-display minibuffer)
    (%%minibuffer-evaluate-javascript
     *interface* (%%window-active *interface*)
     (ps:ps (ps:chain (ps:chain document (get-element-by-id "selected"))
                      (scroll-into-view true))))))

(defmethod echo ((minibuffer minibuffer) text)
  (let ((active-window (%%window-active *interface*)))
    (unless (eql (display-mode minibuffer) :read)
      (setf (display-mode minibuffer) :echo)
      (erase-document minibuffer)
      (%%window-set-minibuffer-height *interface*
                                      active-window
                                      (minibuffer-echo-height active-window))
      (let ((style (cl-css:css
                    '((* :font-family "monospace,monospace"
                         :font-size "14px")
                      (body :border-top "4px solid dimgray"
                            :margin "0"
                            :padding "0 6px")
                      (p :margin "0")))))
        (set-input minibuffer
                   (cl-markup:markup
                    (:head (:style style))
                    (:body
                     (:p text))))))))

(defmethod echo-dismiss ((minibuffer minibuffer))
  (when (eql (display-mode minibuffer) :echo)
    (hide *interface*)
    (erase-document minibuffer)))

(define-command paste (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Paste clipboard text to input."
  (insert (trivial-clipboard:text) minibuffer))
