;;; minibuffer.lisp --- major mode for input

(in-package :next)

(define-mode minibuffer-mode ()
    "Mode for the minibuffer."
    ((name :accessor name :initform "minibuffer")
     (keymap-schemes
      :initform
      (let ((map (make-keymap)))
        (define-key "HYPHEN" #'self-insert
          "SPACE" #'self-insert
          "C-f" #'cursor-forwards
          "M-f" #'cursor-forwards-word
          "C-b" #'cursor-backwards
          "M-b" #'cursor-backwards-word
          "M-d" #'delete-forwards-word
          "M-BACKSPACE" #'delete-backwards-word
          "Right" #'cursor-forwards
          "Left" #'cursor-backwards
          "C-d" #'delete-forwards
          "DELETE" #'delete-forwards
          "BACKSPACE" #'delete-backwards
          "C-a" #'cursor-beginning
          "C-e" #'cursor-end
          "C-k" #'kill-line
          "RETURN" #'return-input
          "C-RETURN" #'return-immediate
          "C-g" #'cancel-input
          "ESCAPE" #'cancel-input
          "C-n" #'select-next
          "C-p" #'select-previous
          "Down" #'select-next
          "Up" #'select-previous
          "C-v" #'paste
          "C-y" #'paste
          "C-w" #'copy-candidate
          "TAB" #'insert-candidate
          :keymap map)
        (list :emacs map
              ;; TODO: We could have VI bindings for the minibuffer too.
              ;; But we need to make sure it's optional + to have an indicator
              ;; for the mode.  This requires either a change of cursor or a
              ;; echo area separate from the minibuffer.
              :vi-normal map
              :vi-insert map)))))

(defclass minibuffer (buffer)
  ((default-modes :initform '(minibuffer-mode))
   (completion-function :accessor completion-function :initform nil)
   (callback-function :accessor callback-function)
   (callback-buffer :accessor callback-buffer
                    :documentation "The active buffer when the minibuffer was
brought up.  This can be useful to know which was the original buffer in the
`callback-function' in case the buffer was changed.")
   (setup-function :accessor setup-function)
   (cleanup-function :accessor cleanup-function
                     :documentation "Function run after a completion has been selected.
This should not rely on the minibuffer's content.")
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
  ;; Warning: `hide' modifies the content of the minibuffer, the
  ;; callback-function and the cleanup-function cannot rely on the minibuffer
  ;; content safely.
  (hide *interface*)
  (setf (display-mode minibuffer) :nil)
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
                (return-immediate (first (modes minibuffer)) minibuffer))))
        ;; if there's no completion function
        (return-immediate (first (modes minibuffer)) minibuffer))
    (when cleanup-function
      (funcall cleanup-function))))

(define-command return-immediate (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Return with minibuffer input, ignoring the selection."
  (hide *interface*)
  (setf (display-mode minibuffer) :nil)
  (with-slots (callback-function cleanup-function) minibuffer
    (let ((normalized-input (cl-strings:replace-all (input-buffer minibuffer)
                                                    " " " ")))
      (funcall callback-function normalized-input))
    (when cleanup-function
      (funcall cleanup-function))))

(define-command cancel-input (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Close the minibuffer query without further action."
  (setf (display-mode minibuffer) :nil)
  (with-slots (cleanup-function) minibuffer
    (when cleanup-function
      (funcall cleanup-function)))
  (hide *interface*))

(defmethod set-input ((minibuffer minibuffer) input)
  (when input
    (rpc-minibuffer-evaluate-javascript
     *interface* (rpc-window-active *interface*)
     (ps:ps (ps:chain document (write (ps:lisp input)))))))

(defmethod erase-document ((minibuffer minibuffer))
  (rpc-minibuffer-evaluate-javascript
   *interface* (rpc-window-active *interface*)
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
  (let ((active-window (rpc-window-active interface)))
    (setf (minibuffer-active active-window) t)
    (rpc-window-set-minibuffer-height interface
                                      active-window
                                      (minibuffer-open-height active-window))))

(defmethod hide ((interface remote-interface))
  (let ((active-window (rpc-window-active interface)))
    (setf (minibuffer-active active-window) nil)
    ;; TODO: We need a mode-line before we can afford to really hide the
    ;; minibuffer.  Until then, we use "blank" it.
    (with-result (url (buffer-get-url))
      (echo "~a" url))
    (rpc-window-set-minibuffer-height interface
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
           (new-cursor-position (cursor-forwards-word (first (modes minibuffer)) minibuffer))
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
          (new-cursor-position (cursor-backwards-word (first (modes minibuffer)) minibuffer)))
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

(define-command kill-whole-line (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Delete all characters in the input."
    (with-slots (input-buffer input-buffer-cursor) minibuffer
      (setf input-buffer ""
            input-buffer-cursor 0))
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
                                     ;; TODO: Instead of object string, we
                                     ;; should use the display value, as with
                                     ;; the `accessor-function' in
                                     ;; `fuzzy-match'.  Or should we do the
                                     ;; other way around, replace
                                     ;; `accessor-function' with `object-string'
                                     ;; in `fuzzy-match'?
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
      (rpc-minibuffer-evaluate-javascript
       *interface* (rpc-window-active *interface*)
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
    (rpc-minibuffer-evaluate-javascript
     *interface* (rpc-window-active *interface*)
     (ps:ps (ps:chain (ps:chain document (get-element-by-id "selected"))
                      (scroll-into-view false))))))

(define-command select-previous (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Select previous entry in minibuffer."
  (when (> (completion-cursor minibuffer) 0)
    (decf (completion-cursor minibuffer))
    (update-display minibuffer)
    (rpc-minibuffer-evaluate-javascript
     *interface* (rpc-window-active *interface*)
     (ps:ps (ps:chain (ps:chain document (get-element-by-id "selected"))
                      (scroll-into-view true))))))

(defun echo (&rest args)
  "Echo ARGS in the minibuffer.
Accepted keyword argument:

  :minibuffer
  :window

The first argument can be a format string and the following arguments will be
interpreted by `format'. "
  (let ((minibuffer (when *interface* (minibuffer *interface*)))
        (window (when *interface* (rpc-window-active *interface*))))
    (when (evenp (length args))
      (when (getf args :minibuffer)
        (setf minibuffer (getf args :minibuffer)))
      (when (getf args :window)
        (setf window (getf args :window)))
      (dolist (key (remove-if-not #'keywordp args))
        (remf args key)))
    (if (and minibuffer window)
        (unless (eql (display-mode minibuffer) :read)
          (setf (display-mode minibuffer) :echo)
          (erase-document minibuffer)
          (rpc-window-set-minibuffer-height *interface*
                                            window
                                            (minibuffer-echo-height window))
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
                         (:p (apply #'format nil args)))))))
        (log:warn "Can't echo '~a' without minibuffer or interface" (apply #'format nil args)))))

(defmethod echo-dismiss ((minibuffer minibuffer))
  (when (eql (display-mode minibuffer) :echo)
    (hide *interface*)
    ;; TODO: If we erase the document here, it will show a blank widget instead
    ;; of the minibuffer when we don't fully hide it.  We can only erase the
    ;; document when we have a mode-line we can fully hide the minibuffer.
    ;; (erase-document minibuffer)
    ))

(define-command paste (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Paste clipboard text to input."
  (insert (trivial-clipboard:text) minibuffer))

(defmethod get-candidate ((minibuffer minibuffer))
  "Return the current candidate in the minibuffer."
  (with-slots (completions completion-cursor)
      minibuffer
    (and completions
         (format nil "~a" (nth completion-cursor completions)))))

(define-command copy-candidate (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Paste clipboard text to input."
  (let ((candidate (get-candidate minibuffer)))
    (when candidate
      (trivial-clipboard:text candidate))))

(define-command insert-candidate (minibuffer-mode &optional (minibuffer (minibuffer *interface*)))
  "Paste clipboard text to input."
  (let ((candidate (get-candidate minibuffer)))
    (when candidate
      (kill-whole-line (first (modes minibuffer)) minibuffer)
      (insert candidate minibuffer))))
