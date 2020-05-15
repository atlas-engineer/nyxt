;;; minibuffer.lisp --- major mode for input

(in-package :next)

(declaim (type (list-of-characters) *word-separation-characters*))
(defparameter *word-separation-characters* '(#\: #\/ #\- #\. #\Space #\ )
  "Characters delimiting words (space, colon, slash, dot, etc).")

(defun word-separation-character-p (char)
  (intersection *word-separation-characters* (list char)))

;; TODO: Move minibuffer-mode to a separate package?
(define-mode minibuffer-mode ()
  "Mode for the minibuffer."
  ((keymap-scheme
    :initform
    (define-scheme "minibuffer"
      scheme:cua
      (list
       "hyphen" 'self-insert
       "space" 'self-insert
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
       "return" 'return-input
       "C-return" 'return-immediate
       "C-g" 'cancel-input
       "escape" 'cancel-input
       "C-n" 'select-next
       "C-p" 'select-previous
       "M-n" 'select-next-follow
       "M-p" 'select-previous-follow
       "button4" 'select-previous
       "button5" 'select-next
       "down" 'select-next
       "up" 'select-previous
       "C-v" 'minibuffer-paste
       "C-y" 'minibuffer-paste
       "C-w" 'copy-candidate
       "C-c" 'copy-candidate
       "tab" 'insert-candidate
       "M-h" 'minibuffer-history
       "C-space" 'minibuffer-toggle-mark
       "shift-space" 'minibuffer-toggle-mark
       "M-space" 'minibuffer-toggle-mark
       "M-a" 'minibuffer-mark-all
       "M-u" 'minibuffer-unmark-all))
    ;; TODO: We could have VI bindings for the minibuffer too.
    ;; But we need to make sure it's optional + to have an indicator
    ;; for the mode.
    )))

(defclass-export minibuffer (buffer)
  ((default-modes :initarg :default-modes
                  :initform '(minibuffer-mode))
   (completion-function :initarg :completion-function :accessor completion-function
                        :initform nil
                        :documentation "Function that takes the user
                        input string and returns a list of candidate
                        strings")
   (callback :initarg :callback :accessor callback
             :initform nil
             :documentation "Function to call over the selected candidate.")
   (callback-buffer :initarg :callback-buffer
                    :accessor callback-buffer
                    :initform (when *browser* (current-buffer))
                    :documentation "The active buffer when the
                    minibuffer was brought up.  This can be useful to
                    know which was the original buffer in the
                    `callback' in case the buffer was changed.")
   (setup-function :initarg :setup-function :accessor setup-function
                   :initform #'setup-default
                   :documentation "Function of no argument that fills
                   the `content' on when the minibuffer is created.
                   Called only once.")
   (cleanup-function :initarg :cleanup-function :accessor cleanup-function
                     :initform nil
                     :documentation "Function run after a completion has been selected.
This should not rely on the minibuffer's content.")
   (changed-callback :initarg :changed-callback
                     :accessor changed-callback
                     :initform nil
                     :documentation "Function to call whenever a change happens.")
   (empty-complete-immediate :initarg :empty-complete-immediate ; TODO: Rename?
                             :accessor empty-complete-immediate
                             :initform nil
                             :documentation "If non-nil, allow input
                             matching no candidates.")
   (input-prompt :initarg :input-prompt :accessor input-prompt :initform "Input"
                 :type string)
   (input-buffer :initarg :input-buffer :reader input-buffer :initform ""
                 :type string
                 :documentation "Initial text to place at the prompt, ready to edit.")
   (input-cursor-position :accessor input-cursor-position :initform 0 :type integer)
   (invisible-input-p :initarg :invisible-input-p :accessor invisible-input-p
                      :initform nil
                      :documentation "If non-nil, input is replaced by
                      placeholder character.  This is useful to
                      conceal passwords.")
   (history :initarg :history :accessor history
            :initform (minibuffer-generic-history *browser*)
            :type containers:ring-buffer-reverse
            :documentation "History of inputs for the minibuffer. If
            nil, no history is used.")
   (multi-selection-p :initarg :multi-selection-p :accessor multi-selection-p
                      :initform nil
                      :type boolean
                      :documentation "If non-nil, allow for selecting
                      multiple candidates.")
   (completions :accessor completions :initform nil)
   (marked-completions :accessor marked-completions :initform nil)
   (show-completion-count :accessor show-completion-count
                          :initarg :show-completion-count :initform t
                          :type boolean
                          :documentation "Show the number of chosen
                          candidates inside brackets. In the case of
                          yes/no questions, there is no need for it.")
   (completion-head :accessor completion-head :initform 0)
   (completion-cursor :accessor completion-cursor :initform 0)
   (content :initform "" :type string
            :documentation "The HTML content of the minibuffer.")
   (max-lines :initarg :max-lines
              :accessor max-lines
              :type integer
              :initform 10
              :documentation "Max number of candidate lines to show.
              You will want edit this to match the changes done to
              `minibuffer-font-size', `minibuffer-line-height' and
              `minibuffer-open-height'.")
   (minibuffer-font-size :initarg :minibuffer-font-size
                         :accessor minibuffer-font-size
                         :type string
                         :initform "14px"
                         :documentation "CSS font size for the
                         minibuffer.  Value is a string, e.g. '1em'.
                         You might want to configure the value on
                         HiDPI screen.")
   (minibuffer-line-height :initarg :minibuffer-line-height
                           :accessor minibuffer-line-height
                           :type string
                           :initform "18px"
                           :documentation "CSS line height for the
                           minibuffer.  Value is a string, e.g. '1em'.
                           You might want to configure the value on
                           HiDPI screen.")
   (minibuffer-style :accessor minibuffer-style
                     :initform (cl-css:css
                                '((* :font-family "monospace,monospace")
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
                                  (.marked :background-color "darkgray"
                                           :font-weight "bold"
                                           :color "white")
                                  (.selected :background-color "gray"
                                             :color "white")))
                     :documentation "The CSS applied to a minibuffer when it is set-up."))
  (:documentation "The minibuffer is the interface for user interactions.  Each
prompt spawns a new minibuffer object: this makes it possible to nest minibuffer
calls, such as invoking `minibuffer-history'."))

(export-always '*minibuffer-class*)
(defparameter *minibuffer-class* 'minibuffer)
(export-always 'make-minibuffer)
(defun make-minibuffer (&key
                          (default-modes nil explicit-default-modes)
                          (completion-function nil explicit-completion-function)
                          (callback nil explicit-callback)
                          (callback-buffer nil explicit-callback-buffer)
                          (setup-function nil explicit-setup-function)
                          (cleanup-function nil explicit-cleanup-function)
                          (changed-callback nil explicit-changed-callback)
                          (empty-complete-immediate nil explicit-empty-complete-immediate)
                          (input-prompt nil explicit-input-prompt)
                          (input-buffer nil explicit-input-buffer)
                          (invisible-input-p nil explicit-invisible-input-p)
                          (show-completion-count t explicit-show-completion-count) ; TODO: Rename to hide-completion-count and reverse default value.
                          (history nil explicit-history)
                          (multi-selection-p nil explicit-multi-selection-p))
  "See the `minibuffer' class for the argument documentation."
  (apply #'make-instance *minibuffer-class*
         `(,@(if explicit-default-modes
                 `(:default-modes ,default-modes)
                 '())
           ,@(if explicit-completion-function
                 `(:completion-function ,completion-function)
                 '())
           ,@(if explicit-callback
                 `(:callback ,callback)
                 '())
           ,@(if explicit-callback-buffer
                 `(:callback-buffer ,callback-buffer)
                 '())
           ,@(if explicit-setup-function
                 `(:setup-function ,setup-function)
                 '())
           ,@(if explicit-cleanup-function
                `(:cleanup-function ,cleanup-function)
                '())
           ,@(if explicit-changed-callback
                `(:changed-callback ,changed-callback)
                '())
           ,@(if explicit-empty-complete-immediate
                 `(:empty-complete-immediate ,empty-complete-immediate)
                 '())
           ,@(if explicit-input-prompt
                 `(:input-prompt ,input-prompt)
                 '())
           ,@(if explicit-input-buffer
                 `(:input-buffer ,input-buffer)
                 '())
           ,@(if explicit-invisible-input-p
                 `(:invisible-input-p ,invisible-input-p)
                 '())
           ,@(if explicit-show-completion-count
                 `(:show-completion-count ,show-completion-count)
                 '())
           ,@(if explicit-history
                 `(:history ,history)
                 '())
           ,@(if explicit-multi-selection-p
                 `(:multi-selection-p ,multi-selection-p)
                 '()))))

(defmethod update-candidates ((minibuffer minibuffer))
  (with-slots (completion-function completions input-buffer empty-complete-immediate )
      minibuffer
    (if completion-function
        (setf completions (funcall-safely completion-function minibuffer))
        (setf completions nil))
    (when (and empty-complete-immediate
               (not (str:emptyp input-buffer)))
      ;; Don't add input-buffer to completions that don't accept arbitrary
      ;; inputs (i.e. empty-complete-immediate is nil).
      (push input-buffer completions))))

(defmethod (setf input-buffer) (value (minibuffer minibuffer))
  "Reset the minibuffer state on every input change.
  This is necessary or else completion cursor / head could be beyond
  the updated list length."
  (with-slots (input-buffer completion-cursor completion-head) minibuffer
    (setf input-buffer value)
    (update-candidates minibuffer)
    (setf completion-cursor 0)
    (setf completion-head 0)))

(defmethod content ((minibuffer minibuffer))
  (slot-value minibuffer 'content))

(defmethod minibuffer-line-style ((minibuffer minibuffer))
  (cl-css:css
   `((* :font-size ,(minibuffer-font-size minibuffer)
        :line-height ,(minibuffer-line-height minibuffer)))))

(defmethod (setf content) (html-content minibuffer)
  "Set the `content' of the MINIBUFFER to HTML-CONTENT.
   This runs a call"
  (setf (slot-value minibuffer 'content) html-content)
  (ffi-minibuffer-evaluate-javascript
   (current-window)
   (ps:ps (ps:chain document
                    (write (ps:lisp (content minibuffer)))))))

(defmethod initialize-instance :after ((minibuffer minibuffer) &key)
  (hooks:run-hook (minibuffer-make-hook *browser*) minibuffer)
  ;; We don't want to show the input in the candidate list when invisible.
  (unless (completion-function minibuffer)
    ;; If we have no completion function, then we have no candidates beside
    ;; immediate input, so we must allow them as valid completion.
    (setf (empty-complete-immediate minibuffer) t))
  (setf (empty-complete-immediate minibuffer)
        (if (invisible-input-p minibuffer)
            nil
            (empty-complete-immediate minibuffer)))
  (initialize-modes minibuffer))

(declaim (ftype (function (minibuffer &key (:callback function))) read-from-minibuffer))
(export-always 'read-from-minibuffer)
(defun read-from-minibuffer (minibuffer &key callback)
  "Open the minibuffer, ready for user input.
Example use:

\(read-from-minibuffer
 (make-minibuffer
  :completion-function #'my-completion-filter))

See the documentation of `minibuffer' to know more about the minibuffer options."
  (when callback
    ;; We need a :callback key argument so that `read-from-minibuffer' can be
    ;; called in `with-result'.
    (setf (callback minibuffer) callback))
  ;; TODO: Shall we leave it to the caller to decide which is the callback-buffer?
  (setf (callback-buffer minibuffer) (current-buffer))
  (if *keep-alive*
      (match (setup-function minibuffer)
        ((guard f f) (funcall f minibuffer)))
      (handler-case (match (setup-function minibuffer)
                      ((guard f f) (funcall f minibuffer)))
        (error (c)
          (echo-warning "Minibuffer error: ~a" c)
          (return-from read-from-minibuffer))))
  (state-changed minibuffer)
  (update-display minibuffer)
  (push minibuffer (active-minibuffers (current-window)))
  (apply #'show
         (unless (completion-function minibuffer)
           ;; We don't need so much height since there is no candidate to display.
           (list :height (minibuffer-open-single-line-height (current-window))))))

(define-command return-input (&optional (minibuffer (current-minibuffer)))
  "Return with minibuffer selection."
  (with-slots (callback empty-complete-immediate completions completion-cursor
               invisible-input-p
               multi-selection-p marked-completions input-buffer)
      minibuffer
    (match (or marked-completions
               (and completions
                    (list (nth completion-cursor completions)))
               (and empty-complete-immediate
                    (list input-buffer)))
      ((guard completions completions)
       ;; Note that "immediate input" is also in completions, so it's caught here.
       (setf completions
             (mapcar (lambda (completion) (if (stringp completion)
                                              (str:replace-all " " " " completion)
                                              completion))
                     completions))
       (funcall-safely callback (if multi-selection-p
                                    completions
                                    (first completions))))
      (nil (when invisible-input-p
             (funcall-safely callback (str:replace-all " " " " input-buffer))))))
  (quit-minibuffer minibuffer))

(define-command return-immediate (&optional (minibuffer (current-minibuffer)))
  "Return with minibuffer input, ignoring the selection."
  (with-slots (callback) minibuffer
    (let ((normalized-input (str:replace-all " " " " (input-buffer minibuffer))))
      (funcall-safely callback normalized-input)))
  (quit-minibuffer minibuffer))

(defun quit-minibuffer (&optional (minibuffer (current-minibuffer)))
  (unless (or (null (history minibuffer))
              (str:empty? (input-buffer minibuffer)))
    (let ((normalized-input (str:replace-all " " " " (input-buffer minibuffer))))
      (containers:insert-item (history minibuffer) normalized-input)))
  (cancel-input minibuffer))

(define-command cancel-input (&optional (minibuffer (current-minibuffer)))
  "Close the minibuffer query without further action."
  (match (cleanup-function minibuffer)
    ((guard f f) (funcall-safely f)))
  (hide minibuffer))

(export-always 'erase-input)
(defmethod erase-input ((minibuffer minibuffer))
  "Clean-up the minibuffer input."
  (setf (input-buffer minibuffer) "")
  (setf (input-cursor-position minibuffer) 0)
  (setf (content minibuffer) ""))

;; TODO: Move `erase-document' into the `show' function?
(defmethod erase-document ((minibuffer minibuffer))
  (evaluate-script minibuffer
                   (ps:ps
                     (ps:chain document (open))
                     (ps:chain document (close)))))

(defmethod setup-default ((minibuffer minibuffer))
  (erase-document minibuffer)
  (update-candidates minibuffer)
  (setf (input-cursor-position minibuffer) 0)
  (setf (content minibuffer)
        (markup:markup
         (:head (:style (minibuffer-style minibuffer))
                (:style (minibuffer-line-style minibuffer)))
         (:body
          (:div :id "container"
                (:div :id "input" (:span :id "prompt" "") (:span :id "input-buffer" ""))
                (:div :id "completions" ""))))))

(defmethod evaluate-script ((minibuffer minibuffer) script)
  "Evaluate SCRIPT into MINIBUFFER's webview.
The new webview HTML content it set as the MINIBUFFER's `content'."
  (when minibuffer
    (with-result (new-content (ffi-minibuffer-evaluate-javascript
                               (current-window)
                               (str:concat
                                script
                                ;; Return the new HTML body.
                                (ps:ps (ps:chain document body |outerHTML|)))))
      ;; Since the script may have changed the content on the platform port,
      ;; we need to update the slot's value.
      (setf (slot-value minibuffer 'content) new-content))))

(defun show (&key
               (minibuffer (first (active-minibuffers (current-window))))
               height)
  "Show the last active minibuffer, if any."
  (when minibuffer
    (ffi-window-set-minibuffer-height
     (current-window)
     (or height
         (minibuffer-open-height (current-window))))))

(defun hide (minibuffer)
  "Hide MINIBUFFER and display next active one, if any."
  ;; Note that MINIBUFFER is not necessarily first in the list, e.g. a new
  ;; minibuffer was invoked before the old one reaches here.
  (alex:deletef (active-minibuffers (current-window)) minibuffer)
  (if (active-minibuffers (current-window))
      (progn
        (show)
        ;; We need to refresh so that the nested minibuffers don't have to do it.
        (state-changed (first (active-minibuffers (current-window))))
        (update-display (first (active-minibuffers (current-window)))))
      (progn
        (ffi-window-set-minibuffer-height (current-window) 0))))

(defun insert (characters &optional (minibuffer (current-minibuffer)))
  (with-accessors ((buffer input-buffer) (cursor input-cursor-position)) minibuffer
    ;; Set cursor before buffer to ensure cursor is never higher than buffer length.
    (let ((old-cursor-position cursor))
      (incf cursor (length characters))
      (setf buffer (str:insert characters
                               old-cursor-position
                               buffer))))
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command self-insert ()
  "Insert first key from `*browser*' `key-stack' in the minibuffer."
  (let ((key-string (keymap:key-value (first (key-stack *browser*))))
        (translation-table '(("hyphen" "-")
                             ;; Regular spaces are concatenated into a single
                             ;; one by HTML rendering, so we use a non-breaking
                             ;; space to avoid confusing the user.
                             ("space" " "))))
    (setf key-string (or (cadr (assoc key-string translation-table :test #'string=))
                         key-string))
    (insert key-string)))

(define-command delete-forwards (&optional (minibuffer (current-minibuffer)))
  "Delete character after cursor."
  (with-accessors ((buffer input-buffer) (cursor input-cursor-position)) minibuffer
    (unless (= cursor (length buffer))
      (setf buffer
            (concatenate 'string
                         (subseq buffer 0 cursor)
                         (subseq buffer
                                 (+ 1 cursor)
                                 (length buffer))))))
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command delete-backwards (&optional (minibuffer (current-minibuffer)))
  "Delete character before cursor."
  (with-accessors ((buffer input-buffer) (cursor input-cursor-position)) minibuffer
    (unless (= cursor 0)
      (let ((old-cursor cursor))
        ;; Change cursor before buffer or else we could have a cursor that's
        ;; beyond the buffer length.
        (decf cursor)
        (setf buffer
              (concatenate 'string
                           (subseq buffer 0 (- old-cursor 1))
                           (subseq buffer old-cursor (length buffer)))))))
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command cursor-forwards (&optional (minibuffer (current-minibuffer)))
  "Move cursor forward by one."
  (with-slots (input-buffer input-cursor-position) minibuffer
    (when (< input-cursor-position (length input-buffer))
      (incf input-cursor-position)))
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command cursor-backwards (&optional (minibuffer (current-minibuffer)))
  "Move cursor backwards by one."
  (with-slots (input-cursor-position) minibuffer
    (when (> input-cursor-position 0)
      (decf input-cursor-position)))
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command cursor-beginning (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the beginning of the input area."
  (with-slots (input-cursor-position) minibuffer
    (setf input-cursor-position 0))
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command cursor-end (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the end of the input area."
  (with-slots (input-buffer input-cursor-position) minibuffer
    (setf input-cursor-position (length input-buffer)))
  (state-changed minibuffer)
  (update-display minibuffer))

(defun char-at-cursor (&optional (minibuffer (current-minibuffer)))
  "Return the character the cursor it at in the minibuffer."
  (with-slots (input-buffer input-cursor-position) minibuffer
    (if (< input-cursor-position (length input-buffer))
        (char (input-buffer minibuffer) (input-cursor-position minibuffer)))))

(defun char-at-position (input position)
  "Return the character at `position' in `input', or nil."
  (if (< position (length input))
      (char input position)))

(define-command cursor-forwards-word (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the end of the word at point."
  (with-slots (input-buffer input-cursor-position) minibuffer
    (if (intersection *word-separation-characters* (list (char-at-cursor minibuffer)))
        (loop while (and
                     (intersection *word-separation-characters* (list (char-at-cursor minibuffer)))
                     (< input-cursor-position (length input-buffer)))
              do (incf input-cursor-position))
        (loop while (and
                     (not (intersection *word-separation-characters* (list (char-at-cursor minibuffer))))
                     (< input-cursor-position (length input-buffer)))
              do (incf input-cursor-position))))
  (state-changed minibuffer)
  (update-display minibuffer)
  (input-cursor-position minibuffer))

(defun backwards-word-position (input position)
  "Return the cursor position to move one word backwards."
  (flet ((on-delimiter-p (input position)
           (word-separation-character-p (char-at-position input position)))
         (ahead-delimiter-p (input position)
           ;; In the minibuffer, the cursor is actually one position *after* the last char.
           (word-separation-character-p (char-at-position input
                                                          (max 0 (1- position))))))
    ;; Move past all delimiters at the end of input.
    (loop while (and (ahead-delimiter-p input position)
                     (plusp position))
       do (decf position))
    ;; Move past one word.
    (loop while (and (not (ahead-delimiter-p input position))
                     (plusp position))
       do (decf position))
    ;; Don't erase the last delimiter.
    (if (on-delimiter-p input position)
        (incf position)
        position)))

(defvar +white-spaces+ '(#\space #\no-break_space))

(defun word-start (s position)
  "Return the index of the beginning word at POSITION in string S."
  (apply #'max
         (mapcar (lambda (char)
                   (let ((pos (position char s
                                        :end position
                                        :from-end t)))
                     (if pos
                         (1+ pos)
                         0)))
                 +white-spaces+)))

(defun word-end (s position)
  "Return the index of the end of the word at POSITION in string S."
  (apply #'min
         (mapcar (lambda (char)
                   (or (position char s :start position)
                       (length s)))
                 +white-spaces+)))

(defun word-at-cursor (minibuffer)
  "Return word at cursor.
If cursor is between two words, return the first one."
  (with-accessors ((input-buffer input-buffer) (input-cursor-position input-cursor-position)) minibuffer
    (subseq input-buffer
            (word-start input-buffer input-cursor-position)
            (word-end input-buffer input-cursor-position))))

;; TODO: Re-use cursor-forwards-word
(define-command cursor-backwards-word (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the beginning of the word at point."
  (with-slots (input-buffer input-cursor-position) minibuffer
    (setf input-cursor-position (backwards-word-position input-buffer input-cursor-position)))
  (state-changed minibuffer)
  (update-display minibuffer)
  (input-cursor-position minibuffer))

(define-command delete-forwards-word (&optional (minibuffer (current-minibuffer)))
  "Delete characters from cursor position until the end of the word at point."
  (with-accessors ((buffer input-buffer) (cursor input-cursor-position)) minibuffer
    (let* ((current-cursor-position cursor)
           (new-cursor-position (cursor-forwards-word minibuffer))
           (transpose-distance (- new-cursor-position current-cursor-position))
           (new-buffer (concatenate 'string
                         (subseq buffer 0 current-cursor-position)
                         (subseq buffer new-cursor-position (length buffer)))))
      (setf cursor (- cursor transpose-distance))
      (setf buffer new-buffer)))
  (state-changed minibuffer)
  (update-display minibuffer))

(defun %delete-backwards-word (input position)
  "Delete one word backwards, starting from `position' in `input'.
  Return two values: the new string and the new cursor position."
  (let ((new-position (backwards-word-position input position)))
    (values (concatenate 'string
                         (str:substring 0 new-position input)
                         (str:substring position nil input))
            new-position)))

(define-command delete-backwards-word (&optional (minibuffer (current-minibuffer)))
  "Delete characters from cursor position until the beginning of the word at point."
  (with-accessors ((buffer input-buffer) (cursor input-cursor-position)) minibuffer
    (multiple-value-bind (new-string new-position) (%delete-backwards-word buffer cursor)
      ;; Set cursor before buffer to ensure cursor is never higher than buffer length.
      (setf cursor new-position
            buffer new-string)))
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command kill-line (&optional (minibuffer (current-minibuffer)))
  "Delete all characters from cursor position until the end of the line."
  (with-accessors ((buffer input-buffer) (cursor input-cursor-position)) minibuffer
    (setf buffer (subseq buffer 0 cursor)))
  (state-changed minibuffer)
  (update-display minibuffer))

(define-command kill-whole-line (&optional (minibuffer (current-minibuffer)))
  "Delete all characters in the input."
  (with-accessors ((buffer input-buffer) (cursor input-cursor-position)) minibuffer
    ;; Set cursor before buffer to ensure cursor is never higher than buffer length.
    (setf cursor 0
          buffer ""))
  (state-changed minibuffer)
  (update-display minibuffer))

(defun generate-input-html (input-buffer cursor-index)
  (cond ((equal "" input-buffer) (markup:markup (:span :id "cursor" (markup:raw "&nbsp;"))))
        ((eql cursor-index (length input-buffer)) (markup:markup (:span input-buffer)
                                                                    (:span :id "cursor" (markup:raw "&nbsp;"))))
        (t (markup:markup (:span (subseq input-buffer 0 cursor-index))
                             (:span :id "cursor" (subseq input-buffer cursor-index (+ 1 cursor-index)))
                             (:span (subseq input-buffer (+ 1  cursor-index)))))))

(defun generate-input-html-invisible (input-buffer cursor-index)
  (let ((input-buffer-password (make-string (length input-buffer) :initial-element #\*)))
    (cond ((equal "" input-buffer-password) (markup:markup (:span :id "cursor" (markup:raw "&nbsp;"))))
          ((eql cursor-index (length input-buffer-password))
           (markup:markup (:span input-buffer-password)
                             (:span :id "cursor" (markup:raw "&nbsp;"))))
          (t (markup:markup (:span (subseq input-buffer-password 0 cursor-index))
                               (:span :id "cursor" (subseq input-buffer-password cursor-index (+ 1 cursor-index)))
                               (:span (subseq input-buffer-password (+ 1  cursor-index))))))))

(defun generate-completion-html (completions cursor-index minibuffer)
  (let ((lines (max-lines minibuffer))) ; TODO: Compute lines dynamically.
    (when (>= (- cursor-index (completion-head minibuffer)) lines)
      (setf (completion-head minibuffer)
            (min
             (+ (completion-head minibuffer) 1)
             (length completions))))
    (when (< (- cursor-index (completion-head minibuffer)) 0)
      (setf (completion-head minibuffer)
            (max
             (- (completion-head minibuffer) 1)
             0)))
    (markup:markup (:ul (loop repeat lines
                                 for i from (completion-head minibuffer)
                                 for completion in (nthcdr i completions)
                                 collect
                                 (markup:markup
                                  (:li :class (let ((selected-p (= i cursor-index))
                                                    (marked-p (member completion (marked-completions minibuffer)))
                                                    (head-p (= i (completion-head minibuffer))))
                                                (str:join " " (delete-if #'null (list (and marked-p "marked")
                                                                                      (and selected-p "selected")
                                                                                      (and head-p "head")))))
                                       :id (cond ; TODO: Unused?
                                             ((= i cursor-index) "selected")
                                             ((member completion (marked-completions minibuffer)) "marked")
                                             ((= i (completion-head minibuffer)) "head"))
                                       (match (object-display completion)
                                         ((guard s (not (str:emptyp s))) s)
                                         (_ " ")))))))))

(defmethod set-completions ((minibuffer minibuffer) completions)
  "Set the completions and update the display."
  (setf (completions minibuffer) completions)
  (state-changed minibuffer)
  (update-display minibuffer))

(export-always 'update-display)
(defmethod update-display ((minibuffer minibuffer))
  (with-slots (input-buffer input-cursor-position
               completions marked-completions completion-cursor)
      minibuffer
    (let ((input-text (if (invisible-input-p minibuffer)
                          (generate-input-html-invisible input-buffer input-cursor-position)
                          (generate-input-html input-buffer input-cursor-position)))
          (completion-html (generate-completion-html completions completion-cursor minibuffer)))
      (evaluate-script minibuffer
                       (ps:ps
                         (setf (ps:chain document (get-element-by-id "prompt") |innerHTML|)
                               (ps:lisp
                                (format nil "~a~a:"
                                        (input-prompt minibuffer)
                                        (cond
                                          ((not completions)
                                           "")
                                          ((not (show-completion-count minibuffer))
                                           "")
                                          (marked-completions
                                           (format nil " [~a/~a]"
                                                   (length marked-completions)
                                                   (length completions)))
                                          ((and (not marked-completions)
                                                (multi-selection-p minibuffer))
                                           (format nil " [0/~a]"
                                                   (length completions)))
                                          ((not marked-completions)
                                           (format nil " [~a]"
                                                   (length completions)))
                                          (t
                                           "[?]")))))
                         (setf (ps:chain document (get-element-by-id "input-buffer") |innerHTML|)
                               (ps:lisp input-text))
                         (setf (ps:chain document (get-element-by-id "completions") |innerHTML|)
                               (ps:lisp completion-html)))))))

(defmethod state-changed ((minibuffer minibuffer))
  (when (changed-callback minibuffer)
    (funcall-safely (changed-callback minibuffer))))

(define-command select-next (&optional (minibuffer (current-minibuffer)))
  "Select next entry in minibuffer."
  (when (< (completion-cursor minibuffer) (- (length (completions minibuffer)) 1))
    (incf (completion-cursor minibuffer))
    (state-changed minibuffer)
    (update-display minibuffer)
    (evaluate-script minibuffer
                     (ps:ps (ps:chain (ps:chain document (get-element-by-id "selected"))
                                      (scroll-into-view false))))))

(define-command select-next-follow (&optional (minibuffer (current-minibuffer)))
  "Select next entry in minibuffer and focus the referencing hint/match
if there is one such."
  (select-next minibuffer)
  (update-selection-highlight-hint :follow t :scroll t))

(define-command select-previous (&optional (minibuffer (current-minibuffer)))
  "Select previous entry in minibuffer."
  (when (> (completion-cursor minibuffer) 0)
    (decf (completion-cursor minibuffer))
    (state-changed minibuffer)
    (update-display minibuffer)
    (evaluate-script minibuffer
                     (ps:ps (ps:chain (ps:chain document (get-element-by-id "head"))
                                      (scroll-into-view false))))))

(define-command select-previous-follow (&optional (minibuffer (current-minibuffer)))
  "Select previous entry in minibuffer and focus the referencing hint/match
if there is one such."
  (select-previous minibuffer)
  (update-selection-highlight-hint :follow t :scroll t))

(defun %echo (text &key (message (list text))
                     (window (current-window)))
  "Echo TEXT in the message buffer.
When given, add MESSAGE to the `browser's `message-content' (for the `messages' buffer).
MESSAGE is a cl-markup list."
  (unless (or (null message)
              (null *browser*)
              (equal message '("")))
    (push `(:p (:i "["
                   ,(local-time:format-timestring
                     nil
                     (local-time:now)
                     :format local-time:+asctime-format+)
                   "]")
               " "
               ,@message)
          (messages-content *browser*)))
  ;; This function could be called before the renderer up.
  (when window
    (print-message text)))

(export-always 'echo)
(defun echo (&rest args)
  "Echo ARGS in the message view.
The first argument can be a format string and the following arguments will be
interpreted by `format'.
Untrusted content should be given as argument with a format string."
  (handler-case
      (let ((text (apply #'format nil args)))
        ;; We might still want to echo the empty string to clear the echo area.
        (%echo text)
        (unless (str:emptyp text)
          (log:info "~a" text)))
    (error ()
      (log:warn "Failed to echo these args: ~s
Possible improvements:
- Pass multiple arguments and use format strings for untrusted content. Don't pre-construct a single string that could contain tildes.
  Example: do (echo \"directory is\ ~~a \"~~/Downloads/\")
           instead of (echo \"directory is ~~/Downloads/\")
- Use `echo-safe' or use the ~~s directive directly." args))))

(export-always 'echo-safe)
(defun echo-safe (&rest args)
  "Echo strings without expanding format directives unlike other `echo' commands."
  (let ((text (str:join " " args)))
    (%echo text)
    (unless (str:emptyp text)
      (log:info "~s" text))))

(export-always 'echo-warning)
(defun echo-warning (&rest args)
  "Like `echo' but prefix with \"Warning\" and output to the standard error."
  (let ((text (apply #'format nil args)))
    (%echo text
                  :message `((:b "Warning:") " " ,text))
    (unless (str:emptyp text)
      (log:warn "~a" text))))

(export-always 'echo-dismiss)
(defmethod echo-dismiss ()
  ;; Don't add to the *Messages* buffer:
  (%echo "" :message nil))

(declaim (ftype (function (containers:ring-buffer-reverse) string) ring-insert-clipboard))
(export-always 'ring-insert-clipboard)
(defun ring-insert-clipboard (ring)
  "Check if clipboard-content is most recent entry in RING.
If not, insert clipboard-content into RING.
Return most recent entry in RING."
  (let ((clipboard-content (trivial-clipboard:text)))
    (unless (string= clipboard-content (unless (containers:empty-p ring)
                                         (containers:first-item ring)))
      (containers:insert-item ring clipboard-content)))
  (string (containers:first-item ring)))

(define-command minibuffer-paste (&optional (minibuffer (current-minibuffer)))
  "Paste clipboard text to input."
  (insert (ring-insert-clipboard (clipboard-ring *browser*)) minibuffer))

(export-always 'get-candidate)
(defmethod get-candidate ((minibuffer minibuffer))
  "Return the string for the current candidate in the minibuffer."
  (with-slots (completions completion-cursor)
      minibuffer
    (and completions
         (object-string (nth completion-cursor completions)))))

(defmethod get-marked-candidates ((minibuffer minibuffer))
  "Return the list of strings for the marked candidate in the minibuffer."
  (mapcar #'object-string (marked-completions minibuffer)))

(define-command copy-candidate (&optional (minibuffer (current-minibuffer)))
  "Copy candidate to clipboard."
  (let ((candidate (if (and (multi-selection-p minibuffer)
                            (not (null (marked-completions minibuffer))))
                       (str:join (string #\newline) (get-marked-candidates minibuffer))
                       (get-candidate minibuffer))))
    (unless (str:emptyp candidate)
      (trivial-clipboard:text candidate))))

(define-command insert-candidate (&optional (minibuffer (current-minibuffer)))
  "Paste selected candidate to input.
As a special case, if the inserted candidate is a URI, we decode it to make it
readable."
  (let ((candidate (get-candidate minibuffer)))
    (when candidate
      (kill-whole-line minibuffer)
      (insert (if (valid-url-p candidate)
                  (url-display candidate)
                  candidate)
              minibuffer))))

(declaim (ftype (function (containers:ring-buffer-reverse))
                minibuffer-history-completion-filter))
(defun minibuffer-history-completion-filter (history)
  (when history
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer)
                   (delete-duplicates (containers:container->list history)
                                      :test #'equal)))))

(define-command minibuffer-history (&optional (minibuffer (current-minibuffer)))
  "Choose a minibuffer input history entry to insert as input."
  (when (history minibuffer)
    (with-result (input (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Input history"
                          :history nil
                          :completion-function (minibuffer-history-completion-filter (history minibuffer)))))
      (unless (str:empty? input)
        (log:debug input minibuffer)
        (setf (input-buffer minibuffer) "")
        (setf (input-cursor-position minibuffer) 0)
        (insert input minibuffer)))))

(define-command minibuffer-toggle-mark (&optional (minibuffer (current-minibuffer)))
  "Toggle candidate.
Only available if minibuffer `multi-selection-p' is non-nil."
  (when (multi-selection-p minibuffer)
    (with-slots (completions completion-cursor marked-completions) minibuffer
      (let ((candidate (nth completion-cursor completions)))
        (match (member candidate marked-completions)
          ((guard n n) (setf marked-completions (delete candidate marked-completions)))
          (_ (push candidate marked-completions)))))
    (state-changed minibuffer)
    (update-display minibuffer)
    (select-next minibuffer)))

(define-command minibuffer-mark-all (&optional (minibuffer (current-minibuffer)))
  "Mark all visible candidates.
Only available if minibuffer `multi-selection-p' is non-nil."
  (when (multi-selection-p minibuffer)
    (with-slots (completions marked-completions) minibuffer
      (setf marked-completions (union completions marked-completions)))
    (state-changed minibuffer)
    (update-display minibuffer)))

(define-command minibuffer-unmark-all (&optional (minibuffer (current-minibuffer)))
  "Unmark all visible candidates.
Only available if minibuffer `multi-selection-p' is non-nil."
  (when (multi-selection-p minibuffer)
    (with-slots (completions marked-completions) minibuffer
      (setf marked-completions (set-difference marked-completions completions)))
    (state-changed minibuffer)
    (update-display minibuffer)))
