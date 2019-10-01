;;; minibuffer.lisp --- major mode for input

(in-package :next)
(annot:enable-annot-syntax)

;; TODO: We need to separate the minibuffer from the echo area.  The
;; `show'/`hide' functions are not dealing well with `echo'/`echo-dismiss'.

(defparameter *word-separation-characters* '(#\: #\/ #\- #\. #\Space #\ )
  "Characters delimiting words (space, colon, slash, dot, etc).")

(defun word-separation-character-p (char)
  (intersection *word-separation-characters* (list char)))

;; TODO: Move minibuffer-mode to a separate package?
(define-mode minibuffer-mode ()
  "Mode for the minibuffer."
  ((keymap-schemes
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
        "button4" #'select-previous
        "button5" #'select-next
        "Down" #'select-next
        "Up" #'select-previous
        "C-v" #'minibuffer-paste
        "C-y" #'minibuffer-paste
        "C-w" #'copy-candidate
        "TAB" #'insert-candidate
        "M-p" #'minibuffer-history
        "C-SPACE" #'minibuffer-toggle-mark
        "M-a" #'minibuffer-mark-all
        "M-u" #'minibuffer-unmark-all
        :keymap map)
      (list :emacs map
            ;; TODO: We could have VI bindings for the minibuffer too.
            ;; But we need to make sure it's optional + to have an indicator
            ;; for the mode.  This requires either a change of cursor or a
            ;; echo area separate from the minibuffer.
            :vi-normal map
            :vi-insert map)))))

@export
@export-accessors
(defclass minibuffer (buffer)
  ((default-modes :initarg :default-modes
                  :initform '(minibuffer-mode))
   (completion-function :initarg :completion-function :accessor completion-function
                        :initform nil
                        :documentation "Function that takes the user input
string and returns a list of candidate strings")
   (callback :initarg :callback :accessor callback
             :initform nil
             :documentation "Function to call over the selected candidate.")
   (callback-buffer :initarg :callback-buffer
                    :accessor callback-buffer
                    :initform (when *interface* (current-buffer))
                    :documentation "The active buffer when the minibuffer was
brought up.  This can be useful to know which was the original buffer in the
`callback' in case the buffer was changed.")
   (setup-function :initarg :setup-function :accessor setup-function
                   :initform #'setup-default
                   :documentation "Function of no argument that fills the
`content' on when the minibuffer is created.  Called only once.")
   (cleanup-function :initarg :cleanup-function :accessor cleanup-function
                     :initform nil
                     :documentation "Function run after a completion has been selected.
This should not rely on the minibuffer's content.")
   (empty-complete-immediate :initarg :empty-complete-immediate :accessor empty-complete-immediate ; TODO: Rename?
                             :initform nil
                             :documentation "If non-nil, allow input matching no
candidates.")
   ;; TODO: Move input-* slots to a separate text class?
   (input-prompt :initarg :input-prompt :accessor input-prompt :initform "Input:")
   (input-buffer :accessor input-buffer :initform "")
   ;; cursor position ?
   (input-buffer-cursor :accessor input-buffer-cursor :initform 0)
   (invisible-input-p :initarg :invisible-input-p :accessor invisible-input-p
                      :initform nil
                      :documentation "If non-nil, input is replaced by
placeholder character.  This is useful to conceal passwords.")
   (history :initarg :history :accessor history
            :initform (minibuffer-generic-history *interface*)
            :type ring:ring
            :documentation "History of inputs for the minibuffer.
If nil, no history is used.")
   (multi-selection-p :initarg :multi-selection-p :accessor multi-selection-p
                      :initform nil
                      :type boolean
                      :documentation "If non-nil, allow for selecting multiple
candidates.")
   (completions :accessor completions :initform nil)
   (marked-completions :accessor marked-completions :initform nil)
   (completion-head :accessor completion-head :initform 0)
   (completion-cursor :accessor completion-cursor :initform 0) ; TODO: Rename to completion-index?
   (content :initform ""
            :documentation "The HTML content of the minibuffer.")
   (max-lines :initarg :max-lines
              :accessor max-lines
              :type integer
              :initform 8
              :documentation "Max number of candidate lines to show.
You will want edit this to match the changes done to `minibuffer-font-size',
`minibuffer-line-height' and `minibuffer-open-height'.")
   (minibuffer-font-size :initarg :minibuffer-font-size
                         :accessor minibuffer-font-size
                         :type string
                         :initform "1em"
                         :documentation "CSS font size for the minibuffer.
Value is a string, e.g. '1em'.
You might want to configure the value on HiDPI screen.")
   (minibuffer-line-height :initarg :minibuffer-line-height
                           :accessor minibuffer-line-height
                           :type string
                           :initform ""
                           :documentation "CSS line height for the minibuffer.
Value is a string, e.g. '1em'.
You might want to configure the value on HiDPI screen.")
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
                                  (.selected :background-color "gray"
                                             :color "white")
                                  ;; Make sure .selected and .marked can stack
                                  ;; since the candidate can be both marked on
                                  ;; selected.
                                  (.marked :background-color "darkgray"
                                           :font-weight "bold"
                                           :color "white")))
                     :documentation "The CSS applied to a minibuffer when it is set-up.")))

(defmethod (setf input-buffer) (value (minibuffer minibuffer))
  "Reset the minibuffer state on every input change.
This is necessary or else completion cursor / head could be beyond the updated
list length."
  (with-slots (completion-function completions input-buffer
               empty-complete-immediate completion-cursor completion-head)
      minibuffer
    (setf input-buffer value)
    (if completion-function
        (setf completions (funcall completion-function input-buffer))
        (setf completions nil))
    (when (and empty-complete-immediate
               (not (str:emptyp input-buffer)))
      ;; Don't add input-buffer to completions that don't accept arbitrary
      ;; inputs (i.e. empty-complete-immediate is nil).
      (push input-buffer completions))
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
  (rpc-minibuffer-evaluate-javascript
   (last-active-window *interface*)
   (ps:ps (ps:chain document
                    (write (ps:lisp (content minibuffer)))))))

(defmethod initialize-instance :after ((minibuffer minibuffer) &key)
  (hooks:run-hook (hooks:object-hook *interface* 'minibuffer-make-hook) minibuffer)
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
@export
(defun read-from-minibuffer (minibuffer &key callback)
  "Open the minibuffer, ready for user input.
Example use:

  (read-from-minibuffer
   (make-instance 'minibuffer
                  :completion-function #'my-completion-filter))

See the documentation of `minibuffer' to know more about the minibuffer options."
  (when callback
    ;; We need a :callback key argument so that `read-from-minibuffer' can be
    ;; called in `with-result'.
    (setf (callback minibuffer) callback))
  ;; TODO: Shall we leave it to the caller to decide which is the callback-buffer?
  (setf (callback-buffer minibuffer) (current-buffer))
  (match (setup-function minibuffer)
    ((guard f f) (funcall f minibuffer)))
  (handler-case
      (update-display minibuffer)
    (error (c)
      (echo "~a" c)
      (return-from read-from-minibuffer)))
  (push minibuffer (active-minibuffers (last-active-window *interface*)))
  (apply #'show
         (unless (completion-function minibuffer)
           ;; We don't need so much height since there is no candidate to display.
           (list :height (status-buffer-height (last-active-window *interface*))))))

(define-command return-input (&optional (minibuffer (current-minibuffer)))
  "Return with minibuffer selection."
  (with-slots (callback empty-complete-immediate completions completion-cursor
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
       (funcall callback (if multi-selection-p
                             completions
                             (first completions))))))
  (quit-minibuffer minibuffer))

(define-command return-immediate (&optional (minibuffer (current-minibuffer)))
  "Return with minibuffer input, ignoring the selection."
  (with-slots (callback) minibuffer
    (let ((normalized-input (str:replace-all " " " " (input-buffer minibuffer))))
      (funcall callback normalized-input)))
  (quit-minibuffer minibuffer))

(defun quit-minibuffer (&optional (minibuffer (current-minibuffer)))
  (unless (or (null (history minibuffer))
              (str:empty? (input-buffer minibuffer)))
    (let ((normalized-input (str:replace-all " " " " (input-buffer minibuffer))))
      (ring:insert (history minibuffer) normalized-input)))
  (cancel-input minibuffer))

(define-command cancel-input (&optional (minibuffer (current-minibuffer)))
  "Close the minibuffer query without further action."
  (match (cleanup-function minibuffer)
    ((guard f f) (funcall f)))
  (hide minibuffer))

@export
(defmethod erase-input ((minibuffer minibuffer))
  "Clean-up the minibuffer input."
  (setf (input-buffer minibuffer) "")
  (setf (input-buffer-cursor minibuffer) 0)
  (setf (content minibuffer) ""))

;; TODO: Move `erase-document' into the `show' function?
(defmethod erase-document ((minibuffer minibuffer))
  (evaluate-script minibuffer
                   (ps:ps
                     (ps:chain document (open))
                     (ps:chain document (close)))))

(defmethod setup-default ((minibuffer minibuffer))
  (erase-document minibuffer)
  (setf (input-buffer minibuffer) "")
  (setf (input-buffer-cursor minibuffer) 0)
  (setf (content minibuffer)
        (cl-markup:markup
         (:head (:style (minibuffer-style minibuffer))
                (:style (minibuffer-line-style minibuffer)))
         (:body
          (:div :id "container"
                (:div :id "input" (:span :id "prompt" "") (:span :id "input-buffer" ""))
                (:div :id "completions" ""))))))

(defmethod evaluate-script ((minibuffer minibuffer) script)
  "Evaluate SCRIPT into MINIBUFFER's webview.
The new webview HTML content it set as the MINIBUFFER's `content'."
  (let ((active-window (rpc-window-active)))
    (when minibuffer
      (with-result (new-content (rpc-minibuffer-evaluate-javascript
                                 active-window
                                 (str:concat
                                  script
                                  ;; Return the new HTML body.
                                  (ps:ps (ps:chain document body |outerHTML|)))))
        ;; Since the script may have changed the content on the platform port,
        ;; we need to update the slot's value.
        (setf (slot-value minibuffer 'content) new-content)))))

(defun show (&key
             (minibuffer (first (active-minibuffers
                                 (last-active-window *interface*))))
             height)
  "Show the last active minibuffer, if any."
  (let ((active-window (last-active-window *interface*)))
    (when minibuffer
      (rpc-window-set-minibuffer-height
       active-window
       (or height
           (minibuffer-open-height active-window))))))

(defun hide (minibuffer)
  "Hide MINIBUFFER and display next active one, if any."
  (let ((active-window (rpc-window-active)))
    ;; Note that MINIBUFFER is not necessarily first in the list, e.g. a new
    ;; minibuffer was invoked before the old one reaches here.
    (setf (active-minibuffers active-window)
          (delete minibuffer (active-minibuffers active-window)))
    (if (active-minibuffers active-window)
        (progn
          (show)
          ;; We need to refresh so that the nested minibuffers don't have to do it.
          (update-display (first (active-minibuffers active-window))))
        (progn
          ;; TODO: We need a mode-line before we can afford to really hide the
          ;; minibuffer.  Until then, we "blank" it.
          (echo "")                     ; Or echo-dismiss?
          (rpc-window-set-minibuffer-height
           active-window
           ;; TODO: Until we have a mode-line, it's best to keep the
           ;; closed-height equal to the echo-height to avoid the stuttering,
           ;; especially when hovering over links.
           (status-buffer-height active-window))))))

(defun insert (characters &optional (minibuffer (current-minibuffer)))
  (setf (input-buffer minibuffer)
        (str:insert characters
                    (input-buffer-cursor minibuffer)
                    (input-buffer minibuffer)))
  (incf (input-buffer-cursor minibuffer) (length characters))
  (update-display minibuffer))

(define-command self-insert ()
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

(define-command delete-forwards (&optional (minibuffer (current-minibuffer)))
  "Delete character after cursor."
  (with-accessors ((buffer input-buffer) (cursor input-buffer-cursor)) minibuffer
    (unless (= cursor (length buffer))
      (setf buffer
            (concatenate 'string
                         (subseq buffer 0 cursor)
                         (subseq buffer
                                 (+ 1 cursor)
                                 (length buffer))))))
  (update-display minibuffer))

(define-command delete-backwards (&optional (minibuffer (current-minibuffer)))
  "Delete character before cursor."
  (with-accessors ((buffer input-buffer) (cursor input-buffer-cursor)) minibuffer
    (unless (= cursor 0)
      (setf buffer
            (concatenate 'string
                         (subseq buffer 0 (- cursor 1))
                         (subseq buffer cursor (length buffer))))
      (decf cursor)))
  (update-display minibuffer))

(define-command cursor-forwards (&optional (minibuffer (current-minibuffer)))
  "Move cursor forward by one."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (when (< input-buffer-cursor (length input-buffer))
      (incf input-buffer-cursor)))
  (update-display minibuffer))

(define-command cursor-backwards (&optional (minibuffer (current-minibuffer)))
  "Move cursor backwards by one."
  (with-slots (input-buffer-cursor) minibuffer
    (when (> input-buffer-cursor 0)
      (decf input-buffer-cursor)))
  (update-display minibuffer))

(define-command cursor-beginning (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the beginning of the input area."
  (with-slots (input-buffer-cursor) minibuffer
    (setf input-buffer-cursor 0))
  (update-display minibuffer))

(define-command cursor-end (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the end of the input area."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (setf input-buffer-cursor (length input-buffer)))
  (update-display minibuffer))

(defun char-at-cursor (&optional (minibuffer (current-minibuffer)))
  "Return the character the cursor it at in the minibuffer."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (if (< input-buffer-cursor (length input-buffer))
        (char (input-buffer minibuffer) (input-buffer-cursor minibuffer)))))

(defun char-at-position (input position)
  "Return the character at `position' in `input', or nil."
  (if (< position (length input))
      (char input position)))

(define-command cursor-forwards-word (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the end of the word at point."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (if (intersection *word-separation-characters* (list (char-at-cursor minibuffer)))
        (loop while (and
                     (intersection *word-separation-characters* (list (char-at-cursor minibuffer)))
                     (< input-buffer-cursor (length input-buffer)))
              do (incf input-buffer-cursor))
        (loop while (and
                     (not (intersection *word-separation-characters* (list (char-at-cursor minibuffer))))
                     (< input-buffer-cursor (length input-buffer)))
              do (incf input-buffer-cursor))))
  (update-display minibuffer)
  (input-buffer-cursor minibuffer))

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

;; TODO: Re-use cursor-forwards-word
(define-command cursor-backwards-word (&optional (minibuffer (current-minibuffer)))
  "Move cursor to the beginning of the word at point."
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (setf input-buffer-cursor (backwards-word-position input-buffer input-buffer-cursor)))

  (update-display minibuffer)
  (input-buffer-cursor minibuffer))

(define-command delete-forwards-word (&optional (minibuffer (current-minibuffer)))
  "Delete characters from cursor position until the end of the word at point."
  (with-accessors ((buffer input-buffer) (cursor input-buffer-cursor)) minibuffer
    (let* ((current-cursor-position cursor)
           (new-cursor-position (cursor-forwards-word minibuffer))
           (transpose-distance (- new-cursor-position current-cursor-position)))
      (setf buffer
            (concatenate 'string
                         (subseq buffer 0 current-cursor-position)
                         (subseq buffer new-cursor-position (length buffer))))
      (setf cursor (- cursor transpose-distance))))
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
  (with-accessors ((buffer input-buffer) (cursor input-buffer-cursor)) minibuffer
    (multiple-value-bind (new-string new-position) (%delete-backwards-word buffer cursor)
      (setf buffer new-string
            cursor new-position)))
  (update-display minibuffer))

(define-command kill-line (&optional (minibuffer (current-minibuffer)))
  "Delete all characters from cursor position until the end of the line."
  (with-accessors ((buffer input-buffer) (cursor input-buffer-cursor)) minibuffer
    (setf buffer (subseq buffer 0 cursor)))
  (update-display minibuffer))

(define-command kill-whole-line (&optional (minibuffer (current-minibuffer)))
  "Delete all characters in the input."
  (with-accessors ((buffer input-buffer) (cursor input-buffer-cursor)) minibuffer
    (setf buffer ""
          cursor 0))
  (update-display minibuffer))

(defun generate-input-html (input-buffer cursor-index)
  (cond ((equal "" input-buffer) (cl-markup:markup (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
        ((eql cursor-index (length input-buffer)) (cl-markup:markup (:span input-buffer)
                                                                    (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
        (t (cl-markup:markup (:span (subseq input-buffer 0 cursor-index))
                             (:span :id "cursor" (subseq input-buffer cursor-index (+ 1 cursor-index)))
                             (:span (subseq input-buffer (+ 1  cursor-index)))))))

(defun generate-input-html-invisible (input-buffer cursor-index)
  (let ((input-buffer-password (make-string (length input-buffer) :initial-element #\*)))
    (cond ((equal "" input-buffer-password) (cl-markup:markup (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
          ((eql cursor-index (length input-buffer-password))
           (cl-markup:markup (:span input-buffer-password)
                             (:span :id "cursor" (cl-markup:raw "&nbsp;"))))
          (t (cl-markup:markup (:span (subseq input-buffer-password 0 cursor-index))
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
    (cl-markup:markup (:ul (loop repeat lines
                                 for i from (completion-head minibuffer)
                                 for completion in (nthcdr i completions)
                                 collect
                                 (cl-markup:markup
                                  (:li :class (cond
                                                ((= i cursor-index) "selected")
                                                ((member completion (marked-completions minibuffer)) "marked")
                                                ((= i (completion-head minibuffer)) "head"))
                                       :id (cond
                                             ((= i cursor-index) "selected")
                                             ((member completion (marked-completions minibuffer)) "marked")
                                             ((= i (completion-head minibuffer)) "head"))
                                       (object-string completion))))))))

@export
(defmethod update-display ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor
               completions completion-cursor)
      minibuffer
    (let ((input-text (if (invisible-input-p minibuffer)
                          (generate-input-html-invisible input-buffer input-buffer-cursor)
                          (generate-input-html input-buffer input-buffer-cursor)))
          (completion-html (generate-completion-html completions completion-cursor minibuffer)))
      (evaluate-script minibuffer
                       (ps:ps
                         (setf (ps:chain document (get-element-by-id "prompt") |innerHTML|)
                               (ps:lisp (input-prompt minibuffer)))
                         (setf (ps:chain document (get-element-by-id "input-buffer") |innerHTML|)
                               (ps:lisp input-text))
                         (setf (ps:chain document (get-element-by-id "completions") |innerHTML|)
                               (ps:lisp completion-html)))))))

(define-command select-next (&optional (minibuffer (current-minibuffer)))
  "Select next entry in minibuffer."
  (when (< (completion-cursor minibuffer) (- (length (completions minibuffer)) 1))
    (incf (completion-cursor minibuffer))
    (update-display minibuffer)
    (evaluate-script minibuffer
                     (ps:ps (ps:chain (ps:chain document (get-element-by-id "selected"))
                                      (scroll-into-view false))))))

(define-command select-previous (&optional (minibuffer (current-minibuffer)))
  "Select previous entry in minibuffer."
  (when (> (completion-cursor minibuffer) 0)
    (decf (completion-cursor minibuffer))
    (update-display minibuffer)
    (evaluate-script minibuffer
                     (ps:ps (ps:chain (ps:chain document (get-element-by-id "head"))
                                      (scroll-into-view false))))))

(defun %echo-status (text &key (message (list text))
                          ;; Need to ignore RPC errors in case platform port is
                          ;; not available and we use this function before
                          ;; checking for it.
                            (window (ignore-errors (when *interface* (rpc-window-active))))
                            (status-buffer (when window (status-buffer window))))
  "Echo TEXT in the status buffer.
MESSAGE is a cl-markup list."
  (log:info text)
  (if (and status-buffer window)
      (progn
        (unless (or (null message)
                    (equal message '("")))
          (push `(:p (:i "["
                         ,(local-time:format-timestring
                           nil
                           (local-time:now)
                           :format local-time:+asctime-format+)
                         "]")
                     " "
                     ,@message)
                (messages-content *interface*)))
        (unless (active-minibuffers window)
          (erase-document status-buffer)
          (let ((style (cl-css:css
                        `((body :border-top "4px solid dimgray"
                                :margin "0"
                                :padding "0 6px")
                          (p :margin "0")))))
            (setf (content status-buffer)
                  (cl-markup:markup
                   (:head (:style style)
                          (:style (minibuffer-line-style status-buffer)))
                   (:body
                    (:p text)))))
          (show :minibuffer status-buffer
                :height (status-buffer-height window))))
      (log:warn "Can't echo '~a' without status buffer or interface" text)))

@export
(defun echo (&rest args)
  "Echo ARGS in the status buffer.
The first argument can be a format string and the following arguments will be
interpreted by `format'.
Untrusted content should be given as argument with a format string."
  (handler-case
      (%echo-status (apply #'format nil args))
    (error ()
      (log:warn "Failed to echo these args: ~s~&Possible improvements:
- pass multiple arguments and use format strings for untrusted content. Don't pre-construct a single string that could contain tildes.
  example: do (echo \"directory is\ ~~a \"~~/Downloads/\")
           instead of (echo \"directory is ~~/Downloads/\")
- use echo-safe or use the ~~s directive directly." args))))

@export
(defun echo-safe (arg)
  "Echo one untrusted string that could contain format directives (that is, a tilde).
It uses the ~s directive.
Unlike echo, this only accepts one argument."
  (%echo-status (format nil "~s" arg)))

@export
(defun echo-warning (&rest args)
  "Like `echo' but prefix with \"Warning\" and output to the standard error."
  (let ((text (apply #'format nil args)))
    (%echo-status text
                  :message `((:b "Warning:") " " ,text))
    (log:warn "~a" text)))

@export
(defmethod echo-dismiss ()
  ;; TODO: If we erase the document here, it will show a blank widget instead
  ;; of the minibuffer when we don't fully hide it.  We can only erase the
  ;; document when we have a mode-line we can fully hide the minibuffer.
  ;; (erase-document minibuffer)
  ;; TODO: We should only display this default text until we have a mode-line.
  (let ((buffer (current-buffer)))
    (%echo-status (format nil "[~{~a~^ ~}] ~a — ~a"
                          (mapcar (lambda (m) (str:replace-all "-mode" ""
                                                               (str:downcase
                                                                (class-name (class-of m)))))
                                  (modes buffer))
                          (url buffer)
                          (title buffer))
                  ;; Don't add to the *Messages* buffer:
                  :message nil)))

(declaim (ftype (function (ring:ring) string) ring-insert-clipboard))
@export
(defun ring-insert-clipboard (ring)
  "Check if clipboard-content is most recent entry in RING.
If not, insert clipboard-content into RING.
Return most recent entry in RING."
  (let ((clipboard-content (trivial-clipboard:text)))
    (unless (string= clipboard-content (ring:ref ring 0))
      (ring:insert ring clipboard-content)))
  (string (ring:ref ring 0)))

(define-command minibuffer-paste (&optional (minibuffer (current-minibuffer)))
  "Paste clipboard text to input."
  (insert (ring-insert-clipboard (clipboard-ring *interface*)) minibuffer))

@export
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
  "Paste clipboard text to input."
  (let ((candidate (get-candidate minibuffer)))
    (when candidate
      (kill-whole-line minibuffer)
      (insert candidate minibuffer))))

(declaim (ftype (function (ring:ring)) minibuffer-history-completion-filter))
(defun minibuffer-history-completion-filter (history)
  (when history
    (lambda (input)
      (fuzzy-match input (delete-duplicates (ring:recent-list history)
                                            :test #'equal)))))

(define-command minibuffer-history (&optional (minibuffer (current-minibuffer)))
  "Choose a minibuffer input history entry to insert as input."
  (when (history minibuffer)
    (with-result (input (read-from-minibuffer
                         (make-instance 'minibuffer
                                        :input-prompt "Input history:"
                                        :history nil
                                        :completion-function (minibuffer-history-completion-filter (history minibuffer)))))
      (unless (str:empty? input)
        (log:debug input minibuffer)
        (setf (input-buffer minibuffer) "")
        (setf (input-buffer-cursor minibuffer) 0)
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
    (select-next minibuffer)))

(define-command minibuffer-mark-all (&optional (minibuffer (current-minibuffer)))
  "Mark all visible candidates.
Only available if minibuffer `multi-selection-p' is non-nil."
  (when (multi-selection-p minibuffer)
    (with-slots (completions marked-completions) minibuffer
      (setf marked-completions (union completions marked-completions)))
    (update-display minibuffer)))

(define-command minibuffer-unmark-all (&optional (minibuffer (current-minibuffer)))
  "Unmark all visible candidates.
Only available if minibuffer `multi-selection-p' is non-nil."
  (when (multi-selection-p minibuffer)
    (with-slots (completions marked-completions) minibuffer
      (setf marked-completions (set-difference marked-completions completions)))
    (update-display minibuffer)))
