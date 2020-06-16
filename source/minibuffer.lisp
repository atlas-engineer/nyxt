(in-package :nyxt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'minibuffer)
  (export
   '(default-modes
     suggestion-function
     setup-function
     cleanup-function
     changed-callback
     must-match-p
     input-prompt
     input-buffer
     input-cursor
     invisible-input-p
     history
     multi-selection-p
     show-suggestion-count-p
     reset-suggestion-state
     max-lines
     minibuffer-style
     user-style
     override-map)))

(defclass minibuffer (buffer)
  ((default-modes :initarg :default-modes
                  :initform '(minibuffer-mode))
   (suggestion-function :initarg :suggestion-function
                        :accessor suggestion-function
                        :initform nil
                        :type (or function null)
                        :documentation "Take the input
string and returns a list of suggestion strings.
Example: `buffer-suggestion-filter'.")
   (callback :initform nil
             :documentation "Function to call over the selected suggestion.")
   (callback-buffer :initarg :callback-buffer
                    :accessor callback-buffer
                    :initform (when *browser* (current-buffer))
                    :documentation "The active buffer when the minibuffer was
brought up.
This can be useful to know which was the original buffer in the `callback' in
case the buffer was changed.")
   (setup-function :initarg :setup-function :accessor setup-function
                   :initform #'setup-default
                   :type (or function null)
                   :documentation "Fills the `content' on when the minibuffer is created.
Takes no argument.  Called only once.")
   (cleanup-function :initarg :cleanup-function
                     :accessor cleanup-function
                     :initform nil
                     :type (or function null)
                     :documentation "Run after a suggestion has been selected.
This should not rely on the minibuffer's content.")
   (changed-callback :initarg :changed-callback
                     :accessor changed-callback
                     :initform nil
                     :type (or function null)
                     :documentation "Called whenever a change happens.")
   (must-match-p :initarg :must-match-p
                 :accessor must-match-p
                 :initform t
                 :type boolean
                 :documentation "If nil, allow input matching no suggestions.")
   (input-prompt :initarg :input-prompt
                 :accessor input-prompt
                 :initform "Input"
                 :type string
                 :documentation "Text to prompt to the user, before `input-buffer'.")
   (input-buffer :initarg :input-buffer
                 :initform (make-instance 'text-buffer:text-buffer)
                 :documentation "Buffer used to capture keyboard input.")
   (input-cursor :accessor input-cursor
                 :initarg :input-cursor
                 :initform (make-instance 'text-buffer:cursor)
                 :documentation "Cursor used in conjunction with the input-buffer.")
   (invisible-input-p :initarg :invisible-input-p
                      :accessor invisible-input-p
                      :type boolean
                      :initform nil
                      :documentation "If non-nil, input is replaced by placeholder character.
This is useful to conceal passwords.")
   (history :initarg :history
            :accessor history
            :initform (minibuffer-generic-history *browser*)
            :type (or containers:ring-buffer-reverse null)
            :documentation "History of inputs for the minibuffer.
If nil, no history is used.")
   (multi-selection-p :initarg :multi-selection-p :accessor multi-selection-p
                      :initform nil
                      :type boolean
                      :documentation "If non-nil, allow for selecting multiple suggestions.")
   (suggestions :accessor suggestions :initform nil)
   (marked-suggestions :accessor marked-suggestions :initform nil)
   (show-suggestion-count-p :accessor show-suggestion-count-p
                            :initarg :show-suggestion-count-p
                            :initform t
                            :type boolean
                            :documentation "Show the number of chosen suggestions
inside brackets. It can be useful to disable, for instance for a yes/no question.")
   (suggestion-head :accessor suggestion-head
                    :initform 0
                    :type integer)
   (suggestion-cursor :accessor suggestion-cursor
                      :initform 0
                      :type integer)
   (content :initform "" :type string
            :documentation "The HTML content of the minibuffer.")
   (max-lines :initarg :max-lines
              :accessor max-lines
              :type integer
              :initform 10
              :documentation "Max number of suggestion lines to show.  You will
want edit this to match the changes done to `minibuffer-style'.")
   (minibuffer-style
    :accessor minibuffer-style
    :initform (cl-css:css
               '((* :font-family "monospace,monospace"
                    :font-size "14px"
                    :line-height "18px")
                 (body :border-top "4px solid dimgray"
                       :margin "0"
                       :padding "0 6px")
                 ("#container" :display "flex"
                               :flex-flow "column"
                               :height "100%")
                 ("#input" :padding "6px 0"
                           :border-bottom "solid 1px lightgray")
                 ("#suggestions" :flex-grow "1"
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
    :documentation "The CSS applied to a minibuffer when it is set-up.")
   (user-style
    :accessor user-style
    :initform ""
    :documentation "User CSS that is applied after the
minibuffer-style. This can be used to override any styles in the
minibuffer-style.")
   (override-map :accessor override-map
                 :initform (let ((map (make-keymap "overide-map")))
                             (define-key map
                               "escape"
                               ;; We compute symbol at runtime because
                               ;; nyxt/minibuffer-mode does not exist at
                               ;; compile-time since it's loaded afterwards.
                               (find-symbol (string 'cancel-input)
                                            (find-package 'nyxt/minibuffer-mode))))
                 :type keymap:keymap
                 :documentation "Keymap that takes precedence over all modes' keymaps."))
  (:documentation "The minibuffer is the interface for user interactions.  Each
prompt spawns a new minibuffer object: this makes it possible to nest minibuffer
calls, such as invoking `minibuffer-history'.

A minibuffer query is typically done as follows:

\(with-result (tags (read-from-minibuffer
                    (make-minibuffer
                     :input-prompt \"Space-separated tag (s) \"
                     :default-modes '(set-tag-mode minibuffer-mode)
                     :suggestion-function (tag-suggestion-filter))))
  ;; Write form here in which `tags' is bound to the resulting element(s).
  )"))

(export-always '*minibuffer-class*)
(defparameter *minibuffer-class* 'minibuffer)
(export-always 'make-minibuffer)
(defun make-minibuffer
    (&key (default-modes nil explicit-default-modes)
       (suggestion-function nil explicit-suggestion-function)
       (callback-buffer nil explicit-callback-buffer)
       (setup-function nil explicit-setup-function)
       (cleanup-function nil explicit-cleanup-function)
       (changed-callback nil explicit-changed-callback)
       (must-match-p t explicit-must-match-p)
       (input-prompt nil explicit-input-prompt)
       (input-buffer nil explicit-input-buffer)
       (invisible-input-p nil explicit-invisible-input-p)
       (show-suggestion-count-p t explicit-show-suggestion-count-p) ; TODO: Rename to hide-suggestion-count and reverse default value.
       (history nil explicit-history)
       (multi-selection-p nil explicit-multi-selection-p))
  "See the `minibuffer' class for the argument documentation."
  (let ((tmp-input-buffer (make-instance 'text-buffer:text-buffer))
        (tmp-input-cursor (make-instance 'text-buffer:cursor)))
    (cluffer:attach-cursor tmp-input-cursor tmp-input-buffer)
    (apply #'make-instance *minibuffer-class*
           `(:input-buffer ,tmp-input-buffer
             :input-cursor ,tmp-input-cursor
             ,@(if explicit-default-modes
                   `(:default-modes ,default-modes)
                   '())
             ,@(if explicit-suggestion-function
                   `(:suggestion-function ,suggestion-function)
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
             ,@(if explicit-must-match-p
                   `(:must-match-p ,must-match-p)
                   '())
             ,@(if explicit-input-prompt
                   `(:input-prompt ,input-prompt)
                   '())
             ,@(if explicit-invisible-input-p
                   `(:invisible-input-p ,invisible-input-p)
                   '())
             ,@(if explicit-show-suggestion-count-p
                   `(:show-suggestion-count-p ,show-suggestion-count-p)
                   '())
             ,@(if explicit-history
                   `(:history ,history)
                   '())
             ,@(if explicit-multi-selection-p
                   `(:multi-selection-p ,multi-selection-p)
                   '())))))

(defmethod input-buffer ((minibuffer minibuffer))
  (str:replace-all " " " " (text-buffer::string-representation (slot-value minibuffer 'input-buffer))))

(defmethod update-suggestions ((minibuffer minibuffer))
  (with-slots (suggestion-function suggestions must-match-p )
      minibuffer
    (if suggestion-function
        (setf suggestions (funcall-safely suggestion-function minibuffer))
        (setf suggestions nil))
    (when (and (not must-match-p)
               (not (str:emptyp (input-buffer minibuffer))))
      ;; Don't add input to suggestions that don't accept arbitrary
      ;; inputs (i.e. must-match-p is t).
      (push (input-buffer minibuffer) suggestions))))

(defmethod (setf input-buffer) (value (minibuffer minibuffer))
  "Reset the minibuffer state on every input change.
  This is necessary or else suggestion cursor / head could be beyond
  the updated list length."
  (text-buffer::kill-line (input-cursor minibuffer))
  (insert minibuffer value)
  (reset-suggestion-state))

(defmethod reset-suggestion-state ((minibuffer minibuffer))
  "Update the suggestions and move the suggestion cursor to the
beginning."
  (update-suggestions minibuffer)
  (setf suggestion-cursor 0)
  (setf suggestion-head 0))

(defmethod content ((minibuffer minibuffer))
  (slot-value minibuffer 'content))

(defmethod (setf content) (html-content minibuffer)
  "Set the `content' of the MINIBUFFER to HTML-CONTENT.
   This runs a call"
  (setf (slot-value minibuffer 'content) html-content)
  (ffi-minibuffer-evaluate-javascript
   (current-window)
   (ps:ps (ps:chain document
                    (write (ps:lisp (content minibuffer)))))))

(defmethod initialize-instance :after ((minibuffer minibuffer) &key)
  (unless (cluffer:cursor-attached-p (input-cursor minibuffer))
    (cluffer:attach-cursor (input-cursor minibuffer) (input-buffer minibuffer)))
  (hooks:run-hook (minibuffer-make-hook *browser*) minibuffer)
  ;; We don't want to show the input in the suggestion list when invisible.
  (unless (suggestion-function minibuffer)
    ;; If we have no suggestion function, then we have no suggestions beside
    ;; immediate input, so we must allow them as valid suggestion.
    (setf (must-match-p minibuffer) nil))
  (setf (must-match-p minibuffer)
        (if (invisible-input-p minibuffer)
            t ; This way the minibuffer won't display the input as a suggestion.
            (must-match-p minibuffer)))
  (initialize-modes minibuffer))

(declaim (ftype (function (minibuffer)) read-from-minibuffer))
(export-always 'read-from-minibuffer)
(defun read-from-minibuffer (minibuffer)
  "Open the minibuffer, ready for user input.
Example use:

\(read-from-minibuffer
 (make-minibuffer
  :suggestion-function #'my-suggestion-filter))

See the documentation of `minibuffer' to know more about the minibuffer options."
  (when %callback
    (setf (slot-value minibuffer 'callback) %callback))
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
         (unless (suggestion-function minibuffer)
           ;; We don't need so much height since there is no suggestion to display.
           (list :height (minibuffer-open-single-line-height (current-window))))))

(export-always 'erase-input)
(defmethod erase-input ((minibuffer minibuffer))
  "Erase the minibuffer input."
  (cluffer:beginning-of-line (input-cursor minibuffer))
  (text-buffer::kill-forward-line (input-cursor minibuffer)))

(defmethod erase-document ((minibuffer minibuffer))
  (evaluate-script minibuffer (ps:ps
                                (ps:chain document (open))
                                (ps:chain document (close)))))

(defmethod setup-default ((minibuffer minibuffer))
  (erase-document minibuffer)
  (update-suggestions minibuffer)
  (setf (content minibuffer)
        (markup:markup
         (:head (:style (minibuffer-style minibuffer))
                (:style (user-style minibuffer)))
         (:body
          (:div :id "container"
                (:div :id "input" (:span :id "prompt" "") (:span :id "input-buffer" ""))
                (:div :id "suggestions" ""))))))

(export-always 'evaluate-script)
(defmethod evaluate-script ((minibuffer minibuffer) script)
  "Evaluate SCRIPT into MINIBUFFER's webview.
The new webview HTML content it set as the MINIBUFFER's `content'."
  (when minibuffer
    (with-result (new-content
                  (ffi-minibuffer-evaluate-javascript
                   (current-window)
                   (str:concat script (ps:ps (ps:chain document body |outerHTML|)))))
      (setf (slot-value minibuffer 'content) new-content))))

(defun show (&key (minibuffer (first (active-minibuffers (current-window)))) height)
  "Show the last active minibuffer, if any."
  (when minibuffer
    (ffi-window-set-minibuffer-height
     (current-window)
     (or height
         (minibuffer-open-height (current-window))))))

(export-always 'hide)
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

(export-always 'insert)
(defmethod insert ((minibuffer minibuffer) characters)
  (text-buffer::insert-string (input-cursor minibuffer) characters)
  (reset-suggestion-state minibuffer)
  (state-changed minibuffer)
  (update-display minibuffer))

(defmethod generate-input-html ((minibuffer minibuffer))
  (let ((buffer-string-representation (if (invisible-input-p minibuffer)
                                          (text-buffer::invisible-string-representation (slot-value minibuffer 'input-buffer))
                                          (text-buffer::string-representation (slot-value minibuffer 'input-buffer)))))
    (cond ((eql 0 (cluffer:item-count (slot-value minibuffer 'input-buffer)))
           (markup:markup (:span :id "cursor" (markup:raw "&nbsp;"))))
          ((eql (cluffer:cursor-position (input-cursor minibuffer)) (cluffer:item-count (slot-value minibuffer 'input-buffer)))
           (markup:markup (:span buffer-string-representation)
                          (:span :id "cursor" (markup:raw "&nbsp;"))))
          (t (markup:markup (:span (subseq buffer-string-representation 0 (cluffer:cursor-position (input-cursor minibuffer))))
                            (:span :id "cursor" (subseq buffer-string-representation 
                                                        (cluffer:cursor-position (input-cursor minibuffer))
                                                        (+ 1 (cluffer:cursor-position (input-cursor minibuffer)))))
                            (:span (subseq buffer-string-representation (+ 1  (cluffer:cursor-position (input-cursor minibuffer))))))))))

(defmethod generate-suggestion-html ((minibuffer minibuffer))
  (with-slots (suggestions suggestion-cursor) minibuffer
    (let ((lines (max-lines minibuffer))) ; TODO: Compute lines dynamically.
      (when (>= (- suggestion-cursor (suggestion-head minibuffer)) lines)
        (setf (suggestion-head minibuffer)
              (min
               (+ (suggestion-head minibuffer) 1)
               (length suggestions))))
      (when (< (- suggestion-cursor (suggestion-head minibuffer)) 0)
        (setf (suggestion-head minibuffer)
              (max
               (- (suggestion-head minibuffer) 1)
               0)))
      (markup:markup (:ul (loop repeat lines
                                for i from (suggestion-head minibuffer)
                                for suggestion in (nthcdr i suggestions)
                                collect
                                (markup:markup
                                 (:li :class (let ((selected-p (= i suggestion-cursor))
                                                   (marked-p (member suggestion (marked-suggestions minibuffer)))
                                                   (head-p (= i (suggestion-head minibuffer))))
                                               (str:join " " (delete-if #'null (list (and marked-p "marked")
                                                                                     (and selected-p "selected")
                                                                                     (and head-p "head")))))
                                      :id (cond ; TODO: Unused?
                                            ((= i suggestion-cursor) "selected")
                                            ((member suggestion (marked-suggestions minibuffer)) "marked")
                                            ((= i (suggestion-head minibuffer)) "head"))
                                      (match (object-display suggestion)
                                        ((guard s (not (str:emptyp s))) s)
                                        (_ " "))))))))))

(defmethod generate-prompt-html ((minibuffer minibuffer))
  (with-slots (suggestions marked-suggestions) minibuffer
    (format nil "~a~a:"
            (input-prompt minibuffer)
            (cond
              ((not suggestions)
               "")
              ((not (show-suggestion-count-p minibuffer))
               "")
              (marked-suggestions
               (format nil " [~a/~a]"
                       (length marked-suggestions)
                       (length suggestions)))
              ((and (not marked-suggestions)
                    (multi-selection-p minibuffer))
               (format nil " [0/~a]"
                       (length suggestions)))
              ((not marked-suggestions)
               (format nil " [~a]"
                       (length suggestions)))
              (t
               "[?]")))))

(defmethod set-suggestions ((minibuffer minibuffer) suggestions)
  "Set the suggestions and update the display."
  (setf (suggestions minibuffer) suggestions)
  (state-changed minibuffer)
  (update-display minibuffer))

(export-always 'update-input-buffer-display)
(defmethod update-input-buffer-display ((minibuffer minibuffer))
  "Update the display for the input buffer including the prompt and
completion count."
  (with-slots (suggestions marked-suggestions) minibuffer
    (let ((input-text (generate-input-html minibuffer))
          (prompt-text (generate-prompt-html minibuffer)))
      (evaluate-script
       minibuffer
       (ps:ps
         (setf (ps:chain document (get-element-by-id "prompt") |innerHTML|)
               (ps:lisp prompt-text))
         (setf (ps:chain document (get-element-by-id "input-buffer") |innerHTML|)
               (ps:lisp input-text)))))))

(export-always 'update-suggestions-display)
(defmethod update-suggestions-display ((minibuffer minibuffer))
  (let ((suggestion-html (generate-suggestion-html minibuffer)))
    (evaluate-script minibuffer
                     (ps:ps
                       (setf (ps:chain document (get-element-by-id "suggestions") |innerHTML|)
                             (ps:lisp suggestion-html))))))

(export-always 'update-display)
(defmethod update-display ((minibuffer minibuffer))
  (update-input-buffer-display minibuffer)
  (update-suggestions-display minibuffer))

(export-always 'state-changed)
(defmethod state-changed ((minibuffer minibuffer))
  (when (changed-callback minibuffer)
    (funcall-safely (changed-callback minibuffer))))

(export-always 'get-suggestion)
(defmethod get-suggestion ((minibuffer minibuffer))
  "Return the string for the current suggestion in the minibuffer."
  (with-slots (suggestions suggestion-cursor)
      minibuffer
    (and suggestions
         (object-string (nth suggestion-cursor suggestions)))))

(export-always 'get-marked-suggestions)
(defmethod get-marked-suggestions ((minibuffer minibuffer))
  "Return the list of strings for the marked suggestion in the minibuffer."
  (mapcar #'object-string (marked-suggestions minibuffer)))
