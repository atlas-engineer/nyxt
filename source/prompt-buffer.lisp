;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class prompt-buffer (user-internal-buffer)
  ((channel (error "Channel required"))
   (interrupt-channel (error "Interrupt channel required"))
   (default-modes '(prompt-buffer-mode))
   ;; TODO: Need a changed-callback?
   ;; TODO: Need a invisible-input-p slot?
   ;; (invisible-input-p nil
;;                       :documentation "If non-nil, input is replaced by placeholder character.
;; This is useful to conceal passwords.")
   (history (minibuffer-generic-history *browser*)
            :type (or containers:ring-buffer-reverse null)
            :documentation "History of inputs for the minibuffer.
If nil, no history is used.")
   (hide-suggestion-count-p nil
                            :documentation "Show the number of chosen suggestions
inside brackets. It can be useful to disable, for instance for a yes/no question.")
   (suggestion-head 0
                    :export nil)
   (suggestion-cursor 0
                      :export nil)
   (content ""
            :accessor nil
            :export nil
            :documentation "The HTML content of the minibuffer.")
   ;; TODO: Need max-lines?
   ;; (max-lines 10
;;               :documentation "Max number of suggestion lines to show.
;; You will want edit this to match the changes done to `style'.")
   (style #.(cl-css:css
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
   (override-map (let ((map (make-keymap "overide-map")))
                   (define-key map
                     "escape"
                     ;; We compute symbol at runtime because
                     ;; nyxt/minibuffer-mode does not exist at
                     ;; compile-time since it's loaded afterwards.
                     (find-symbol (string 'cancel-input)
                                  (find-package 'nyxt/minibuffer-mode))))
                 :type keymap:keymap
                 :documentation "Keymap that takes precedence over all modes' keymaps."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "The prompt buffer is the interface for user interactions.
Each prompt spawns a new object: this makes it possible to nest prompts , such
as invoking `prompt-history'.

A prompt query is typically done as follows:

\(let ((tags (prompt-minibuffer
              :input-prompt \"Space-separated tag (s) \"
              :default-modes '(set-tag-mode minibuffer-mode)
              :suggestion-function (tag-suggestion-filter))))
  ;; Write form here in which `tags' is bound to the resulting element(s).
  )"))

(define-user-class prompt-buffer)

(defmethod initialize-instance :after ((prompt-buffer prompt-buffer) &key)
  (hooks:run-hook (prompt-buffer-make-hook *browser*) prompt-buffer)
  ;; We don't want to show the input in the suggestion list when invisible.
  (unless (suggestion-function prompt-buffer)
    ;; If we have no suggestion function, then we have no suggestions beside
    ;; immediate input, so we must allow them as valid suggestion.
    (setf (must-match-p prompt-buffer) nil))
  (setf (must-match-p prompt-buffer)
        (if (invisible-input-p prompt-buffer)
            t ; This way the minibuffer won't display the input as a suggestion.
            (must-match-p prompt-buffer)))
  (initialize-modes prompt-buffer))

(defun show (&key (prompt-buffer (first (active-prompt-buffers (current-window)))) height)
  "Show the last active prompt-buffer, if any."
  (when prompt-buffer
    (ffi-window-set-prompt-buffer-height
     (current-window)
     (or height
         (prompt-buffer-open-height (current-window))))))

(defun hide (prompt-buffer)
  "Hide PROMPT-BUFFER and display next active one, if any."
  (match (cleanup-function prompt-buffer)
    ((guard f f) (funcall-safely f)))
  ;; Note that PROMPT-BUFFER is not necessarily first in the list, e.g. a new
  ;; prompt-buffer was invoked before the old one reaches here.
  (alex:deletef (active-prompt-buffers (current-window)) prompt-buffer)
  (if (active-prompt-buffers (current-window))
      (progn
        (show)
        ;; TODO: Remove?
        ;; We need to refresh so that the nested prompt-buffers don't have to do it.
        ;; (state-changed (first (active-prompt-buffers (current-window))))
        ;; (update-display (first (active-prompt-buffers (current-window))))
        )
      (progn
        (ffi-window-set-prompt-buffer-height (current-window) 0))))

(defmethod erase-document ((prompt-buffer prompt-buffer)) ; TODO: Remove, empty automatically when `content' is set.
  (evaluate-script minibuffer (ps:ps
                                (ps:chain document (open))
                                (ps:chain document (close)))))

(defmethod setup-default ((prompt-buffer prompt-buffer)) ; TODO: Rename.
  (erase-document prompt-buffer)
  (setf (content prompt-buffer)         ; TODO: Make content setter that sets the HTML.
        (markup:markup
         (:head (:style (style prompt-buffer)))
         (:body
          (:div :id "container"
                (:div :id "prompt-input"
                      (:span :id "prompt" "")
                      (:input :type "text" :id "input"))
                (:div :id "suggestions" ""))))))

(defmethod generate-prompt-html ((prompt-buffer prompt-buffer))
  (with-slots (suggestions marked-suggestions) prompt-buffer
    (format nil "~a~a:"
            (input-prompt prompt-buffer)
            (cond
              ((not suggestions)
               "")
              ((hide-suggestion-count-p prompt-buffer)
               "")
              (marked-suggestions
               (format nil " [~a/~a]"
                       (length marked-suggestions)
                       (length suggestions)))
              ((and (not marked-suggestions)
                    (multi-selection-p prompt-buffer))
               (format nil " [0/~a]"
                       (length suggestions)))
              ((not marked-suggestions)
               (format nil " [~a]"
                       (length suggestions)))
              (t
               "[?]")))))

(defmethod generate-suggestion-html ((minibuffer minibuffer)) ; TODO: Generate suggestions as table.
  (with-slots (suggestions suggestion-cursor) minibuffer
    (let ((lines (max-lines minibuffer)))
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
      (markup:markup
       (:ul (loop repeat lines
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
                        (match (object-display suggestion)
                          ((guard s (not (str:emptyp s))) s)
                          (_ " "))))))))))

(defmethod update-display ((prompt-buffer prompt-buffer))
  (flet ((update-input-buffer-display ((prompt-buffer prompt-buffer))
           "Update the display for the input buffer including the prompt and
completion count."
           (with-slots (suggestions marked-suggestions) prompt-buffer
             (let ((input-text (generate-input-html prompt-buffer))
                   (prompt-text (generate-prompt-html prompt-buffer)))
               (evaluate-script
                prompt-buffer
                (ps:ps
                  (setf (ps:chain document (get-element-by-id "prompt") |innerHTML|)
                        (ps:lisp prompt-text))
                  (setf (ps:chain document (get-element-by-id "input-buffer") |innerHTML|)
                        (ps:lisp input-text)))))))
         (update-suggestions-display ((prompt-buffer prompt-buffer))
           (let ((suggestion-html (generate-suggestion-html prompt-buffer)))
             (evaluate-script
              prompt-buffer
              (ps:ps
                (setf (ps:chain document (get-element-by-id "suggestions") |innerHTML|)
                      (ps:lisp suggestion-html)))))))
    (update-input-buffer-display prompt-buffer)
    (update-suggestions-display prompt-buffer)))


(defmethod selection ((minibuffer minibuffer))
  "Return the selected suggestions.
If no suggestion is marked, return the currently selected suggestion."

  (with-slots (suggestions suggestion-cursor)
      minibuffer
    (and suggestions
         (object-string (nth suggestion-cursor suggestions)))))

(export-always 'get-marked-suggestions)
(defmethod get-marked-suggestions ((minibuffer minibuffer))
  "Return the list of strings for the marked suggestion in the minibuffer."
  (mapcar #'object-string (marked-suggestions minibuffer)))

(export-always 'prompter-if-confirm)    ; TODO: Rename to `if-confirm' once `minibuffer' is gone.
(defmacro prompter-if-confirm (prompt yes-form &optional no-form)
  "Ask the user for confirmation before executing either YES-FORM or NO-FORM.
YES-FORM is executed on  \"yes\" answer, NO-FORM -- on \"no\".
PROMPT is a list fed to `format nil'.

Example usage defaulting to \"no\":

\(let ((*yes-no-choices* '(:no \"no\" :yes \"yes\")))
  (if-confirm (\"Are you sure to kill ~a buffers?\" count)
     (delete-buffers)))"
  ;; TODO: Can we keep the `*yes-no-choices*' customization option?
  `(let ((answer (prompt
                  :input-prompt (format nil ,@prompt)
                  :suggestion-function (yes-no-suggestion-filter)
                  :hide-suggestion-count-p t)))
     (if (confirmed-p answer)
         ,yes-form
         ,no-form)))
