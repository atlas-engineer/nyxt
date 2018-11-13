;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar *minibuffer-mode-map* (make-hash-table :test 'equalp))

(defclass minibuffer (buffer mode)
  ((name :accessor name :initform "minibuffer")
   (completion-function :accessor completion-function)
   (callback-function :accessor callback-function)
   (callback-buffer :accessor callback-buffer)
   (setup-function :accessor setup-function)
   (cleanup-function :accessor cleanup-function)
   (empty-complete-immediate :accessor empty-complete-immediate)
   (input-prompt :accessor input-prompt :initform "Input:")
   (input-buffer :accessor input-buffer :initform "")
   (input-buffer-cursor :accessor input-buffer-cursor :initform 0)
   (completions :accessor completions)
   (completion-cursor :accessor completion-cursor :initform 0)))

(defmethod initialize-instance :after ((minibuffer minibuffer)
				       &key &allow-other-keys)
  (setf (keymap minibuffer) *minibuffer-mode-map*)
  (setf (mode minibuffer) minibuffer))

(defmethod read-from-minibuffer (callback-function
                                 (minibuffer minibuffer)
                                 &key input-prompt completion-function setup-function
                                   cleanup-function empty-complete-immediate)
  (if input-prompt
      (setf (input-prompt minibuffer) input-prompt)
      (setf (input-prompt minibuffer) "Input:"))
  (setf (callback-function minibuffer) callback-function)
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
  (show minibuffer)
  (setf (active-buffer (window-active *interface*)) *minibuffer*))

(defmethod return-input ((minibuffer minibuffer))
  (hide minibuffer)
  (set-active-buffer (callback-buffer minibuffer))
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
	        (return-immediate minibuffer))))
	;; if there's no completion function
	(return-immediate minibuffer))
    (when cleanup-function
      (funcall cleanup-function))))

(defmethod return-immediate ((minibuffer minibuffer))
  "Return without completion"
  (set-active-buffer (callback-buffer minibuffer))
  (with-slots (callback-function cleanup-function) minibuffer
    (funcall callback-function (input-buffer minibuffer))
    (when cleanup-function
      (funcall cleanup-function)))
  (hide minibuffer))

(defmethod cancel-input ((minibuffer minibuffer))
  (set-active-buffer (callback-buffer minibuffer))
  (with-slots (cleanup-function) minibuffer
    (when cleanup-function
      (funcall cleanup-function)))
  (hide minibuffer))

(defmethod set-input ((minibuffer minibuffer) input)
  (when input
    (minibuffer-execute-javascript
     *interface* (window-active *interface*)
     (ps:ps (ps:chain document (write (ps:lisp input)))))))

(defmethod erase-document ((minibuffer minibuffer))
  (minibuffer-execute-javascript
   *interface* (window-active *interface*)
   (ps:ps
     (ps:chain document (open))
     (ps:chain document (close)))))

(defmethod setup-default ((minibuffer minibuffer))
  (erase-document minibuffer)
  (setf (input-buffer minibuffer) "")
  (setf (input-buffer-cursor minibuffer) 0)
  (let ((style (cl-css:css '((* :font-family "monospace,monospace"
                                :font-size "14px")
                             (body :border-top "4px solid dimgray"
                                   :margin "0"
                                   :padding "4px 6px")
                             ("#prompt" :padding-right "4px"
                                         :color "dimgray")
                             (ul :list-style "none"
                                 :padding "0")
                             (li :padding "2px")
                             (.selected :background-color "gray"
                                        :color "white")))))
    (set-input minibuffer
               (cl-markup:markup
                (:head (:style style))
                (:body
                 (:div (:span :id "prompt" "") (:span :id "input" ""))
                 (:div :id "completions" ""))))))

(defmethod show ((minibuffer minibuffer))
  (minibuffer-set-height *interface*
                         (window-active *interface*)
                         *minibuffer-open-height*))

(defmethod hide ((minibuffer minibuffer))
  (minibuffer-set-height *interface*
                         (window-active *interface*)
                         *minibuffer-closed-height*))

(defmethod self-insert ((minibuffer minibuffer) character)
  (setf (input-buffer minibuffer)
        (cl-strings:insert character
                           (input-buffer minibuffer)
                           :position (input-buffer-cursor minibuffer)))
  (incf (input-buffer-cursor minibuffer))
  (setf (completion-cursor minibuffer) 0)
  (update-display minibuffer))

(defmethod delete-forwards ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (unless (= input-buffer-cursor (length input-buffer))
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 input-buffer-cursor)
                         (subseq input-buffer
                                 (+ 1 input-buffer-cursor)
                                 (length input-buffer))))))
  (update-display minibuffer))

(defmethod delete-backwards ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (unless (= input-buffer-cursor 0)
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 (- input-buffer-cursor 1))
                         (subseq input-buffer input-buffer-cursor (length input-buffer))))
      (decf input-buffer-cursor)))
  (update-display minibuffer))

(defmethod cursor-forwards ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (when (< input-buffer-cursor (length input-buffer))
      (incf input-buffer-cursor)))
  (update-display minibuffer))

(defmethod cursor-backwards ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (when (> input-buffer-cursor 0)
      (decf input-buffer-cursor)))
  (update-display minibuffer))

(defmethod cursor-beginning ((minibuffer minibuffer))
  (with-slots (input-buffer-cursor) minibuffer
    (setf input-buffer-cursor 0))
  (update-display minibuffer))

(defmethod cursor-end ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (setf input-buffer-cursor (length input-buffer)))
  (update-display minibuffer))

(defun generate-input-html (input-buffer cursor-index)
  (concatenate 'string (subseq input-buffer 0 cursor-index) "[]"
               (subseq input-buffer cursor-index (length input-buffer))))

(defun generate-completion-html (completions cursor-index)
  (cl-markup:markup (:ul (loop for i from 0 for completion in completions
                               collect
                               (cl-markup:markup
                                (:li :class (when (equal i cursor-index) "selected")
                                     completion))))))

(defmethod update-display ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor completion-function
               completions completion-cursor)
      minibuffer
    (if completion-function
        (setf completions (funcall completion-function input-buffer))
        (setf completions nil))
    (let ((input-text (generate-input-html input-buffer input-buffer-cursor))
          (completion-html (generate-completion-html completions completion-cursor)))
      (minibuffer-execute-javascript
       *interface* (window-active *interface*)
       (ps:ps
         (setf (ps:chain document (get-element-by-id "prompt") |innerHTML|)
               (ps:lisp (input-prompt minibuffer)))
         (setf (ps:chain document (get-element-by-id "input") |innerHTML|)
               (ps:lisp input-text))
         (setf (ps:chain document (get-element-by-id "completions") |innerHTML|)
               (ps:lisp completion-html)))))))

(defmethod select-next ((minibuffer minibuffer))
  (when (< (completion-cursor minibuffer) (- (length (completions minibuffer)) 1))
    (incf (completion-cursor minibuffer))
    (update-display minibuffer)))

(defmethod select-previous ((minibuffer minibuffer))
  (when (> (completion-cursor minibuffer) 0)
    (decf (completion-cursor minibuffer))
    (update-display minibuffer)))
