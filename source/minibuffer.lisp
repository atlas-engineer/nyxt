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
   (input-buffer :accessor input-buffer :initform "")
   (cursor-index :accessor cursor-index :initform 0)
   (completions :accessor completions)
   (completion-index :accessor completion-index)))

(defmethod initialize-instance :after ((minibuffer minibuffer)
				       &key &allow-other-keys)
  (setf (keymap minibuffer) *minibuffer-mode-map*)
  (setf (mode minibuffer) minibuffer))

(defmethod read-from-minibuffer (callback-function
                                 (minibuffer minibuffer)
                                 &key completion-function setup-function
                                   cleanup-function empty-complete-immediate)
  (setf (callback-function minibuffer) callback-function)
  (setf (completion-function minibuffer) completion-function)
  (setf (setup-function minibuffer) setup-function)
  (setf (cleanup-function minibuffer) cleanup-function)
  (setf (empty-complete-immediate minibuffer) empty-complete-immediate)
  (setf (callback-buffer minibuffer) (active-buffer *interface*))
  (setup-default minibuffer)
  (update-display minibuffer)
  (when setup-function (funcall setup-function))
  (show minibuffer)
  (setf (active-buffer (window-active *interface*)) *minibuffer*))

(defmethod return-input ((minibuffer minibuffer))
  (hide minibuffer)
  (set-active-buffer (callback-buffer minibuffer))
  (with-slots (callback-function cleanup-function
               empty-complete-immediate completion-function)
      minibuffer
    (if completion-function
	;; if there's a completion function
	(progn
	  (let ((completion (get-input-complete *minibuffer*)))
	    (if completion
		;; if we're able to find a completion
		(funcall callback-function completion)
		;; if we can't find a completion
		(when empty-complete-immediate
		  ;; if we accept immediate output in place of completion
		  (return-immediate minibuffer)))))
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

(defmethod get-input-complete ((minibuffer minibuffer)))

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
  (setf (cursor-index minibuffer) 0)
  (let ((style (cl-css:css '((* :font-family "monospace,monospace"
                                :font-size "14px")
                             (body :border-top "4px solid gray"
                                   :margin "0"
                                   :padding "4px 6px")
                             (ul :list-style "none"
                                 :padding "0")))))
    (set-input minibuffer
               (cl-markup:markup
                (:head (:style style))
                (:div :id "input" "")
                (:div :id "completions" "")))))

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
                           :position (cursor-index minibuffer)))
  (incf (cursor-index minibuffer))
  (update-display minibuffer))

(defmethod delete-forwards ((minibuffer minibuffer))
  (with-slots (input-buffer cursor-index) minibuffer
    (unless (= cursor-index (length input-buffer))
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 cursor-index)
                         (subseq input-buffer
                                 (+ 1 cursor-index)
                                 (length input-buffer))))))
  (update-display minibuffer))

(defmethod delete-backwards ((minibuffer minibuffer))
  (with-slots (input-buffer cursor-index) minibuffer
    (unless (= cursor-index 0)
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 (- cursor-index 1))
                         (subseq input-buffer cursor-index (length input-buffer))))
      (decf cursor-index)))
  (update-display minibuffer))

(defmethod cursor-forwards ((minibuffer minibuffer))
  (with-slots (input-buffer cursor-index) minibuffer
    (when (< cursor-index (length input-buffer))
      (incf cursor-index)))
  (update-display minibuffer))

(defmethod cursor-backwards ((minibuffer minibuffer))
  (with-slots (input-buffer cursor-index) minibuffer
    (when (> cursor-index 0)
      (decf cursor-index)))
  (update-display minibuffer))

(defmethod cursor-beginning ((minibuffer minibuffer))
  (with-slots (cursor-index) minibuffer
    (setf cursor-index 0))
  (update-display minibuffer))

(defmethod cursor-end ((minibuffer minibuffer))
  (with-slots (input-buffer cursor-index) minibuffer
    (setf cursor-index (length input-buffer)))
  (update-display minibuffer))

(defmethod update-display ((minibuffer minibuffer))
  (with-slots (input-buffer cursor-index completions completion-function) minibuffer
    (if completion-function
        (setf completions (funcall completion-function input-buffer))
        (setf completions nil))
    (let ((input-text
            (concatenate 'string
                         (subseq input-buffer 0 cursor-index)
                         "[]"
                         (subseq input-buffer
                                 cursor-index
                                 (length input-buffer))))
          (completion-html
            (cl-markup:markup (:ul (loop for completion in completions
                                         collect (cl-markup:markup (:li completion)))))))
      (minibuffer-execute-javascript
       *interface* (window-active *interface*)
       (ps:ps
         (setf (ps:chain document (get-element-by-id "input") inner-h-t-m-l)
               (ps:lisp input-text))
         (setf (ps:chain document (get-element-by-id "completions") inner-h-t-m-l)
               (ps:lisp completion-html)))))))

(defmethod select-next ((minibuffer minibuffer)))

(defmethod select-previous ((minibuffer minibuffer)))
