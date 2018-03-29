;;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar *minibuffer-mode-map* (make-hash-table :test 'equalp))

(defclass minibuffer-mode (mode)
  ((completion-function :accessor completion-function)
   (callback-function :accessor callback-function)
   (callback-buffer :accessor callback-buffer)
   (setup-function :accessor setup-function)
   (cleanup-function :accessor cleanup-function)
   (empty-complete-immediate :accessor empty-complete-immediate)
   (input-buffer :accessor input-buffer :initform "")
   (cursor-index :accessor cursor-index :initform 0)
   (completions :accessor completions)
   (completion-index :accessor completion-index)))

(defmethod read-from-minibuffer (callback (self minibuffer-mode)
                                 &key completion setup cleanup empty-complete)
  (with-slots (callback-function completion-function callback-buffer
               setup-function cleanup-function empty-complete-immediate)
      self
    (setf callback-function callback)
    (setf completion-function completion)
    (setf setup-function setup)
    (setf cleanup-function cleanup)
    (setf empty-complete-immediate empty-complete)
    (setf callback-buffer *active-buffer*)
    (setup-default self)
    (when setup (funcall setup-function)))
  (set-active-buffer *minibuffer*)
  (show self))

(defmethod return-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (with-slots (callback-function cleanup-function
               empty-complete-immediate completion-function)
      self
    (if completion-function
	;; if there's a completion function
	(progn
	  (let ((completion (get-input-complete (mode *minibuffer*))))
	    (if completion
		;; if we're able to find a completion
		(funcall callback-function completion)
		;; if we can't find a completion
		(when empty-complete-immediate
		  ;; if we accept immediate output in place of completion
		  (return-immediate self)))))
	;; if there's no completion function
	(return-immediate self))
    (when cleanup-function
      (funcall cleanup-function)))
  (hide *interface*))

(defmethod return-immediate ((self minibuffer-mode))
  "Return without completion"
  (set-active-buffer (callback-buffer self))
  (with-slots (callback-function cleanup-function) self
    (funcall callback-function (input-buffer self))
    (when cleanup-function
      (funcall cleanup-function)))
  (hide *interface*))

(defmethod get-input-complete ((self minibuffer-mode)))

(defmethod cancel-input ((self minibuffer-mode))
  (set-active-buffer (callback-buffer self))
  (with-slots (cleanup-function) self
    (when cleanup-function
      (funcall cleanup-function)))
  (hide self))

(defmethod set-input ((self minibuffer-mode) input)
  (when input
    (minibuffer-execute-javascript
     *interface* "0"
     (ps:ps (ps:chain document (write (ps:lisp input)))))))

(defmethod erase-input ((self minibuffer-mode))
  (minibuffer-execute-javascript
   *interface* "0"
   (ps:ps
     (ps:chain document (open))
     (ps:chain document (close)))))

(defmethod setup-default ((self minibuffer-mode))
  (erase-input self)
  (set-input
   self
   (cl-markup:markup
    (:div :id "input" "input")
    (:div :id "completions" "completions"))))

(defmethod show ((self minibuffer-mode))
  (minibuffer-set-height *interface* "0" 300))

(defmethod hide ((self minibuffer-mode))
  (minibuffer-set-height *interface* "0" 10))

(defmethod self-insert ((self minibuffer-mode) character)
  (setf (input-buffer self)
        (cl-strings:insert character
                           (input-buffer self)
                           :position (cursor-index self)))
  (incf (cursor-index self))
  (update-display self))

(defmethod delete-forwards ((self minibuffer-mode))
  (with-slots (input-buffer cursor-index) self
    (unless (= cursor-index (length input-buffer))
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 cursor-index)
                         (subseq input-buffer (+ 1 cursor-index) (length input-buffer))))))
  (update-display self))

(defmethod delete-backwards ((self minibuffer-mode))
  (with-slots (input-buffer cursor-index) self
    (unless (= cursor-index 0)
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 (- cursor-index 1))
                         (subseq input-buffer cursor-index (length input-buffer))))
      (decf cursor-index)))
  (update-display self))

(defmethod cursor-forwards ((self minibuffer-mode))
  (with-slots (input-buffer cursor-index) self
    (when (< cursor-index (length input-buffer))
      (incf cursor-index)))
  (update-display self))

(defmethod cursor-backwards ((self minibuffer-mode))
  (with-slots (input-buffer cursor-index) self
    (when (> cursor-index 0)
      (decf cursor-index)))
  (update-display self))

(defmethod cursor-beginning ((self minibuffer-mode))
  (with-slots (cursor-index) self
    (setf cursor-index 0))
  (update-display self))

(defmethod cursor-end ((self minibuffer-mode))
  (with-slots (input-buffer cursor-index) self
    (setf cursor-index (length input-buffer)))
  (update-display self))

(defmethod update-display ((self minibuffer-mode))
  (with-slots (input-buffer cursor-index completions completion-function) self
    (when completion-function
      (setf completions (funcall completion-function input-buffer)))
    (minibuffer-execute-javascript
     *interface* "0"
     (ps:ps
       (setf (ps:chain document (get-element-by-id "input") inner-h-t-m-l)
             (ps:lisp
              (concatenate 'string
                           (subseq input-buffer 0 cursor-index)
                           "&#x25fc;"
                           (subseq input-buffer cursor-index (length input-buffer)))))))
    (minibuffer-execute-javascript
     *interface* "0"
     (ps:ps
       (setf (ps:chain document (get-element-by-id "completions") inner-h-t-m-l)
             (ps:lisp (cl-markup:markup
                       (:ul (loop for item in completions
                                  collect (cl-markup:markup (:li item)))))))))))

(defmethod select-next ((self minibuffer-mode)))

(defmethod select-previous ((self minibuffer-mode)))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-instance 'minibuffer-mode
		 :name "minibuffer"
		 :keymap *minibuffer-mode-map*))
