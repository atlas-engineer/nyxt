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

(defmethod read-from-minibuffer (callback-function
                                 (minibuffer minibuffer-mode)
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

(defmethod return-input ((minibuffer minibuffer-mode))
  (hide minibuffer)
  (set-active-buffer (callback-buffer minibuffer))
  (with-slots (callback-function cleanup-function
               empty-complete-immediate completion-function)
      minibuffer
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
		  (return-immediate minibuffer)))))
	;; if there's no completion function
	(return-immediate minibuffer))
    (when cleanup-function
      (funcall cleanup-function))))

(defmethod return-immediate ((minibuffer minibuffer-mode))
  "Return without completion"
  (set-active-buffer (callback-buffer minibuffer))
  (with-slots (callback-function cleanup-function) minibuffer
    (funcall callback-function (input-buffer minibuffer))
    (when cleanup-function
      (funcall cleanup-function)))
  (hide minibuffer))

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
     *interface* (window-active *interface*)
     (ps:ps (ps:chain document (write (ps:lisp input)))))))

(defmethod erase-document ((self minibuffer-mode))
  (minibuffer-execute-javascript
   *interface* (window-active *interface*)
   (ps:ps
     (ps:chain document (open))
     (ps:chain document (close)))))

(defmethod setup-default ((minibuffer minibuffer-mode))
  (erase-document minibuffer)
  (setf (input-buffer minibuffer) "")
  (setf (cursor-index minibuffer) 0)
  (set-input
   minibuffer
   (cl-markup:markup
    (:div :id "input" "input")
    (:div :id "completions" "completions"))))

(defmethod show ((self minibuffer-mode))
  (minibuffer-set-height *interface*
                         (window-active *interface*)
                         *minibuffer-open-height*))

(defmethod hide ((self minibuffer-mode))
  (minibuffer-set-height *interface*
                         (window-active *interface*)
                         *minibuffer-closed-height*))

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
     *interface* (window-active *interface*)
     (ps:ps
       (setf (ps:chain document (get-element-by-id "input") inner-h-t-m-l)
             (ps:lisp
              (concatenate 'string
                           (subseq input-buffer 0 cursor-index)
                           "[]"
                           (subseq input-buffer cursor-index (length input-buffer)))))))))

(defmethod select-next ((self minibuffer-mode)))

(defmethod select-previous ((self minibuffer-mode)))

(defun minibuffer-mode ()
  "Base mode for input"
  (make-instance 'minibuffer-mode
		 :name "minibuffer"
		 :keymap *minibuffer-mode-map*))
