;;; minibuffer.lisp --- major mode for input

(in-package :next)

(defvar *minibuffer-mode-map* (make-hash-table :test 'equal))

(define-mode minibuffer (buffer mode)
  ((name :accessor name :initform "minibuffer")
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
   (completion-cursor :accessor completion-cursor :initform 0))
  (define-key *minibuffer-mode-map* (key "HYPHEN") #'(lambda () (self-insert *minibuffer* "-")))
  (define-key *minibuffer-mode-map* (key "SPACE") #'(lambda () (self-insert *minibuffer* " ")))
  (define-key *minibuffer-mode-map* (key "C-f") #'(lambda () (cursor-forwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "M-f") #'(lambda () (cursor-forwards-word *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-b") #'(lambda () (cursor-backwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "M-b") #'(lambda () (cursor-backwards-word *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "M-d") #'(lambda () (delete-forwards-word *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "M-BACKSPACE") #'(lambda () (delete-backwards-word *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "Right") #'(lambda () (cursor-forwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "Left") #'(lambda () (cursor-backwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-d") #'(lambda () (delete-forwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "DELETE") #'(lambda () (delete-forwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "BACKSPACE") #'(lambda () (delete-backwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-a") #'(lambda () (cursor-beginning *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-e") #'(lambda () (cursor-end *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-k") #'(lambda () (kill-line *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "RETURN") #'(lambda () (return-input *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-RETURN") #'(lambda () (return-immediate *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-g") #'(lambda () (cancel-input *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "ESCAPE") #'(lambda () (cancel-input *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-n") #'(lambda () (select-next *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-p") #'(lambda () (select-previous *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "Down") #'(lambda () (select-next *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "Up") #'(lambda () (select-previous *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-v") #'(lambda () (paste *minibuffer*)))
  (define-key *minibuffer-mode-map* (key "C-y") #'(lambda () (paste *minibuffer*))))

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
  (setf (display-mode minibuffer) :read)
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
	        (return-immediate minibuffer))))
	;; if there's no completion function
	(return-immediate minibuffer))
    (when cleanup-function
      (funcall cleanup-function))))

(defmethod return-immediate ((minibuffer minibuffer))
  (hide minibuffer)
  (setf (display-mode minibuffer) :nil)
  (set-active-buffer *interface* (callback-buffer minibuffer))
  (with-slots (callback-function cleanup-function) minibuffer
    (funcall callback-function (input-buffer minibuffer))
    (when cleanup-function
      (funcall cleanup-function))))

(defmethod cancel-input ((minibuffer minibuffer))
  (setf (display-mode minibuffer) :nil)
  (set-active-buffer *interface* (callback-buffer minibuffer))
  (with-slots (cleanup-function) minibuffer
    (when cleanup-function
      (funcall cleanup-function)))
  (hide minibuffer))

(defmethod set-input ((minibuffer minibuffer) input)
  (when input
    (minibuffer-evaluate-javascript
     *interface* (window-active *interface*)
     (ps:ps (ps:chain document (write (ps:lisp input)))))))

(defmethod erase-document ((minibuffer minibuffer))
  (minibuffer-evaluate-javascript
   *interface* (window-active *interface*)
   (ps:ps
     (ps:chain document (open))
     (ps:chain document (close)))))

(defmethod setup-default ((minibuffer minibuffer))
  (erase-document minibuffer)
  (setf (input-buffer minibuffer) "")
  (setf (input-buffer-cursor minibuffer) 0)
  (let ((style (cl-css:css
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
                                  :overflow-y "auto")
                  ("#cursor" :background-color "gray"
                             :color "white")
                  ("#prompt" :padding-right "4px"
                             :color "dimgray")
                  (ul :list-style "none"
                      :padding "0"
                      :margin "0")
                  (li :padding "2px")
                  (.selected :background-color "gray"
                             :color "white")))))
    (set-input minibuffer
               (cl-markup:markup
                (:head (:style style))
                (:body
                 (:div :id "container"
                  (:div :id "input" (:span :id "prompt" "") (:span :id "input-buffer" ""))
                  (:div :id "completions" "")))))))

(defmethod show ((minibuffer minibuffer))
  (window-set-minibuffer-height *interface*
                                (window-active *interface*)
                                *minibuffer-open-height*))

(defmethod hide ((minibuffer minibuffer))
  (window-set-minibuffer-height *interface*
                                (window-active *interface*)
                                *minibuffer-closed-height*))

(defmethod self-insert ((minibuffer minibuffer) characters)
  (setf (input-buffer minibuffer)
        (cl-strings:insert characters
                           (input-buffer minibuffer)
                           :position (input-buffer-cursor minibuffer)))
  (incf (input-buffer-cursor minibuffer) (length characters))
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

(defmethod char-at-cursor ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (if (< input-buffer-cursor (length input-buffer))
        (char (input-buffer *minibuffer*) (input-buffer-cursor *minibuffer*)))))

(defmethod cursor-forwards-word ((minibuffer minibuffer))
  (let ((stop-characters '(#\: #\/ #\- #\.)))
    (with-slots (input-buffer input-buffer-cursor) minibuffer
      (if (intersection stop-characters (list (char-at-cursor *minibuffer*)))
          (loop while (and
                       (intersection stop-characters (list (char-at-cursor *minibuffer*)))
                       (< input-buffer-cursor (length input-buffer)))
                do (incf input-buffer-cursor))
          (loop while (and
                       (not (intersection stop-characters (list (char-at-cursor *minibuffer*))))
                       (< input-buffer-cursor (length input-buffer)))
                do (incf input-buffer-cursor)))))
  (update-display minibuffer)
  (input-buffer-cursor minibuffer))

(defmethod cursor-backwards-word ((minibuffer minibuffer))
  (let ((stop-characters '(#\: #\/ #\- #\.)))
    (with-slots (input-buffer input-buffer-cursor) minibuffer
      (if (intersection stop-characters (list (char-at-cursor *minibuffer*)))
          (loop while (and
                       (intersection stop-characters (list (char input-buffer input-buffer-cursor)))
                       (> input-buffer-cursor 0))
                do (decf input-buffer-cursor))
          (loop while (and
                       (not (intersection stop-characters (list (char-at-cursor *minibuffer*))))
                       (> input-buffer-cursor 0))
                do (decf input-buffer-cursor)))))
  (update-display minibuffer)
  (input-buffer-cursor minibuffer))

(defmethod delete-forwards-word ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (let* ((current-cursor-position input-buffer-cursor)
           (new-cursor-position (cursor-forwards-word minibuffer))
           (transpose-distance (- new-cursor-position current-cursor-position)))
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 current-cursor-position)
                         (subseq input-buffer new-cursor-position (length input-buffer))))
      (setf input-buffer-cursor (- input-buffer-cursor transpose-distance))))
  (update-display minibuffer))

(defmethod delete-backwards-word ((minibuffer minibuffer))
  (with-slots (input-buffer input-buffer-cursor) minibuffer
    (let ((current-cursor-position input-buffer-cursor)
          (new-cursor-position (cursor-backwards-word minibuffer)))
      (setf input-buffer
            (concatenate 'string
                         (subseq input-buffer 0 new-cursor-position)
                         (subseq input-buffer current-cursor-position (length input-buffer))))))
  (update-display minibuffer))

(defmethod kill-line ((minibuffer minibuffer))
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
      (minibuffer-evaluate-javascript
       *interface* (window-active *interface*)
       (ps:ps
         (setf (ps:chain document (get-element-by-id "prompt") |innerHTML|)
               (ps:lisp (input-prompt minibuffer)))
         (setf (ps:chain document (get-element-by-id "input-buffer") |innerHTML|)
               (ps:lisp input-text))
         (setf (ps:chain document (get-element-by-id "completions") |innerHTML|)
               (ps:lisp completion-html)))))))

(defmethod select-next ((minibuffer minibuffer))
  (when (< (completion-cursor minibuffer) (- (length (completions minibuffer)) 1))
    (incf (completion-cursor minibuffer))
    (update-display minibuffer)
    (minibuffer-evaluate-javascript
     *interface* (window-active *interface*)
     (ps:ps (ps:chain (ps:chain document (get-element-by-id "selected"))
                      (scroll-into-view false))))))

(defmethod select-previous ((minibuffer minibuffer))
  (when (> (completion-cursor minibuffer) 0)
    (decf (completion-cursor minibuffer))
    (update-display minibuffer)
        (minibuffer-evaluate-javascript
     *interface* (window-active *interface*)
     (ps:ps (ps:chain (ps:chain document (get-element-by-id "selected"))
                      (scroll-into-view true))))))

(defmethod echo ((minibuffer minibuffer) text)
  (unless (eql (display-mode minibuffer) :read)
    (setf (display-mode minibuffer) :echo)
    (erase-document minibuffer)
    (window-set-minibuffer-height *interface*
                                  (window-active *interface*)
                                  *minibuffer-echo-height*)
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
                   (:p text)))))))

(defmethod echo-dismiss ((minibuffer minibuffer))
  (when (eql (display-mode minibuffer) :echo)
    (hide minibuffer)
    (erase-document minibuffer)))

(defmethod paste ((minibuffer minibuffer))
  (self-insert *minibuffer* (trivial-clipboard:text)))
