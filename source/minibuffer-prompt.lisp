(in-package :nyxt)

(defmacro define-function (name args docstring &body body)
  "Eval ARGS and DOCSTRING then define function over the resulting lambda list
and string.
All ARGS are declared as `ignorable'."
  (let ((evaluated-args (eval args))
        (evaluated-docstring (eval docstring)))
    `(defun ,name ,evaluated-args
       ,evaluated-docstring
       (declare (ignorable ,@(set-difference (mapcar (lambda (arg) (if (listp arg) (first arg) arg))
                                                     evaluated-args)
                                             lambda-list-keywords)))
       ,@body)))

(export-always 'make-minibuffer)
(define-function make-minibuffer        ; TODO: Unexport since it's only used by prompt-minibuffer?  Wait until we've got a proper fix for meta-search.
    (append '(&rest args)
            '(&key)
            (delete 'input-buffer
                    (public-initargs 'minibuffer))
            '((input-buffer nil explicit-input-buffer)))
    "Return a new minibuffer instance."
  (let ((tmp-input-buffer (make-instance 'text-buffer:text-buffer))
        (tmp-input-cursor (make-instance 'text-buffer:cursor)))
    (cluffer:attach-cursor tmp-input-cursor tmp-input-buffer)
    (when explicit-input-buffer
      (text-buffer::insert-string tmp-input-cursor input-buffer))
    (apply #'make-instance 'user-minibuffer
           `(:input-buffer ,tmp-input-buffer
             :input-cursor ,tmp-input-cursor
             ,@args))))

(export-always 'prompt-minibuffer)
(define-function prompt-minibuffer (append '(&rest args)
                                           `(&key minibuffer
                                                 ,@(public-initargs 'minibuffer)))
  "Open the minibuffer, ready for user input.
ARGS are passed to the minibuffer constructor.
Example use:

\(prompt-minibuffer
  :suggestion-function #'my-suggestion-filter)

See the documentation of `minibuffer' to know more about the minibuffer options."
  (when (ready-p *browser*)
    (let ((channel (make-channel 1))
          (interrupt-channel (make-channel 1)))
      (ffi-within-renderer-thread
       *browser*
       (lambda ()
         (let ((minibuffer (or minibuffer
                               (apply #'make-minibuffer (append (list :channel channel
                                                                      :interrupt-channel interrupt-channel)
                                                                args)))))
           (with-muffled-body ("Minibuffer error: ~a" :condition)
             (alex:when-let ((f (setup-function minibuffer)))
               (funcall f minibuffer)))
           (state-changed minibuffer)
           (update-display minibuffer)
           (push minibuffer (active-minibuffers (current-window)))
           (apply #'show
                  (unless (suggestion-function minibuffer)
                    ;; We don't need so much height since there is no suggestion to display.
                    (list :height (minibuffer-open-single-line-height (current-window))))))))
      (calispel:fair-alt
        ((calispel:? channel result)
         result)
        ((calispel:? interrupt-channel)
         (error 'nyxt-minibuffer-canceled))))))
