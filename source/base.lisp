;;; base.lisp --- main entry point into Next

(in-package :next)

(defun handle-malformed-cli-arg (condition)
  (format t "Error parsing argument ~a: ~a.~&" (opts:option condition) condition)
  (opts:describe)
  (uiop:quit))

(defun parse-cli-args ()
  "Parse command line arguments."
  (opts:define-opts
    (:name :help
           :description "Print this help and exit."
           :short #\h
           :long "help")
    (:name :verbose
           :short #\v
           :long "verbose"
           :description "Print debugging information to stdout."))

  (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                 (opts:missing-arg #'handle-malformed-cli-arg)
                 (opts:arg-parser-failed #'handle-malformed-cli-arg))
    (opts:get-opts)))

(defun start ()
  (map nil 'funcall *deferred-variables*)
  (ensure-directories-exist (xdg-data-home))
  (cond
    ((uiop:os-macosx-p) (set-aqua-conversion-table))
    ((uiop:os-unix-p) (set-x-conversion-table)))
  (map nil 'funcall *deferred-mode-initializations*)
  (initialize-bookmark-db)
  (initialize-history-db)
  (initialize-default-key-bindings)
  (setf *default-new-buffer-mode* #'document-mode)
  ;; load the user configuration if it exists
  (load *init-file-path* :if-does-not-exist nil)
  ;; create the interface object
  (setf *interface* (make-instance 'remote-interface))
  (start-interface *interface*)
  ;; initialize default state
  (setf *minibuffer* (make-instance 'minibuffer))
  (multiple-value-bind (window buffer) (make-window)
    (declare (ignore window))
    (set-url-buffer *start-page-url* buffer))
  t)

(defun initialize-default-key-bindings ()
  (define-key *global-map* (kbd "C-x C-c") '(lambda () (kill-interface *interface*)))
  (define-key *global-map* (kbd "C-x b") 'switch-buffer)
  (define-key *global-map* (kbd "C-x k") 'delete-buffer)
  (define-key *global-map* (kbd "M-l") 'set-url-new-buffer)
  (define-key *global-map* (kbd "S-b k") 'bookmark-delete)
  (define-key *global-map* (kbd "C-t") 'make-visible-new-buffer)
  (define-key *global-map* (kbd "S-b u") 'bookmark-url)
  (define-key *global-map* (kbd "C-x w") 'delete-active-buffer)
  (define-key *global-map* (kbd "S-h v") 'variable-inspect)
  (define-key *global-map* (kbd "S-h c") 'command-inspect)
  (define-key *global-map* (kbd "C-o") 'load-file)
  (define-key *global-map* (kbd "S-h s") 'start-swank)
  (define-key *global-map* (kbd "M-x") 'execute-extended-command)
  (define-key *global-map* (kbd "C-x 5 3") #'(lambda () (print (window-active *interface*))))
  (define-key *global-map* (kbd "C-x 5 2") 'make-window)
  (define-key *global-map* (kbd "C-x 5 0") 'delete-window)
  ;;; define self-insert commands for minibuffer for every type of character
  (loop for code below 128 do
    (let ((char (code-char code)))
      (define-key *minibuffer-mode-map* (kbd (string char))
        (lambda () (self-insert *minibuffer* (string char))))))
  (define-key *minibuffer-mode-map* (kbd "SPACE") #'(lambda () (self-insert *minibuffer* " ")))
  (define-key *minibuffer-mode-map* (kbd "C-f") #'(lambda () (cursor-forwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-b") #'(lambda () (cursor-backwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-d") #'(lambda () (delete-forwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "BACKSPACE") #'(lambda () (delete-backwards *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-a") #'(lambda () (cursor-beginning *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-e") #'(lambda () (cursor-end *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "RETURN") #'(lambda () (return-input *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-RETURN") #'(lambda () (return-immediate *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-g") #'(lambda () (cancel-input *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "ESCAPE") #'(lambda () (cancel-input *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-n") #'(lambda () (select-next *minibuffer*)))
  (define-key *minibuffer-mode-map* (kbd "C-p") #'(lambda () (select-previous *minibuffer*))))
