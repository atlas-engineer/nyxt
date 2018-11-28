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
           :description "Print debugging information to stdout.")
    (:name :init-file
           :short #\i
           :long "init-file"
           :arg-parser #'identity
           :description "Set path to initialization file."))

  (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                 (opts:missing-arg #'handle-malformed-cli-arg)
                 (opts:arg-parser-failed #'handle-malformed-cli-arg))
    (opts:get-opts)))

(define-command kill ()
  "Quit Next."
  (kill-interface *interface*)
  (kill-program *port*))

(defun start-with-port ()
  (multiple-value-bind (options free-args)
      (parse-cli-args)
    (when (getf options :help)
      (opts:describe :prefix "Next command line usage:")
      (uiop:quit))
    (when (getf options :verbose)
      (format t "Arguments parsed: ~a and ~a~&" options free-args))
    (setf *options* options
          *free-args* free-args))
  (run-program *port*)
  (start)
  (run-loop *port*))

(defun initialize-port ()
  (let ((port-running nil))
    (loop while (not port-running) do
      (handler-case
          (multiple-value-bind (window buffer) (make-window)
            (declare (ignore window))
            (when *free-args*
              (setf *start-page-url* (car *free-args*)))
            (set-url-buffer *start-page-url* buffer)
            ;; We can have many URLs as positional arguments.
            (loop for url in (cdr *free-args*) do
              (let ((buffer (make-buffer)))
                (set-url-buffer url buffer)
                (set-active-buffer *interface* buffer)))
            (setf port-running t))
        (SB-BSD-SOCKETS:CONNECTION-REFUSED-ERROR ()
          (print "Connection refused")
          (sleep *platform-port-poll-interval*)
          (setf port-running nil))
        (SB-BSD-SOCKETS:HOST-NOT-FOUND-ERROR ()
          (print "Host not found")
          (sleep *platform-port-poll-interval*))))))

(defun start ()
  (map nil 'funcall *deferred-variables*)
  (when (getf *options* :init-file)
    (setf *init-file-path* (getf *options* :init-file)))
  (ensure-directories-exist (xdg-data-home))
  (map nil 'funcall *deferred-mode-initializations*)
  (ensure-directories-exist (xdg-data-home))
  (initialize-bookmark-db)
  (initialize-history-db)
  (setf *default-new-buffer-mode* #'document-mode)
  ;; load the user configuration if it exists
  (if (not (string= *init-file-path* "-"))
      (load *init-file-path* :if-does-not-exist nil)
      ;; TODO: Don't block when nothing is found
      (format t "Initializing from standard input...")
      (loop for object = (read *standard-input* nil :eof)
            until (eq object :eof)
            do (eval object)))
  ;; create the interface object
  (setf *interface* (make-instance 'remote-interface))
  (start-interface *interface*)
  ;; initialize default state
  (setf *minibuffer* (make-instance 'minibuffer))
  (initialize-port)
  t)

(setf *port* (make-instance 'port))
(set-conversion-table *port*)
(define-key *global-map* (key "C-x C-c") 'kill)
(define-key *global-map* (key "C-[") 'switch-buffer-previous)
(define-key *global-map* (key "C-]") 'switch-buffer-next)
(define-key *global-map* (key "C-x b") 'switch-buffer)
(define-key *global-map* (key "C-x k") 'delete-buffer)
(define-key *global-map* (key "M-l") 'set-url-new-buffer)
(define-key *global-map* (key "S-b k") 'bookmark-delete)
(define-key *global-map* (key "C-t") 'make-visible-new-buffer)
(define-key *global-map* (key "C-m u") 'bookmark-url)
(define-key *global-map* (key "C-x w") 'delete-active-buffer)
(define-key *global-map* (key "C-h v") 'variable-inspect)
(define-key *global-map* (key "C-h c") 'command-inspect)
(define-key *global-map* (key "C-o") 'load-file)
(define-key *global-map* (key "C-h s") 'start-swank)
(define-key *global-map* (key "M-x") 'execute-extended-command)
(define-key *global-map* (key "C-x 5 3") #'(lambda () (print (window-active *interface*))))
(define-key *global-map* (key "C-x 5 2") 'make-window)
(define-key *global-map* (key "C-x 5 0") 'delete-window)
  ;;; define self-insert commands for minibuffer for every type of character
(loop for code below char-code-limit do
  (let ((char (code-char code)))
    (define-key *minibuffer-mode-map* (key (string char))
      (lambda () (self-insert *minibuffer* (string char))))))
(define-key *minibuffer-mode-map* (key "HYPHEN") #'(lambda () (self-insert *minibuffer* "-")))
(define-key *minibuffer-mode-map* (key "SPACE") #'(lambda () (self-insert *minibuffer* " ")))
(define-key *minibuffer-mode-map* (key "C-f") #'(lambda () (cursor-forwards *minibuffer*)))
(define-key *minibuffer-mode-map* (key "C-b") #'(lambda () (cursor-backwards *minibuffer*)))
(define-key *minibuffer-mode-map* (key "Right") #'(lambda () (cursor-forwards *minibuffer*)))
(define-key *minibuffer-mode-map* (key "Left") #'(lambda () (cursor-backwards *minibuffer*)))
(define-key *minibuffer-mode-map* (key "C-d") #'(lambda () (delete-forwards *minibuffer*)))
(define-key *minibuffer-mode-map* (key "DELETE") #'(lambda () (delete-forwards *minibuffer*)))
(define-key *minibuffer-mode-map* (key "BACKSPACE") #'(lambda () (delete-backwards *minibuffer*)))
(define-key *minibuffer-mode-map* (key "C-a") #'(lambda () (cursor-beginning *minibuffer*)))
(define-key *minibuffer-mode-map* (key "C-e") #'(lambda () (cursor-end *minibuffer*)))
(define-key *minibuffer-mode-map* (key "RETURN") #'(lambda () (return-input *minibuffer*)))
(define-key *minibuffer-mode-map* (key "C-RETURN") #'(lambda () (return-immediate *minibuffer*)))
(define-key *minibuffer-mode-map* (key "C-g") #'(lambda () (cancel-input *minibuffer*)))
(define-key *minibuffer-mode-map* (key "ESCAPE") #'(lambda () (cancel-input *minibuffer*)))
(define-key *minibuffer-mode-map* (key "C-n") #'(lambda () (select-next *minibuffer*)))
(define-key *minibuffer-mode-map* (key "C-p") #'(lambda () (select-previous *minibuffer*)))
(define-key *minibuffer-mode-map* (key "Down") #'(lambda () (select-next *minibuffer*)))
(define-key *minibuffer-mode-map* (key "Up") #'(lambda () (select-previous *minibuffer*)))
