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
  (port:run-program)
  (map nil 'funcall *deferred-variables*)
  (ensure-directories-exist (xdg-data-home))
  (map nil 'funcall *deferred-mode-initializations*)
  (initialize-bookmark-db)
  (initialize-history-db)
  (setf *default-new-buffer-mode* #'document-mode)
  ;; load the user configuration if it exists
  (load *init-file-path* :if-does-not-exist nil)
  ;; create the interface object
  (setf *interface* (make-instance 'remote-interface))
  (start-interface *interface*)
  ;; initialize default state
  (setf *minibuffer* (make-instance 'minibuffer))
  (let ((port-running nil))
    (loop while (not port-running) do
      (handler-case
          (multiple-value-bind (window buffer) (make-window)
            (declare (ignore window))
            (set-url-buffer *start-page-url* buffer)
            (setf port-running t))
        (SB-BSD-SOCKETS:CONNECTION-REFUSED-ERROR ()
          (print "Connection refused")
          (sleep *platform-port-poll-interval*)
          (setf port-running nil))
        (SB-BSD-SOCKETS:HOST-NOT-FOUND-ERROR ()
          (print "Host not found")
          (sleep *platform-port-poll-interval*)))))
  ;; stay alive while running as a standalone program
  (port:run-loop)
  t)

(port:set-conversion-table)
(define-key *global-map* (key "C-x C-c") '(lambda () (kill-interface *interface*)))
(define-key *global-map* (key "C-[") 'switch-buffer-previous)
(define-key *global-map* (key "C-]") 'switch-buffer-next)
(define-key *global-map* (key "C-x b") 'switch-buffer)
(define-key *global-map* (key "C-x k") 'delete-buffer)
(define-key *global-map* (key "M-l") 'set-url-new-buffer)
(define-key *global-map* (key "S-b k") 'bookmark-delete)
(define-key *global-map* (key "C-t") 'make-visible-new-buffer)
(define-key *global-map* (key "S-b u") 'bookmark-url)
(define-key *global-map* (key "C-x w") 'delete-active-buffer)
(define-key *global-map* (key "S-h v") 'variable-inspect)
(define-key *global-map* (key "S-h c") 'command-inspect)
(define-key *global-map* (key "C-o") 'load-file)
(define-key *global-map* (key "S-h s") 'start-swank)
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
