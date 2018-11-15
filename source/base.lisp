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
                 (opts:arg-parser-failed #'handle-malformed-cli-arg)
                 ;; (opts:missing-required-option #'handle-malformed-cli-arg)
                 )
    (opts:get-opts)))

(defun start ()
  (multiple-value-bind (options free-args)
      (parse-cli-args)
    (when (getf options :help)
      (opts:describe :prefix "Next command line usage:")
      (uiop:quit))
    (when (getf options :verbose)
      (format t "Arguments parsed: ~a and ~a~&" options free-args))
    (map nil 'funcall *deferred-variables*)
    (uiop:setup-temporary-directory)
    (ensure-directories-exist (xdg-data-home))
    (initialize-default-key-bindings)
    ;; load the user configuration if it exists
    (load *init-file-path* :if-does-not-exist nil)
    (initialize-bookmark-db)
    (initialize-history-db)
    (interface:initialize)
    (interface:start)
    ;; create the default buffers
    (setf *minibuffer*
          (make-instance 'buffer :name "minibuffer" :mode (minibuffer-mode)))
    (set-visible-active-buffer (generate-new-buffer "default" (document-mode)))

    ;; We can have many urls as positional arguments.
    (if free-args
        (loop for url in free-args do
             (%set-url-new-buffer url t))
        (set-url *start-page-url*))))

(defun initialize-default-key-bindings ()
  (define-key *global-map* (kbd "C-x C-c") 'interface:kill)
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
  (define-key *global-map* (kbd "C-y") 'interface:paste)
  (define-key *global-map* (kbd "C-w") 'interface:cut)
  (define-key *global-map* (kbd "M-w") 'interface:copy)
  (define-key *global-map* (kbd "M-x") 'execute-extended-command)
  (define-key *global-map* (kbd "ESCAPE x") 'execute-extended-command)
  (define-key *minibuffer-mode-map* (kbd "RETURN") #'(lambda () (return-input (mode *minibuffer*))))
  (define-key *minibuffer-mode-map* (kbd "C-RETURN") #'(lambda () (return-immediate (mode *minibuffer*))))
  (define-key *minibuffer-mode-map* (kbd "C-g") #'(lambda () (cancel-input (mode *minibuffer*))))
  (define-key *minibuffer-mode-map* (kbd "Escape") #'(lambda () (cancel-input (mode *minibuffer*))))
  (define-key *minibuffer-mode-map* (kbd "C-n") 'interface:minibuffer-select-next)
  (define-key *minibuffer-mode-map* (kbd "C-p") 'interface:minibuffer-select-previous)
  (define-key *document-mode-map* (kbd "M-f") 'history-forwards-query)
  (define-key *document-mode-map* (kbd "M-b") 'history-backwards)
  (define-key *document-mode-map* (kbd "C-g") 'go-anchor)
  (define-key *document-mode-map* (kbd "M-g") 'go-anchor-new-buffer)
  (define-key *document-mode-map* (kbd "S-g") 'go-anchor-new-buffer-focus)
  (define-key *document-mode-map* (kbd "C-f") 'history-forwards)
  (define-key *document-mode-map* (kbd "C-b") 'history-backwards)
  (define-key *document-mode-map* (kbd "C-p") 'scroll-up)
  (define-key *document-mode-map* (kbd "C-n") 'scroll-down)
  (define-key *document-mode-map* (kbd "C-x C-=") 'zoom-in-page)
  (define-key *document-mode-map* (kbd "C-x C-HYPHEN") 'zoom-out-page)
  (define-key *document-mode-map* (kbd "C-x C-0") 'unzoom-page)
  (define-key *document-mode-map* (kbd "C-l") 'set-url-current-buffer)
  (define-key *document-mode-map* (kbd "S-b o") 'set-url-from-bookmark)
  (define-key *document-mode-map* (kbd "S-b s") 'bookmark-current-page)
  (define-key *document-mode-map* (kbd "S-b g") 'bookmark-anchor)
  (define-key *document-mode-map* (kbd "C-[") 'switch-buffer-previous)
  (define-key *document-mode-map* (kbd "C-]") 'switch-buffer-next)
  (define-key *document-mode-map* (kbd "S-s s") 'add-search-boxes)
  (define-key *document-mode-map* (kbd "S-s n") 'next-search-hint)
  (define-key *document-mode-map* (kbd "S-s p") 'previous-search-hint)
  (define-key *document-mode-map* (kbd "S-s k") 'remove-search-hints)
  (define-key *document-mode-map* (kbd "C-.") 'jump-to-heading)
  (define-key *document-mode-map* (kbd "M->") 'scroll-to-bottom)
  (define-key *document-mode-map* (kbd "M-<") 'scroll-to-top))
