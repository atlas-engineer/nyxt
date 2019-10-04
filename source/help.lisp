;;; help.lisp --- functions for the user to get help on Next

(in-package :next)

;; TODO: Move to a separate package like other modes?
(define-mode help-mode ()
    "Mode for displaying documentation."
    ((keymap-schemes
      :initform
      (let ((emacs-map (make-keymap))
            (vi-map (make-keymap)))
        (define-key :keymap emacs-map
          "C-p" #'scroll-up
          "C-n" #'scroll-down)
        (define-key :keymap vi-map
          "k" #'scroll-up
          "j" #'scroll-down)
        (list :emacs emacs-map
              :vi-normal vi-map)))))

(defun package-symbols (p)
  (let (l) (do-symbols (s p l)
             (push s l))))

(defun variable-completion-filter (input)
  (fuzzy-match input (package-variables)))

(defun function-completion-filter (input)        ; TODO: Use `command-completion-filter'? And show packages?
  (fuzzy-match input (list-commands)))

;; TODO: This is barely useful as is since we don't have many globals.  We need to
;; augment the latter function so that we can inspect classes like remote-interface.
(define-command variable-inspect ()
  "Inspect a variable and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (make-instance 'minibuffer
                                      :completion-function #'variable-completion-filter
                                      :input-prompt "Inspect variable:")))
    (let* ((help-buffer (make-buffer
                         :title (str:concat "*Help-" (symbol-name input) "*")
                         :modes (cons 'help-mode
                                      (get-default 'buffer 'default-modes))))
           (help-contents (cl-markup:markup
                           (:h1 (symbol-name input))
                           (:p (documentation input 'variable))
                           (:h2 "Current Value:")
                           (:p (write-to-string (symbol-value input)))))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (rpc-buffer-evaluate-javascript help-buffer insert-help)
   (set-current-buffer help-buffer))))

;; TODO: Have both "function-inspect" and "command-inspect"?
(define-command command-inspect ()
  "Inspect a function and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (make-instance 'minibuffer
                                      :input-prompt "Inspect command:"
                                      :completion-function #'function-completion-filter)))
    (let* ((help-buffer (make-buffer
                         :title (str:concat "*Help-" (symbol-name (sym input)) "*")
                         :modes (cons 'help-mode
                                      (get-default 'buffer 'default-modes))))
           (help-contents (cl-markup:markup
                           (:h1 (symbol-name (sym input)))
                           (:h2 "Documentation")
                           (:p (write-to-string
                                ;; TODO: This only display the first method, i.e. the first command of one of the modes.
                                ;; Ask for modes instead?
                                (documentation (command-function input)
                                               t)))))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (rpc-buffer-evaluate-javascript help-buffer insert-help)
   (set-current-buffer help-buffer))))

(defun evaluate (string)
  "Evaluate all expressions in string and return a list of values.
This does not use an implicit PROGN to allow evaluating top-level expressions."
  (with-input-from-string (input string)
    (loop for object = (read input nil :eof)
          until (eq object :eof)
          collect (eval object))))

(define-command command-evaluate ()
  "Evaluate a form."
  (with-result (input (read-from-minibuffer
                       (make-instance 'minibuffer
                                      :input-prompt "Evaluate Lisp:")))
    (let* ((result-buffer (make-buffer
                           :title "*List Evaluation*" ; TODO: Reuse buffer / create REPL mode.
                           :modes (cons 'help-mode
                                        (get-default 'buffer 'default-modes))))
           (results (handler-case
                        (mapcar #'write-to-string (evaluate input))
                      (error (c) (format nil "~a" c))))
           (result-contents (apply #'concatenate 'string
                                   (cl-markup:markup
                                    (:h1 "Form")
                                    (:p input)
                                    (:h1 "Result"))
                                   (loop for result in results
                                         collect (cl-markup:markup (:p result)))))
           (insert-results (ps:ps (setf (ps:@ document Body |innerHTML|)
                                        (ps:lisp result-contents)))))
      (rpc-buffer-evaluate-javascript result-buffer insert-results)
   (set-current-buffer result-buffer))))

(define-command help ()
  "Print some help."
  (let* ((help-buffer (make-buffer
                       :title "*Help*"
                       :modes (cons 'help-mode
                                    (get-default 'buffer 'default-modes))))
         (help-contents
           (cl-markup:markup
            (:h1 "Getting started")
            (:p (:b "Warning: ") "Next is under active development. Feel free to "
                (:a :href "https://github.com/atlas-engineer/next/issues"
                    "report")
                " bugs, instabilities or feature wishes.")
            (:p "You can help with Next development by supporting us in various ways:"
                (:ul
                 (:li "Become a backer of the "
                      (:a :href "https://www.indiegogo.com/projects/next-browser-v1-4-0#/"
                          "1.4.0 crowdfunding campaign")
                      ".")
                 (:li "Support continuous development on "
                      (:a :href "https://www.patreon.com/next_browser"
                          "Patreon")
                      ".")
                 (:li "Spread the word on social media and "
                      (:a :href "https://github.com/atlas-engineer/next"
                          "star the project on GitHub")
                      "."))

                (:div
                      (:a :href "https://www.indiegogo.com/projects/next-browser-v1-4-0/x/13474269#/"
                       (:img :title "Help make our campaign a success"
                             :src "https://raw.githubusercontent.com/atlas-engineer/next/master/assets/indiegogo-logo-small.png"))
                      (:a :href "https://www.patreon.com/next_browser"
                       (:img :title "Support us on Patreon"
                             :src "https://raw.githubusercontent.com/atlas-engineer/next/master/assets/patreon-25x.png"))))

            (:h2 "Quickstart keys")
            (:ul
             (:li (:code "C-l") ": Load URL in tab")
             (:li (:code "M-l") ": Load URL in new tab")
             (:li (:code "C-x b") ", " (:code "C-x left/right") ": Switch tab")
             (:li (:code "C-b")  ": Backwards history")
             (:li (:code "C-f")  ": Forwards history")
             (:li (:code "C-g")  ": Follow link in current buffer")
             (:li (:code "M-g") ": Follow link in new buffer")
             (:li (:code "C-x C-c")  ": Quit")
             (:li (:code "M-x")  ": Run a command by name"))
            (:p "With VI bindings:")
            (:ul
             (:li (:code "o") ": Load URL in tab")
             (:li (:code "O") ": Load URL in new tab")
             (:li (:code "g b") ", " (:code "[") ", " (:code "]") ": Switch tab")
             (:li (:code "H")  ": Backwards history")
             (:li (:code "L")  ": Forwards history")
             (:li (:code "f")  ": Follow link in current buffer")
             (:li (:code "F") ": Follow link in new buffer")
             (:li (:code "C-x C-c")  ": Quit")
             (:li (:code ":")  ": Run a command by name"))
            (:p "The following keys exist as special keys:")
            (:ul
             (:li (:code "C") ": Control key")
             (:li (:code "S") ": Super (Windows key, Command key)")
             (:li (:code "M") ": Meta (Alt key, Option key)"))
            (:h2 "Customize and extend Next")
            (:p "Customization is possible through the creation of a "
                (:code "~/.config/next/init.lisp")
                " file. From here you can override and redefine any of the functions by defining your init file as part of the "
                (:code ":next")
                " package. For more information please see: "
                (:a :href "https://next.atlas.engineer/documentation#customization"
                    "customizing Next" )
                ".")
            (:h2 "Documentation")
            (:p "For full documentation about Next, how it works, and how to extend it please see the "
                (:a :href "https://next.atlas.engineer/documentation"
                    "user manual")
                ".")))

         (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                   (ps:lisp help-contents)))))
      (rpc-buffer-evaluate-javascript help-buffer insert-help)
  (set-current-buffer help-buffer)
    help-buffer))

(define-command next-version ()
  "Version number of this version of Next.
The version number is stored in the clipboard."
  (trivial-clipboard:text +version+)
  (echo "Version ~a" +version+))

(define-command messages ()
  "Show the *Messages* buffer."
  (let ((buffer (find-if (lambda (b)
                           (string= "*Messages*" (title b)))
                         (alexandria:hash-table-values (buffers *interface*)))))
    (unless buffer
      (setf buffer (make-buffer
                    :title "*Messages*"
                    :modes (cons 'help-mode
                                 (get-default 'buffer 'default-modes)))))
    (let* ((content
             (apply #'cl-markup:markup*
                    '(:h1 "Messages")
                    (reverse (messages-content *interface*))))
           (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                        (ps:lisp content)))))
      (rpc-buffer-evaluate-javascript buffer insert-content))
    (set-current-buffer buffer)
    buffer))

(define-command clear-messages ()
  "Clear the *Messages* buffer."
  (setf (messages-content *interface*) '())
  (echo "Messages cleared."))
