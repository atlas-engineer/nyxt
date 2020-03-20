;;; help.lisp --- functions for the user to get help on Next

(in-package :next)

;; TODO: Move to a separate package like other modes?
(define-mode help-mode ()
    "Mode for displaying documentation."
    ((keymap-schemes
      :initform
      (let ((emacs-map (keymap:make-keymap "help-emacs-map"))
            (vi-map (keymap:make-keymap "help-vi-map")))
        (define-key emacs-map
          "C-p" #'scroll-up
          "C-n" #'scroll-down)
        (define-key vi-map
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
;; augment the latter function so that we can inspect classes like browser.
(define-command variable-inspect ()
  "Inspect a variable and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :completion-function #'variable-completion-filter
                        :input-prompt "Inspect variable")))
    (let* ((help-buffer (help-mode :activate t
                                   :buffer (make-buffer
                                            :title (str:concat "*Help-"
                                                               (symbol-name input)
                                                               "*"))))
           (help-contents (cl-markup:markup
                           (:h1 (symbol-name input))
                           (:p (documentation input 'variable))
                           (:h2 "Current Value:")
                           (:p (write-to-string (symbol-value input)))))
           (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                     (ps:lisp help-contents)))))
      (ipc-buffer-evaluate-javascript help-buffer insert-help)
      (set-current-buffer help-buffer))))

(declaim (ftype (function (command)) describe-command*))
(defun describe-command* (command)
  "Display NAME command documentation in a new focused buffer."
  (let* ((title (str:concat "*Help-" (symbol-name (sym command)) "*"))
         (help-buffer (help-mode :activate t
                                 :buffer (make-buffer :title title)))
         (key-keymap-pairs (nth-value 1 (keymap:binding-keys
                                         (command-function command)
                                         (all-keymaps))))
         (key-keymapname-pairs (mapcar (lambda (pair)
                                         (list (first pair)
                                               (keymap:name (second pair))))
                                       key-keymap-pairs))
         (help-contents (cl-markup:markup
                         (:h1 (symbol-name (sym command)))
                         (:h2 "Documentation")
                         (:p (write-to-string
                              ;; TODO: This only display the first method, i.e. the first command of one of the modes.
                              ;; Ask for modes instead?
                              (documentation (command-function command)
                                             t)))
                         (:p "Bindings: "
                             (format nil "~:{ ~S (~a)~:^, ~}" key-keymapname-pairs))
                         (:p "Source file: "
                             (getf (getf (swank:find-definition-for-thing (command-function command))
                                         :location)
                                   :file))
                         (:h2 "Source:")
                         (:pre (:code (write-to-string (sexp command))))))
         (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                   (ps:lisp help-contents)))))
    (ipc-buffer-evaluate-javascript help-buffer insert-help)
    (set-current-buffer help-buffer)))

;; TODO: Have both "function-inspect" and "command-inspect"?
(define-command command-inspect ()
  "Inspect a function and show it in a help buffer."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Inspect command"
                        :completion-function #'function-completion-filter)))
    (describe-command* input)))

(define-command bindings-inspect ()
  "Show a buffer with the list of all known bindings for the current buffer."
  (let* ((title (str:concat "*Help-bindings"))
         (help-buffer (help-mode :activate t
                                 :buffer (make-buffer :title title)))
         (window (ipc-window-active *browser*))
         (help-contents
                        (cl-markup:markup
                         (:h1 "Bindings")
                         (:p
                          (loop for keymap in (current-keymaps window)
                                collect (cl-markup:markup
                                         (:p (keymap:name keymap))
                                         (:table
                                          (loop for keyspec being the hash-keys in (keymap:keymap-with-parents->map keymap)
                                                  using (hash-value bound-value)
                                                collect (cl-markup:markup
                                                         (:tr
                                                          (:td keyspec)
                                                          (:td (string-downcase (sym (function-command bound-value)))))))))))))
         (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                   (ps:lisp help-contents)))))
    (ipc-buffer-evaluate-javascript help-buffer insert-help)
    (set-current-buffer help-buffer)))

(defun describe-key-dispatch-input (event buffer window)
  "Display bound value documentation.
Cancel with 'escape escape'.
Input is not forwarded.
This function can be used as a `window' `input-dispatcher'."
  (declare (ignore event buffer))
  (handler-case
      (progn
        (with-accessors ((key-stack key-stack)) *browser*
          (log:debug "Intercepted key ~a" (first (last key-stack)))
          (let ((escape-key (keymap:make-key :value "escape"))
                (bound-value (keymap:lookup-key key-stack
                                        (current-keymaps window))))
            (cond
              ((and bound-value (not (keymap:keymap-p bound-value)))
               ;; TODO: Highlight hit bindings and display translation if any.
               ;; For this, we probably need to call `lookup-key' on key-stack.
               (describe-command* (function-command bound-value))
               (setf key-stack nil)
               (setf (input-dispatcher window) #'dispatch-input-event))
              ((and (<= 2 (length key-stack))
                    (every (lambda (key) (keymap:key= key escape-key))
                           (last key-stack 2)))
               (echo "Cancelled.")
               (setf key-stack nil)
               (setf (input-dispatcher window) #'dispatch-input-event))
              (t
               (echo "Press a key sequence to describe (cancel with 'escape escape'): ~a"
                      (keyspecs-with-optional-keycode key-stack)))))))
    (error (c)
      (declare (ignore c))
      (setf (key-stack *browser*) nil)
      (setf (input-dispatcher window) #'dispatch-input-event)))
  ;; Never forward events.
  t)

(define-command key-inspect ()
  "Display binding of user-inputted keys."
  (setf (input-dispatcher (ipc-window-active *browser*)) #'describe-key-dispatch-input)
  (echo "Press a key sequence to describe (cancel with 'escape escape'):"))

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
                       (make-minibuffer
                        :input-prompt "Evaluate Lisp")))
    (let* ((result-buffer (help-mode :activate t
                                     :buffer (make-buffer
                                              ;; TODO: Reuse buffer / create REPL mode.
                                              :title "*List Evaluation*")))
           (results (handler-case
                        (mapcar #'write-to-string (evaluate input))
                      (error (c) (list (format nil "Error: ~a" c)))))
           (result-contents (apply #'concatenate 'string
                                   (cl-markup:markup
                                    (:h1 "Form")
                                    (:p input)
                                    (:h1 "Result"))
                                   (loop for result in results
                                         collect (cl-markup:markup (:p result)))))
           (insert-results (ps:ps (setf (ps:@ document Body |innerHTML|)
                                        (ps:lisp result-contents)))))
      (ipc-buffer-evaluate-javascript result-buffer insert-results)
      (set-current-buffer result-buffer))))

(define-command help ()
  "Print some help."
  (let* ((help-buffer (help-mode :activate t :buffer (make-buffer :title "*Help*")))
         (help-contents
           (cl-markup:markup
            (:h1 "Getting started")
            (:p (:b "Warning: ") "Next is under active development. Feel free to "
                (:a :href "https://github.com/atlas-engineer/next/issues"
                    "report")
                " bugs, instabilities or feature wishes.")
            (:p "You can help with Next development by supporting us in various ways:"
                (:ul
                 (:li "Support continuous development on "
                      (:a :href "https://www.patreon.com/next_browser"
                          "Patreon")
                      ".")
                 (:li "Spread the word on social media and "
                      (:a :href "https://github.com/atlas-engineer/next"
                          "star the project on GitHub")
                      ".")))
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
      (ipc-buffer-evaluate-javascript help-buffer insert-help)
  (set-current-buffer help-buffer)
    help-buffer))

(define-command next-version ()
  "Version number of this version of Next.
The version number is stored in the clipboard."
  (trivial-clipboard:text +version+)
  (echo "Version ~a" +version+))

(defclass messages-appender (log4cl-impl:appender)
  ())

(defmethod log4cl-impl:appender-do-append ((appender messages-appender) logger level log-func)
  (push
   `(:p ,(with-output-to-string (s)
           (log4cl-impl:layout-to-stream
            (slot-value appender 'log4cl-impl:layout) s logger level log-func)))
   (messages-content *browser*)))

(define-command messages ()
  "Show the *Messages* buffer."
  (let ((buffer (find-if (lambda (b)
                           (string= "*Messages*" (title b)))
                         (buffer-list))))
    (unless buffer
      (setf buffer (help-mode :activate t :buffer (make-buffer :title "*Messages*"))))
    (let* ((content
             (apply #'cl-markup:markup*
                    '(:h1 "Messages")
                    (reverse (messages-content *browser*))))
           (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                        (ps:lisp content)))))
      (ipc-buffer-evaluate-javascript buffer insert-content))
    (set-current-buffer buffer)
    buffer))

(define-command clear-messages ()
  "Clear the *Messages* buffer."
  (setf (messages-content *browser*) '())
  (echo "Messages cleared."))
