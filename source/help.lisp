(in-package :next)

(define-command help ()
  "Print some help."
  (let ((help-buffer (help-mode :activate t :buffer (make-buffer :title "*Help*"))))
    (set-current-buffer help-buffer)
    (flet ((binding-keys (fn)
             (let* ((buffer (current-buffer))
                   (keymaps (cons (override-map buffer)
                                  (delete nil (mapcar #'keymap (modes buffer))))))
               (or (first (keymap:binding-keys fn keymaps))
                   "<NONE>"))))
      (let* ((help-contents
               (markup:markup
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
                 (:li (:code (binding-keys #'set-url-current-buffer)) ": Load URL")
                 (:li (:code (binding-keys #'set-url-new-buffer)) ": Load URL in new tab")
                 (:li (:code (binding-keys #'switch-buffer-previous)) ", " (:code (binding-keys #'switch-buffer-next)) ": Switch tab")
                 (:li (:code (binding-keys #'next/web-mode:history-backwards)) ": Backwards history")
                 (:li (:code (binding-keys #'next/web-mode:history-forwards)) ": Forwards history")
                 (:li (:code (binding-keys #'follow-hint)) ": Follow link in current buffer")
                 (:li (:code (binding-keys #'follow-hint-new-buffer)) ": Follow link in new buffer")
                 (:li (:code (binding-keys #'quit)) ": Quit")
                 (:li (:code (binding-keys #'execute-command)) ": Run a command by name")
                 (:li (:code (binding-keys #'describe-bindings)) ": List all bindings for the current tab"))
                (:p "Legend:")
                (:ul
                 (:li (:code "contol") " (" (:code "C") "): Control key")
                 (:li (:code "super") " (" (:code "S") "): Windows key, Command key")
                 (:li (:code "meta") " (" (:code "M") "): Alt key, Option key")
                 (:li (:code "shift") " (" (:code "s") "): Shift key"))

                (:p "Next proposes several " (:i "binding schemes") ", for instance CUA, Emacs, VI."
                    " For instance, call the " (:code "vi-normal-mode") " command to switch to VI bindings."
                    " To enable it by default, see the command documentation with "
                    (:code (binding-keys #'execute-command) " describe-command") " (bound to "
                    (:code (binding-keys #'describe-command)) ").")

                (:h2 "Customize and extend Next")
                (:p "Customization is possible through the creation of a "
                    (:code "~/.config/next/init.lisp")
                    " file. From here you can override and redefine any of the functions by defining your init file as part of the "
                    (:code ":next")
                    " package. For more information please see: "
                    (:a :href "https://next.atlas.engineer/documentation#customization"
                        "customizing Next")
                    ".")
                (:h2 "Documentation")
                (:p "The " (:i "minibuffer") " lets you fuzzy-search all commands."
                    " Press " (:code (binding-keys #'execute-command))
                    " then type " (:code "describe") " to list all documentation-related commands."
                    " These commands can display the documentation of all Next components.")
                (:p "For full documentation about Next, how it works, and how to extend it please see the "
                    (:a :href "https://next.atlas.engineer/documentation"
                        "user manual")
                    ".")))
             (insert-help (ps:ps (setf (ps:@ document Body |innerHTML|)
                                       (ps:lisp help-contents)))))
        (ffi-buffer-evaluate-javascript help-buffer insert-help)))
    help-buffer))
