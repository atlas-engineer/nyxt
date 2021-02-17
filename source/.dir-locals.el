((nil . ((fill-column . 80)))
 (lisp-mode
  . ((eval . (cl-flet ((enhance-imenu-lisp
                        (&rest keywords)
                        (dolist (keyword keywords)
                          (add-to-list
                           'lisp-imenu-generic-expression
                           (list (purecopy (concat (capitalize keyword)
                                                   (if (string= (substring-no-properties keyword -1) "s")
                                                       "es"
                                                       "s")))
                                 (purecopy (concat "^\\s-*("
                                                   (regexp-opt
                                                    (list (concat "define-" keyword))
                                                    t)
                                                   "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                                 2)))))
               ;; This adds the argument to the list of imenu known keywords.
               (enhance-imenu-lisp
                "bookmarklet-command"
                "class"
                "command"
                "ffi-method"
                "function"
                "mode"
                "parenscript"
                "user-class"))))))
