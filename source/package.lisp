;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.

#+sb-package-locks
(serapeum:eval-always
  (when (find-package :nyxt)
    (sb-ext:unlock-package :nyxt)))

(uiop:define-package nyxt
  (:use #:common-lisp)
  #+nyxt-debug-make-instance
  (:shadow #:make-instance)
  (:export #:use-nyxt-package-nicknames)
  (:documentation "The core package of Nyxt, the infinitely extensible browser.

This package should not be modified by the users.

It's recommended to use the `nyxt-user' package instead to create new functions,
modes, commands, etc."))
#+sb-package-locks
(sb-ext:lock-package :nyxt)

(in-package :nyxt)
(defvar *imports* '((:alexandria :compose :curry :rcurry)
                    (:trivia :match :multiple-value-match :lambda-match :guard)
                    (:keymap :define-key :define-scheme)
                    (:class-star :define-class)
                    (:serapeum :export-always :->))
  "Default list of symbol imports used by `nyxt:define-package'.")

(loop :for (package . symbols) in *imports*
      :do (import (mapcar (lambda (symbol) (intern (symbol-name symbol) package))
                          symbols)
                  :nyxt))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop :for (nickname package) in
        '((:alex :alexandria-2)
          (:sera :serapeum)
          (:lpara :lparallel)
          (:class* :hu.dwim.defclass-star)
          (:hooks :nhooks)
          (:files :nfiles))
        :do (trivial-package-local-nicknames:add-package-local-nickname nickname package :nyxt)))

(defmacro nyxt::use-nyxt-package-nicknames (&optional (package *package*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((*package* (find-package ,package)))
       (dolist (pkgs (trivial-package-local-nicknames:package-local-nicknames :nyxt))
         (trivial-package-local-nicknames:add-package-local-nickname (first pkgs) (package-name (rest pkgs))
                                                                     (find-package ,package))))))

(defmacro without-package-locks (&body body)
  #+sb-package-locks
  `(sb-ext:without-package-locks
     ,@body)
  #-sb-package-locks
  `(progn ,@body))

(serapeum:export-always 'define-package :nyxt)
(defmacro define-package (name &rest options)
  "A helper around `uiop:define-package'.
`:common-lisp' and `:nyxt' are automatically used.
`nyxt::*imports*' are automatically imported."
  (let* ((uses (append (serapeum:keep :use options :key #'first)
                       '((:use :common-lisp :nyxt))))
         (imports (append (serapeum:keep :import-from options :key #'first)
                          (mapcar (lambda (import) (cons :import-from import))
                                  *imports*)))
         (options (remove :use (remove :import-from options :key #'first)
                          :key #'first)))
    `(progn
       (serapeum:eval-always
         (without-package-locks
           (uiop:define-package ,name
             ,@uses
             ,@imports
             ,@options)))
       (nyxt::use-nyxt-package-nicknames ',name)
       #+sb-package-locks
       (sb-ext:lock-package ',name))))

(deftype class-symbol ()
  `(and symbol (satisfies find-class)))

#+nyxt-debug-make-instance
(serapeum:-> make-instance (class-symbol &rest t) t)
#+nyxt-debug-make-instance
(defun make-instance (sym &rest args)
  "This wrapper of `make-instance' can be used from a test suite to check if all
calls are made on valid classes.

The check seems to only work on CCL, but not even everywhere, for instance slot
initforms may not be caught."
  (apply #'cl:make-instance sym args))

(uiop:define-package nyxt-user
  (:use #:common-lisp #:nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:documentation "Package left for the user to fiddle with.  If the
configuration file package is left unspecified, it defaults to this.  It's not
recommended to use `nyxt' itself to avoid clobbering internal symbols.

By default, the `:nyxt' and `:common-lisp' packages are `:use'd.

To import more symbols, you can use the `import' function.
For instance, to access `match' directly (without having to prefix it with
`trivia:', add this at the top of your configuration file:

  (import 'trivia:match)

You can also use package local nicknames if you want to abbreviate package
prefix.
For instance, to be able to use `alex:' and `sera:' in place of `alexandria:'
and `serapeum:':

  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria-2 :nyxt-user)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum :nyxt-user)"))

(trivial-package-local-nicknames:add-package-local-nickname :hooks :nhooks :nyxt-user)
(trivial-package-local-nicknames:add-package-local-nickname :files :nfiles :nyxt-user)
