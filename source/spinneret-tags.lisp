;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :spinneret)

(deftag :mayberaw (body attrs &key &allow-other-keys)
  attrs
  `(:raw (if (nyxt:html-string-p (progn ,@body))
             (progn ,@body)
             (escape-string (progn ,@body)))))

(deftag :nstyle (body attrs &key &allow-other-keys)
  `(:style ,@attrs (:raw ,@body)))

(deftag :nscript (body attrs &key &allow-other-keys)
  `(:script ,@attrs (:raw ,@body)))

(spinneret:deftag :nxref (body attr &key slot class-name function command variable package &allow-other-keys)
  (let ((symbol (or package variable function command slot class-name))
        (printable (or (first body) package variable function command slot class-name)))
    `(:a :href ,(cond
                  (package `(nyxt:nyxt-url (read-from-string "nyxt:describe-package") :package ,package))
                  (variable `(nyxt:nyxt-url (read-from-string "nyxt:describe-variable")
                                            :universal t :variable ,variable))
                  (function `(nyxt:nyxt-url (read-from-string "nyxt:describe-function")
                                            :universal t :fn ,function))
                  (command `(nyxt:nyxt-url (read-from-string "nyxt:describe-command")
                                           :universal t :command ,command))
                  (slot `(nyxt:nyxt-url (read-from-string "nyxt:describe-slot")
                                        :universal t :name ,slot :class ,class-name))
                  (class-name `(nyxt:nyxt-url (read-from-string "nyxt:describe-class")
                                              :universal t :class ,class-name))
                  (t `(nyxt:javascript-url
                       (ps:ps (nyxt/ps:lisp-eval
                               (:title "describe-any")
                               ;; Not defined yet:
                               (funcall (nyxt:resolve-symbol :describe-any :function)
                                        (princ-to-string ,symbol)))))))
         ;; TODO: Add :title so that documentation is available on hover.
         ;; TODO: Add keybindings for commands, like in `nyxt::command-markup'.
         (:code ,@(progn
                    (remf attr :class-name)
                    (remf attr :slot)
                    (remf attr :function)
                    (remf attr :command)
                    (remf attr :variable)
                    (remf attr :package)
                    attr)
                (let ((*print-case* :downcase))
                  (format nil "~a" ,printable))))))
