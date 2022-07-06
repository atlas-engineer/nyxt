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

(spinneret:deftag :nxref (body attr &key class slot-of function command variable package &allow-other-keys)
  `(:a :href ,(cond
                (package `(nyxt:nyxt-url (read-from-string "nyxt:describe-package")
                                         :universal t :package ,package))
                (variable `(nyxt:nyxt-url (read-from-string "nyxt:describe-variable")
                                          :universal t :variable ,variable))
                (function `(nyxt:nyxt-url (read-from-string "nyxt:describe-function")
                                          :universal t :fn ,function))
                (command `(nyxt:nyxt-url (read-from-string "nyxt:describe-command")
                                         :universal t :command ,command))
                (class `(nyxt:nyxt-url (read-from-string "nyxt:describe-class")
                                       :universal t :class ,class))
                (slot-of `(nyxt:nyxt-url (read-from-string "nyxt:describe-slot")
                                         :universal t :name ,(first body) :class ,slot-of))
                (t `(nyxt:javascript-url
                     (ps:ps (nyxt/ps:lisp-eval
                             (:title "describe-any")
                             ;; Not defined yet:
                             (funcall (nyxt:resolve-symbol :describe-any :function)
                                      (princ-to-string ,@body)))))))
       ;; TODO: Add :title so that documentation is available on hover.
       ;; TODO: Add keybindings for commands, like in `nyxt::command-markup'.
       (:code ,@(progn
                  (remf attr :class)
                  (remf attr :slot-of)
                  (remf attr :function)
                  (remf attr :command)
                  (remf attr :variable)
                  (remf attr :package)
                  attr)
              (let ((*print-case* :downcase))
                (format nil "~a" ,(or (first body) package variable function command class))))))
