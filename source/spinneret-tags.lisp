;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :spinneret)

(deftag :mayberaw (body attrs &key &allow-other-keys)
  "Spinneret's :raw, but with HTML escaping if BODY _does not_ look like HTML."
  attrs
  `(:raw (if (nyxt:html-string-p (progn ,@body))
             (progn ,@body)
             (escape-string (progn ,@body)))))

(deftag :nstyle (body attrs &key &allow-other-keys)
  "Regular <style>, but with contents staying unescaped."
  `(:style ,@attrs (:raw ,@body)))

(deftag :nscript (body attrs &key &allow-other-keys)
  "Regular <script>, but with contents staying unescaped."
  `(:script ,@attrs (:raw ,@body)))

;; TODO: Store the location it's defined in as a :title or link for discoverability?
;; TODO: Allow editing it in REPL? Built-in editor? External editor?
;; TODO: Allow adding the snippet to the config.
;; FIXME: Maybe use :nyxt-user as the default package to not quarrel with REPL & config?
(deftag :ncode (body attrs &key (package :nyxt) &allow-other-keys)
  "Generate the <pre> listing from the provided Lisp BODY.

Forms in  BODY can be unquoted, benefiting from the editor formatting.

Forms in BODY can be quoted, in which case Spinneret won't even try to look at
its contents (useful if there are forms that start with a keyword, Spinneret
unconditionally converts those to tags unless the whole form is quoted.)"
  (remf attrs :package)
  (let ((code (let ((*package* (find-package package)))
                (serapeum:mapconcat
                 (alexandria:rcurry #'write-to-string :readably t :pretty t :case :downcase :right-margin 70)
                 ;; Process quoted arguments properly too.
                 (mapcar (lambda (form) (if (eq 'quote (first form))
                                            (second form)
                                            form))
                         body)
                 (make-string 2 :initial-element #\newline)))))
    `(:pre ,@attrs (:code ,code))))

(deftag :nxref (body attr &key slot class-name function command variable package &allow-other-keys)
  "Create a link to a respective describe-* page for BODY symbol.

Relies on the type keywords (SLOT, CLASS-NAME, FUNCTION, COMMAND, VARIABLE,
PACKAGE) to guess the right page, always provide those.

CLASS-NAME should be the symbol designating a class. It's not called CLASS
because Spinneret has special behavior for CLASS pre-defined and
non-overridable."
  (let ((symbol (or package variable function command slot class-name))
        (printable (or (first body) package variable function command slot class-name)))
    `(:a :href ,(cond
                  (package `(nyxt:nyxt-url (read-from-string "nyxt:describe-package") :package ,package))
                  (variable `(nyxt:nyxt-url (read-from-string "nyxt:describe-variable")
                                            :universal t :variable ,variable))
                  (function `(nyxt:nyxt-url (read-from-string "nyxt:describe-function")
                                            :universal t :fn ,function))
                  (command `(nyxt:nyxt-url (read-from-string "nyxt:describe-command")
                                           :command ,command))
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
         ,@(when (and (getf attr :class)
                      (or (getf attr :slot)
                          (every #'null (list slot class-name function command variable package))))
             (error ":class attribute used ambiguously in :nxref tag. Use :class-name instead.")
             nil)
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

(deftag :nsection (body attrs &key (title (alexandria:required-argument 'title))
                        (open-p t)
                        (id (str:remove-punctuation (str:downcase title)
                                                    :replacement "-"))
                        &allow-other-keys)
  "Collapsible and reference-able <section> with a neader.
TITLE should be a human-readable title for a section.
ID is the identifier with which to reference the section elsewhere. Is
auto-generated from title by replacing all the punctuation and spaces with
hyphens.
OPEN-P mandates whether the section is collapsed or not. True (= not collapsed)
by default"
  `(:section
    :id ,id
    (:details
     :open ,open-p
     (:summary (:h* :style "display: inline" ,@attrs ,title))
     ,@body)))
