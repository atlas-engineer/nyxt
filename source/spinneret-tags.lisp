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
(deftag :ncode (body attrs &key (package :nyxt) literal-p (repl-p t) (config-p t) (copy-p t)
                     &allow-other-keys)
  "Generate the <pre> listing from the provided Lisp BODY.

REPL-P, CONFIG-P, and COPY-P mandate whether to add the buttons
for (respectively):
- Editing the BODY in the built-in REPL.
- Appending the BODY to the auto-config.lisp.
- Copying the source to clipboard.

Forms in BODY can be unquoted, benefiting from the editor formatting.

If LITERAL-P, BODY can be a single string that will be inserted verbatim with no
correctness checks. Try to avoid this option, as it does not guarantee that the
code is at the very least readable.

Forms in BODY can be quoted, in which case Spinneret won't even try to look at
its contents (useful if there are forms that start with a keyword, Spinneret
unconditionally converts those to tags unless the whole form is quoted.)"
  (remf attrs :package)
  (remf attrs :literal-p)
  (remf attrs :repl-p)
  (remf attrs :config-p)
  (remf attrs :copy-p)
  (let ((code (if literal-p
                  (first body)
                  (let ((*package* (find-package package)))
                    (serapeum:mapconcat
                     (alexandria:rcurry #'write-to-string :readably t :pretty t :case :downcase :right-margin 70)
                     ;; Process quoted arguments properly too.
                     (mapcar (lambda (form)
                               (if (and (listp form)
                                        (eq 'quote (first form)))
                                   (second form)
                                   form))
                             body)
                     (make-string 2 :initial-element #\newline))))))
    `(:div (:pre ,@attrs (:code (the string ,code)))
           ;; TODO: Add to config, Open in Nyxt editor, Open in external editor
           ,@(when  repl-p
               `((when (nyxt:current-buffer)
                   (:button.button
                           :onclick (ps:ps (nyxt/ps:lisp-eval
                                            (:title "edit-ncode-in-repl")
                                            (nyxt:buffer-load-internal-page-focus
                                             (read-from-string "nyxt/repl-mode:repl")
                                             :form ,code)))
                           :title "Open this code in Nyxt REPL to experiment with it."
                           "Edit in REPL"))))
           ,@(when config-p
               `((when (nyxt:current-buffer)
                   (:button.button
                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                     (:title "append-ncode-to-config")
                                     (alexandria:write-string-into-file
                                      ,code (nfiles:expand nyxt::*auto-config-file*)
                                      :if-exists :append)
                                     (nyxt:buffer-load-internal-page-focus
                                      (read-from-string "nyxt/repl-mode:repl")
                                      :form ,code)))
                    :title (format nil "Append this code to the auto-configuration file (~a)."
                                   (nfiles:expand nyxt::*auto-config-file*))
                    "Add to auto-config"))))
           ,@(when copy-p
               `((when (nyxt:current-buffer)
                   (:button.button
                    :onclick (ps:ps (nyxt/ps:lisp-eval
                                     (:title "copy-ncode")
                                     (funcall (read-from-string "nyxt:ffi-buffer-copy")
                                              (nyxt:current-buffer) ,code)))
                    :title "Copy the code to clipboard."
                    "Copy")))))))

(deftag :nxref (body attrs &key slot class-name function command variable package &allow-other-keys)
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
         ,@(when (and (getf attrs :class)
                      (or (getf attrs :slot)
                          (every #'null (list slot class-name function command variable package))))
             (error ":class attribute used ambiguously in :nxref tag. Use :class-name instead.")
             nil)
         (:code ,@(progn
                    (remf attrs :class-name)
                    (remf attrs :slot)
                    (remf attrs :function)
                    (remf attrs :command)
                    (remf attrs :variable)
                    (remf attrs :package)
                    attrs)
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
