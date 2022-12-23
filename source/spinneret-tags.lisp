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

(serapeum:eval-always
  (defun remove-smart-quoting (form)
    "If the form is quoted or quazi-quoted, return the unquoted/evaluated variant.
Otherwise, return the form as is."
    (cond
      ((and (listp form)
            (eq 'quote (first form)))
       (second form))
      #+(or sbcl ecl)
      ((and (listp form)
            (eq #+sbcl 'sb-int:quasiquote
                #+ecl 'si:quasiquote
                ;; FIXME: CCL expands quasiquote to
                ;; `list*' call.
                ;; TODO: Other implementations?
                (first form)))
       (eval form))
      (t form))))

(deftag :nselect (body attrs &key (id (alexandria:required-argument 'id)) &allow-other-keys)
  "Generate <select> tag from the BODY resembling cond clauses.

BODY forms can be of two kinds:
- (SYMBOL . FORMS) -- creates <option value=\"symbol\">symbol</option> and runs
  FORMS when it's selected.
- ((SYMBOL DISPLAY) . FORMS) -- creates <option value=\"symbol\">display</option>
  and runs FORMS when it's selected. DISPLAY can be an HTML string producing form."
  (with-gensyms (value)
    (once-only (id)
      `(:select.button
        ,@attrs
        :id ,id
        :onchange
        (when (nyxt:current-buffer)
          (ps:ps (nyxt/ps:lisp-eval
                  (:title "nselect-choice")
                  (let ((,value (nyxt:ps-eval (ps:chain (nyxt/ps:qs document (+ "#" (ps:lisp ,id))) value))))
                    (str:string-case ,value
                      ,@(loop for (clause . forms) in body
                              for symbol = (first (uiop:ensure-list clause))
                              collect (cons (nyxt:prini-to-string symbol)
                                            forms)))))))
        ,@(loop for (clause . forms) in body
                for symbol = (first (uiop:ensure-list clause))
                for display = (second (uiop:ensure-list clause))
                for title = (third (uiop:ensure-list clause))
                collect `(:option
                          :value ,(nyxt:prini-to-string symbol)
                          ,@(when title
                              (list :title title))
                          ,(string-capitalize (or display symbol))))))))

;; TODO: Store the location it's defined in as a :title or link for discoverability?
;; FIXME: Maybe use :nyxt-user as the default package to not quarrel with REPL & config?
(deftag :ncode (body attrs &key (package :nyxt) inline-p literal-p (repl-p t) (config-p t) (copy-p t)
                     file (editor-p file) (external-editor-p file)
                     &allow-other-keys)
  "Generate the <pre> listing from the provided Lisp BODY.

Most *-P arguments mandate whether to add the buttons for:
- Editing the BODY in the built-in REPL (REPL-P).
- Appending the BODY to the auto-config.lisp (CONFIG-P).
- Copying the source to clipboard (COPY-P).
- Editing the FILE it comes from (if present), in
  - Nyxt built-in `editor-mode' (EDITOR-P).
  - External-editor (EXTERNAL-EDITOR-P).

Forms in BODY can be unquoted, benefiting from the editor formatting.

If LITERAL-P, BODY can be a single string that will be inserted verbatim with no
correctness checks. Try to avoid this option, as it does not guarantee that the
code is at the very least readable.

Forms in BODY can be quoted, in which case Spinneret won't even try to look at
its contents (useful if there are forms that start with a keyword, Spinneret
unconditionally converts those to tags unless the whole form is quoted.)"
  (remf attrs :package)
  (remf attrs :inline-p)
  (remf attrs :literal-p)
  (remf attrs :repl-p)
  (remf attrs :config-p)
  (remf attrs :copy-p)
  (remf attrs :file)
  (remf attrs :editor-p)
  (remf attrs :external-editor-p)
  (let* ((code (if literal-p
                   (first body)
                   (let ((*package* (find-package package)))
                     (serapeum:mapconcat
                      (alexandria:rcurry #'nyxt:prini-to-string :readably t :right-margin 70)
                      ;; Process quoted arguments properly too.
                      (mapcar #'remove-smart-quoting body)
                      (make-string 2 :initial-element #\newline)))))
         (*print-escape* nil)
         (id (nyxt:prini-to-string (gensym)))
         (select-code
           `(:nselect
              :id ,id
              :style ,(unless inline-p
                        "position: absolute; top: 0; right: 0; margin: 0; padding: 2px")
              ,@(when copy-p
                  `(((copy "Copy" "Copy the code to clipboard.")
                     (funcall (read-from-string "nyxt:ffi-buffer-copy")
                              (nyxt:current-buffer) ,code))))
              ,@(when config-p
                  `(((config
                      "Add to auto-config"
                      (format nil "Append this code to the auto-configuration file (~a)."
                              (nfiles:expand nyxt::*auto-config-file*)))
                     (alexandria:write-string-into-file
                      ,code (nfiles:expand nyxt::*auto-config-file*)
                      :if-exists :append))))
              ,@(when repl-p
                  `(((repl
                      "Try in REPL"
                      "Open this code in Nyxt REPL to experiment with it.")
                     (nyxt:buffer-load-internal-page-focus
                      (read-from-string "nyxt/repl-mode:repl")
                      :form ,code))))
              ,@(when (and file editor-p)
                  `(((editor
                      "Open in built-in editor"
                      "Open the file this code comes from in Nyxt built-in editor-mode.")
                     (funcall (read-from-string "nyxt/editor-mode:edit-file")
                              ,file))))
              ,@(when (and file external-editor-p)
                  `(((external-editor
                      "Open in external editor"
                      "Open the file this code comes from in external editor.")
                     (uiop:launch-program
                      (append (funcall (read-from-string "nyxt:external-editor-program")
                                       (symbol-value (read-from-string "nyxt:*browser*")))
                              (list (uiop:native-namestring ,file))))))))))
    (if inline-p
        `(:span (:code ,@attrs (the string ,code)) ,select-code)
        ;; https://spdevuk.com/how-to-create-code-copy-button/
        `(:div :style "position:relative"
               (:pre ,@attrs ,select-code
                     (:code (the string ,code)))))))

(deftag :nxref (body attrs &key slot mode class-name function command variable package &allow-other-keys)
  "Create a link to a respective describe-* page for BODY symbol.

Relies on the type keywords (SLOT, CLASS-NAME, FUNCTION, COMMAND, VARIABLE,
PACKAGE) to guess the right page, always provide those.

CLASS-NAME should be the symbol designating a class. It's not called CLASS
because Spinneret has special behavior for CLASS pre-defined and
non-overridable."
  (let* ((first (first body))
         (symbol (or package variable function command slot class-name (when (symbolp first) first)))
         (printable (or (when (and (symbolp first) (eq first symbol))
                          (second body))
                        (first body) package variable function command slot class-name)))
    `(:code
      ,@(progn
          (remf attrs :class-name)
          (remf attrs :mode)
          (remf attrs :slot)
          (remf attrs :function)
          (remf attrs :command)
          (remf attrs :variable)
          (remf attrs :package)
          attrs)
      (:a.link
       :href ,(cond
                (package `(nyxt:nyxt-url (read-from-string "nyxt:describe-package") :package ,package))
                (variable `(nyxt:nyxt-url (read-from-string "nyxt:describe-variable")
                                          :variable ,variable))
                ((or command function) `(nyxt:nyxt-url (read-from-string "nyxt:describe-function")
                                                       :fn ,(or command function)))
                (slot `(nyxt:nyxt-url (read-from-string "nyxt:describe-slot")
                                      :name ,slot :class ,class-name))
                ((or mode class-name)
                 `(nyxt:nyxt-url (read-from-string "nyxt:describe-class")
                                 :class ,(or mode class-name)))
                (t `(nyxt:nyxt-url (read-from-string "nyxt:describe-any")
                                   :input ,symbol)))
       :title
       (uiop:strcat
        ,(cond
           (package "[PACKAGE] ")
           (variable "[VARIABLE] ")
           (command "[COMMAND] ")
           (function "[FUNCTION] ")
           ((and slot class-name) `(format nil "[SLOT in ~s]" ,class-name))
           (mode "[MODE] ")
           (class-name "[CLASS] "))
        (first (serapeum:lines
                (documentation
                 ,(cond
                    (package `(find-package ,package))
                    (variable variable)
                    ((or command function) `(symbol-function ,(or command function)))
                    (slot slot)
                    ((or mode class-name) `(find-class ,(or mode class-name)))
                    (t symbol))
                 (quote ,(cond
                           (variable 'variable)
                           ((or command function) 'function)
                           ((or mode class-name) 'type)
                           (t t)))))))
       ,@(when (and (getf attrs :class)
                    (or (getf attrs :slot)
                        (every #'null (list slot class-name function command variable package))))
           (error ":class attribute used ambiguously in :nxref tag. Use :class-name instead.")
           nil)
       (let ((*print-escape* nil))
         (nyxt:prini-to-string ,printable)))
      ,@(when command
          `(" ("
            (funcall (read-from-string "nyxt::binding-keys")
                     ,command ,@(when mode
                                  `(:modes (cl:list (make-instance ,mode)))))
            ")")))))

(deftag :nsection (body attrs &key (title (alexandria:required-argument 'title))
                        level
                        (open-p t)
                        (id (str:remove-punctuation (str:downcase title)
                                                    :replacement "-"))
                        &allow-other-keys)
  "Collapsible and reference-able <section> with a neader.
TITLE should be a human-readable title for a section.
LEVEL (if provided), is the level of heading for the section. If it's 2, the
heading is <h2>, if it's 3, then <h3> etc. If not provided, uses <h*> Spinneret
tag to intelligently guess the current heading level.
ID is the identifier with which to reference the section elsewhere. Is
auto-generated from title by replacing all the punctuation and spaces with
hyphens, if not provided.
OPEN-P mandates whether the section is collapsed or not. True (= not collapsed)
by default"
  (check-type level (or null (integer 2 6)))
  (remf attrs :title)
  (remf attrs :level)
  (remf attrs :open-p)
  (remf attrs :id)
  `(let ((spinneret::*html-path*
           ;; Push as many :section tags into the path, as necessary to imply
           ;; LEVEL for the sections inside this one. A trick on Spinneret to
           ;; make it think it's deeply nested already.
           (append
            spinneret::*html-path*
            (make-list ,(if level
                            `(1- (- ,level (spinneret::heading-depth)))
                            0)
                       :initial-element :section))))
     (:section.section
      :id ,id
      (:details
       :open ,open-p
       (:summary (:h* :style "display: inline"
                   ,@attrs ,title
                   " " (:a.link :href ,(uiop:strcat "#" id) "#")))
       ,@body))))

(deftag :nbutton (body attrs &key (text (alexandria:required-argument 'text)) title buffer &allow-other-keys)
  "A Lisp-invoking button with TEXT text and BODY action.
Evaluates (via `nyxt/ps:lisp-eval') the BODY in BUFFER when clicked.
Forms in BODY can be unquoted, benefiting from the editor formatting."
  `(:button.button
    :onclick (ps:ps
               (nyxt/ps:lisp-eval
                (:title ,(or title text)
                        ,@(when buffer
                            (list :buffer buffer)))
                ,@(mapcar #'remove-smart-quoting body)))
    ,@attrs
    ,text))

(deftag :ninput (body attrs &key rows cols &allow-other-keys)
  "Nicely styled <textarea> with a reasonable number of ROWS to accommodate the BODY."
  ;; This is to prevent Spinneret from stripping newlines off the tag contents.
  (once-only ((input-contents `(or (progn ,@(mapcar #'remove-smart-quoting body)) "")))
    `(:textarea.input
      :rows (or ,rows (1+ (count #\Newline ,input-contents)) 1)
      :cols (or ,cols (ignore-errors (apply #'max (mapcar #'length (str:lines ,input-contents)))) 80)
      ,@attrs
      (:raw (the string ,input-contents)))))
