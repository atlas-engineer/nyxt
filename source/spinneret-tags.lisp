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
    "If the form is quoted or quasi-quoted, return the unquoted/evaluated variant.
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

- (VALUE . FORMS) -- creates <option value=\"value\">value</option> and runs
  FORMS when it's selected.

- ((VALUE DISPLAY TITLE) . FORMS) -- creates an
  <option value=\"value\" title=\"title\">display</option>
  and runs FORMS when it's selected. DISPLAY and TITLE are optional literal
  strings.

In both cases, VALUE should be a literal (and printable) atom. For instance,
symbol, number, string, or keyword.

Example:
\(:nselect :id \"number-guessing\"
  (1 (nyxt:echo \"Too low!\"))
  (2 (nyxt:echo \"Correct!\"))
  (3 (nyxt:echo \"Too high!\")))"
  (with-gensyms (var)
    (once-only (id)
      `(:select.button
        ,@attrs
        :id ,id
        :onchange
        (when (nyxt:current-buffer)
          (ps:ps (nyxt/ps:lisp-eval
                  (:title "nselect-choice")
                  (let ((,var (nyxt:ps-eval (ps:chain (nyxt/ps:qs document (+ "#" (ps:lisp ,id))) value))))
                    (str:string-case ,var
                      ,@(loop for (clause . forms) in (mapcar #'remove-smart-quoting body)
                              for value = (first (uiop:ensure-list clause))
                              collect (cons (nyxt:prini-to-string value)
                                            forms)))))))
        ,@(loop for (clause) in (mapcar #'remove-smart-quoting body)
                for value = (first (uiop:ensure-list clause))
                for display = (second (uiop:ensure-list clause))
                for title = (third (uiop:ensure-list clause))
                collect `(:option
                          :value ,(nyxt:prini-to-string value)
                          ,@(when title
                              (list :title title))
                          ,(string-capitalize (or display (nyxt:prini-to-string value)))))))))

(deftag :nxref (body attrs &key slot mode class-name function macro command (command-key-p t) variable package &allow-other-keys)
  "Create a link to a respective describe-* page for BODY symbol.

Relies on the type keywords (SLOT, MODE, CLASS-NAME, FUNCTION, MACRO, COMMAND,
VARIABLE, PACKAGE) to guess the right page, always provide those.

CLASS-NAME, if present, should be the symbol designating a class. It's not
called CLASS because Spinneret has special behavior for CLASS pre-defined and
non-overridable."
  (let* ((first (first body))
         (symbol (or package variable function macro command slot class-name mode
                     (when (symbolp first) first)))
         (printable (or (when (and (symbolp first) (eq first symbol))
                          (second body))
                        first package variable function macro command slot class-name mode)))
    `(:a.link
      :target "_blank"
      ,@attrs
      :href ,(cond
               (package `(nyxt:nyxt-url (read-from-string "nyxt:describe-package") :package ,package))
               (variable `(nyxt:nyxt-url (read-from-string "nyxt:describe-variable")
                                         :variable ,variable))
               ((or command function macro)
                `(nyxt:nyxt-url (read-from-string "nyxt:describe-function")
                                :fn ,(or command function macro)))
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
          (macro "[MACRO] ")
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
                   (macro macro)
                   ((or command function) `(symbol-function ,(or command function)))
                   (slot slot)
                   ((or mode class-name) `(find-class ,(or mode class-name)))
                   (t symbol))
                (quote ,(cond
                          (variable 'variable)
                          ((or command function macro) 'function)
                          ((or mode class-name) 'type)
                          (t t)))))))
      ,@(when (and (getf attrs :class)
                   (or (getf attrs :slot)
                       (every #'null (list slot class-name mode function macro command variable package))))
          (error ":class attribute used ambiguously in :nxref tag. Use :class-name instead.")
          nil)
      (:code
       (let ((*print-escape* nil))
         (nyxt:prini-to-string ,printable))
       ,@(when (and command command-key-p)
           `(" ("
             (funcall (read-from-string "nyxt::binding-keys")
                      ,command ,@(when mode
                                   `(:modes (cl:list (make-instance ,mode)))))
             ")"))))))

(serapeum:eval-always
  (defun resolve-linkable-symbols (form)
    "Helper function for :NCODE tag.
Returns all the linkable symbols from FORM as multiple values:
- Function symbols.
- Variable symbols.
- Macro symbols.
- All the special forms (including some macros and functions needing extra care).
- All the strings that may potentially be resolvable with
  `nyxt:resolve-backtick-quote-links'."
    (let ((functions (list))
          (classes (list))
          (variables (list))
          (macros (list))
          (specials (list))
          (all-specials '(quote
                          flet labels symbol-macrolet macrolet
                          block catch eval-when progv lambda
                          progn prog1 unwind-protect tagbody setf setq multiple-value-prog1
                          let let* prog prog*
                          return-from throw the
                          multiple-value-call funcall apply
                          function
                          go locally))
          (linkable-strings (list)))
      (labels ((resolve-symbols-internal (form)
                 (typecase form
                   (boolean nil)
                   (keyword nil)
                   (cons
                    (let ((first (first form)))
                      (alexandria:destructuring-case form
                        ;; More forms: def*, make-instance, slots, special forms?
                        ((make-instance class &rest args)
                         (push first functions)
                         (if (and (listp class)
                                  (eq 'quote (first class)))
                             (push (second class) classes)
                             (resolve-symbols-internal class))
                         (resolve-symbols-internal args))
                        (((flet labels symbol-macrolet macrolet)
                          (&rest bindings) &body body)
                         (push first specials)
                         (mapcar (lambda (b)
                                   (resolve-symbols-internal (cddr b)))
                                 bindings)
                         (mapc #'resolve-symbols-internal body))
                        (((block catch eval-when progv lambda) arg &body body)
                         (declare (ignore arg))
                         (push first specials)
                         (mapc #'resolve-symbols-internal body))
                        (((progn prog1 unwind-protect tagbody setf setq multiple-value-prog1)
                          &body body)
                         (push first specials)
                         (mapc #'resolve-symbols-internal body))
                        (((let let* prog prog*) (&rest bindings) &body body)
                         (push first specials)
                         (mapcar (alexandria:compose
                                  #'resolve-symbols-internal #'second #'uiop:ensure-list)
                                 bindings)
                         (mapc #'resolve-symbols-internal body))
                        (((return-from throw the) arg &optional value)
                         (declare (ignore arg))
                         (push first specials)
                         (resolve-symbols-internal value))
                        (((multiple-value-call funcall apply) function &rest args)
                         (push first specials)
                         (match function
                           ((list 'quote name)
                            (pushnew name functions))
                           ((list 'function name)
                            (pushnew name functions)))
                         (mapc #'resolve-symbols-internal args))
                        ((function value)
                         (push first specials)
                         (pushnew value functions))
                        (((go locally) &rest values)
                         (declare (ignore values))
                         (push first specials))
                        ((t &rest rest)
                         (cond
                           ((listp first)
                            (resolve-symbols-internal first)
                            (mapc #'resolve-symbols-internal rest))
                           ((member first all-specials)
                            (pushnew first specials))
                           ((and (symbolp first)
                                 (nsymbols:macro-symbol-p first))
                            (pushnew first macros)
                            (let* ((arglist (uiop:symbol-call :nyxt :arglist first))
                                   (rest-position (or (position '&rest arglist)
                                                      (position '&body arglist))))
                              (if rest-position
                                  (mapc #'resolve-symbols-internal (nthcdr rest-position rest))
                                  (mapc #'resolve-symbols-internal rest))))
                           ((and (symbolp first)
                                 (nsymbols:function-symbol-p first))
                            (pushnew first functions)
                            (mapc #'resolve-symbols-internal rest)))))))
                   (symbol
                    (when (nsymbols:variable-symbol-p form)
                      (pushnew form variables)))
                   (string
                    (pushnew form linkable-strings)))))
        (resolve-symbols-internal form)
        (values (set-difference functions all-specials) classes variables macros specials linkable-strings))))
  (defun prini* (object package)
    (nyxt:prini-to-string object :readably t :right-margin 70 :package package))
  (defun htmlize-body (form listing package)
    (let ((*suppress-inserted-spaces* t)
          (*html-style* :tree)
          (*print-pretty* nil))
      (when (listp form)
        (multiple-value-bind (functions classes variables macros specials linkable-strings)
            (resolve-linkable-symbols form)
          ;; We use \\s, because lots of Lisp symbols include non-word
          ;; symbols and would break if \\b was used.
          (macrolet ((replace-symbol-occurences (symbols type &key (prefix "(\\()") (suffix "(\\)|\\s)") (style :plain))
                       (alexandria:with-gensyms (sym sym-listing)
                         `(dolist (,sym ,symbols)
                            (when (search (prini* ,sym package) listing)
                              (let ((,sym-listing (prini* ,sym package)))
                                (setf listing
                                      (ppcre:regex-replace-all
                                       (uiop:strcat
                                        ,prefix (ppcre:quote-meta-chars ,sym-listing) ,suffix)
                                       listing
                                       (list
                                        0 ,(case style
                                             (:link `(with-html-string
                                                       (:nxref ,type ,sym ,sym-listing)))
                                             (:plain `(with-html-string
                                                        (:nxref :style "color: inherit; background-color: inherit;" ,type ,sym ,sym-listing)))
                                             (:span `(with-html-string
                                                       (:span.accent ,sym-listing))))
                                        1)))))))))
            (replace-symbol-occurences macros :macro :style :link)
            (replace-symbol-occurences functions :function :prefix "(\\(|#'|')")
            (replace-symbol-occurences classes :class-name :prefix "(')")
            (replace-symbol-occurences
             variables :variable :prefix "(\\s)" :suffix "(\\)|\\s)")
            (replace-symbol-occurences specials nil :style :span))
          (dolist (string linkable-strings)
            (setf listing (str:replace-all (prini* string package)
                                           (prini* (nyxt:resolve-backtick-quote-links string package) package)
                                           listing)))))
      listing)))

;; TODO: Store the location it's defined in as a :title or link for discoverability?
;; FIXME: Maybe use :nyxt-user as the default package to not quarrel with REPL & config?
(deftag :ncode (body attrs &key (package :nyxt) inline-p literal-p (repl-p t) (config-p t) (copy-p t)
                     file (editor-p file) (external-editor-p file)
                     &allow-other-keys)
  "Generate the <pre>/<code> listing from the provided Lisp BODY.

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
  (let* ((body (mapcar #'remove-smart-quoting body))
         (printed-body (mapcar (alexandria:rcurry #'prini* package) body))
         (code (if literal-p
                   (first body)
                   (str:join (make-string 2 :initial-element #\newline) printed-body)))
         (htmlized-code
           (unless literal-p
             (str:join
              (make-string 2 :initial-element #\newline)
              (mapcar (alexandria:rcurry #'htmlize-body package) body printed-body))))
         (*print-escape* nil)
         (id (nyxt:prini-to-string (gensym)))
         (printable-code `(if (stringp ,code)
                              ,code
                              (prini* ,code ,package)))
         (injectable-code
           (if literal-p
               `(if (stringp ,code)
                    ,code
                    (:raw (htmlize-body ,code (prini* ,code ,package) ,package)))
               `(:raw (the string ,htmlized-code))))
         (select-code
           `(:nselect
              :id ,id
              :style ,(unless inline-p
                        "position: absolute; top: 0; right: 0; margin: 0; padding: 2px")
              ,@(when copy-p
                  `(((copy "Copy" "Copy the code to clipboard.")
                     (funcall (read-from-string "nyxt:ffi-buffer-copy")
                              (nyxt:current-buffer) ,printable-code))))
              ,@(when config-p
                  `(((config
                      "Add to auto-config"
                      (format nil "Append this code to the auto-configuration file (~a)."
                              (nfiles:expand nyxt::*auto-config-file*)))
                     (alexandria:write-string-into-file
                      ,printable-code (nfiles:expand nyxt::*auto-config-file*)
                      :if-exists :append))))
              ,@(when repl-p
                  `(((repl
                      "Try in REPL"
                      "Open this code in Nyxt REPL to experiment with it.")
                     (nyxt:buffer-load-internal-page-focus
                      (read-from-string "nyxt/repl-mode:repl")
                      :form ,printable-code))))
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
        `(:span (:code ,@attrs ,injectable-code) ,select-code)
        ;; https://spdevuk.com/how-to-create-code-copy-button/
        `(:div :style "position: relative"
               (:pre ,@attrs ,select-code
                     (:code ,injectable-code))))))

(deftag :nsection (body attrs &key (title (alexandria:required-argument 'title))
                        level
                        (open-p t)
                        (id (if (stringp title)
                                (str:remove-punctuation (str:downcase title) :replacement "-")
                                (alexandria:required-argument 'id)))
                        &allow-other-keys)
  "Collapsible and reference-able <section> with a neader.
TITLE should be a human-readable title for a section, or the form producing one.
LEVEL (if provided), is the level of heading for the section. If it's 2, the
heading is <h2>, if it's 3, then <h3> etc. If not provided, uses <h*> Spinneret
tag to intelligently guess the current heading level.
ID is the string identifier with which to reference the section elsewhere. Is
auto-generated from title by replacing all the punctuation and spaces with
hyphens, if not provided AND if the TITLE is a string.
OPEN-P mandates whether the section is collapsed or not. True (= not collapsed)
by default."
  (check-type level (or null (integer 2 6)))
  (with-gensyms (id-var)
    `(let ((spinneret::*html-path*
             ;; Push as many :section tags into the path, as necessary to imply
             ;; LEVEL for the sections inside this one. A trick on Spinneret to
             ;; make it think it's deeply nested already.
             (append
              spinneret::*html-path*
              (make-list ,(if level
                              `(1- (- ,level (spinneret::heading-depth)))
                              0)
                         :initial-element :section)))
           (,id-var ,id))
       (:section.section
        :id ,id-var
        (:details
         :open ,open-p
         (:summary (:h* :style "display: inline"
                     ,@attrs ,title
                     " " (:a.link :href (uiop:strcat "#" ,id-var) "#")))
         ,@body)))))

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
    ,@(when title
        (list :title title))
    ,@attrs
    ,text))

(deftag :ninput (body attrs &key rows cols onfocus onchange buffer &allow-other-keys)
  "Nicely styled <textarea> with a reasonable number of ROWS/COLS to accommodate the BODY.
Calls Lisp forms in ONFOCUS and ONCHANGE when one focuses and edits the input (respectively)."
  (once-only ((input-contents `(or (progn ,@(mapcar #'remove-smart-quoting body)) "")))
    `(:textarea.input
      :rows (or ,rows (1+ (count #\Newline ,input-contents)) 1)
      :cols (or ,cols (ignore-errors (apply #'max (mapcar #'length (str:lines ,input-contents)))) 80)
      ,@(when onfocus
          `(:onfocus (ps:ps (nyxt/ps:lisp-eval
                             (:title "ninput onfocus"
                                     ,@(when buffer
                                         (list :buffer buffer)))
                             ,onfocus))))
      ,@(when onchange
          ;; More events here.
          `(:onkeydown (ps:ps (nyxt/ps:lisp-eval
                               (:title "ninput onchange/onkeydown"
                                       ,@(when buffer
                                           (list :buffer buffer)))
                               ,onchange))))
      ,@attrs
      (:raw (the string ,input-contents)))))
