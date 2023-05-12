;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :spinneret)

(deftag :mayberaw (body attrs &rest keys &key &allow-other-keys)
  "Spinneret's :raw, but with HTML escaping if BODY _does not_ look like HTML."
  ;; Because (declare (ignorable ...)) doesn't work.
  (let ((attrs attrs)
        (keys keys))
    (declare (ignorable attrs keys))
    `(:raw (if (nyxt:html-string-p (progn ,@body))
               (progn ,@body)
               (escape-string (progn ,@body))))))

(deftag :nstyle (body attrs &rest keys &key &allow-other-keys)
  "Regular <style>, but with contents staying unescaped."
  (let ((keys keys))
    (declare (ignorable keys))
    `(:style ,@attrs (:raw ,@body))))

(deftag :nscript (body attrs &rest keys &key &allow-other-keys)
  "Regular <script>, but with contents staying unescaped."
  (let ((keys keys))
    (declare (ignorable keys))
    `(:script ,@attrs (:raw ,@body))))

(serapeum:-> %nselect-onchange (string (nyxt:maybe nyxt:buffer) (nyxt:list-of list)) t)
(defun %nselect-onchange (id buffer clauses)
  "Compiles the CLAUSES body into Parenscript code.
Parenscript fetches values from <select> with ID and evaluates the respective
forms in BUFFER."
  (alexandria:when-let ((buffer (or buffer (nyxt:current-buffer))))
    (ps:ps*
     (with-ps-gensyms (var inner-var)
       `(nyxt/ps:lisp-eval
         (:title ,(format nil "nselect ~a choice" id)
                 ,@(when buffer
                     `(:buffer ,buffer)))
         (let ((,var (alexandria:ensure-list
                      (nyxt:ps-eval
                        (ps:chain *Array (from (ps:chain (nyxt/ps:qs document (+ "#" (ps:lisp ,id)))
                                                         selected-options))
                                  (map (lambda (e) (ps:@ e value))))))))
           (dolist (,inner-var ,var)
             (str:string-case ,inner-var
               ,@(loop for (clause . forms) in clauses
                       for value = (first (uiop:ensure-list clause))
                       collect (cons (nyxt:prini-to-string value)
                                     forms))))))))))

(serapeum:-> %nselect-options ((nyxt:list-of list)) t)
(defun %nselect-options (clauses)
  "Produces a set of options for :nselect based on CLAUSES list."
  (spinneret:with-html-string
    (loop for (value display title) in (mapcar (alexandria:compose #'uiop:ensure-list #'first) clauses)
          collect (:option
                   :value (nyxt:prini-to-string value)
                   (when title
                     (list :title title))
                   (string-capitalize (or display (nyxt:prini-to-string value)))))))

(deftag :nselect (body attrs &rest keys &key default (id (alexandria:required-argument 'id)) buffer &allow-other-keys)
  "Generate <select> tag from the BODY resembling cond clauses.

BODY can be:
- Multiple forms, each following/producing one of the formats:
  - (VALUE . FORMS) -- creates <option value=\"value\">value</option> and runs
    FORMS when it's selected.
  - ((VALUE DISPLAY TITLE) . FORMS) -- creates an <option value=\"value\"
    title=\"title\">display</option> and runs FORMS when it's selected. DISPLAY
    and TITLE are optional literal strings.
- A single form, expected to produce a list of forms like above.

In both cases, VALUEs should be literal (and printable) atoms. For instance,
symbol, number, string, or keyword.

BUFFER is a buffer to bind the actions of this tag to.

DEFAULT is a string or a string-producing form that is used as the default
option.

In case some variable from the outer scope should be captured, injecting a
closure into the clause would work best.

Example:
\(:nselect :id \"number-guessing\"
  :default \"Guess the number\"
  '(1 (nyxt:echo \"Too low!\"))
  (list 2 (nyxt:echo \"Correct!\"))
  `(3 (funcall ,#'(lambda () (nyxt:echo \"Too high!\")))))"
  (once-only (id)
    (with-gensyms (body-var)
      (let ((keys keys))
        (declare (ignorable keys))
        `(let ((,body-var ,(if (serapeum:single body)
                               (first body)
                               `(list ,@body))))
           (:select.button
            ,@attrs
            :id ,id
            :onchange (%nselect-onchange ,id ,buffer ,body-var)
            ,@(when default
                `((:option :selected t :disabled t ,default)))
            (:raw (%nselect-options ,body-var))))))))

(defun %nxref-doc (type symbol &optional (class-name (when (eq type :slot)
                                                       (alexandria:required-argument 'class-name))))
  "NOTE: TYPE for classes is :CLASS, not :CLASS-NAME (as in `:nxref')."
  (format nil "[~a]~@[ ~a~]"
          (if class-name
              (format nil "SLOT of ~a" class-name)
              type)
          (when-let ((doc (case type
                            (:package (documentation (find-package symbol) t))
                            (:variable (documentation symbol 'variable))
                            ((:slot   ; KLUDGE: Any simple way to get slot docs?
                              :macro :function :command)
                             (documentation symbol 'function))
                            ((:mode :class)
                             (documentation symbol 'type)))))
            ;; Copied from describe.lisp to avoid `nyxt::first-line' use.
            (find-if (complement #'uiop:emptyp) (serapeum:lines doc)))))

(defun %nxref-link (type symbol &optional (class-name (when (eq type :slot)
                                                        (alexandria:required-argument 'class-name))))
  "Generate a nyxt: link to the describe-* page based on SYMBOL's TYPE.
CLASS-NAME is specific to :slot type."
  (case type
    (:package (nyxt:nyxt-url (read-from-string "nyxt:describe-package") :package symbol))
    (:variable (nyxt:nyxt-url (read-from-string "nyxt:describe-variable")
                              :variable symbol))
    ((:command :function :macro)
     (nyxt:nyxt-url (read-from-string "nyxt:describe-function")
                    :fn symbol))
    (:slot (nyxt:nyxt-url (read-from-string "nyxt:describe-slot")
                          :name symbol :class class-name))
    ((:mode :class)
     (nyxt:nyxt-url (read-from-string "nyxt:describe-class")
                    :class symbol))
    (t (nyxt:nyxt-url (read-from-string "nyxt:describe-any")
                      :input symbol))))

(deftag :nxref (body attrs &rest keys &key slot mode class-name function macro command (command-key-p t) variable package &allow-other-keys)
  "Create a link to a respective describe-* page for BODY symbol.

Relies on the type keywords (SLOT, MODE, CLASS-NAME, FUNCTION, MACRO, COMMAND,
VARIABLE, PACKAGE) to guess the right page, always provide those.

CLASS-NAME, if present, should be the symbol designating a class. It's not
called CLASS because Spinneret has special behavior for CLASS pre-defined and
non-overridable."
  (let* ((keys keys)
         (first (first body))
         (symbol (or package variable function macro command slot class-name mode
                     (when (symbolp first) first)))
         (printable (or (when (and (symbolp first) (eq first symbol))
                          (second body))
                        first package variable function macro command slot class-name mode))
         (type (cond
                 (package :package)
                 (variable :variable)
                 (macro :macro)
                 (command :command)
                 (function :function)
                 ((and slot class-name) :slot)
                 (mode :mode)
                 (class-name :class))))
    (declare (ignorable keys))
    (when (and (getf attrs :class)
               (or (getf attrs :slot)
                   (every #'null (list slot class-name mode function macro command variable package))))
      (error ":class attribute used ambiguously in :nxref tag. Use :class-name instead."))
    `(:a.link
      :target "_blank"
      ,@attrs
      :href (%nxref-link ,type ,symbol
                         ,@(when (and slot class-name)
                             (list class-name)))
      :title (%nxref-doc ,type ,symbol
                         ,@(when (and slot class-name)
                             (list class-name)))
      (:code
       (let ((*print-escape* nil))
         (nyxt:prini-to-string ,printable))
       ,@(when (and command command-key-p)
           `(" ("
             (funcall (read-from-string "nyxt::binding-keys")
                      ,command ,@(when mode
                                   `(:modes (cl:list (make-instance ,mode)))))
             ")"))))))

(defun %ncode-resolve-linkable-symbols (form)
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
                          (when (symbolp name)
                            (pushnew name functions))))
                       (mapc #'resolve-symbols-internal args))
                      ((function value)
                       (push first specials)
                       (when (symbolp value)
                         (pushnew value functions)))
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

(defun %ncode-prini (object package)
  "Custom `:ncode'-specific `nyxt:prini-to-string' with narrower margins."
  (nyxt:prini-to-string object :readably t :right-margin 70 :package package))

(defun %ncode-htmlize-body (form package &optional (listing (%ncode-prini form package)))
  "Turn the FORM into an HTMLized rich text, augmented with `:nxref's to the used entities.
LISTING is the string to enrich, autogenerated from FORM on demand."
  (let ((*suppress-inserted-spaces* t)
        (*html-style* :tree)
        (*print-pretty* nil))
    (when (listp form)
      (multiple-value-bind (functions classes variables macros specials linkable-strings)
          (%ncode-resolve-linkable-symbols form)
        ;; We use \\s, because lots of Lisp symbols include non-word
        ;; symbols and would break if \\b was used.
        (macrolet ((replace-symbol-occurences (symbols type &key (prefix "(\\()") (suffix "(\\)|\\s)") (style :plain))
                     (alexandria:with-gensyms (sym sym-listing)
                       `(dolist (,sym ,symbols)
                          (when (search (%ncode-prini ,sym package) listing)
                            (let ((,sym-listing (%ncode-prini ,sym package)))
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
          (setf listing (str:replace-all (%ncode-prini string package)
                                         (nyxt:resolve-backtick-quote-links string package)
                                         listing)))))
    listing))

(defun %ncode-htmlize-unless-string (form package)
  (typecase form
    (string form)
    (list (%ncode-htmlize-body form package))))

(defun %ncode-inline-p (body package)
  "BODY is only inline if it actually is a one-liner, literal or printed out."
  (and (serapeum:single body)
       (zerop (count #\newline
                     (if (stringp (first body))
                         (first body)
                         (%ncode-prini (first body) package))))))

;; TODO: Store the location it's defined in as a :title or link for discoverability?
;; FIXME: Maybe use :nyxt-user as the default package to not quarrel with REPL & config?
(deftag :ncode (body attrs &rest keys &key
                     (package :nyxt)
                     (inline-p nil inline-provided-p)
                     (repl-p t) (config-p t) (copy-p t)
                     file (editor-p file) (external-editor-p file)
                     &allow-other-keys)
  "Generate the <pre>/<code> listing from the provided Lisp BODY.

Forms in BODY should be quoted.

INLINE-P is about omitting newlines and <pre> tags---basically a <code> tag with
syntax highlighting and actions. If not provided, is determined automatically
based on BODY length.

Most *-P arguments mandate whether to add the buttons for:
- Editing the BODY in the built-in REPL (REPL-P).
- Appending the BODY to the auto-config.lisp (CONFIG-P).
- Copying the source to clipboard (COPY-P).
- Editing the FILE it comes from (if present), in
  - Nyxt built-in `nyxt/mode/editor:editor-mode' (EDITOR-P).
  - `nyxt:external-editor-program' (EXTERNAL-EDITOR-P)."
  (once-only (package)
    (with-gensyms (body-var inline-var file-var first plaintext htmlized)
      (let* ((keys keys)
             (*print-escape* nil)
             (id (nyxt:prini-to-string (gensym)))
             (select-code
               `(:nselect
                  :id ,id
                  :default "Act on code"
                  :style (unless ,inline-var
                           "position: absolute; top: 0; right: 0; margin: 0; padding: 2PX")
                  ,@(when copy-p
                      `(`((copy "Copy" "Copy the code to clipboard.")
                          (funcall (read-from-string "nyxt:ffi-buffer-copy")
                                   (nyxt:current-buffer) ,,plaintext))))
                  ,@(when config-p
                      `(`((config
                           "Add to auto-config"
                           (format nil "Append this code to the auto-configuration file (~a)."
                                   (nfiles:expand nyxt::*auto-config-file*)))
                          (alexandria:write-string-into-file
                           ,,plaintext (nfiles:expand nyxt::*auto-config-file*)
                           :if-exists :append
                           :if-does-not-exist :create))))
                  ,@(when repl-p
                      `(`((repl
                           "Try in REPL"
                           "Open this code in Nyxt REPL to experiment with it.")
                          (nyxt:buffer-load-internal-page-focus
                           (read-from-string "nyxt/mode/repl:repl")
                           :form ,,plaintext))))
                  ,@(when (and file editor-p)
                      `(`((editor
                           "Open in built-in editor"
                           "Open the file this code comes from in Nyxt built-in editor-mode.")
                          (funcall (read-from-string "nyxt/mode/editor:edit-file")
                                   ,,file-var))))
                  ,@(when (and file external-editor-p)
                      `(`((external-editor
                           "Open in external editor"
                           "Open the file this code comes from in external editor.")
                          (uiop:launch-program
                           (append (funcall (read-from-string "nyxt:external-editor-program")
                                            (symbol-value (read-from-string "nyxt:*browser*")))
                                   (list (uiop:native-namestring ,,file-var))))))))))
        (declare (ignorable keys))
        `(let* ((,body-var (list ,@body))
                (,first (first ,body-var))
                (,inline-var ,(if inline-provided-p
                                  inline-p
                                  `(%ncode-inline-p ,body-var ,package)))
                (,file-var ,file)
                (,plaintext (cond
                              ((and (serapeum:single ,body-var)
                                    (stringp ,first))
                               ,first)
                              ((serapeum:single ,body-var)
                               (%ncode-prini ,first ,package))
                              (t (str:join
                                  (make-string 2 :initial-element #\newline)
                                  (mapcar (lambda (f) (if (stringp f)
                                                          f
                                                          (%ncode-prini f ,package)))
                                          ,body-var)))))
                (,htmlized (if (serapeum:single ,body-var)
                               (%ncode-htmlize-unless-string ,first ,package)
                               (str:join
                                (make-string 2 :initial-element #\newline)
                                (mapcar (lambda (f) (%ncode-htmlize-unless-string f ,package)) ,body-var)))))
           (declare (ignorable ,plaintext ,file-var))
           ,(if inline-p
                `(:span (:code ,@attrs (:raw ,htmlized)) ,select-code)
                ;; https://spdevuk.com/how-to-create-code-copy-button/
                `(:div :style "position: relative"
                       (:pre ,@attrs ,select-code
                             (:code (:raw ,htmlized))))))))))

(deftag :nsection (body attrs &rest keys
                        &key (title (alexandria:required-argument 'title))
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
  (let ((keys keys))
    (declare (ignorable keys))
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
           (:summary
            (:header
             :style "display: inline"
             (:h* :style "display: inline"
               ,@attrs ,title)
             " " (:a.link :href (uiop:strcat "#" ,id-var) "#")))
           ,@body))))))

(serapeum:-> %nbutton-onclick (string (nyxt:maybe nyxt:buffer) (nyxt:list-of list)) t)
(defun %nbutton-onclick (title buffer clauses)
  "Produce Parenscript to run Lisp CLAUSES in BUFFER.
TITLE is the debuggable name for the callback."
  (when (or buffer (nyxt:current-buffer))
    (ps:ps*
     `(nyxt/ps:lisp-eval
       (:title ,(format nil "nbutton ~a" title)
               ,@(when buffer
                   (list :buffer buffer)))
       ,@clauses))))

(deftag :nbutton (body attrs &rest keys
                       &key (text (alexandria:required-argument 'text)) title buffer
                       &allow-other-keys)
  "A Lisp-invoking button with TEXT text and BODY action.
Evaluates (via `nyxt/ps:lisp-eval') the BODY in BUFFER when clicked.

BODY can consist of quoted lists or forms producing those. These will be
compiled, so, if you want to close over some value, inject a closure right
inside the forms.

Example:
\(:nbutton
  :buffer buffer
  :text \"Do something\"
  '(nyxt:echo \"Hello!\")
  (list 'foo)
  `(funcall ,#'(lambda () (do-something-with closed-over-value))))"
  (let ((keys keys))
    (declare (ignorable keys))
    `(:button.button
      :onclick (%nbutton-onclick ,(or title text) ,buffer (list ,@body))
      ,@(when title
          (list :title title))
      ,@attrs
      ,text)))

(serapeum:-> %ninput-onfocus (list (nyxt:maybe nyxt:buffer)) t)
(defun %ninput-onfocus (onfocus buffer)
  "Produce Parenscript to run ONFOCUS expression in BUFFER when :ninput is focused."
  (when (or buffer (nyxt:current-buffer))
    (ps:ps*
     `(nyxt/ps:lisp-eval
       (:title "ninput onfocus"
               ,@(when buffer
                   (list :buffer buffer)))
       ,onfocus))))

(serapeum:-> %ninput-onchange (list (nyxt:maybe nyxt:buffer)) t)
(defun %ninput-onchange (onchange buffer)
  "Produce Parenscript to run ONCHANGE expression in BUFFER when :ninput is modified."
  (when (or buffer (nyxt:current-buffer))
    (ps:ps*
     `(nyxt/ps:lisp-eval
       (:title "ninput onchange"
               ,@(when buffer
                   (list :buffer buffer)))
       ,onchange))))

(deftag :ninput (body attrs &rest keys &key rows cols onfocus onchange buffer &allow-other-keys)
  "Nicely styled <textarea> with a reasonable number of ROWS/COLS to accommodate the BODY.
Calls Lisp forms in ONFOCUS and ONCHANGE when one focuses and edits the input (respectively).

BODY should be a string or an implicit progn producing a string.

ONFOCUS, and ONCHANGE can consist of quoted lists or forms producing
those. These lists will be compiled, so, if you want to close over some value,
inject a closure right inside the forms."
  (let ((keys keys))
    (declare (ignorable keys))
    (once-only (buffer)
      (with-gensyms (input)
        ;; NOTE: It's unlikely that BODY will have multiple forms, but better
        ;; prepare for it, just in case someone goes stateful.
        `(let ((,input (progn ,@body)))
           (:textarea.input
            :rows (or ,rows (1+ (count #\Newline ,input)) 1)
            :cols (or ,cols (ignore-errors (reduce #'max (mapcar #'length (str:lines ,input)))) 80)
            ,@(when onfocus
                `(:onfocus (%ninput-onfocus ,onfocus ,buffer)))
            ,@(when onchange
                ;; More events here.
                `(:onkeydown (%ninput-onchange ,onchange ,buffer)))
            ,@attrs
            (:raw (the string ,input))))))))

(serapeum:-> %ntoc-create-toc ((integer 2 6) string) *)
(defun %ntoc-create-toc (depth body)
  "Generate the code for the table of contents based on string BODY."
  (labels ((parent-section (elem)
             (find-if #'nyxt/dom:section-element-p (nyxt/dom:parents elem)))
           (format-section (heading level)
             (with-html-string
               (let ((parent-section (parent-section heading)))
                 (:li (:a :href (format nil "#~a" (plump:attribute parent-section "id"))
                          (plump:text heading)))
                 (serapeum:and-let* ((_ (< level depth))
                                     (inner-level (1+ level))
                                     (inner-headers
                                      (clss:ordered-select (format nil "h~a" inner-level) parent-section)))
                   (:ul (loop for inner-header across inner-headers
                              collect (:raw (format-section inner-header inner-level)))))))))
    (let* ((dom (nyxt/dom:named-html-parse body))
           (h2s (clss:ordered-select "h2" dom)))
      (with-html-string
        (loop for h2 across h2s
              collect (:ul (:raw (format-section h2 2))))))))

(deftag :ntoc (body attrs &rest keys &key (title "Table of contents") (depth 3) &allow-other-keys)
  "Generate table of contents for BODY up to DEPTH.
Looks for section tags with ID-s to link to.
:nsection sections are perfectly suitable for that."
  (let ((keys keys))
    (declare (ignorable keys))
    (with-gensyms (body-var)
      `(let ((,body-var (with-html-string ,@body)))
         (:nav#toc
          ,@attrs
          (:nsection
            :title ,title
            (:raw (%ntoc-create-toc ,depth ,body-var))))
         (:raw ,body-var)))))
