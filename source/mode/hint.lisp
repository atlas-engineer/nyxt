;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/hint-mode
    (:documentation "Mode for element hints."))
(in-package :nyxt/hint-mode)

(define-mode hint-mode ()
  "Interact with elements by typing a short character sequence."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (auto-follow-hints-p
    nil
    :type boolean
    :documentation "Whether the hints are automatically followed when matching user input.")
   (fit-to-prompt-p
    nil
    :type boolean
    :documentation "Whether the hinting prompt buffer is collapsed to the input line.")
   (style
    (theme:themed-css (theme *browser*)
      (".nyxt-hint"
       :background-color theme:background
       :color theme:on-background
       :font-family "monospace,monospace"
       :padding "0px 0.3em"
       :border-color theme:primary
       :border-radius "0.3em"
       :border-width "0.2em"
       :border-style "solid"
       :z-index #.(1- (expt 2 31)))
      (".nyxt-hint.nyxt-mark-hint"
       :background-color theme:secondary
       :color theme:on-secondary
       :font-weight "bold")
      (".nyxt-hint.nyxt-select-hint"
       :background-color theme:accent
       :color theme:on-accent)
      (".nyxt-element-hint"
       :background-color theme:accent))
    :documentation "The style of the hint overlays.")
   (show-hint-scope-p
    nil
    :type boolean
    :documentation "Whether `style' is applied to the hinted element.
When t, the hinted element is, by default, shown its scope by applying a
background color.")
   (hints-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    :type string
    :documentation "The alphabet (charset) to use for hints.
Order matters -- the ones that go first are more likely to appear more often and
to index the top of the page.")
   (hints-selector
    "a, button, input, textarea, details, select"
    :type string
    :documentation "The elements to be hinted.
The hints-selector syntax is that of CLSS, and broadly, that of CSS. Use it to
define which elements are picked up by element hinting.

For instance, to include images:

    a, button, input, textarea, details, select, img:not([alt=\"\"])")
   (compute-hints-in-view-port-p
    nil
    :documentation "Whether hints are computed in view port.")
   (keyscheme-map
    (define-keyscheme-map "hint-mode" ()
      keyscheme:cua
      (list
       "C-j" 'follow-hint
       "C-J" 'follow-hint-new-buffer
       "C-u C-j" 'follow-hint-new-buffer-focus
       "C-u C-M-j" 'follow-hint-nosave-buffer
       "C-M-j" 'follow-hint-nosave-buffer-focus
       "M-c h" 'copy-hint-url)
      keyscheme:emacs
      (list
       "M-g M-g" 'follow-hint           ; Corresponds to Emacs' `goto-line'.
       "C-u M-g M-g" 'follow-hint-new-buffer
       "C-u M-g g" 'follow-hint-new-buffer
       "M-g g" 'follow-hint-new-buffer-focus
       "C-M-g g" 'follow-hint-nosave-buffer
       "C-M-g C-M-g" 'follow-hint-nosave-buffer-focus
       "C-x C-w" 'copy-hint-url)
      keyscheme:vi-normal
      (list
       ;; TODO bind copy-hint-url!
       "f" 'follow-hint
       "; f" 'follow-hint-new-buffer
       "F" 'follow-hint-new-buffer-focus
       "g f" 'follow-hint-nosave-buffer
       "g F" 'follow-hint-nosave-buffer-focus)))))

(define-parenscript-async add-stylesheet ()
  (unless (nyxt/ps:qs document "#nyxt-stylesheet")
    (ps:try
     (ps:let ((style-element (ps:chain document (create-element "style"))))
       (setf (ps:@ style-element id) "nyxt-stylesheet")
       (ps:chain document head (append-child style-element))
       (setf (ps:chain style-element inner-text)
             (ps:lisp (style (find-submode 'hint-mode)))))
     (:catch (error)))))

(define-parenscript-async hint-elements (hints)
  (defun create-hint-overlay (original-element hint)
    "Create a DOM element to be used as a hint."
    (ps:let* ((rect (ps:chain original-element (get-bounding-client-rect)))
              (element (ps:chain document (create-element "span"))))
      (setf (ps:@ element class-name) "nyxt-hint")
      (setf (ps:@ element style position) "absolute")
      (setf (ps:@ element style left) (+ (ps:@ window page-x-offset) (ps:@ rect left) "px"))
      (setf (ps:@ element style top) (+ (ps:@ window page-y-offset) (ps:@ rect top) "px"))
      (setf (ps:@ element id) (+ "nyxt-hint-" hint))
      (setf (ps:@ element text-content) hint)
      element))

  (let ((fragment (ps:chain document (create-document-fragment)))
        (hints (ps:lisp (list 'quote hints)))
        (i 0))
    (dolist (element (nyxt/ps:qsa document "[nyxt-hintable]"))
      (let ((hint (aref hints i)))
        (ps:chain element (set-attribute "nyxt-hint" hint))
        (ps:chain fragment (append-child (create-hint-overlay element hint)))
        (when (ps:lisp (show-hint-scope-p (find-submode 'hint-mode)))
          (ps:chain element class-list (add "nyxt-element-hint")))
        (setf i (1+ i))))
    (ps:chain document body (append-child fragment))
    ;; Returning fragment makes WebKit choke.
    nil))

(-> select-from-alphabet (t alex:positive-integer string) (values string &optional))
(defun select-from-alphabet (code subsequence-length alphabet)
  (let* ((exponents (nreverse (loop for pow below subsequence-length
                                    collect (expt (length alphabet) pow)))))
    (coerce (loop for exp in exponents
                  for quotient = (floor (/ code exp))
                  collect (aref alphabet quotient)
                  do (decf code (* quotient exp)))
            'string)))

(-> generate-hints (alex:positive-integer) (list-of string))
(defun generate-hints (length)
  (let ((alphabet (hints-alphabet (find-submode 'hint-mode))))
    (cond
      ((sera:single alphabet)
       (loop for i from 1 to length
             collect (select-from-alphabet 0 i alphabet)))
      (t
       (loop for i below length
             collect (select-from-alphabet i
                                           (max (ceiling (log length (length alphabet)))
                                                1)
                                           alphabet))))))

(define-parenscript set-hintable-attribute (selector)
  (let ((elements (nyxt/ps:qsa document (ps:lisp selector)))
        (in-view-port-p (ps:lisp (compute-hints-in-view-port-p (find-submode 'hint-mode)))))
    (ps:dolist (element elements)
      (if in-view-port-p
          (when (nyxt/ps:element-in-view-port-p element)
            (ps:chain element (set-attribute "nyxt-hintable" "")))
          (ps:chain element (set-attribute "nyxt-hintable" ""))))))

(define-parenscript remove-hintable-attribute ()
  (ps:dolist (element (nyxt/ps:qsa document "[nyxt-hintable]"))
    (ps:chain element (remove-attribute "nyxt-hintable"))))

(defun add-hints (&key selector (buffer (current-buffer)))
  (add-stylesheet)
  (set-hintable-attribute selector)
  (setf (document-model buffer) (nyxt/dom::named-json-parse (nyxt/dom::get-document-body-json)))
  (let* ((hintable-elements (clss:select "[nyxt-hintable]" (document-model buffer)))
         (hints (generate-hints (length hintable-elements))))
    (hint-elements hints)
    (loop for elem across hintable-elements
          for hint in hints
          do (plump:set-attribute elem "nyxt-hint" hint)
          collect elem)))

(define-parenscript-async remove-hint-elements ()
  (ps:dolist (element (nyxt/ps:qsa document ":not(.nyxt-search-node) > .nyxt-hint"))
    (ps:chain element (remove)))
  (when (ps:lisp (show-hint-scope-p (find-submode 'hint-mode)))
    (ps:dolist (element (nyxt/ps:qsa document ".nyxt-element-hint"))
      (ps:chain element class-list (remove "nyxt-element-hint")))))

(defun remove-hints (&key (buffer (current-buffer)))
  (remove-hint-elements)
  (remove-hintable-attribute)
  (setf (document-model buffer) (nyxt/dom::named-json-parse (nyxt/dom::get-document-body-json))))

(export-always 'identifier)
(defmethod identifier ((element plump:element))
  (plump:attribute element "nyxt-hint"))

(export-always 'highlight-selected-hint)
(define-parenscript highlight-selected-hint (&key element scroll)
  (let ((%element (nyxt/ps:qs document (ps:lisp (format nil "#nyxt-hint-~a"
                                                        (identifier element))))))
    (when %element
      (unless (ps:chain %element class-list (contains "nyxt-select-hint"))
        ;; There should be, at most, a unique element with the
        ;; "nyxt-select-hint" class.
        ;; querySelectAll, unlike querySelect, handles the case when none are
        ;; found.
        (ps:dolist (selected-hint (nyxt/ps:qsa document ".nyxt-select-hint"))
          (ps:chain selected-hint class-list (remove "nyxt-select-hint"))))
      (ps:chain %element class-list (add "nyxt-select-hint"))
      (when (ps:lisp scroll)
        (ps:chain %element (scroll-into-view (ps:create block "nearest")))))))

(export-always 'unhighlight-selected-hint)
(define-parenscript unhighlight-selected-hint ()
  ;; There should be, at most, a unique element with the
  ;; "nyxt-select-hint" class.
  ;; querySelectAll, unlike querySelect, handles the case when none are
  ;; found.
  (ps:dolist (selected-hint (nyxt/ps:qsa document ".nyxt-select-hint"))
    (ps:chain selected-hint class-list (remove "nyxt-select-hint"))))

(define-class hint-source (prompter:source)
  ((prompter:name "Hints")
   (prompter:selection-actions-enabled-p t)
   (prompter:filter
    (if (and (auto-follow-hints-p (find-submode 'hint-mode))
             (fit-to-prompt-p (find-submode 'hint-mode)))
        (lambda (suggestion source input)
          (declare (ignore source))
          (str:starts-with-p input
                             (prompter:attributes-default suggestion)
                             :ignore-case t))
        #'prompter:fuzzy-match))
   (prompter:filter-postprocessor
    (lambda (suggestions source input)
      (declare (ignore source))
      (multiple-value-bind (matching-hints other-hints)
          (sera:partition
           (lambda (element)
             (str:starts-with-p input (plump:attribute element "nyxt-hint") :ignore-case t))
           suggestions
           :key #'prompter:value)
        (append matching-hints other-hints))))
   (prompter:selection-actions
    (unless (fit-to-prompt-p (find-submode 'hint-mode))
      (lambda (suggestion)
        (highlight-selected-hint :element suggestion
                                 :scroll nil))))
   (prompter:marks-actions
    (lambda (marks)
      (let ((%marks (mapcar (lambda (mark) (str:concat "#nyxt-hint-" (identifier mark)))
                            marks)))
        (ps-eval
          (dolist (marked-overlay (nyxt/ps:qsa document ".nyxt-mark-hint"))
            (ps:chain marked-overlay class-list (remove "nyxt-mark-hint")))
          (dolist (mark (ps:lisp (list 'quote %marks)))
            (ps:chain (nyxt/ps:qs document mark) class-list (add "nyxt-mark-hint")))))))
   (prompter:return-actions
    (list 'identity
          (lambda-command click* (elements)
            (dolist (element (rest elements))
              (nyxt/dom:click-element element))
            (nyxt/dom:click-element (first elements))
            nil)
          (lambda-command focus* (elements)
            (dolist (element (rest elements))
              (nyxt/dom:focus-select-element element))
            (nyxt/dom:focus-select-element (first elements))
            nil)
          (lambda-command hover* (elements)
            (dolist (element (rest elements))
              (nyxt/dom:hover-element element))
            (nyxt/dom:hover-element (first elements))
            nil)))))

(export-always 'query-hints)
(defun query-hints (prompt function
                    &key (multi-selection-p t)
                         (selector (hints-selector (find-submode 'hint-mode))))
  "Prompt for elements matching SELECTOR, hinting them visually.
MULTI-SELECTION-P defines whether several elements can be chosen.
PROMPT is a text to show while prompting for hinted elements.
FUNCTION is the action to perform on the selected elements."
  (alex:when-let*
      ((buffer (current-buffer))
       (result (prompt
                :prompt prompt
                ;; TODO: No need to find the symbol if we move this code (and
                ;; the rest) to the hint-mode package.
                :extra-modes (list (resolve-symbol :hint-prompt-buffer-mode :mode))
                :auto-return-p (auto-follow-hints-p (find-submode 'hint-mode))
                :history nil
                :height (if (fit-to-prompt-p (find-submode 'hint-mode))
                            :fit-to-prompt
                            :default)
                :hide-suggestion-count-p (fit-to-prompt-p (find-submode 'hint-mode))
                :sources (make-instance 'hint-source
                                        :multi-selection-p multi-selection-p
                                        :constructor
                                        (lambda (source)
                                          (declare (ignore source))
                                          (delete-duplicates
                                           (add-hints :selector selector)
                                           :test (lambda (h1 h2) (and h1 h2 (string= h1 h2)))
                                           :key (alex:rcurry #'plump:attribute "href"))))
                :after-destructor (lambda () (with-current-buffer buffer (remove-hints))))))
    (funcall function result)))

(defmethod prompter:object-attributes :around ((element plump:element) (source hint-source))
  `(,@(when (plump:attribute element "nyxt-hint")
        `(("Hint" ,(plump:attribute element "nyxt-hint"))))
    ;; Ensure that all of Body, URL and Value are there, even if empty.
    ,@(let ((attributes (call-next-method)))
        (dolist (attr '("Body" "URL" "Value"))
          (unless (assoc attr attributes :test 'string=)
            (alex:nconcf attributes `((,attr "")))))
        attributes)
    ("Type" ,(str:capitalize (str:string-case
                                 (plump:tag-name element)
                               ("a" "link")
                               ("img" "image")
                               (otherwise (plump:tag-name element)))))))

(defmethod prompter:object-attributes ((input nyxt/dom:input-element) (source prompter:source))
  (declare (ignore source))
  (when (nyxt/dom:body input)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body input))))))

(defmethod prompter:object-attributes ((textarea nyxt/dom:textarea-element) (source prompter:source))
  (declare (ignore source))
  (when (nyxt/dom:body textarea)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body textarea))))))

(defmethod prompter:object-attributes ((a nyxt/dom:a-element) (source prompter:source))
  (declare (ignore source))
  (append
   (sera:and-let* ((has-href? (plump:has-attribute a "href"))
                   (url-string (plump:attribute a "href")))
     `(("URL" ,url-string)))
   (when (nyxt/dom:body a)
     `(("Body" ,(str:shorten 80 (nyxt/dom:body a)))))))

(defmethod prompter:object-attributes ((button nyxt/dom:button-element) (source prompter:source))
  (declare (ignore source))
  (when (nyxt/dom:body button)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body button))))))

(defmethod prompter:object-attributes ((details nyxt/dom:details-element) (source prompter:source))
  (declare (ignore source))
  (when (nyxt/dom:body details)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body details))))))

(defmethod prompter:object-attributes ((select nyxt/dom:select-element) (source prompter:source))
  (declare (ignore source))
  `(("Body" ,(str:shorten 80 (nyxt/dom:body select)))))

(defmethod prompter:object-attributes ((option nyxt/dom:option-element) (source prompter:source))
  (declare (ignore source))
  `(("Body" ,(nyxt/dom:body option))
    ,@(when (plump:attribute option "value")
        `(("Value" ,(plump:attribute option "value"))))))

(defmethod prompter:object-attributes ((img nyxt/dom:img-element) (source hint-source))
  (append
   (sera:and-let* ((has-href? (plump:has-attribute img "href"))
                   (url-string (plump:attribute img "href")))
     `(("URL" ,url-string)))
   (when (nyxt/dom:body img)
     `(("Body" ,(str:shorten 80 (nyxt/dom:body img)))))))

(defmethod %follow-hint ((element plump:element))
  (nyxt/dom:click-element element))

(defmethod %follow-hint ((input nyxt/dom:input-element))
  (str:string-case (plump:attribute input "type")
    ("button" (nyxt/dom:click-element input))
    ("radio" (nyxt/dom:check-element input))
    ("checkbox" (nyxt/dom:check-element input))
    (otherwise (nyxt/dom:focus-select-element input))))

(defmethod %follow-hint ((textarea nyxt/dom:textarea-element))
  (nyxt/dom:focus-select-element textarea))

(defmethod %follow-hint ((details nyxt/dom:details-element))
  (nyxt/dom:toggle-details-element details))

(define-class options-source (prompter:source)
  ((prompter:name "Options"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "Prompt source for select tag options."))

(defmethod %follow-hint ((select nyxt/dom:select-element))
  (sera:and-let* ((options (coerce (clss:select "option" select) 'list))
                  (values (prompt :prompt "Value to select"
                                  :sources (make-instance 'options-source
                                                          :constructor options
                                                          :multi-selection-p
                                                          (plump:attribute select "multiple")))))
    (dolist (option (mapcar (rcurry #'find options :test #'equalp) values))
      (nyxt/dom:select-option-element option select))))

(defmethod %follow-hint-new-buffer-focus ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer-focus :url (url a)
                     :parent-buffer parent-buffer
                     :nosave-buffer-p (nosave-buffer-p parent-buffer)))

(defmethod %follow-hint-new-buffer-focus ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-new-buffer ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer :url (url a) :parent-buffer parent-buffer :load-url-p nil))

(defmethod %follow-hint-new-buffer ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-nosave-buffer-focus ((a nyxt/dom:a-element))
  (make-buffer-focus :url (url a) :nosave-buffer-p t))

(defmethod %follow-hint-nosave-buffer-focus ((element plump:element))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-nosave-buffer ((a nyxt/dom:a-element))
  (make-nosave-buffer :url (url a)))

(defmethod %follow-hint-nosave-buffer ((element plump:element))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-with-current-modes-new-buffer ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer :url (url a)
               :modes (mapcar #'sera:class-name-of (modes (current-buffer)))
               :parent-buffer parent-buffer))

(defmethod %follow-hint-with-current-modes-new-buffer ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %copy-hint-url ((a nyxt/dom:a-element))
  (ffi-buffer-copy (current-buffer) (render-url (url a))))

(defmethod %copy-hint-url ((img nyxt/dom:img-element))
  (ffi-buffer-copy (current-buffer) (render-url (url img))))

(defmethod %copy-hint-url ((element plump:element))
  (echo "Unsupported operation for hint: can't copy URL."))

(define-command follow-hint ()
  "Prompt for element hints and open them in the current buffer."
  (let ((buffer (current-buffer)))
    (query-hints "Interact with element"
                 (lambda (results)
                   (%follow-hint (first results))
                   (mapcar (rcurry #'%follow-hint-new-buffer buffer)
                           (rest results))))))

(define-command follow-hint-new-buffer ()
  "Like `follow-hint', but open the selected hints in new buffers (no focus)."
  (let ((buffer (current-buffer)))
    (query-hints "Open element in new buffer"
                 (lambda (result) (mapcar (rcurry #'%follow-hint-new-buffer buffer)
                                          result)))))

(define-command follow-hint-new-buffer-focus ()
  "Like `follow-hint-new-buffer', but with focus."
  (let ((buffer (current-buffer)))
    (query-hints "Open element in new buffer"
                 (lambda (result)
                   (%follow-hint-new-buffer-focus (first result) buffer)
                   (mapcar (rcurry #'%follow-hint-new-buffer buffer)
                           (rest result))))))

(define-command follow-hint-nosave-buffer ()
  "Like `follow-hint', but open the selected hints in new `nosave-buffer's (no
focus)."
  (query-hints "Open element in new buffer"
               (lambda (result) (mapcar #'%follow-hint-nosave-buffer result))))

(define-command follow-hint-nosave-buffer-focus ()
  "Like `follow-hint-nosave-buffer', but with focus."
  (query-hints "Open element in new buffer"
               (lambda (result)
                 (%follow-hint-nosave-buffer-focus (first result))
                 (mapcar #'%follow-hint-nosave-buffer (rest result)))))

(define-command follow-hint-with-current-modes-new-buffer ()
  "Prompt for element hints and open them in a new buffer with current
modes."
  (let ((buffer (current-buffer)))
    (query-hints "Open element with current modes in new buffer"
                 (lambda (result)
                   (mapcar (rcurry #'%follow-hint-with-current-modes-new-buffer buffer)
                           result)))))

(define-command copy-hint-url ()
  "Prompt for element hints and save its corresponding URLs to clipboard."
  (query-hints "Copy element URL"
               (lambda (result) (%copy-hint-url (first result)))
               :multi-selection-p nil
               :selector "a"))
