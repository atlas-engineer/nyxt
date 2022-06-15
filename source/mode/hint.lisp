;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/hint-mode
    (:documentation "Mode for element hints."))
(in-package :nyxt/hint-mode)

(define-mode hint-mode ()
  "Mode to interact with links using keyword only."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (auto-follow-hints-p
    nil
    :type boolean
    :documentation "Whether the hints are automatically followed when matching user input.")
   (box-style (theme:themed-css (theme *browser*)
                (".nyxt-hint"
                 :background-color theme:primary
                 :color theme:on-primary
                 :font-weight "bold"
                 :padding "0px 3px 0px 3px"
                 :border-radius "2px"
                 :z-index #.(1- (expt 2 31))))
              :documentation "The style of the boxes, e.g. link hints.")
   (highlighted-box-style (theme:themed-css (theme *browser*)
                            (".nyxt-hint.nyxt-highlight-hint"
                             :background-color theme:accent
                             :color theme:on-accent))
                          :documentation "The style of highlighted boxes, e.g. link hints.")

   (hints-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                   :type string
                   :documentation "The alphabet (charset) to use for hints.
Order matters -- the ones that go first are more likely to appear more often
and to index the top of the page.")
   (hints-selector "a, button, input, textarea, details, select, img:not([alt=\"\"])"
                   :type string
                   :documentation "Defines which elements are to be hinted. The
hints-selector syntax is that of CLSS, and broadly, that of CSS. Use it to
define which elements are picked up by element hinting.")
   (keymap-scheme
    (define-scheme "web"
      scheme:cua
      (list
       "C-j" 'follow-hint
       "C-u C-j" 'follow-hint-new-buffer-focus
       "C-J" 'follow-hint-new-buffer
       "C-M-j" 'follow-hint-nosave-buffer-focus
       "C-u C-M-j" 'follow-hint-nosave-buffer
       "M-c h" 'copy-hint-url)
      scheme:emacs
      (list
       "M-g M-g" 'follow-hint           ; Corresponds to Emacs' `goto-line'.
       "M-g g" 'follow-hint-new-buffer-focus
       "C-u M-g M-g" 'follow-hint-new-buffer
       "C-u M-g g" 'follow-hint-new-buffer
       "C-M-g C-M-g" 'follow-hint-nosave-buffer-focus
       "C-M-g g" 'follow-hint-nosave-buffer
       "C-x C-w" 'copy-hint-url)

      scheme:vi-normal
      (list
       "f" 'follow-hint
       "F" 'follow-hint-new-buffer-focus
       "; f" 'follow-hint-new-buffer
       "g f" 'follow-hint-nosave-buffer
       "g F" 'follow-hint-nosave-buffer-focus)))))

(define-parenscript add-stylesheet ()
  (unless (nyxt/ps:qs document "#nyxt-stylesheet")
    (ps:try
     (ps:let* ((style-element (ps:chain document (create-element "style")))
               (box-style (ps:lisp (box-style (find-submode 'nyxt/hint-mode:hint-mode))))
               (highlighted-style (ps:lisp (highlighted-box-style (find-submode 'nyxt/hint-mode:hint-mode)))))
       (setf (ps:@ style-element id) "nyxt-stylesheet")
       (ps:chain document head (append-child style-element))
       (ps:chain style-element sheet (insert-rule box-style 0))
       (ps:chain style-element sheet (insert-rule highlighted-style 1)))
     (:catch (error)))))

(define-parenscript hint-elements (nyxt-identifiers hints)
  (defun hint-create-element (original-element hint)
    "Creates a DOM element to be used as a hint"
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
        (ids (ps:lisp (list 'quote nyxt-identifiers)))
        (hints (ps:lisp (list 'quote hints))))
    (dotimes (i (ps:lisp (length nyxt-identifiers)))
      (let ((element (nyxt/ps:qs document (+ "[nyxt-identifier=\""
                                             (aref ids i)
                                             "\"]")))
            (hint (aref hints i)))
        (when element
          (ps:chain element (set-attribute "nyxt-hint" hint))
          (ps:let ((hint-element (hint-create-element element hint)))
            (ps:chain fragment (append-child hint-element))))))
    (ps:chain document body (append-child fragment))
    ;; Returning fragment makes WebKit choke.
    nil))

(-> select-from-alphabet (t fixnum string) (values string &optional))
(defun select-from-alphabet (code char-length alphabet)
  (let* ((exponents (nreverse (loop for pow below char-length
                                    collect (expt (length alphabet) pow)))))
    (coerce (loop for exp in exponents
                  for quotinent = (floor (/ code exp))
                  collect (aref alphabet quotinent)
                  do (decf code (* quotinent exp)))
            'string)))

(-> generate-hints (integer) list-of-strings)
(defun generate-hints (length)
  (unless (zerop length)
    (let* ((alphabet (hints-alphabet (find-submode 'hint-mode)))
           (char-length (ceiling (log length (length alphabet)))))
      (loop for i below length collect (select-from-alphabet i char-length alphabet)))))

(defun add-hints (&key selector)
  (let* ((dom (document-model (current-buffer)))
         (hintable-elements (clss:select selector dom))
         (hints (generate-hints (length hintable-elements))))
    (run-thread "stylesheet adder"
      (add-stylesheet))
    (run-thread "element hint drawing"
      (hint-elements (map 'list #'get-nyxt-id hintable-elements) hints))
    (loop for elem across hintable-elements
          for hint in hints
          do (plump:set-attribute elem "nyxt-hint" hint)
          collect elem)))

(define-parenscript remove-hints ()
  (defun hints-remove-all ()
    "Removes all the elements"
    (ps:dolist (element (nyxt/ps:qsa document ":not(.nyxt-search-node) > .nyxt-hint"))
      (ps:chain element (remove))))
  (hints-remove-all))

(export-always 'identifier)
(defmethod identifier ((element plump:element))
  (plump:get-attribute element "nyxt-hint"))

(define-parenscript highlight-selected-hint (&key element scroll)
  (defun update-hints ()
    (ps:let* ((new-element (nyxt/ps:qs document
                                       (ps:lisp (format
                                                 nil "#nyxt-hint-~a"
                                                 (identifier element))))))
      (when new-element
        (unless ((ps:@ new-element class-list contains) "nyxt-highlight-hint")
          (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-highlight-hint")))
            (ps:dolist (e old-elements)
              (setf (ps:@ e class-name) "nyxt-hint"))))
        (setf (ps:@ new-element class-name) "nyxt-hint nyxt-highlight-hint")
        (when (ps:lisp scroll)
          (ps:chain new-element (scroll-into-view (ps:create block "nearest")))))))

  (update-hints))

(export-always 'remove-focus)
(define-parenscript remove-focus ()
  (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-highlight-hint")))
    (ps:dolist (e old-elements)
      (setf (ps:@ e class-name) "nyxt-hint"))))

(define-class hint-source (prompter:source)
  ((prompter:name "Hints")
   (prompter:selection-actions-enabled-p t)
   (prompter:filter-postprocessor
    (lambda (suggestions source input)
      (declare (ignore source))
      (multiple-value-bind (matching-hints other-hints)
          (sera:partition
           (lambda (element)
             (str:starts-with-p input (plump:get-attribute element "nyxt-hint") :ignore-case t))
           suggestions
           :key #'prompter:value)
        (append matching-hints other-hints))))
   (prompter:selection-actions
    (lambda (suggestion)
      (highlight-selected-hint :element suggestion
                               :scroll nil)))
   (prompter:return-actions (list 'identity
                           (lambda-command click* (elements)
                             (dolist (element (rest elements))
                               (nyxt/dom:click-element :nyxt-identifier (get-nyxt-id element)))
                             (nyxt/dom:click-element :nyxt-identifier (get-nyxt-id (first elements)))
                             nil)
                           (lambda-command focus* (elements)
                             (dolist (element (rest elements))
                               (nyxt/dom:focus-select-element :nyxt-identifier (get-nyxt-id element)))
                             (nyxt/dom:focus-select-element :nyxt-identifier (get-nyxt-id (first elements)))
                             nil)
                           (lambda-command hover* (elements)
                             (dolist (element (rest elements))
                               (nyxt/dom:hover-element :nyxt-identifier (get-nyxt-id element)))
                             (nyxt/dom:hover-element :nyxt-identifier (get-nyxt-id (first elements)))
                             nil)))))

(serapeum:export-always 'query-hints)
(defun query-hints (prompt function
                    &key (multi-selection-p t)
                      (selector (hints-selector (find-submode 'hint-mode))))
  "Prompt for elements matching SELECTOR, hinting them visually.
MULTI-SELECTION-P defines whether several elements can be chosen.
PROMPT is a text to show while prompting for hinted elements.
FUNCTION is the action to perform on the selected elements."
  (alex:when-let* ((buffer (current-buffer))
                   (result (prompt
                            :prompt prompt
                            ;; TODO: No need to find the symbol if we move this code (and
                            ;; the rest) to the hint-mode package.
                            :extra-modes (list (resolve-symbol :hint-prompt-buffer-mode :mode))
                            :auto-return-p (auto-follow-hints-p (find-submode 'hint-mode))
                            :history nil
                            :sources
                            (make-instance
                             'hint-source
                             :multi-selection-p multi-selection-p
                             :constructor (lambda (source)
                                            (declare (ignore source))
                                            (add-hints :selector selector)))
                            :after-destructor (lambda () (with-current-buffer buffer
                                                      (remove-hints))))))
    (funcall function result)))

(defmethod prompter:object-attributes :around ((element plump:element))
  `(,@(when (plump:get-attribute element "nyxt-hint")
        `(("Hint" ,(plump:get-attribute element "nyxt-hint"))))
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

(defmethod prompter:object-attributes ((input nyxt/dom:input-element))
  (when (nyxt/dom:body input)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body input))))))

(defmethod prompter:object-attributes ((textarea nyxt/dom:textarea-element))
  (when (nyxt/dom:body textarea)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body textarea))))))

(defmethod prompter:object-attributes ((a nyxt/dom:a-element))
  (append
   (sera:and-let* ((has-href? (plump:has-attribute a "href"))
                   (url-string (plump:get-attribute a "href")))
     `(("URL" ,url-string)))
   (when (nyxt/dom:body a)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body a)))))))

(defmethod prompter:object-attributes ((button nyxt/dom:button-element))
  (when (nyxt/dom:body button)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body button))))))

(defmethod prompter:object-attributes ((details nyxt/dom:details-element))
  (when (nyxt/dom:body details)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body details))))))

(defmethod prompter:object-attributes ((select nyxt/dom:select-element))
  `(("Body" ,(str:shorten 80 (nyxt/dom:body select)))))

(defmethod prompter:object-attributes ((option nyxt/dom:option-element))
  `(("Body" ,(nyxt/dom:body option))
    ,@(when (plump:get-attribute option "value")
        `(("Value" ,(plump:get-attribute option "value"))))))

(defmethod prompter:object-attributes ((img nyxt/dom:img-element))
  (append
   (sera:and-let* ((has-href? (plump:has-attribute img "href"))
                   (url-string (plump:get-attribute img "href")))
     `(("URL" ,url-string)))
   (when (nyxt/dom:body img)
    `(("Body" ,(str:shorten 80 (nyxt/dom:body img)))))))

(defmethod %follow-hint ((element plump:element))
  (nyxt/dom:click-element :nyxt-identifier (get-nyxt-id element)))

(defmethod %follow-hint ((input nyxt/dom:input-element))
  (str:string-case (plump:get-attribute input "type")
    ("button" (nyxt/dom:click-element :nyxt-identifier (get-nyxt-id input)))
    ("radio" (nyxt/dom:check-element :nyxt-identifier (get-nyxt-id input)))
    ("checkbox" (nyxt/dom:check-element :nyxt-identifier (get-nyxt-id input)))
    (otherwise (nyxt/dom:focus-select-element :nyxt-identifier (get-nyxt-id input)))))

(defmethod %follow-hint ((textarea nyxt/dom:textarea-element))
  (nyxt/dom:focus-select-element :nyxt-identifier (get-nyxt-id textarea)))

(defmethod %follow-hint ((details nyxt/dom:details-element))
  (nyxt/dom:toggle-details-element :nyxt-identifier (get-nyxt-id details)))

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
                                                          (plump:get-attribute select "multiple")))))
    (dolist (option (mapcar (alex:rcurry #'find options :test #'equalp) values))
      (nyxt/dom:select-option-element :nyxt-identifier (get-nyxt-id option)
                                      :parent-select-identifier (get-nyxt-id select)))))

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
  (trivial-clipboard:text (render-url (url a))))

(defmethod %copy-hint-url ((img nyxt/dom:img-element))
  (trivial-clipboard:text (render-url (url img))))

(defmethod %copy-hint-url ((element plump:element))
  (echo "Unsupported operation for hint: can't copy URL."))

(define-command follow-hint ()
  "Prompt for element hints and open them in the current buffer."
  (let ((buffer (current-buffer)))
    (query-hints "Go to element"
                 (lambda (results)
                   (%follow-hint (first results))
                   (mapcar (alex:rcurry #'%follow-hint-new-buffer buffer)
                           (rest results))))))

(define-command follow-hint-new-buffer ()
  "Like `follow-hint', but open the selected hints in new buffers (no focus)."
  (let ((buffer (current-buffer)))
    (query-hints "Open element in new buffer"
                 (lambda (result) (mapcar (alex:rcurry #'%follow-hint-new-buffer buffer)
                                          result)))))

(define-command follow-hint-new-buffer-focus ()
  "Like `follow-hint-new-buffer', but with focus."
  (let ((buffer (current-buffer)))
    (query-hints "Go to element in new buffer"
                 (lambda (result)
                   (%follow-hint-new-buffer-focus (first result) buffer)
                   (mapcar (alex:rcurry #'%follow-hint-new-buffer buffer)
                           (rest result))))))

(define-command follow-hint-nosave-buffer ()
  "Like `follow-hint', but open the selected hints in new `nosave-buffer's (no
focus)."
  (query-hints "Open element in new buffer"
               (lambda (result) (mapcar #'%follow-hint-nosave-buffer result))))

(define-command follow-hint-nosave-buffer-focus ()
  "Like `follow-hint-nosave-buffer', but with focus."
  (query-hints "Go to element in new buffer"
               (lambda (result)
                 (%follow-hint-nosave-buffer-focus (first result))
                 (mapcar #'%follow-hint-nosave-buffer (rest result)))))

(define-command follow-hint-with-current-modes-new-buffer ()
  "Prompt for element hints and open them in a new buffer with current
modes."
  (let ((buffer (current-buffer)))
    (query-hints "Open element with current modes in new buffer"
                 (lambda (result)
                   (mapcar (alex:rcurry #'%follow-hint-with-current-modes-new-buffer buffer)
                           result)))))

(define-command copy-hint-url ()
  "Prompt for element hints and save its corresponding URLs to clipboard."
  (query-hints "Copy element URL" (lambda (result)  (%copy-hint-url (first result)))
               :multi-selection-p nil
               :selector "a, img"))

(define-command download-hint-url ()
  "Prompt for element hints and download them."
  (let ((buffer (current-buffer)))
    (query-hints "Download link URL"
                 (lambda (selected-links)
                   (loop for link in selected-links
                         ;; TODO: sleep should NOT be necessary to avoid breaking download
                         do (nyxt/download-mode:download buffer (url link))
                            (sleep 0.25)))
                 :selector "a, img")))
