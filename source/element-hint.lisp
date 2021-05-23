;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-parenscript add-stylesheet ()
  (unless (ps:chain document (query-selector "#nyxt-stylesheet"))
    (ps:try
     (ps:let* ((style-element (ps:chain document (create-element "style")))
               (box-style (ps:lisp (box-style (current-mode 'web))))
               (highlighted-style (ps:lisp (highlighted-box-style (current-mode 'web)))))
       (setf (ps:@ style-element id) "nyxt-stylesheet")
       (ps:chain document head (append-child style-element))
       (ps:chain style-element sheet (insert-rule box-style 0))
       (ps:chain style-element sheet (insert-rule highlighted-style 1)))
     (:catch (error)))))

(define-parenscript hint-elements (nyxt-identifiers hints)
  (defun hint-determine-position (rect)
    "Determines the position of a hint according to the element"
    (ps:create :top  (+ (ps:@ window page-y-offset) (ps:@ rect top))
               :left (+ (ps:@ window page-x-offset) (ps:@ rect left))))

  (defun hint-create-element (original-element hint)
    "Creates a DOM element to be used as a hint"
    (ps:let* ((rect (ps:chain original-element (get-bounding-client-rect)))
              (position (hint-determine-position rect))
              (element (ps:chain document (create-element "span"))))
      (setf (ps:@ element class-name) "nyxt-hint")
      (setf (ps:@ element style position) "absolute")
      (setf (ps:@ element style left) (+ (ps:@ position left) "px"))
      (setf (ps:@ element style top) (+ (ps:@ position top) "px"))
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

(declaim (ftype (function (t fixnum string) string) select-from-alphabet))
(defun select-from-alphabet (code char-length alphabet)
  (let* ((exponents (nreverse (loop for pow below char-length
                                    collect (expt (length alphabet) pow)))))
    (coerce (loop for exp in exponents
                  for quotinent = (floor (/ code exp))
                  collect (aref alphabet quotinent)
                  do (decf code (* quotinent exp)))
            'string)))

(declaim (ftype (function (integer) list-of-strings) generate-hints))
(defun generate-hints (length)
  (let* ((alphabet (hints-alphabet (current-mode 'web)))
         (char-length (ceiling (log length (length alphabet)))))
    (loop for i below length collect (select-from-alphabet i char-length alphabet))))

(defun add-element-hints (&key selector)
  (let* ((dom (document-model (current-mode 'web)))
         (hintable-elements (clss:select selector dom))
         (hints (generate-hints (length hintable-elements))))
    (add-stylesheet)
    (hint-elements (map 'list #'get-nyxt-id hintable-elements) hints)
    (loop for elem across hintable-elements
          for hint in hints
          do (plump:set-attribute elem "nyxt-hint" hint)
          collect elem)))

(define-parenscript remove-element-hints ()
  (defun hints-remove-all ()
    "Removes all the elements"
    (ps:dolist (element (nyxt/ps:qsa document ".nyxt-hint"))
      (ps:chain element (remove))))
  (hints-remove-all))

(define-parenscript click-element (&key nyxt-identifier)
  (ps:chain (nyxt/ps:qs-nyxt-id document nyxt-identifier) (click)))

(define-parenscript focus-element (&key nyxt-identifier)
  (ps:chain (nyxt/ps:qs-nyxt-id document nyxt-identifier) (focus))
  (ps:chain (nyxt/ps:qs-nyxt-id document nyxt-identifier) (select)))

(define-parenscript check-element (&key nyxt-identifier (value t))
  (ps:chain (nyxt/ps:qs-nyxt-id document nyxt-identifier)
            (set-attribute "checked" (ps:lisp value))))

(define-parenscript toggle-details-element (&key nyxt-identifier)
  (ps:let ((element (nyxt/ps:qs-nyxt-id document nyxt-identifier)))
    (if (ps:chain element (get-attribute "open"))
        (ps:chain element (remove-attribute "open"))
        (ps:chain element (set-attribute "open" t)))))

(define-parenscript select-option-element (&key nyxt-identifier parent-select-identifier)
  (ps:let* ((element (nyxt/ps:qs-nyxt-id document nyxt-identifier))
            (parent-select (nyxt/ps:qs-nyxt-id document parent-select-identifier)))
    (if (ps:chain element (get-attribute "multiple"))
        (ps:chain element (set-attribute "selected" t))
        (setf (ps:@ parent-select value) (ps:@ element value)))))

(define-parenscript highlight-selected-hint (&key element scroll)
  (defun update-hints ()
    (ps:let* ((new-element (nyxt/ps:qs document
                                       (ps:lisp (format
                                                 nil "#nyxt-hint-~a"
                                                 (typecase element
                                                   (plump:element (plump:get-attribute element "nyxt-hint"))
                                                   (search-match (identifier element))))))))
      (when new-element
        (unless ((ps:@ new-element class-list contains) "nyxt-highlight-hint")
          (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-highlight-hint")))
            (ps:dolist (e old-elements)
              (setf (ps:@ e class-name) "nyxt-hint"))))
        (setf (ps:@ new-element class-name) "nyxt-hint nyxt-highlight-hint")
        (if (ps:lisp scroll)
            (ps:chain new-element (scroll-into-view
                                   (ps:create block "nearest")))))))

  (update-hints))

(define-parenscript remove-focus ()
  (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-highlight-hint")))
    (ps:dolist (e old-elements)
      (setf (ps:@ e class-name) "nyxt-hint"))))

(define-class hint-source (prompter:source)
  ((prompter:name "Hints")
   (prompter:follow-p t)
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
   (prompter:follow-mode-functions (lambda (suggestion)
                                     (highlight-selected-hint :element suggestion)))))

(serapeum:export-always 'query-hints)
(defun query-hints (prompt function &key multi-selection-p
                                      (selector "a, button, input, textarea, details, select"))
  (let* ((buffer (current-buffer)))
    (let ((result (prompt
                   :prompt prompt
                   :extra-modes '(element-hint-mode)
                   :history nil
                   :sources
                   (make-instance
                    'hint-source
                    :multi-selection-p multi-selection-p
                    :constructor (add-element-hints :selector selector))
                   :after-destructor
                   (lambda ()
                     (with-current-buffer buffer
                       (remove-element-hints))))))
      (funcall function result))))

(defmethod prompter:object-attributes :around ((element plump:element))
  `(,@(when (plump:get-attribute element "nyxt-hint")
        `(("Hint" ,(plump:get-attribute element "nyxt-hint"))))
    ,@(call-next-method)
    ("Type" ,(str:capitalize (str:string-case
                                 (plump:tag-name element)
                               ("a" "link")
                               (otherwise (plump:tag-name element)))))))

(defmethod prompter:object-attributes ((input nyxt/dom:input-element))
  (when (plump:get-attribute input "placeholder")
    `(("Placeholder" ,(plump:get-attribute input "placeholder")))))

(defmethod prompter:object-attributes ((textarea nyxt/dom:textarea-element))
  (append
   (when (plump:get-attribute textarea "placeholder")
     `(("Placeholder" ,(plump:get-attribute textarea "placeholder"))))
   (when (plump:get-attribute textarea "value")
     `(("Value" ,(str:shorten 80 (plump:get-attribute textarea "value")))))))

(defmethod prompter:object-attributes ((a nyxt/dom:a-element))
  (append
   (when (plump:get-attribute a "href")
     `(("URL" ,(plump:get-attribute a "href"))))
   (when (plump:children a)
     `(("Body" ,(plump:text a))))))

(defmethod prompter:object-attributes ((button nyxt/dom:button-element))
  (when (plump:children button)
    `(("Body" ,(plump:text button)))))

(defmethod prompter:object-attributes ((details nyxt/dom:details-element))
  (when (clss:select "summary" details)
    `(("Summary" ,(plump:text (elt (clss:select "summary" details) 0))))))

(defmethod prompter:object-attributes ((select nyxt/dom:select-element))
  `(("Options" ,(str:shorten 80 (str:join ", " (map 'list #'plump:text
                                                    (clss:select "option" select)))))))

(defmethod prompter:object-attributes ((option nyxt/dom:option-element))
  `(("Text" ,(plump:text option))
    ,@(when (plump:get-attribute option "value")
        `(("Value" ,(plump:get-attribute option "value"))))))

(defmethod %follow-hint ((a nyxt/dom:a-element))
  (click-element :nyxt-identifier (get-nyxt-id a)))

(defmethod %follow-hint ((button nyxt/dom:button-element))
  (click-element :nyxt-identifier (get-nyxt-id button)))

(defmethod %follow-hint ((checkbox nyxt/dom:checkbox-element))
  (check-element :nyxt-identifier (get-nyxt-id checkbox)))

(defmethod %follow-hint ((radio nyxt/dom:radio-element))
  (check-element :nyxt-identifier (get-nyxt-id radio)))

(defmethod %follow-hint ((input nyxt/dom:input-element))
  (focus-element :nyxt-identifier (get-nyxt-id input)))

(defmethod %follow-hint ((textarea nyxt/dom:textarea-element))
  (focus-element :nyxt-identifier (get-nyxt-id textarea)))

(defmethod %follow-hint ((details nyxt/dom:details-element))
  (toggle-details-element :nyxt-identifier (get-nyxt-id details)))

(define-class options-source (prompter:source)
  ((prompter:name "Options"))
  (:export-class-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Prompt source for select tag options."))

(defmethod %follow-hint ((select nyxt/dom:select-element))
  (sera:and-let* ((options (coerce (clss:select "option" select) 'list))
                  (values (prompt :prompt "Value to select"
                                  :sources (make-instance 'options-source
                                                          :constructor options
                                                          :multi-selection-p
                                                          (plump:get-attribute select "multiple")))))
    (dolist (option (mapcar (alex:rcurry #'find options :test #'equalp) values))
      (select-option-element :nyxt-identifier (get-nyxt-id option)
                             :parent-select-identifier (get-nyxt-id select)))))

(defmethod %follow-hint-new-buffer-focus ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer-focus :url (plump:get-attribute "href" a)
                     :parent-buffer parent-buffer
                     :nosave-buffer-p (nosave-buffer-p parent-buffer)))

(defmethod %follow-hint-new-buffer-focus ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-new-buffer ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer :url (plump:get-attribute "href" a) :parent-buffer parent-buffer))

(defmethod %follow-hint-new-buffer ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-nosave-buffer-focus ((a nyxt/dom:a-element))
  (make-buffer-focus :url (plump:get-attribute "href" a) :nosave-buffer-p t))

(defmethod %follow-hint-nosave-buffer-focus ((element plump:element))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-nosave-buffer ((a nyxt/dom:a-element))
  (make-nosave-buffer :url (plump:get-attribute "href" a)))

(defmethod %follow-hint-nosave-buffer ((element plump:element))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %copy-hint-url ((a nyxt/dom:a-element))
  (trivial-clipboard:text (plump:get-attribute "href" a)))

(defmethod %copy-hint-url ((element plump:element))
  (echo "Unsupported operation for hint: can't copy URL."))

(defun prompt-buffer-selection-highlight-hint (&key suggestions scroll follow
                                                 (prompt-buffer (current-prompt-buffer))
                                                 (buffer (current-buffer)))
  (let ((hint (flet ((hintp (hint-suggestion)
                       (if (typep hint-suggestion '(or plump:element search-match))
                           hint-suggestion
                           nil)))
                (if suggestions
                    (hintp (prompter:value (first suggestions)))
                    (when prompt-buffer
                      (hintp (current-suggestion-value)))))))
    (when hint
      (when (and follow
                 (slot-exists-p hint 'buffer)
                 (not (equal (buffer hint) buffer)))
        (set-current-buffer (buffer hint))
        (setf buffer (buffer hint)))
      (if (or
           (not (slot-exists-p hint 'buffer))
           (and (slot-exists-p hint 'buffer)
                (equal (buffer hint) buffer)))
          (with-current-buffer buffer
            (highlight-selected-hint :element hint :scroll scroll))
          (remove-focus)))))

(define-command follow-hint ()
  "Show a set of element hints, and go to the user inputted one in the current
buffer."
  (query-hints "Go to element" (lambda (results) (%follow-hint (first results)))))

(define-command follow-hint-new-buffer ()
  "Show a set of element hints, and open the user inputted one in a new
buffer (not set to visible active buffer)."
  (let ((buffer (current-buffer)))
    (query-hints "Open element in new buffer"
                 (lambda (result) (mapcar (alex:rcurry #'%follow-hint-new-buffer buffer)
                                          result))
                 :multi-selection-p t)))

(define-command follow-hint-new-buffer-focus ()
  "Show a set of element hints, and open the user inputted one in a new
visible active buffer."
  (let ((buffer (current-buffer)))
    (query-hints "Go to element in new buffer"
                 (lambda (result)
                   (%follow-hint-new-buffer-focus (first result) buffer)
                   (mapcar (alex:rcurry #'%follow-hint-new-buffer buffer)
                           (rest result)))
                 :multi-selection-p t)))

(define-command follow-hint-nosave-buffer ()
  "Show a set of element hints, and open the user inputted one in a new
nosave buffer (not set to visible active buffer)."
  (query-hints "Open element in new buffer"
               (lambda (result) (mapcar #'%follow-hint-nosave-buffer result))
               :multi-selection-p t))

(define-command follow-hint-nosave-buffer-focus ()
  "Show a set of element hints, and open the user inputted one in a new
visible nosave active buffer."
  (query-hints "Go to element in new buffer"
               (lambda (result)
                 (%follow-hint-nosave-buffer-focus (first result))
                 (mapcar #'%follow-hint-nosave-buffer (rest result)))
               :multi-selection-p t))

(define-command copy-hint-url ()
  "Show a set of element hints, and copy the URL of the user inputted one."
  (query-hints "Copy element URL" (lambda (result)  (%copy-hint-url (first result)))))

(define-command bookmark-hint ()
  "Show link hints on screen, and allow the user to bookmark one"
  (query-hints "Bookmark hint"
               (lambda (result)
                 (let ((url (url (first result))))
                   (bookmark-url :url url)))
               :multi-selection-p t))

(define-command download-hint-url ()
  "Download the file under the URL(s) hinted by the user."
  (let ((buffer (current-buffer)))
    (query-hints "Download link URL"
                 (lambda (selected-links)
                   (loop for link in selected-links
                         ;; TODO: sleep should NOT be necessary to avoid breaking download
                         do (download buffer (url link))
                            (sleep 0.25)))
                 :multi-selection-p t)))
