;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/hint
  (:documentation "Package for element hints infrastructure and `hint-mode'.

Exposes the APIs below:
- `query-hints' as the main driver for hinting procedures.
- `hint-source' for `prompt-buffer' interaction."))
(in-package :nyxt/mode/hint)

(define-mode hint-mode ()
  "Interact with elements by typing a short character sequence."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (hinting-type
    :emacs
    :type (member :emacs :vi)
    :documentation "Set the hinting mechanism.
In :emacs, hints are computed for the whole page, and the usual `prompt-buffer'
facilities are available.
In :vi, the `prompt-buffer' is collapsed to the input area, hints are computed
in viewport only and they're followed when user input matches the hint string.")
   (style
    (theme:themed-css (theme *browser*)
      `(".nyxt-hint"
        :background-color ,(cl-colors2:print-hex theme:background- :alpha 0.925)
        :color ,theme:on-background
        :font-family "monospace,monospace"
        :font-size ".85rem"
        :padding "0px 0.3em"
        :border-color ,(cl-colors2:print-hex theme:primary- :alpha 0.80)
        :border-radius "3px"
        :border-width "2px"
        :border-style "solid"
        :z-index #.(1- (expt 2 31)))
      `(".nyxt-hint.nyxt-mark-hint"
        :background-color ,theme:secondary
        :color ,theme:on-secondary
        :font-weight "bold")
      `(".nyxt-hint.nyxt-select-hint"
        :background-color ,theme:action
        :color ,theme:on-action)
      `(".nyxt-hint.nyxt-match-hint"
        :padding "0px"
        :border-style "none"
        :opacity "0.5")
      `(".nyxt-element-hint"
        :background-color ,theme:action))
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
   (hints-offset-x
    0
    :type integer
    :documentation "The number of pixels that hint overlays are horizontally shifted by.
A positive value shifts to the right.")
   (hints-offset-y
    0
    :type integer
    :documentation "The number of pixels that hint overlays are vertically shifted by.
A positive value shifts to the bottom.")
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

(define-parenscript-async hint-elements (hints nyxt-identifiers)
  (defun create-hint-overlay (original-element hint)
    "Create a DOM element to be used as a hint."
    (ps:let ((user-x-offset (ps:lisp (hints-offset-x (find-submode 'hint-mode))))
             (user-y-offset (ps:lisp (hints-offset-y (find-submode 'hint-mode))))
             (rect (ps:chain original-element (get-bounding-client-rect)))
             (element (ps:chain document (create-element "span"))))
      (setf (ps:@ element class-name) "nyxt-hint"
            (ps:@ element style position) "absolute"
            (ps:@ element style left) (+ (ps:max (+ (ps:@ window page-x-offset)
                                                    (ps:@ rect left)
                                                    user-x-offset)
                                                 0)
                                         "px")
            (ps:@ element style top) (+ (ps:max (+ (ps:@ window page-y-offset)
                                                   (ps:@ rect top)
                                                   user-y-offset)
                                                0)
                                        "px")
            (ps:@ element id) (+ "nyxt-hint-" hint)
            (ps:@ element text-content) hint)
      element))

  (let ((fragment (ps:chain document (create-document-fragment)))
        (hints (ps:lisp (list 'quote hints)))
        (nyxt-identifiers (ps:lisp (list 'quote nyxt-identifiers))))
    (dotimes (i (length hints))
      (let* ((hint (aref hints i))
             (nyxt-identifier (aref nyxt-identifiers i))
             (element (nyxt/ps:rqs-nyxt-id document nyxt-identifier)))
        (ps:chain element (set-attribute "nyxt-hint" hint))
        (ps:chain fragment (append-child (create-hint-overlay element hint)))
        (when (ps:lisp (show-hint-scope-p (find-submode 'hint-mode)))
          (ps:chain element class-list (add "nyxt-element-hint")))))
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
  (let ((elements (nyxt/ps:rqsa document (ps:lisp selector)))
        (in-view-port-p (ps:lisp (eq :vi (hinting-type (find-submode 'hint-mode))))))
    (ps:dolist (element elements)
      (if in-view-port-p
          (unless (nyxt/ps:element-overlapped-p element)
            (ps:chain element (set-attribute "nyxt-hintable" "")))
          (ps:chain element (set-attribute "nyxt-hintable" ""))))))

(define-parenscript remove-hintable-attribute ()
  (ps:dolist (element (nyxt/ps:rqsa document "[nyxt-hintable]"))
    (ps:chain element (remove-attribute "nyxt-hintable"))))

(defun add-hints (&key selector (buffer (current-buffer)))
  (add-stylesheet "nyxt-hint-stylesheet"
                  (style (find-submode 'hint-mode))
                  buffer)
  (set-hintable-attribute selector)
  (update-document-model :buffer buffer)
  (let* ((hintable-elements (clss:select "[nyxt-hintable]" (document-model buffer)))
         (hints (generate-hints (length hintable-elements))))
    (loop for elem across hintable-elements
          for hint in hints
          do (plump:set-attribute elem "nyxt-hint" hint)
          collect elem
          finally (hint-elements hints (map 'list #'(lambda (elem)
                                                      (plump:attribute elem "nyxt-identifier"))
                                            hintable-elements)))))

(define-parenscript-async remove-hint-elements ()
  (ps:dolist (element (nyxt/ps:rqsa document ":not(.nyxt-search-node) > .nyxt-hint"))
    (ps:chain element (remove)))
  (when (ps:lisp (show-hint-scope-p (find-submode 'hint-mode)))
    (ps:dolist (element (nyxt/ps:rqsa document ".nyxt-element-hint"))
      (ps:chain element class-list (remove "nyxt-element-hint")))))

(defun remove-hints (&key (buffer (current-buffer)))
  (remove-hint-elements)
  (remove-hintable-attribute)
  (update-document-model :buffer buffer))

(export-always 'identifier)
(define-generic identifier ((element plump:element))
  "ELEMENT's on-page identifier (constructed from `hint-alphabet' characters.)"
  (plump:attribute element "nyxt-hint"))

(export-always 'highlight-selected-hint)
(define-parenscript highlight-selected-hint (&key element scroll)
  ;; TODO: Also change border or something else to augment color change.
  "Accent the hint for the ELEMENT to be distinguishable from other hints.
If SCROLL (default to NIL), scroll the hint into view."
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
        (ps:chain %element (scroll-into-view (ps:create block "center")))))))

(define-parenscript-async set-hint-visibility (hint state)
  "Set visibility STATE of HINT element.

Consult https://developer.mozilla.org/en-US/docs/Web/CSS/visibility."
  (let ((element (nyxt/ps:qs document (ps:lisp (format nil "#nyxt-hint-~a"
                                                       (identifier hint))))))
    (when element (setf (ps:@ element style "visibility") (ps:lisp state)))))

(define-parenscript-async dim-hint-prefix (hint prefix-length)
  "Dim the first PREFIX-LENGTH characters of HINT element."
  (let ((element (nyxt/ps:qs document (ps:lisp (format nil "#nyxt-hint-~a"
                                                       (identifier hint))))))
    (when element
      (let ((span-element (ps:chain document (create-element "span"))))
        (setf (ps:@ span-element class-name) "nyxt-hint nyxt-match-hint")
        (setf (ps:@ span-element text-content)
              (ps:lisp (subseq (identifier hint) 0 prefix-length)))
        (setf (ps:chain element inner-h-t-m-l)
              (+ (ps:@ span-element outer-h-t-m-l)
                 (ps:lisp (subseq (identifier hint) prefix-length))))))))

(define-class hint-source (prompter:source)
  ((prompter:name "Hints")
   (prompter:actions-on-current-suggestion-enabled-p t)
   (prompter:filter-preprocessor
    (if (eq :vi (hinting-type (find-submode 'hint-mode)))
        (lambda (suggestions source input)
          (declare (ignore source))
          (loop for suggestion in suggestions
                for hint = (prompter:value suggestion)
                for hinted-element-id = (nyxt/dom:get-nyxt-id hint)
                if (str:starts-with-p input
                                      (prompter:attributes-default suggestion)
                                      :ignore-case t)
                  do (set-hint-visibility hint "visible")
                  and do (when (show-hint-scope-p (find-submode 'hint-mode))
                           (ps-eval
                             (nyxt/ps:add-class-nyxt-id hinted-element-id
                                                        "nyxt-element-hint")))
                  and do (dim-hint-prefix hint (length input))
                  and collect suggestion
                else do (set-hint-visibility hint "hidden")
                     and do (when (show-hint-scope-p (find-submode 'hint-mode))
                              (ps-eval
                                (nyxt/ps:remove-class-nyxt-id hinted-element-id
                                                              "nyxt-element-hint")))))
        #'prompter:delete-inexact-matches))
   (prompter:filter
    (if (eq :vi (hinting-type (find-submode 'hint-mode)))
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
   (prompter:actions-on-current-suggestion
    (when (eq :emacs (hinting-type (find-submode 'hint-mode)))
      (lambda-command highlight-selected-hint* (suggestion)
        "Highlight hint."
        (highlight-selected-hint
         :element suggestion
         :scroll nil))))
   (prompter:actions-on-marks
    (lambda (marks)
      (let ((%marks (mapcar (lambda (mark) (str:concat "#nyxt-hint-" (identifier mark)))
                            marks)))
        (ps-eval
          (dolist (marked-overlay (nyxt/ps:qsa document ".nyxt-mark-hint"))
            (ps:chain marked-overlay class-list (remove "nyxt-mark-hint")))
          (dolist (mark (ps:lisp (list 'quote %marks)))
            (ps:chain (nyxt/ps:qs document mark) class-list (add "nyxt-mark-hint")))))))
   (prompter:actions-on-return
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
            nil)))))

(export-always 'query-hints)
(defun query-hints (prompt function
                    &key (enable-marks-p t)
                         (selector (hints-selector (find-submode 'hint-mode))))
  "Prompt for elements matching SELECTOR, hinting them visually.
ENABLE-MARKS-P defines whether several elements can be chosen.
PROMPT is the text to show while prompting for hinted elements.
FUNCTION is the action to perform on the selected elements."
  (alex:when-let*
      ((buffer (current-buffer))
       (result (prompt
                :prompt prompt
                ;; TODO: No need to find the symbol if we move this code (and
                ;; the rest) to the hint-mode package.
                :extra-modes (list (sym:resolve-symbol :hint-prompt-buffer-mode :mode))
                :auto-return-p (eq :vi (hinting-type (find-submode 'hint-mode)))
                :history nil
                :height (if (eq :vi (hinting-type (find-submode 'hint-mode)))
                            :fit-to-prompt
                            :default)
                :hide-suggestion-count-p (eq :vi (hinting-type (find-submode 'hint-mode)))
                :sources (make-instance 'hint-source
                                        :enable-marks-p enable-marks-p
                                        :constructor
                                        (lambda (source)
                                          (declare (ignore source))
                                          (add-hints :selector selector)))
                :after-destructor (lambda () (with-current-buffer buffer (remove-hints))))))
    (funcall function result)))

(defmethod prompter:object-attributes :around ((element plump:element) (source hint-source))
  `(,@(when (plump:attribute element "nyxt-hint")
        `(("Hint" ,(plump:attribute element "nyxt-hint"))))
    ;; Ensure that all of Body and URL are there, even if empty.
    ,@(loop with attributes = (call-next-method)
            for attr in '("Body" "URL")
            for (same-attr val) = (assoc attr attributes :test 'string=)
            if same-attr
              collect (list same-attr val nil 3)
            else collect (list attr "" nil 3))
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
  `(("Body" ,(nyxt/dom:body option))))

(defmethod prompter:object-attributes ((img nyxt/dom:img-element) (source hint-source))
  (append
   (sera:and-let* ((has-href? (plump:has-attribute img "href"))
                   (url-string (plump:attribute img "href")))
     `(("URL" ,url-string)))
   (when (nyxt/dom:body img)
     `(("Body" ,(str:shorten 80 (nyxt/dom:body img)))))))

(defmethod %follow-hint ((element plump:element))
  (nyxt/dom:click-element element))

(defmethod %follow-hint ((a nyxt/dom:a-element))
  (buffer-load (url a)))

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
  ((prompter:name "Options")
   (prompter:filter-preprocessor #'prompter:filter-exact-matches))
  (:export-class-name-p t)
  (:documentation "Prompt source for select tag options."))

(defmethod %follow-hint ((select nyxt/dom:select-element))
  (sera:and-let* ((options (coerce (clss:select "option" select) 'list))
                  (values (prompt :prompt "Value to select"
                                  :sources (make-instance 'options-source
                                                          :constructor options
                                                          :enable-marks-p
                                                          (plump:attribute select "multiple")))))
    (dolist (option (mapcar (rcurry #'find options :test #'equalp) values))
      (nyxt/dom:select-option-element option select))))

(defmethod %follow-hint-new-buffer-focus ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer-focus :url (url a)
                     :parent-buffer parent-buffer
                     :nosave-buffer-p (nosave-buffer-p parent-buffer)))

(defmethod %follow-hint-new-buffer-focus ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (%follow-hint element))

(defmethod %follow-hint-new-buffer ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer :url (url a) :parent-buffer parent-buffer :load-url-p nil))

(defmethod %follow-hint-new-buffer ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (%follow-hint element))

(defmethod %follow-hint-nosave-buffer-focus ((a nyxt/dom:a-element))
  (make-buffer-focus :url (url a) :nosave-buffer-p t))

(defmethod %follow-hint-nosave-buffer-focus ((element plump:element))
  (%follow-hint element))

(defmethod %follow-hint-nosave-buffer ((a nyxt/dom:a-element))
  (make-nosave-buffer :url (url a)))

(defmethod %follow-hint-nosave-buffer ((element plump:element))
  (%follow-hint element))

(defmethod %follow-hint-with-current-modes-new-buffer ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer :url (url a)
               :modes (mapcar #'sera:class-name-of (modes (current-buffer)))
               :parent-buffer parent-buffer))

(defmethod %follow-hint-with-current-modes-new-buffer ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (%follow-hint element))

(defmethod %copy-hint-url ((a nyxt/dom:a-element))
  (ffi-buffer-copy (current-buffer) (render-url (url a))))

(defmethod %copy-hint-url ((img nyxt/dom:img-element))
  (ffi-buffer-copy (current-buffer) (render-url (url img))))

(defmethod %copy-hint-url ((element plump:element))
  (echo "Unsupported operation for <~a> hint: can't copy hint URL."
        (plump:tag-name element)))

(define-command follow-hint ()
  "Prompt for element hints and open them in the current buffer.
Uses `%follow-hint' internally."
  (let ((buffer (current-buffer)))
    (query-hints "Interact with element"
                 (lambda (results)
                   (%follow-hint (first results))
                   (mapcar (rcurry #'%follow-hint-new-buffer buffer)
                           (rest results))))))

(define-command follow-hint-new-buffer ()
  "Like `follow-hint', but open the selected hints in new buffers (no focus).
Uses `%follow-hint-new-buffer' internally."
  (let ((buffer (current-buffer)))
    (query-hints "Open element in new buffer"
                 (lambda (result) (mapcar (rcurry #'%follow-hint-new-buffer buffer)
                                     result)))))

(define-command follow-hint-new-buffer-focus ()
  "Like `follow-hint-new-buffer', but with focus.
Uses `%follow-hint-new-buffer-focus' internally."
  (let ((buffer (current-buffer)))
    (query-hints "Open element in new buffer"
                 (lambda (result)
                   (%follow-hint-new-buffer-focus (first result) buffer)
                   (mapcar (rcurry #'%follow-hint-new-buffer buffer)
                           (rest result))))))

(define-command follow-hint-nosave-buffer ()
  "Like `follow-hint', but open the selected hints in new `nosave-buffer's (no
focus).
Uses `%follow-hint-nosave-buffer' internally."
  (query-hints "Open element in new buffer"
               (lambda (result) (mapcar #'%follow-hint-nosave-buffer result))))

(define-command follow-hint-nosave-buffer-focus ()
  "Like `follow-hint-nosave-buffer', but with focus.
Uses `%follow-hint-nosave-buffer-focus' internally."
  (query-hints "Open element in new buffer"
               (lambda (result)
                 (%follow-hint-nosave-buffer-focus (first result))
                 (mapcar #'%follow-hint-nosave-buffer (rest result)))))

(define-command follow-hint-with-current-modes-new-buffer ()
  "Prompt for element hints and open them in a new buffer with current modes.
Uses `%follow-hint-with-current-modes-new-buffer' internally."
  (let ((buffer (current-buffer)))
    (query-hints "Open element with current modes in new buffer"
                 (lambda (result)
                   (mapcar (rcurry #'%follow-hint-with-current-modes-new-buffer buffer)
                           result)))))

(define-command copy-hint-url ()
  "Prompt for element hints and save its corresponding URLs to clipboard.
Uses `%copy-hint-url' internally."
  (query-hints "Copy element URL"
               (lambda (result) (%copy-hint-url (first result)))
               :enable-marks-p nil
               :selector "a"))
