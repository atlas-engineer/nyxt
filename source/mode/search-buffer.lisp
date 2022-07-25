;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/search-buffer-mode
    (:documentation "Mode for element hints."))
(in-package :nyxt/search-buffer-mode)

(define-mode search-buffer-mode ()
  "Mode for searching text within the buffer."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (style
    (theme:themed-css (theme *browser*)
      (".nyxt-search-hint"
       :background-color (str:concat theme:primary " !important")
       :color (str:concat theme:on-primary " !important"))
      (".nyxt-highlight-search-hint"
       :background-color (str:concat theme:accent " !important")
       :color (str:concat theme:on-accent " !important")))
    :documentation "The style of the search marks, including the highlighted ones.")
   (test-function
    (lambda (sub string)
      (search sub string :test #'equalp))
    :documentation "The function to match text with the text on the page while searching.

Takes two string arguments: search input and the page element text.

You can redefine it to enable regex-based search, for example:
\(define-configuration nyxt/search-buffer-mode:search-buffer-mode
  ((nyxt/search-buffer-mode:test-function #'cl-ppcre:scan)))")
   (keyscheme-map
    (define-keyscheme-map "search" ()
      keyscheme:cua
      (list
       "C-f" 'search-buffer
       "f3" 'search-buffer
       "M-f" 'remove-search-hints)
      keyscheme:emacs
      (list
       "C-s s" 'search-buffer
       "C-s k" 'remove-search-hints)
      keyscheme:vi-normal
      (list
       "/" 'search-buffer
       "?" 'remove-search-hints)))))

(define-parenscript add-stylesheet ()
  (unless (nyxt/ps:qs document "#nyxt-stylesheet")
    (ps:let* ((style-element (ps:chain document (create-element "style")))
              (style (ps:lisp (style (find-submode 'search-buffer-mode)))))
      (setf (ps:@ style-element id) "nyxt-stylesheet")
      (setf (ps:@ style-element inner-text) style)
      (ps:chain document head (append-child style-element)))))

(define-class search-match ()
  ((identifier)
   (element)
   (body)
   (buffer))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nyxt/hint-mode:identifier ((match search-match))
  (identifier match))

(defmethod prompter:object-attributes ((match search-match) (source prompter:source))
  `(("Text" ,(body match))
    ("Buffer ID" ,(princ-to-string (id (buffer match))))
    ("Buffer title" ,(title (buffer match)))))

(define-parenscript hint-elements (selectors)
  (dolist (selector (ps:lisp selectors))
    (ps:let* ((element (nyxt/ps:qs document selector)))
      (ps:chain element class-list (add "nyxt-search-hint")))))

(define-parenscript highlight-selected-hint (&key element scroll)
  (ps:let* ((element (ps:@ (nyxt/ps:qs document (ps:lisp (nyxt/dom:get-unique-selector element))))))
    (when element
      (unless (ps:chain element class-list (contains "nyxt-highlight-search-hint"))
        (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-highlight-search-hint")))
          (ps:dolist (e old-elements)
            (ps:chain e class-list (remove "nyxt-highlight-search-hint")))))
      (ps:chain element class-list (add "nyxt-highlight-search-hint"))
      (when (ps:lisp scroll)
        (ps:chain element (scroll-into-view (ps:create block "center")))))))

(define-parenscript remove-focus ()
  (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-search-highlight-hint")))
    (ps:dolist (e old-elements)
      (ps:chain e class-list (remove "nyxt-highlight-search-hint"))
      (ps:chain e class-list (add "nyxt-search-hint")))))

(define-command remove-search-hints ()
  "Remove all search hints."
  (peval (ps:dolist (node (nyxt/ps:qsa document ".nyxt-search-hint"))
           (ps:chain node class-list (remove "nyxt-search-hint"))
           (ps:chain node class-list (remove "nyxt-highlight-search-hint")))))

(defun add-search-hints (input buffer)
  (let* ((input (str:replace-all " " " " input))
         (test (test-function (find-submode 'search-buffer-mode buffer)))
         (elements (nyxt/dom:find-text
                    input (elt (clss:select "body" (document-model buffer)) 0)
                    :test test))
         (search-matches (mapcar (lambda (element)
                                   (make-instance 'search-match
                                                  :identifier (nyxt/dom:get-unique-selector element)
                                                  :element element
                                                  :body (plump:text element)
                                                  :buffer buffer))
                                 elements)))
    (remove-search-hints)
    (with-current-buffer buffer
      (add-stylesheet)
      (hint-elements (map 'vector #'identifier search-matches))
      search-matches)))

(define-class search-buffer-source (prompter:source)
  ((buffer (current-buffer))
   (minimum-search-length 3)
   (prompter:name "Search buffer")
   (prompter:selection-actions-enabled-p t)
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions))
      (if (>= (length input) (minimum-search-length source))
          (add-search-hints input (buffer source))
          (progn
            (remove-search-hints)
            '()))))
   (prompter:selection-actions (lambda (suggestion)
                                 ;; TODO: rewrite prompt-buffer-selection-highlight-hint
                                 (set-current-buffer (buffer suggestion) :focus nil)
                                 (with-current-buffer (buffer suggestion)
                                   (highlight-selected-hint :element (element suggestion) :scroll t))))
   (prompter:destructor (lambda (prompter source)
                          (declare (ignore prompter source))
                          (unless (keep-search-hints-p (current-buffer))
                            (remove-search-hints)))))
  (:export-accessor-names-p t)
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class))

(defmethod initialize-instance :after ((source search-buffer-source) &key)
  (setf (prompter:name source)
        (format nil "~a (~a+ characters)"
                (prompter:name source)
                (minimum-search-length source))))

(define-command search-buffer ()
  "Search on the current buffer.
If you want to remove the search hints when you close the search
prompt, Set BUFFER's `keep-search-hints-p' slot to nil.

Example:

  (define-configuration buffer
    ((keep-search-hints-p nil)))"
  (prompt :prompt "Search text"
          :sources (make-instance 'search-buffer-source
                                  :return-actions
                                  (list (lambda (search-match)
                                          (unless (keep-search-hints-p (current-buffer))
                                            (remove-search-hints))
                                          search-match)))))

(define-command search-buffers ()
  "Search multiple buffers."
  (let ((buffers (prompt :prompt "Search buffer(s)"
                         :sources (make-instance 'buffer-source ; TODO: Define class?
                                                 :return-actions '()
                                                 :multi-selection-p t))))
    (prompt
     :prompt "Search text"
     :sources (mapcar (lambda (buffer)
                        (make-instance 'search-buffer-source
                                       :name (format nil "Search ~a" (if (url-empty-p (url buffer))
                                                                         (title buffer)
                                                                         (url buffer)))
                                       :buffer buffer))
                      buffers))))
