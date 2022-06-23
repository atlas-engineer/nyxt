;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/search-buffer-mode
    (:documentation "Mode for element hints."))
(in-package :nyxt/search-buffer-mode)

(define-mode search-buffer-mode (nyxt/hint-mode:hint-mode)
  "Mode for searching text within the buffer."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (style
    (theme:themed-css (theme *browser*)
      (mark
       :all "unset"
       :opacity "0.5")
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
  (defun create-marks (identifier)
    (ps:let* ((element (nyxt/ps:qs document identifier))
              (mark (ps:chain document (create-element "mark"))))
      (setf (ps:@ mark class-name) "nyxt-search-hint")
      (ps:chain element (replace-with mark))
      (ps:chain mark (append-child element))))
  (dolist (selector (ps:lisp selectors))
    (create-marks selector)))

(define-parenscript highlight-selected-hint (&key element scroll)
  (ps:let* ((new-element (nyxt/ps:qs document (ps:lisp (identifier element)))))
    (when new-element
      (unless ((ps:@ new-element class-list contains) "nyxt-highlight-search-hint")
        (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-highlight-search-hint")))
          (ps:dolist (e old-elements)
            (setf (ps:@ e class-name) "nyxt-search-hint"))))
      (setf (ps:@ new-element class-name) "nyxt-highlight-hint")
      (when (ps:lisp scroll)
        (ps:chain new-element (scroll-into-view (ps:create block "nearest")))))))

(define-parenscript remove-focus ()
  (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-search-highlight-hint")))
    (ps:dolist (e old-elements)
      (setf (ps:@ e class-name) "nyxt-search-hint"))))

(defun prompt-buffer-selection-highlight-hint (&key suggestions scroll follow
                                                 (prompt-buffer (current-prompt-buffer))
                                                 (buffer (current-buffer)))
  (alex:when-let ((hint (flet ((hintp (hint-suggestion)
                                 (if (typep hint-suggestion '(or plump:element search-match))
                                     hint-suggestion
                                     nil)))
                          (if suggestions
                              (hintp (prompter:value (first suggestions)))
                              (when prompt-buffer
                                (hintp (current-suggestion-value)))))))
    (when (and follow
               (slot-exists-p hint 'buffer)
               (not (equal (buffer hint) buffer)))
      (set-current-buffer (buffer hint))
      (setf buffer (buffer hint)))
    (if (or (not (slot-exists-p hint 'buffer))
            (and (slot-exists-p hint 'buffer)
                 (equal (buffer hint) buffer)))
        (with-current-buffer buffer
          (nyxt/hint-mode::highlight-selected-hint :element hint :scroll scroll))
        (remove-focus))))

(define-command remove-search-hints ()
  "Remove all search hints."
  (peval (ps:dolist (node (nyxt/ps:qsa document "mark.nyxt-search-hint"))
           (let ((original (ps:chain node first-child)))
             (ps:chain node (replace-with original))))))

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
      (run-thread "stylesheet adder"
        (nyxt/hint-mode::add-stylesheet))
      (run-thread "search hint drawing"
        (hint-elements (coerce (mapcar #'identifier search-matches) 'vector)))
      search-matches)))

(define-class search-buffer-source (prompter:source)
  ((case-sensitive-p nil)
   (buffer (current-buffer))
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
                                 (prompt-buffer-selection-highlight-hint :scroll t)))
   (prompter:destructor (lambda (prompter source)
                          (declare (ignore prompter source))
                          (unless (keep-search-hints-p (current-buffer))
                            (remove-search-hints))
                          (nyxt/hint-mode:remove-focus))))
  (:export-accessor-names-p t)
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class))

(defmethod initialize-instance :after ((source search-buffer-source) &key)
  (setf (prompter:name source)
        (format nil "~a (~a+ characters)"
                (prompter:name source)
                (minimum-search-length source))))

(define-command search-buffer (&key case-sensitive-p)
  "Search on the current buffer.
If you want to remove the search hints when you close the search
prompt, Set BUFFER's `keep-search-hints-p' slot to nil.

Example:

  (define-configuration buffer
    ((keep-search-hints-p nil)))"
  (prompt :prompt "Search text"
          :sources (make-instance 'search-buffer-source
                                  :case-sensitive-p case-sensitive-p
                                  :return-actions
                                  (list (lambda (search-match)
                                          (unless (keep-search-hints-p (current-buffer))
                                            (remove-search-hints))
                                          search-match)))))

(define-command search-buffers (&key case-sensitive-p)
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
                                       :case-sensitive-p case-sensitive-p
                                       :buffer buffer))
                      buffers))))
