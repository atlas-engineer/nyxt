;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/search-buffer-mode
    (:documentation "Mode for element hints."))
(in-package :nyxt/search-buffer-mode)

(define-mode search-buffer-mode (nyxt/hint-mode:hint-mode)
  "Mode for searching text withing."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (style
    (theme:themed-css (theme *browser*)
      (".nyxt-search-node .nyxt-hint"
       :background-color theme:secondary
       :color theme:on-secondary
       :padding "0px"
       :border-radius "0px"
       :z-index #.(1- (expt 2 31)))
      (".nyxt-search-node > .nyxt-hint.nyxt-select-hint"
       :background-color theme:accent
       :color theme:on-accent))
    :documentation "The style of the search overlays.")
   (keyscheme-map
    (define-keyscheme-map "search-buffer-mode" ()
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

(defvar *node-class-name* "nyxt-search-node")

(define-parenscript query-buffer (&key query (case-sensitive-p nil)
                                       keep-previous-hints
                                       (node-class-name *node-class-name*))
  (defvar *identifier* 0)
  (defvar *matches* (array))
  (defvar *nodes* (ps:new (-Object)))
  (defvar *node-replacements* (array))

  (defun add-stylesheet ()
    (unless (nyxt/ps:qs document "#nyxt-stylesheet")
      (ps:try
       (ps:let ((style-element (ps:chain document (create-element "style"))))
         (setf (ps:@ style-element id) "nyxt-stylesheet")
         (ps:chain document head (append-child style-element))
         (setf (ps:chain style-element inner-text)
               (ps:lisp (style (find-submode 'nyxt/search-buffer-mode:search-buffer-mode)))))
       (:catch (error)))))

  (defun create-match-object (body identifier)
    (ps:create "type" "match" "identifier" identifier "body" body))

  (defun create-match-span (body identifier)
    (ps:let* ((el (ps:chain document (create-element "span"))))
      (setf (ps:@ el class-name) "nyxt-hint")
      (setf (ps:@ el text-content) body)
      (setf (ps:@ el id) (+ "nyxt-hint-" identifier))
      el))

  (defun get-substring (string query index)
    "Return the substring and preceding/trailing text for a given
     index."
    (let* ((character-preview-count 40))
      (ps:chain string
                (substring (- index character-preview-count)
                           (+ index (length query) character-preview-count)))))

  (defun get-substring-indices (query string)
    "Get the indices of all matching substrings."
    (let ((query (if (ps:lisp case-sensitive-p)
                     query
                     (ps:chain query (to-lower-case))))
          (string (if (ps:lisp case-sensitive-p)
                      string
                      (ps:chain string (to-lower-case))))
          (index (- (length query))))
      (loop with subindex = 0
            until (= subindex -1)
            do (setf subindex (ps:chain string (index-of query)))
               (setf string (ps:chain string (substring (+ subindex (length query)))))
               (setf index (+ index subindex (length query)))
            when (not (= subindex -1))
              collect index)))

  (defun matches-from-node (node query)
    "Return all of substrings that match the search-string."
    (when (= (ps:chain (typeof (ps:@ node node-value))) "string")
      (let* ((node-text (ps:@ node text-content))
             (substring-indices (get-substring-indices query node-text))
             (node-identifier (incf (ps:chain *nodes* identifier)))
             (new-node (ps:chain document (create-element "span"))))
        (setf (ps:@ new-node class-name) (ps:lisp node-class-name))
        (setf (ps:@ new-node id) node-identifier)
        (setf (aref *nodes* node-identifier) node)
        (when (> (length substring-indices) 0)
          (loop for index in substring-indices
                with last-index = 0
                do (incf *identifier*)
                   (ps:chain new-node (append-child (ps:chain document (create-text-node (ps:chain node-text (substring last-index index))))))
                   (ps:chain new-node (append-child (create-match-span (ps:chain node-text (substring index (+ index (length query)))) *identifier*)))
                   (setf last-index (+ (length query) index))
                   (ps:chain *matches* (push (create-match-object (get-substring node-text query index) *identifier*)))
                finally (progn
                          (ps:chain new-node (append-child (ps:chain document (create-text-node (ps:chain node-text (substring (+ (length query) index)))))))
                          (ps:chain *node-replacements*
                                    (push (list node new-node)))))))))

  (defun replace-original-nodes ()
    "Replace original nodes with recreated search nodes"
    (loop for node-pair in *node-replacements*
          do (ps:chain (elt node-pair 0) (replace-with (elt node-pair 1)))))

  (defun walk-document (node process-node)
    (when (and node (not (ps:chain node first-child)))
      (funcall process-node node (ps:lisp query)))
    (setf node (ps:chain node first-child))
    (loop while node
          do (walk-document node process-node)
          do (setf node (ps:chain node next-sibling))))

  (defun remove-search-nodes ()
    "Removes all the search elements"
    (ps:dolist (node (nyxt/ps:qsa document (+ "." (ps:lisp node-class-name))))
      (ps:chain node (replace-with (aref *nodes* (ps:@ node id))))))

  (let ((*matches* (array))
        (*node-replacements* (array))
        (*identifier* 0))
    (add-stylesheet)
    (unless (ps:lisp keep-previous-hints)
      (remove-search-nodes))
    (setf (ps:chain *nodes* identifier) 0)
    (walk-document (ps:chain document body) matches-from-node)
    (replace-original-nodes)
    *matches*))

(define-class search-match ()
  ((identifier)
   (body)
   (buffer))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod nyxt/hint-mode:identifier ((match search-match))
  (identifier match))

(defmethod prompter:object-attributes ((match search-match) (source prompter:source))
  `(("Default" ,(body match))
    ("ID" ,(identifier match))
    ("Buffer ID" ,(id (buffer match)))
    ("Buffer title" ,(title (buffer match)))))

(defun matches-from-js (matches-js-array &optional (buffer (current-buffer)))
  (loop for element in matches-js-array
        collect (make-instance 'search-match
                               :identifier (gethash "identifier" element)
                               :body (gethash "body" element)
                               :buffer buffer)))

(define-command remove-search-hints (&key (node-class-name *node-class-name*))
  "Remove all search hints."
  (ps-eval (ps:dolist (node (nyxt/ps:qsa document (+ "." (ps:lisp node-class-name))))
             (ps:chain node (replace-with (aref *nodes* (ps:@ node id)))))))

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
            (nyxt/hint-mode:highlight-selected-hint :element hint
                                                    :scroll scroll))
          (nyxt/hint-mode:unhighlight-selected-hint)))))

(define-class search-buffer-source (prompter:source)
  ((case-sensitive-p nil)
   (buffer (current-buffer))
   (minimum-search-length 1)
   (prompter:name "Search buffer")
   (prompter:selection-actions-enabled-p t)
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions))
      (if (>= (length input) (minimum-search-length source))
          (let ((input (str:replace-all " " " " input))
                (buffer (buffer source)))
            (with-current-buffer buffer
              (matches-from-js
               (query-buffer
                :query input
                :case-sensitive-p (case-sensitive-p source))
               buffer)))
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
                          (nyxt/hint-mode:unhighlight-selected-hint))))
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
