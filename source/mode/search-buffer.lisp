;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/search-buffer-mode
    (:documentation "Mode for element hints."))
(in-package :nyxt/search-buffer-mode)

(define-mode search-buffer-mode ()
  "Mode for searching text withing."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (style
    (theme:themed-css (theme *browser*)
      `("span[nyxt-search-hint]"
        :background-color ,(str:concat theme:secondary " !important")
        :color ,(str:concat theme:on-secondary " !important")
        :z-index #.(1- (expt 2 31)))
      `("span[nyxt-search-hint].nyxt-current-search-hint"
        :animation-name "highlight"
        :animation-duration "1s"
        :animation-delay "0.2s"
        :animation-timing-function "ease-in-out"
        :animation-iteration-count "infinite"
        :animation-direction "alternate"
        :background ,(format nil "linear-gradient(90deg, ~a, ~a, ~a)"
                             theme:accent theme:background theme:accent)
        :background-size 300% 100%)
      `(:keyframes "highlight"
                   (0% :background-position 100%)
                   (100% :background-position 0%)))
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
       "?" 'remove-search-hints))))
  (:toggler-command-p nil))

(define-class search-match ()
  ((pattern)
   ;; is element needed?  Most likely yes.  But it doesn't quite identify the
   ;; match in plump does it?
   (element)
   (body)
   (match-index)
   (identifier-beg)
   (node-index-beg)
   (text-index-beg)
   (identifier-end)
   (node-index-end)
   (text-index-end)
   (buffer)
   (highlighted-p)))

(defmethod (setf highlighted-p) (value (match search-match))
  (setf (slot-value match 'highlighted-p) value)
  (when value (highlight-match match)))

;; Rename to hint-match?
(defmethod highlight-match ((match search-match))
  (ps-eval :async t :buffer (or (buffer match) (current-buffer))
    ;; StaticRange may improve performance at the cost of correctness.
    (defun create-range () (ps:chain document (create-range)))

    (defun create-match-element (index)
      (let ((elem (ps:chain document (create-element "span"))))
        (ps:chain elem (set-attribute "nyxt-search-hint" index))
        elem))

    (defun wrap (range new-parent) (ps:chain range (surround-contents new-parent)))

    (defun test-node (node) (ps:chain (eq node this)))

    (defun text-nodes-within-bounds (root-element node-beg node-end)
      "TODO"
      (let ((nodes '())
            ;; 4 means NodeFilter.SHOW_TEXT
            (walk (ps:chain document (create-tree-walker root-element 4 null false))))
        (loop for text-node = (ps:chain walk (next-node))
              while text-node
              do (ps:chain nodes (push text-node)))
        (setf nodes
              (ps:chain nodes
                        (slice (ps:chain nodes (find-index test-node node-beg))
                               ;; Safe to replace with find-last-index?
                               (1+ (ps:chain nodes (find-index test-node node-end))))))))

    (let* ((match-index (ps:lisp (match-index match)))
           (elem-beg (nyxt/ps:qs-nyxt-id (ps:@ document body)
                                         (ps:lisp (identifier-beg match))))
           (elem-end (nyxt/ps:qs-nyxt-id (ps:@ document body)
                                         (ps:lisp (identifier-end match))))
           (node-beg (ps:@ elem-beg child-nodes (ps:lisp (node-index-beg match))))
           (node-end (ps:@ elem-end child-nodes (ps:lisp (node-index-end match))))
           (text-beg (ps:lisp (text-index-beg match)))
           (text-end (ps:lisp (text-index-end match)))
           (range (create-range)))
      (ps:chain range (set-start node-beg text-beg))
      (ps:chain range (set-end node-end text-end))
      (ps:try
       ;; Possible unless a node is partially selected by the Range.
       ;; https://www.w3.org/TR/DOM-Level-2-Traversal-Range/ranges.html#td-partially-selected.
       (wrap range (create-match-element match-index))
       (:catch (error)
         ;; When there are partially selected nodes, wrap each of the text nodes
         ;; that account for the match.
         (let* ((root-elem (if (ps:chain elem-beg (contains node-end))
                               elem-beg
                               (ps:@ elem-end parent-element)))
                (nodes (text-nodes-within-bounds root-elem node-beg node-end)))
           (loop with last-index = (1- (ps:chain nodes length))
                 for i from 0 to last-index
                 do (cond ((= i 0)
                           (let ((range (create-range)))
                             (ps:chain range (select-node-contents node-beg))
                             (ps:chain range (set-start node-beg text-beg))
                             (wrap range (create-match-element match-index))))
                          ((= i last-index)
                           (let ((range (create-range)))
                             (ps:chain range (select-node-contents node-end))
                             (ps:chain range (set-end node-end text-end))
                             (wrap range (create-match-element match-index))))
                          (t
                           (let ((range (create-range)))
                             (ps:chain range (select-node-contents (ps:getprop nodes i)))
                             (wrap range (create-match-element match-index))))))))))))

(defmethod css-selector ((match search-match))
  "TODO"
  (format nil "span[nyxt-search-hint=\"~a\"]" (match-index match)))

;; Rename to hint-current-match?
(defmethod highlight-current-match ((match search-match) &key (scroll t))
  "TODO"
  (ps-eval :buffer (buffer match)
    (let ((selector (ps:lisp (css-selector match))))
      (ps:dolist (elem (nyxt/ps:qsa (ps:@ document body) "span.nyxt-current-search-hint"))
        (ps:chain elem class-list (remove "nyxt-current-search-hint")))
      (ps:dolist (elem (nyxt/ps:qsa (ps:@ document body) selector))
        (ps:chain elem class-list (add "nyxt-current-search-hint")))
      ;; take the first element that has the match span and scroll that one.
      (when (ps:lisp scroll)
        ;; why can this element be null?
        (let ((match (nyxt/ps:qs (ps:@ document body) selector)))
          (when match
            (ps:chain match (scroll-into-view (ps:create block "center")))))))))

(defmethod invisible-p ((match search-match))
  "TODO"
  (ps-eval :buffer (buffer match)
    (let ((elem (nyxt/ps:qs (ps:@ document body) (ps:lisp (css-selector match)))))
          (and elem (nyxt/ps:element-invisible-p elem)))))

;; move to utilities.lisp?
;; the min length is (+ len-match (* len-ellipsis 2))
;; if len-max is lower than that, scream with an error
;; This is much more powerful than sera:ellipsize.  Consider contributing.
;; There is also str:shorten.
(defun centered-ellipsize (str beg end &key (len-max 40) (ellipsis "[...]"))
  "TODO"
  (let ((len-str (length str))
        (len-ellipsis (length ellipsis))
        (len-match (1+ (- end beg))))
    (cond ((or (< beg 0) (> end len-str)) (error "Match out of bounds."))
          ((>= len-max len-str) str)
          ((> len-match len-max)
           (str:concat (subseq str 0 (- len-max len-ellipsis))
                       ellipsis))
          ((> len-str len-max)
           (let* ((delta (floor (/ (- len-max len-match) 2)))
                  (new-beg (max 0 (- beg delta)))
                  (new-end (min len-str (+ end delta)))
                  (beg-omitted-p (not (zerop new-beg)))
                  (end-omitted-p (not (= new-end len-str))))
             (str:concat (when beg-omitted-p ellipsis)
                         (subseq str
                                 (if beg-omitted-p (+ new-beg len-ellipsis) new-beg)
                                 (if end-omitted-p (- new-end len-ellipsis) new-end))
                         (when end-omitted-p ellipsis)))))))

(defmethod prompter:object-attributes ((match search-match) (source prompter:source))
  ;; BUG search for "a" in Quebec and see that the Text attribute is wrong.
  `(("Text" ,(let* ((pattern (pattern match))
                    (body (body match))
                    (beg (search pattern body :test (or (test-function source)
                                                        (smart-case-test pattern)))))
               (centered-ellipsize body beg (+ beg (length pattern))))
            nil 3)
    ;; Is this ID needed?
    ("Buffer ID" ,(id (buffer match)))
    ;; Maybe add an attribute that shows the search number
    ("Buffer title" ,(title (buffer match)) nil 2)))

;; Taken from libraries/prompter/filter-preprocessor.lisp
;; Add to utilities?
(defun smart-case-test (string)
  (if (str:downcasep string)
      #'string-equal
      #'string=))

;; Rename fn name and found-pattern/full-match-p names?
(defun search-contiguous (pattern str &key (found-pattern nil)
                                        (full-match-p nil)
                                        (test #'string=))
  "TODO"
  (cond
    ((or (str:empty? pattern) (str:empty? str)) nil)
    ((string= "" (str:prefix (list found-pattern pattern)))
     (error (format nil "~a isn't a prefix of ~a" found-pattern pattern)))
    ((str:empty? found-pattern)
     (loop with delta = (if full-match-p
                            pattern
                            (subseq pattern 0 (1- (length pattern))))
           with len-str = (length str)
           for i downfrom (min (length delta) (length str)) to 1
           for beg = (search delta str :end1 i :start2 (- len-str i) :test test)
           when beg
             do (return (values (str:concat found-pattern (subseq delta 0 i))
                                (list beg (+ beg i))))
             and do (loop-finish)))
    (t
     (alex:when-let* ((delta (sera:string-replace found-pattern pattern ""))
                      (len-delta (min (length delta) (length str)))
                      (beg (search delta str :end1 len-delta :end2 len-delta :test test)))
       (values (str:concat found-pattern (subseq delta 0 len-delta))
               (list beg (+ beg len-delta)))))))

(defun search-all (pattern str &key (test #'string=))
  "TODO"
  (loop with match-indices with len = (length pattern) with beg = 0
        while beg
        when (setf beg (search pattern str :start2 beg :test test))
          do (push (list beg (incf beg len)) match-indices)
        finally (return (nreverse match-indices))))

;; This used to be called query-buffer.
;; TODO add renderer test that tests highlight-matches-p.
(export-always 'search-document)
(defun search-document (pattern &key buffer node (highlight-matches-p nil)
                                  (test #'string-equal))
  (let ((matches) (partial-match) (seen) (match-index 0))
    (labels
        ((traverse-dfs (node)
           "TODO"
           (loop for child across (plump:children node) and index from 0
                 do (typecase child
                      (plump:fulltext-element)
                      (nyxt/dom:noscript-element)
                      (plump:nesting-node (traverse-dfs child))
                      (plump:text-node
                       (let ((text (plump:text child)))
                         ;; Matches circumscribed to a single node
                         (loop with id = (nyxt/dom:get-nyxt-id node)
                               for match in (search-all pattern text :test test)
                               do (push (make-instance 'search-match
                                                       :pattern pattern
                                                       :element node
                                                       :match-index (incf match-index)
                                                       :identifier-beg id
                                                       :identifier-end id
                                                       :node-index-beg index
                                                       :node-index-end index
                                                       :text-index-beg (first match)
                                                       :text-index-end (second match)
                                                       :body text
                                                       :buffer buffer)
                                        matches))
                         ;; Matches spanning multiple nodes
                         (multiple-value-bind (patt bounds)
                             ;; Either match with what has been found thus
                             ;; far, or from scratch.
                             ;; FIXME This is ugly...
                             (if (search-contiguous pattern text :found-pattern seen :test test)
                                 (search-contiguous pattern text :found-pattern seen :test test)
                                 (search-contiguous pattern text :test test))
                           ;; This wouldn't be needed if patt could be
                           ;; replaced with seen in multiple-value-bind.
                           ;; I think pushing the let inside labels will
                           ;; probably fix it.
                           (setf seen patt)
                           (cond ((str:empty? seen)
                                  (setf partial-match nil))
                                 ((and (null partial-match)
                                       ;; seems useless, test!
                                       (not (funcall test seen pattern)))
                                  (setf partial-match
                                        (make-instance 'search-match
                                         :pattern pattern
                                         ;; TODO Is :element set correctly?
                                         :element (plump:parent node)
                                         :identifier-beg (nyxt/dom:get-nyxt-id node)
                                         :node-index-beg index
                                         :text-index-beg (first bounds)
                                         :body text
                                         :buffer buffer)))
                                 ((not (funcall test seen pattern))
                                  (setf (body partial-match)
                                        (str:concat (body partial-match) text)))
                                 (t
                                  (setf (identifier-end partial-match)
                                        (nyxt/dom:get-nyxt-id node)
                                        (node-index-end partial-match)
                                        index
                                        (text-index-end partial-match)
                                        (second bounds)
                                        (body partial-match)
                                        (str:concat (body partial-match) text)
                                        (match-index partial-match)
                                        (incf match-index))
                                  (push partial-match matches)
                                  (setf partial-match nil
                                        seen nil))))))))))
      (traverse-dfs node))
    ;; Highlighting logic.
    (cond ((null highlight-matches-p)
           (setf matches (nreverse matches)))
          ((integerp highlight-matches-p)
           (setf matches (nreverse matches))
           (loop for match in (nreverse (sera:firstn highlight-matches-p matches))
                 do (setf (highlighted-p match) t))
           matches)
          (t
           (loop for match in matches
                 do (setf (highlighted-p match) t))
           (setf matches (nreverse matches))))))

(define-command remove-search-hints (&optional (buffer (current-buffer)))
  "Remove all search hints."
  (ps-eval :buffer buffer
    (dolist (match (nyxt/ps:qsa (ps:@ document body) "span[nyxt-search-hint]"))
      (let ((parent (ps:chain match parent-element)))
        (ps:chain match (insert-adjacent-h-t-m-l "beforebegin"
                                                 (ps:@ match inner-h-t-m-l)))
        (ps:chain match (remove))
        ;; Ensure text nodes aren't empty and adjacent ones are concatenated.
        (ps:chain parent (normalize))))))

;; Maybe rename to search-buffer-match-source
(define-class search-buffer-source (prompter:source)
  ((prompter:name "Search")
   (buffer (current-buffer))
   ;; This isn't possible because it references the buffer...
   ;; (root-node
   ;;  (elt (clss:select "body" (document-model buffer))
   ;;       0))
   (test-function
    ;; when nil use the logic behind smart-case-test.
    nil
    :type (or null function)
    ;; TODO
    :documentation "The function that determines whether a search match is found.

You can redefine it to enable regex-based search, for example:
\(define-configuration nyxt/search-buffer-mode:search-buffer-mode
  ((nyxt/search-buffer-mode:test-function #'cl-ppcre:scan)))")
   (maximum-hinted-matches
    1001
    :type integer
    :documentation "TODO")
   (initial-delay
    0.25
    :documentation "Seconds to wait before searching.
Takes effect when the search pattern's length is less than `no-delay-length'.")
   (no-delay-length
    3
    :documentation "Search starts immediately for patterns at least this long.
For shorter search patterns, `initial-delay' applies.")
   ;; This is obsolete if I implement lazy highlighting.
   (prompter:actions-on-current-suggestion-enabled-p t)
   (prompter:filter nil)
   ;; filter-preprocessor is being used as the constructor, i.e. the mechanism
   ;; to set the suggestions
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions))
      ;; call smart-case-test here.
      (let ((buffer (buffer source)))
        (remove-search-hints buffer)
        (unless (str:empty? input)
          (when (< (length input) (no-delay-length source))
            ;; Allow time for next keystroke to avoid long computations (see
            ;; `prompter::update-thread').
            (sleep (initial-delay source)))
          (search-document input
                           ;; add node as class slot
                           :buffer buffer
                           :node (alex:first-elt (clss:select "body"
                                                   (document-model buffer)))
                           :test (or (test-function source) (smart-case-test input))
                           :highlight-matches-p (maximum-hinted-matches source))))))
   (prompter:actions-on-current-suggestion
    (lambda-command highlight-match (suggestion)
      ;; TODO
      "Scroll to search match."
      (set-current-buffer (buffer suggestion) :focus nil)
      (maybe-update-highlighting suggestion (current-source))
      (when (invisible-p suggestion)
        (setf (slot-value (current-source) 'prompter:suggestions)
              (delete suggestion
                      (slot-value (current-source) 'prompter:suggestions)
                      :key #'prompter:value))
        ;; FIXME Hack that enables the above deletion to cascade.
        (prompter:run-action-on-current-suggestion (current-prompt-buffer)))
      (highlight-current-match suggestion)))
   (prompter:constructor (lambda (source)
                           (add-stylesheet (style (find-submode 'search-buffer-mode))
                                           (buffer source))))
   (prompter:destructor (lambda (prompter source)
                          (declare (ignore prompter))
                          (unless (keep-search-hints-p (buffer source))
                            (remove-search-hints)))))
  (:export-accessor-names-p t)
  (:export-class-name-p t)
  (:metaclass user-class))

;; TODO Add tests?
(defmethod maybe-update-highlighting (current-match (source search-buffer-source))
  (unless (highlighted-p current-match)
    (let* ((matches (mapcar #'prompter:value (prompter:suggestions source)))
           (match-batch (find current-match
                              (sera:batches matches (maximum-hinted-matches source))
                              :test #'member)))
      ;; Add method that would set highlighted-p to nil?  Not sure, since it's
      ;; probably faster to remove all hints in one go.
      (remove-search-hints (buffer source))
      (mapcar (lambda (match) (setf (highlighted-p match) nil)) matches)
      (mapcar (lambda (match) (setf (highlighted-p match) t)) (nreverse match-batch)))))

;; TODO Add option to pass input?
(define-command search-buffer ()
  "Search on the current buffer.
If you want to remove the search hints when you close the search
prompt, Set BUFFER's `keep-search-hints-p' slot to nil.

Example:

  (define-configuration buffer
    ((keep-search-hints-p nil)))"
  (let ((source (make-instance 'search-buffer-source
                               :actions-on-return
                               (lambda (search-match)
                                 (unless (keep-search-hints-p (current-buffer))
                                   (remove-search-hints))
                                 search-match))))
    (prompt :prompt (prompter:name source)
            :sources source)))

(define-class search-buffers-source (buffer-source)
  ((prompter:actions-on-return #'identity)))

(define-command search-buffers ()
  "Search multiple buffers."
  (let* ((buffers (prompt :prompt "Buffers"
                          :sources (make-instance 'search-buffers-source)))
         (sources (mapcar (lambda (buffer)
                            (make-instance 'search-buffer-source
                                           :name (format nil "Search ~a"
                                                         (if (url-empty-p (url buffer))
                                                             (title buffer)
                                                             (url buffer)))
                                           :buffer buffer))
                          buffers)))
    (prompt :prompt "Search"
            :sources sources)))
