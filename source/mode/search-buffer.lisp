;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/search-buffer
  (:documentation "Package for `search-buffer-mode', for incremental buffer search."))
(in-package :nyxt/mode/search-buffer)

(define-mode search-buffer-mode ()
  "Incremental search on a single or multiple buffers."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (style
    (theme:themed-css (theme *browser*)
      `("span[nyxt-search-mark]"
        :background-color ,(str:concat theme:secondary " !important")
        :color ,(str:concat theme:on-secondary " !important")
        :border-radius 3px
        :z-index #.(1- (expt 2 31)))
      `("span[nyxt-search-mark].nyxt-current-search-mark"
        :background ,(str:concat "#FFEF00" " !important")))
    :documentation "The style of the search overlays.")
   (keyscheme-map
    (define-keyscheme-map "search-buffer-mode" ()
      keyscheme:cua
      (list
       "C-f" 'search-buffer
       "f3" 'search-buffer
       "M-f" 'remove-search-marks)
      keyscheme:emacs
      (list
       "C-s s" 'search-buffer
       "C-s k" 'remove-search-marks)
      keyscheme:vi-normal
      (list
       "/" 'search-buffer
       "?" 'remove-search-marks))))
  (:toggler-command-p nil))

(define-class search-match ()
  ((pattern
    ""
    :type string
    :documentation "The requested search pattern.")
   (body
    ""
    :type string
    :documentation "The full context of the match.
It is the concatenation of text nodes that constitute the match.")
   (buffer
    nil
    :type (maybe buffer)
    :documentation "The buffer where the match is found.")
   (marked-p
    nil
    :type boolean
    :writer nil
    :reader marked-p
    :documentation "Whether the match is shown in its corresponding `buffer'.
Requires running JavaScript code.")
   (nodes
    '()
    :type (list-of plump:node)
    :documentation "The list of text nodes where the match is found.")
   (id
    0
    :type alex:non-negative-fixnum
    :documentation "The unique identifier.
Useful to reference the match via CSS selectors.")
   (identifier-beg
    ""
    :type (maybe string)
    :documentation "DOM coordinate that marks the beginning of the match.")
   (node-index-beg
    0
    :type alex:non-negative-fixnum
    :documentation "DOM coordinate that marks the beginning of the match.")
   (text-index-beg
    0
    :type alex:non-negative-fixnum
    :documentation "DOM coordinate that marks the beginning of the match.")
   (identifier-end
    ""
    :type (maybe string)
    :documentation "DOM coordinate that marks the end of the match.")
   (node-index-end
    0
    :type alex:non-negative-fixnum
    :documentation "DOM coordinate that marks the end of the match.")
   (text-index-end
    0
    :type alex:non-negative-fixnum
    :documentation "DOM coordinate that marks the end of the match."))
  (:documentation "A `search-match' captures the means to manipulate matches via
two complementary ways: (1) the Lisp-side DOM (powered by `plump' and
`nyxt/dom'), and (2) Javascript."))

(defmethod (setf marked-p) (value (match search-match))
  (when value (mark match))
  (setf (slot-value match 'marked-p) value))

(defmethod css-selector ((match search-match))
  "Return a CSS selector that uniquely identifies MATCH."
  (format nil "span[nyxt-search-mark=\"~a\"]" (id match)))

(defmethod mark ((match search-match))
  "Mark MATCH in its corresponding buffer.

The DOM is mutated via Javascript by wrapping MATCH around a span element.  In
some cases, when MATCH spans multiple text nodes, multiple span elements wrap
MATCH.

Style it via CSS selector \"[nyxt-search-mark]\"."
  (ps-eval :async t :buffer (buffer match)
    ;; StaticRange may improve performance at the cost of correctness.
    (defun create-range () (ps:chain document (create-range)))

    (defun create-match-element (value)
      (let ((elem (ps:chain document (create-element "span"))))
        (ps:chain elem (set-attribute "nyxt-search-mark" value))
        elem))

    (defun wrap (range new-parent) (ps:chain range (surround-contents new-parent)))

    (defun test-node (node) (ps:chain (eq node this)))

    (defun text-nodes-within-bounds (root node-beg node-end)
      "Return text nodes under ROOT, bounded by NODE-BEG and NODE-END."
      ;; No need to raise an error when node-beg/end don't descend from root
      ;; since, in the particular context of `mark', it always holds true.
      (let ((nodes '())
            ;; Tersely written due to PS limitations.
            ;; 4 means that only text nodes are collected by the generator.
            ;; https://developer.mozilla.org/en-US/docs/Web/API/TreeWalker/whatToShow
            (walker (ps:chain document (create-tree-walker root 4 null false))))
        (loop for text-node = (ps:chain walker (next-node))
              while text-node
              do (ps:chain nodes (push text-node)))
        (let ((beg (ps:chain nodes (find-index test-node node-beg)))
              (end (1+ (ps:chain nodes (find-last-index test-node node-end)))))
          (setf nodes (ps:chain nodes (slice beg end))))))

    (let* ((id (ps:lisp (id match)))
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
       ;; Errors when a node is partially selected by the Range.
       ;; https://www.w3.org/TR/DOM-Level-2-Traversal-Range/ranges.html#td-partially-selected.
       (wrap range (create-match-element id))
       (:catch (error)
         ;; If there are partially selected nodes, wrap each of the text nodes.
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
                             (wrap range (create-match-element id))))
                          ((= i last-index)
                           (let ((range (create-range)))
                             (ps:chain range (select-node-contents node-end))
                             (ps:chain range (set-end node-end text-end))
                             (wrap range (create-match-element id))))
                          (t
                           (let ((range (create-range)))
                             (ps:chain range (select-node-contents (ps:getprop nodes i)))
                             (wrap range (create-match-element id))))))))))))

(defmethod mark-alternate ((match search-match) &key (scroll t))
  "Mark MATCH and optionally SCROLL it into view.

Differs from `nyxt/mode/::mark' in the sense that is allows for a
more refined styling.  This is particularly useful when MATCH needs to stand out
from others matches.

Style it via CSS selector \".nyxt-current-search-mark\"."
  (ps-eval :buffer (buffer match)
    ;; Note that multiple span elements may feature class
    ;; .nyxt-current-search-mark.  I.e., to a single match may correspond
    ;; several span elements.
    (ps:dolist (elem (nyxt/ps:qsa (ps:@ document body)
                                  "span[nyxt-search-mark].nyxt-current-search-mark"))
      (ps:chain elem class-list (remove "nyxt-current-search-mark")))
    (let ((match-selector (ps:lisp (css-selector match))))
      (ps:dolist (elem (nyxt/ps:qsa (ps:@ document body) match-selector))
        (ps:chain elem class-list (add "nyxt-current-search-mark")))
      (when (ps:lisp scroll)
        (let ((match (nyxt/ps:qs (ps:@ document body) match-selector)))
          (when match
            (ps:chain match (scroll-into-view (ps:create block "center")))))))))

(defmethod invisible-p ((match search-match))
  "Whether MATCH is invisible in its corresponding buffer."
  (ps-eval :buffer (buffer match)
    (let ((elem (nyxt/ps:qs (ps:@ document body) (ps:lisp (css-selector match)))))
          (and elem (nyxt/ps:element-invisible-p elem)))))

;; More powerful than sera:ellipsize or str:shorten.
;; TODO Add tests.
(defun centered-ellipsize (str beg end &key (len-max 80) (ellipsis "[...]"))
  "Return a substring of STR of length LEN-MAX, at most.

The substring of STR bounded by BEG and END is centered in the returned
truncated string, and ELLIPSIS is added at the boundary when needed."
  (let ((len-str (length str))
        (len-ellipsis (length ellipsis))
        (len-match (1+ (- end beg))))
    (cond ((or (< beg 0) (> end len-str))
           (error "Match out of bounds."))
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
  `(("Match ID" ,(id match) nil 1)
    ("Text" ,(centered-ellipsize (body match)
                                 (text-index-beg match)
                                 (if (sera:single (nodes match))
                                     (text-index-end match)
                                     (+ (reduce #'+
                                                (butlast (nodes match))
                                                :key (compose #'length #'plump:text))
                                        (text-index-end match))))
            nil 12)))

(defun search-contiguous (pattern str &key (found-pattern nil)
                                        (full-match-p nil)
                                        (test #'string=))
  "Search for PATTERN in STR given that FOUND-PATTERN has been observed.

When a match is found, return the substring of PATTERN and a list of position
indices relative to STR.

TEST is a function of 2 arguments that returns a boolean.  It determines what
qualifies as a match.

Note that only contiguous matches are considered.  For example:
(search-contiguous \"match\" \"a m\" :found-pattern \"m\") -> NIL
(search-contiguous \"match\" \"m a\" :found-pattern \"m\") -> NIL
(search-contiguous \"match\" \"a\"   :found-pattern \"m\") -> (values \"ma\" (0 1))"
  (declare (type string pattern) (type string str))
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
             do (return (values (subseq delta 0 i) (list beg (+ beg i))))
             and do (loop-finish)))
    (t
     (alex:when-let* ((delta (sera:string-replace found-pattern pattern ""))
                      (len-delta (min (length delta) (length str)))
                      (beg (search delta str :end1 len-delta :end2 len-delta :test test)))
       (values (str:concat found-pattern (subseq delta 0 len-delta))
               (list beg (+ beg len-delta)))))))

(defun search-all (pattern str &key (test #'string=))
  "Return all pairs of indices where PATTERN is found in STR.

TEST is a function of 2 arguments that returns a boolean.  It determines what
qualifies as a match."
  (declare (type string pattern) (type string str))
  (unless (string-equal "" pattern)
    (loop with match-indices with len = (length pattern) with beg = 0
          while beg
          when (setf beg (search pattern str :start2 beg :test test))
            do (push (list beg (incf beg len)) match-indices)
          finally (return (nreverse match-indices)))))

(export-always 'search-document)
(defun search-document (pattern &key buffer node (mark-p nil) (test #'string-equal))
  "Search for PATTERN in BUFFER's DOM NODE.

NODE is a `plump' or `nyxt/dom' object.

TEST is a function of 2 arguments that returns a boolean.  It determines what
qualifies as a match.

MARK-P accepts the following values:
- NIL, to disable marking matches;
- T, to mark all matches;
- INTEGER, to mark those many matches."
  (let ((matches) (partial-match) (seen) (idx 0))
    (labels
        ((traverse-dfs (node)
           "Traverse NODE depth-first-search and collect search matches."
           (loop for child across (plump:children node) and index from 0
                 do (typecase child
                      ;; Filter style, script and noscript elements.
                      (plump:fulltext-element)
                      (plump:nesting-node (traverse-dfs child))
                      (plump:text-node
                       (let ((text (plump:text child)))
                         ;; Matches circumscribed to a single node
                         (loop with id = (nyxt/dom:get-nyxt-id node)
                               for match in (search-all pattern text :test test)
                               do (push (make-instance
                                         'search-match
                                         :pattern pattern
                                         :body text
                                         :buffer buffer
                                         :nodes (list child)
                                         :id (incf idx)
                                         :identifier-beg id
                                         :node-index-beg index
                                         :text-index-beg (first match)
                                         :identifier-end id
                                         :node-index-end index
                                         :text-index-end (second match))
                                        matches))
                         ;; Matches spanning multiple nodes
                         (multiple-value-bind (patt bounds)
                             ;; Either match with what has been found thus
                             ;; far, or from scratch.
                             (if (search-contiguous pattern text :found-pattern seen :test test)
                                 (search-contiguous pattern text :found-pattern seen :test test)
                                 (search-contiguous pattern text :test test))
                           (setf seen patt)
                           (cond ((str:empty? seen)
                                  (setf partial-match nil))
                                 ((null partial-match)
                                  (setf partial-match
                                        (make-instance
                                         'search-match
                                         :pattern pattern
                                         :buffer buffer
                                         :nodes (list child)
                                         :identifier-beg (nyxt/dom:get-nyxt-id node)
                                         :node-index-beg index
                                         :text-index-beg (first bounds))))
                                 ((not (funcall test seen pattern))
                                  (setf (nodes partial-match)
                                        (append (nodes partial-match) (list child))))
                                 (t
                                  (setf (nodes partial-match)
                                        (append (nodes partial-match) (list child))
                                        (body partial-match)
                                        (apply #'concatenate
                                               'string
                                               (mapcar #'plump:text (nodes partial-match)))
                                        (id partial-match)
                                        (incf idx)
                                        (identifier-end partial-match)
                                        (nyxt/dom:get-nyxt-id node)
                                        (node-index-end partial-match)
                                        index
                                        (text-index-end partial-match)
                                        (second bounds))
                                  (push partial-match matches)
                                  (setf partial-match nil
                                        seen nil))))))))))
      (traverse-dfs node))
    ;; Search marks logic.
    (cond ((null mark-p)
           (setf matches (nreverse matches)))
          ((integerp mark-p)
           (setf matches (nreverse matches))
           (loop for match in (nreverse (sera:firstn mark-p matches))
                 do (setf (marked-p match) t))
           matches)
          (t
           (loop for match in matches
                 do (setf (marked-p match) t))
           (setf matches (nreverse matches))))))

(define-command remove-search-marks (&optional (buffer (current-buffer)))
  "Remove all search marks."
  (ps-eval :buffer buffer
    (dolist (match (nyxt/ps:qsa (ps:@ document body) "span[nyxt-search-mark]"))
      (let ((parent (ps:chain match parent-element)))
        (ps:chain match (insert-adjacent-h-t-m-l "beforebegin"
                                                 (ps:@ match inner-h-t-m-l)))
        (ps:chain match (remove))
        ;; Ensure text nodes aren't empty and adjacent ones are concatenated.
        (ps:chain parent (normalize))))))

(defun maybe-remove-search-marks (&optional (buffer (current-buffer)))
  (unless (keep-search-marks-p buffer) (remove-search-marks buffer)))

(define-class search-buffer-source (prompter:source)
  ((prompter:name "Matches")
   (buffer (current-buffer))
   (test-function
    nil
    :type (or null function)
    :documentation "The function that determines whether a search match is found.

When nil, the logic behind `smart-case-test' is applied, i.e. the search becomes
case sensitive if upper case characters are used in the query.

Set it to perform case-insensitive queries only:
\(define-configuration nyxt/mode/search-buffer:search-buffer-source
  ((nyxt/mode/search-buffer:test-function #'string-equal)))")
   (maximum-marked-matches
    1000
    :type integer
    :documentation "Maximum number of marked search matches.
The possible values are:
- NIL, to disable marking matches;
- T, to mark all matches;
- INTEGER, to mark those many matches.

Note that it doesn't set an upper bound on the number of matches returned by
`search-document'.  It only limits the number of marks added by the web
renderer, as it is an expensive computation.")
   (initial-delay
    0.25
    :documentation "Seconds to wait before searching.
Takes effect when the search pattern's length is less than `no-delay-length'.")
   (no-delay-length
    3
    :documentation "Search starts immediately for patterns at least this long.
For shorter search patterns, `initial-delay' applies.")
   (prompter:actions-on-current-suggestion-enabled-p t)
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions))
      (let ((buffer (buffer source)))
        (remove-search-marks buffer)
        (unless (str:empty? input)
          (when (< (length input) (no-delay-length source))
            ;; Allow time for next keystroke to avoid long computations (see
            ;; `prompter::update-thread').
            (sleep (initial-delay source)))
          (search-document input
                           :buffer buffer
                           :node (elt (clss:select "body" (document-model buffer)) 0)
                           :test (or (test-function source) (smart-case-test input))
                           :mark-p (maximum-marked-matches source))))))
   (prompter:actions-on-current-suggestion
    (lambda-command mark-match (suggestion)
      "Scroll to search match."
      (set-current-buffer (buffer suggestion) :focus nil)
      (maybe-update-marks suggestion (current-source))
      (when (invisible-p suggestion)
        (setf (slot-value (current-source) 'prompter:suggestions)
              (delete suggestion
                      (slot-value (current-source) 'prompter:suggestions)
                      :key #'prompter:value))
        ;; FIXME Hack that enables the above deletion to cascade.
        ;; See https://github.com/atlas-engineer/nyxt/issues/2894.
        (prompter:run-action-on-current-suggestion (current-prompt-buffer)))
      (mark-alternate suggestion)))
   (prompter:actions-on-return
    (lambda-command maybe-remove-search-marks (marks)
      (let ((match (first marks)))
        (maybe-remove-search-marks (buffer match))
        match)))
   (prompter:constructor
    (lambda (source) (add-stylesheet "nyxt-search-stylesheet"
                                     (style (find-submode 'search-buffer-mode))
                                     (buffer source))))
   (prompter:destructor
    (lambda (prompter source)
      (declare (ignore source))
      (let ((search-buffers (mapcar #'buffer (prompter:sources prompter))))
        (mapcar #'maybe-remove-search-marks search-buffers)))))
  (:export-accessor-names-p t)
  (:export-class-name-p t)
  (:metaclass user-class))

(defmethod maybe-update-marks (current-match (source search-buffer-source))
  "Recompute search marks, if needed.

At any given time, only `maximum-marked-matches' matches are marked."
  (unless (marked-p current-match)
    (let* ((matches (mapcar #'prompter:value (prompter:suggestions source)))
           (match-batch (find current-match
                              (sera:batches matches (maximum-marked-matches source))
                              :test #'member)))
      (remove-search-marks (buffer source))
      (mapcar (lambda (match) (setf (marked-p match) nil)) matches)
      (mapcar (lambda (match) (setf (marked-p match) t)) (nreverse match-batch)))))

(define-command search-buffer ()
  "Search incrementally on the current buffer.
To remove the search marks when closing the search prompt, set DOCUMENT-BUFFER's
`keep-search-marks-p' slot to nil by adding the following to the config file:

  (define-configuration document-buffer
    ((keep-search-marks-p nil)))"
  (prompt :prompt "Search text"
          :sources (make-instance 'search-buffer-source)))

(define-command search-buffers ()
  "Search incrementally in multiple buffers."
  (let ((buffers (prompt :prompt "Search in buffer(s)"
                         :sources (make-instance 'buffer-source
                                                 :actions-on-return #'identity))))
    (prompt :prompt "Search text"
            :sources (mapcar (lambda (buffer)
                               (make-instance
                                'search-buffer-source
                                :name (format nil "Matches from ~a" (url buffer))
                                :buffer buffer))
                             buffers))))
