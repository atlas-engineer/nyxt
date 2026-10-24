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
   (hinting-type
    :emacs
    :type (member :emacs :vi)
    :documentation "Set the hinting mechanism.
Hints are drawn for the visible viewport in both cases. The prompt lists those
links immediately; their URLs are filled in afterwards.
In :emacs, the usual `prompt-buffer' facilities are available.
In :vi, the `prompt-buffer' is collapsed to the input area and a hint is
followed when the input matches its hint string.")
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
Hints have mixed lengths and none is a prefix of another, so a finished hint
selects one link. Characters earlier in the alphabet are used more often.")
   (hints-selector
    "a, button, input, textarea, details, select"
    :type string
    :documentation "The elements to be hinted.
The hints-selector syntax is that of CLSS, and broadly, that of CSS. Use it to
define which elements are picked up by element hinting.

For instance, to include images:

    a, button, input, textarea, details, select, img:not([alt=\"\"])")
   (x-translation
    0
    :type integer
    :documentation "The horizontal translation as a percentage of the hint's size.
A positive value shifts to the right.")
   (y-translation
    0
    :type integer
    :documentation "The vertical translation as a percentage of the hint's size.
A positive value shifts to the bottom.")
   (x-placement
    :left
    :type (member :left :right)
    :documentation "The horizontal placement of the hints: either `:left' or `:right'.")
   (keyscheme-map
    (define-keyscheme-map "hint-mode" ()
      keyscheme:cua
      (list
       "C-j" 'follow-hint
       "C-J" 'follow-hint-new-buffer
       "C-u C-j" 'follow-hint-new-buffer-focus
       "M-c h" 'copy-hint-url)
      keyscheme:emacs
      (list
       "M-g" 'follow-hint
       "M-G" 'follow-hint-new-buffer
       "C-u M-g" 'follow-hint-new-buffer-focus
       "C-x C-w" 'copy-hint-url)
      keyscheme:vi-normal
      (list
       "f" 'follow-hint
       "; f" 'follow-hint-new-buffer
       "F" 'follow-hint-new-buffer-focus)))))

(defmethod style ((mode hint-mode))
  "The style of the hint overlays.

Badges keep the theme colors. The type is a fixed size, bold, and drawn on an
opaque badge with a halo so the letters stay readable on any page."
  (let* ((theme (theme *browser*))
         (background (theme:background-color- theme))
         (preferred (theme:on-background-color theme))
         (foreground (if (and background preferred
                              (>= (theme:contrast-ratio preferred background) 4.5))
                         preferred
                         (theme:contrasting-color background))))
    (theme:themed-css theme
      `(".nyxt-hint"
        :display "inline-block"
        :background-color ,background
        :color ,foreground
        :font-family ,theme:monospace-font-family
        :font-size "13px"
        :font-weight "bold"
        :font-variant-ligatures "none"
        :letter-spacing "0.04em"
        :line-height "1.25"
        :white-space "nowrap"
        :text-shadow ,(format nil "0 0 1px ~a" background)
        :box-shadow ,(format nil "0 0 0 1px ~a, 0 2px 5px rgba(0, 0, 0, 0.45)" background)
        :transform ,(format nil "translate(~a%,~a%)"
                            (+ (x-translation mode)
                               (if (eq (x-placement mode) :right) -100 0))
                            (y-translation mode))
        :padding "1px 4px 0px 4px"
        :border-color ,theme:primary-color+
        :border-radius "4px"
        :border-width "2px"
        :border-style "solid"
        :opacity "1"
        :pointer-events "none"
        :user-select "none"
        :z-index #.(1- (expt 2 31)))
      `(".nyxt-hint span"
        :color "inherit"
        :font-weight "inherit"
        :pointer-events "none")
      `(".nyxt-hint.nyxt-mark-hint"
        :background-color ,theme:secondary-color
        :color ,theme:on-secondary-color
        :font-weight "bold")
      `(".nyxt-hint.nyxt-current-hint"
        :background-color ,theme:action-color
        :color ,theme:on-action-color)
      '(".nyxt-hint span.nyxt-match-hint"
        :opacity "0.4")
      `(".nyxt-element-hint"
        :background-color ,theme:action-color))))

(define-configuration document-buffer
  ((default-modes (cons 'hint-mode %slot-value%))))

(-> select-from-alphabet (t alex:non-negative-integer string) (values string &optional))
(defun select-from-alphabet (code subsequence-length alphabet)
  (let ((exponents (nreverse (loop for pow below subsequence-length
                                   collect (expt (length alphabet) pow)))))
    (coerce (loop for exp in exponents
                  for quotient = (floor (/ code exp))
                  collect (aref alphabet quotient)
                  do (decf code (* quotient exp)))
            'string)))

(-> generate-hint-strings (alex:non-negative-integer string) (list-of string))
(defun generate-hint-strings (length alphabet)
  "Return LENGTH hint strings drawn from ALPHABET.

Hints may differ in length. None is a prefix of another, so typing a finished
hint — including a two-character one — identifies a single link. Shorter hints
are spread through the list. This is Vimium's alphabet-hint assignment."
  (cond
    ((or (zerop length) (zerop (length alphabet))) '())
    ((<= (length alphabet) 1)
     (loop for i from 1 to length
           collect (select-from-alphabet 0 i alphabet)))
    (t
     (let ((hints (make-array 1 :adjustable t :fill-pointer 0))
           (offset 0))
       (vector-push-extend "" hints)
       (loop while (or (< (- (fill-pointer hints) offset) length)
                       (= (fill-pointer hints) 1))
             do (let ((hint (aref hints offset)))
                  (incf offset)
                  (loop for character across alphabet
                        do (vector-push-extend
                            (uiop:strcat (string character) hint)
                            hints))))
       (mapcar #'reverse
               (sort (loop for index from offset below (+ offset length)
                           collect (aref hints index))
                     #'string<))))))

(-> generate-hints (alex:non-negative-integer) (list-of string))
(defun generate-hints (length)
  (generate-hint-strings length (hints-alphabet (find-submode 'hint-mode))))

(defun hint-record-field (record key)
  "Return the value of KEY in a hint record alist from the renderer."
  (cdr (or (assoc key record :test #'eq)
           (assoc key record :test #'string-equal))))

(defun normalize-hint-records (raw)
  "Return RAW as a list of hint-record alists."
  (cond
    ((null raw) '())
    ((and (consp (first raw))
          (consp (first (first raw)))
          (symbolp (car (first (first raw)))))
     raw)
    ((and (consp (first raw))
          (symbolp (car (first raw))))
     (list raw))
    (t '())))

(defun make-hint-element (record)
  "Build a plump element for RECORD, enough to list the hint immediately."
  (let* ((root (plump:make-root))
         (tag (string-downcase (or (hint-record-field record :tag) "a")))
         (element (plump:make-element root tag)))
    (plump:set-attribute element "nyxt-identifier" (or (hint-record-field record :id) ""))
    (plump:set-attribute element "nyxt-hint" (or (hint-record-field record :hint) ""))
    (when-let ((type (hint-record-field record :type)))
      (unless (str:empty? type)
        (plump:set-attribute element "type" type)))
    (nyxt/dom::name-dom-elements element)
    element))

(defun apply-hint-details (element record)
  "Fill ELEMENT with the URL, text, and option payload in RECORD."
  (flet ((present (key)
           (let ((value (hint-record-field record key)))
             (when (and (stringp value) (not (str:empty? value)))
               value))))
    (when-let ((href (present :href)))
      (plump:set-attribute element "href" href))
    (when-let ((src (present :src)))
      (plump:set-attribute element "src" src))
    (when-let ((text (present :text)))
      (cond
        ((typep element 'nyxt/dom:input-element)
         (unless (string-equal "password" (plump:attribute element "type"))
           (plump:set-attribute element "value" text)))
        ((typep element 'nyxt/dom:textarea-element)
         (plump:set-attribute element "value" text))
        ((typep element 'nyxt/dom:img-element)
         (plump:set-attribute element "alt" text))
        ((typep element 'nyxt/dom:details-element)
         (let ((summary (plump:make-element element "summary")))
           (plump:make-text-node summary text)))
        ((typep element 'nyxt/dom:select-element)
         (dolist (option-record (normalize-hint-records (hint-record-field record :options)))
           (let ((option (plump:make-element element "option")))
             (plump:set-attribute option "nyxt-identifier"
                                  (or (hint-record-field option-record :id) ""))
             (when-let ((value (hint-record-field option-record :value)))
               (plump:set-attribute option "value" value))
             (when (hint-record-field option-record :selected)
               (plump:set-attribute option "selected" "selected"))
             (when-let ((option-text (hint-record-field option-record :text)))
               (plump:make-text-node option option-text))))
         (nyxt/dom::name-dom-elements element))
        (t (plump:make-text-node element text)))))
  element)

(defun show-viewport-hints (buffer selector alphabet css x-placement show-scope-p)
  "Draw hint badges for SELECTOR elements in BUFFER's viewport.
Return a list of records (id, tag, hint, type). URLs are not collected here."
  (normalize-hint-records
   (ps-eval :buffer buffer
     (defun nyxt-hint-next-id ()
       (when (= (ps:typeof (ps:@ window nyxt-next-hint-id)) "undefined")
         (setf (ps:@ window nyxt-next-hint-id) 0)
         (labels ((consider (root)
                    (ps:dolist (node (ps:chain root (query-selector-all "[nyxt-identifier]")))
                      (let ((n (parse-int (ps:chain node (get-attribute "nyxt-identifier")) 10)))
                        (when (and (not (ps:chain *number (is-na-n n)))
                                   (>= n (ps:@ window nyxt-next-hint-id)))
                          (setf (ps:@ window nyxt-next-hint-id) (1+ n)))))
                    (ps:dolist (node (ps:chain root (query-selector-all "*")))
                      (when (ps:@ node shadow-root)
                        (consider (ps:@ node shadow-root))))))
           (consider document)))
       (let ((id (ps:@ window nyxt-next-hint-id)))
         (setf (ps:@ window nyxt-next-hint-id) (1+ id))
         (ps:stringify id)))
     (defun nyxt-hint-ensure-id (element)
       (unless (ps:chain element (has-attribute "nyxt-identifier"))
         (ps:chain element (set-attribute "nyxt-identifier" (nyxt-hint-next-id))))
       (ps:chain element (get-attribute "nyxt-identifier")))
     ;; Keep this assignment in step with `generate-hint-strings'.
     (defun nyxt-hint-strings (link-count alphabet)
       (cond
         ((or (<= link-count 0) (<= (length alphabet) 0))
          (array))
         ((<= (length alphabet) 1)
          (let ((hints (array)))
            (loop for i from 1 to link-count
                  do (ps:chain hints (push (ps:chain (aref alphabet 0) (repeat i)))))
            hints))
         (t
          (let ((hints (array ""))
                (offset 0))
            (loop while (or (< (- (length hints) offset) link-count)
                            (= (length hints) 1))
                  do (let ((hint (aref hints offset)))
                       (incf offset)
                       (loop for index from 0 below (length alphabet)
                             do (ps:chain hints (push (+ (aref alphabet index) hint))))))
            (ps:chain (ps:chain hints (slice offset (+ offset link-count)))
                      (sort)
                      (map (lambda (hint-string)
                             (ps:chain hint-string (split "") (reverse) (join "")))))))))
     (defun nyxt-hint-hits-p (element x y)
       (let ((hit (ps:chain document (element-from-point x y))))
         (loop while (and hit (not (eq hit element)))
               do (setf hit (if (ps:instanceof (ps:@ hit parent-node)
                                               (ps:@ window |ShadowRoot|))
                                (ps:@ hit parent-node host)
                                (ps:@ hit parent-node))))
         (eq hit element)))
     (defun nyxt-hint-visible-anchor (element right-align-p)
       (let ((computed (ps:chain window (get-computed-style element))))
         (when (or (= (ps:getprop computed "visibility") "hidden")
                   (= (ps:@ computed display) "none")
                   (<= (parse-float (ps:@ computed opacity)) 0)
                   (and (= (ps:@ element tag-name) "INPUT")
                        (= (ps:chain (or (ps:@ element type) "") (to-lower-case)) "hidden")))
           (return-from nyxt-hint-visible-anchor nil))
         (let ((rects (ps:chain element (get-client-rects))))
           (loop for rect-index from 0 below (ps:@ rects length)
                 for raw = (aref rects rect-index)
                 for left = (ps:chain *math (max (ps:@ raw left) 0))
                 for top = (ps:chain *math (max (ps:@ raw top) 0))
                 for right = (ps:chain *math (min (ps:@ raw right) (ps:@ window inner-width)))
                 for bottom = (ps:chain *math (min (ps:@ raw bottom) (ps:@ window inner-height)))
                 do (when (and (>= (- right left) 3)
                               (>= (- bottom top) 3)
                               (< top (- (ps:@ window inner-height) 4))
                               (< left (- (ps:@ window inner-width) 4))
                               (or (nyxt-hint-hits-p element (+ left 0.1) (+ top 0.1))
                                   (nyxt-hint-hits-p element (- right 0.1) (+ top 0.1))
                                   (nyxt-hint-hits-p element (+ left 0.1) (- bottom 0.1))
                                   (nyxt-hint-hits-p element (- right 0.1) (- bottom 0.1))
                                   (nyxt-hint-hits-p element
                                                     (/ (+ left right) 2)
                                                     (/ (+ top bottom) 2))))
                      (return-from nyxt-hint-visible-anchor
                        (ps:create :left (if right-align-p right left)
                                   :top top)))))))
     (defun nyxt-hint-place (anchor hint right-align-p)
       (let* ((estimated (+ (* (length hint) 11) 10))
              (left (ps:@ anchor left))
              (top (ps:chain *math (min (ps:chain *math (max (ps:@ anchor top) 0))
                                        (- (ps:@ window inner-height) 18)))))
         (if right-align-p
             (progn
               (when (< (- left estimated) 2)
                 (setf left (+ estimated 2)))
               (when (> left (- (ps:@ window inner-width) 2))
                 (setf left (- (ps:@ window inner-width) 2))))
             (when (> (+ left estimated) (- (ps:@ window inner-width) 2))
               (setf left (ps:chain *math (max 2 (- (ps:@ window inner-width) estimated 2))))))
         (ps:create :left left :top top)))
     (defun nyxt-hint-layout ()
       "Move each badge to its element's current viewport position.
Scrolls inside the page and the page itself both change that position."
       (let ((host (nyxt/ps:qs-id document "nyxt-hints")))
         (when (and host (ps:@ host shadow-root))
           (ps:dolist (badge (nyxt/ps:qsa (ps:@ host shadow-root) ".nyxt-hint"))
             (let ((element (ps:@ badge nyxt-element)))
               (if (or (not element) (not (ps:@ element is-connected)))
                   (setf (ps:@ badge style display) "none")
                   (let ((anchor (nyxt-hint-visible-anchor
                                  element (ps:@ badge nyxt-right-align))))
                     (if anchor
                         (let ((place (nyxt-hint-place anchor
                                                       (ps:@ badge nyxt-hint)
                                                       (ps:@ badge nyxt-right-align))))
                           (setf (ps:@ badge style display) ""
                                 (ps:@ badge style top) (+ (ps:@ place top) "px")
                                 (ps:@ badge style left) (+ (ps:@ place left) "px")))
                         (setf (ps:@ badge style display) "none")))))))))
     (defun nyxt-hint-on-scroll ()
       (unless (ps:@ window nyxt-hint-scroll-frame)
         (setf (ps:@ window nyxt-hint-scroll-frame)
               (ps:chain window
                         (request-animation-frame
                          (lambda ()
                            (setf (ps:@ window nyxt-hint-scroll-frame) 0)
                            (nyxt-hint-layout)))))))
     (let* ((selector (ps:lisp selector))
            (alphabet (ps:lisp alphabet))
            (css (ps:lisp css))
            (right-align-p (ps:lisp (eq x-placement :right)))
            (show-scope-p (ps:lisp show-scope-p))
            (old (nyxt/ps:qs-id document "nyxt-hints"))
            (candidates (array))
            (hints-parent (ps:chain document (create-element "div")))
            (shadow (ps:chain hints-parent (attach-shadow (ps:create mode "open"))))
            (style (ps:new (|CSSStyleSheet|))))
       (when old
         (ps:chain old (remove)))
       (ps:dolist (element (nyxt/ps:rqsa document selector))
         (let ((anchor (nyxt-hint-visible-anchor element right-align-p)))
           (when anchor
             (ps:chain candidates (push (ps:create :element element :anchor anchor))))))
       (let ((hint-strings (nyxt-hint-strings (length candidates) alphabet))
             (records (array)))
         (loop for index from 0 below (ps:chain *math (min (length candidates) (length hint-strings)))
               for candidate = (aref candidates index)
               for hint = (aref hint-strings index)
               for element = (ps:@ candidate element)
               for badge = (ps:chain document (create-element "span"))
               do (nyxt-hint-ensure-id element)
               do (ps:chain element (set-attribute "nyxt-hintable" ""))
               do (ps:chain element (set-attribute "nyxt-hint" hint))
               do (when show-scope-p
                    (ps:chain element class-list (add "nyxt-element-hint")))
               do (setf (ps:@ badge class-name) "nyxt-hint"
                        (ps:@ badge id) (+ "nyxt-hint-" hint)
                        (ps:@ badge nyxt-element) element
                        (ps:@ badge nyxt-hint) hint
                        (ps:@ badge nyxt-right-align) right-align-p
                        (ps:@ badge style position) "absolute")
               do (loop for char-index from 0 below (length hint)
                        for glyph = (ps:chain document (create-element "span"))
                        do (setf (ps:@ glyph text-content) (aref hint char-index))
                        do (ps:chain badge (append-child glyph)))
               do (ps:chain shadow (append-child badge))
               do (ps:chain records
                            (push (ps:create :id (ps:chain element (get-attribute "nyxt-identifier"))
                                             :tag (ps:@ element tag-name)
                                             :hint hint
                                             :type (or (ps:chain element (get-attribute "type")) "")))))
         (ps:chain style (replace-sync css))
         (setf (ps:chain shadow adopted-style-sheets) (array style))
         (setf (ps:@ hints-parent id) "nyxt-hints")
         (ps:chain hints-parent style (set-property "position" "fixed" "important"))
         (ps:chain hints-parent style (set-property "inset" "0" "important"))
         (ps:chain hints-parent style (set-property "width" "100%" "important"))
         (ps:chain hints-parent style (set-property "height" "100%" "important"))
         (ps:chain hints-parent style (set-property "margin" "0" "important"))
         (ps:chain hints-parent style (set-property "padding" "0" "important"))
         (ps:chain hints-parent style (set-property "border" "none" "important"))
         (ps:chain hints-parent style (set-property "background" "transparent" "important"))
         (ps:chain hints-parent style (set-property "overflow" "visible" "important"))
         (ps:chain hints-parent style (set-property "pointer-events" "none" "important"))
         (ps:chain hints-parent style (set-property "z-index" "2147483647" "important"))
         ;; A child of body keeps hint coordinates aligned when the page is zoomed.
         (ps:chain document body (append-child hints-parent))
         ;; The top layer keeps the badges above page stacking contexts.
         (when (ps:@ hints-parent show-popover)
           (setf (ps:@ hints-parent popover) "manual")
           (ps:chain hints-parent (show-popover)))
         ;; The overlay is fixed to the viewport, so a scroll would leave the
         ;; badges behind their links. Follow the elements instead.
         (setf (ps:@ window nyxt-hint-scroll-handler) nyxt-hint-on-scroll)
         (unless (ps:@ window nyxt-hint-scroll-bound)
           (setf (ps:@ window nyxt-hint-scroll-bound) t)
           (ps:chain window (add-event-listener "scroll"
                                                (lambda ()
                                                  (when (ps:@ window nyxt-hint-scroll-handler)
                                                    (ps:chain window (nyxt-hint-scroll-handler))))
                                                t))
           (ps:chain window (add-event-listener "resize"
                                                (lambda ()
                                                  (when (ps:@ window nyxt-hint-scroll-handler)
                                                    (ps:chain window (nyxt-hint-scroll-handler))))
                                                t))
           (when (ps:@ window visual-viewport)
             (ps:chain window visual-viewport
                       (add-event-listener "scroll"
                                           (lambda ()
                                             (when (ps:@ window nyxt-hint-scroll-handler)
                                               (ps:chain window (nyxt-hint-scroll-handler))))))
             (ps:chain window visual-viewport
                       (add-event-listener "resize"
                                           (lambda ()
                                             (when (ps:@ window nyxt-hint-scroll-handler)
                                               (ps:chain window (nyxt-hint-scroll-handler))))))))
         (nyxt-hint-layout)
         records)))))

(defun fetch-hint-details (buffer elements)
  "Return URL and text records for ELEMENTS. Runs in BUFFER."
  (let ((ids (mapcar #'nyxt/dom:get-nyxt-id elements)))
    (normalize-hint-records
     (ps-eval :buffer buffer
       (defun nyxt-hint-text (element)
         (let ((tag (ps:@ element tag-name))
               (text ""))
           (cond
             ((= tag "INPUT")
              (setf text
                    (if (= (ps:chain (or (ps:@ element type) "") (to-lower-case)) "password")
                        ""
                        (or (and (ps:@ element labels)
                                 (< 0 (ps:@ element labels length))
                                 (ps:@ element labels 0 text-content))
                            (ps:@ element value)
                            (ps:@ element placeholder)
                            ""))))
             ((= tag "IMG")
              (setf text (or (ps:@ element alt) (ps:@ element title) "")))
             ((= tag "SELECT")
              (setf text
                    (or (and (ps:@ element selected-options)
                             (< 0 (ps:@ element selected-options length))
                             (ps:@ element selected-options 0 text-content))
                        "")))
             (t (setf text (or (ps:@ element inner-text) (ps:@ element text-content) ""))))
           (ps:chain (ps:chain (ps:chain (or text "")
                                         (replace (ps:new (|RegExp| "\\s+" "g")) " "))
                               (trim))
                     (slice 0 200))))
       (let ((ids (ps:lisp (list 'quote ids)))
             (records (array)))
         (dolist (id ids)
           (let ((element (nyxt/ps:rqs-nyxt-id document id)))
             (when element
               (let ((tag (ps:@ element tag-name))
                     (options (array))
                     (href-attribute (ps:chain element (get-attribute "href"))))
                 (when (= tag "SELECT")
                   (dolist (option (ps:chain *array (from (ps:@ element options))))
                     (unless (ps:chain option (has-attribute "nyxt-identifier"))
                       (when (= (ps:typeof (ps:@ window nyxt-next-hint-id)) "undefined")
                         (setf (ps:@ window nyxt-next-hint-id) 0))
                       (let ((next (ps:@ window nyxt-next-hint-id)))
                         (setf (ps:@ window nyxt-next-hint-id) (1+ next))
                         (ps:chain option (set-attribute "nyxt-identifier" (ps:stringify next)))))
                     (ps:chain options (push (ps:create :id (ps:chain option (get-attribute "nyxt-identifier"))
                                                        :text (or (ps:@ option text-content) "")
                                                        :value (or (ps:@ option value) "")
                                                        :selected (if (ps:@ option selected) t f))))))
                 (ps:chain records (push (ps:create :id id
                                                    :href (if href-attribute
                                                              (or (ps:@ element href) href-attribute)
                                                              "")
                                                    :src (if (= tag "IMG")
                                                             (or (ps:@ element current-src) (ps:@ element src) "")
                                                             "")
                                                    :text (nyxt-hint-text element)
                                                    :options options)))))))
         records)))))

(defun load-hint-details (elements buffer source)
  "Fill ELEMENTS' URLs and text, then refresh SOURCE's prompt listing."
  (unless elements
    (return-from load-hint-details nil))
  (let ((browser *browser*))
    (run-thread "hint-urls"
      (let ((*browser* browser))
        (with-protect ("Hint URL load failed: ~a" :condition)
          (let ((records (fetch-hint-details buffer elements)))
            (dolist (element elements)
              (when-let ((record (find (nyxt/dom:get-nyxt-id element)
                                       records
                                       :key (lambda (record)
                                              (hint-record-field record :id))
                                       :test #'string=)))
                (apply-hint-details element record))))
          (loop for attempt below 50
                for prompt = (current-prompt-buffer)
                do (when (and prompt
                              (find source (prompter:sources prompt))
                              (find prompt (nyxt::active-prompt-buffers (window prompt))))
                     (prompter:refresh prompt)
                     (prompt-render-suggestions prompt)
                     (return))
                   (sleep 0.02)))))))

(defun add-hints (&key selector (buffer (current-buffer)) source css alphabet
                    (x-placement :left) show-hint-scope-p)
  "Hint the visible elements matching SELECTOR and return plump elements for them.
Badges are drawn before this function returns. When SOURCE is given, URLs and
link text are loaded on a background thread and the prompt is refreshed."
  (let* ((records (show-viewport-hints buffer selector alphabet css x-placement show-hint-scope-p))
         (elements (mapcar #'make-hint-element records)))
    (when source
      (load-hint-details elements buffer source))
    elements))

(define-parenscript remove-hintable-attribute ()
  (ps:dolist (element (nyxt/ps:rqsa document "[nyxt-hintable]"))
    (ps:chain element (remove-attribute "nyxt-hintable"))
    (ps:chain element (remove-attribute "nyxt-hint"))))

(define-parenscript-async remove-hint-elements ()
  (ps:let ((hints-parent (nyxt/ps:qs-id document "nyxt-hints")))
    (when hints-parent
      (when (ps:@ hints-parent hide-popover)
        (ps:chain hints-parent (hide-popover)))
      (ps:chain hints-parent (remove))))
  (ps:dolist (element (nyxt/ps:rqsa document ".nyxt-element-hint"))
    (ps:chain element class-list (remove "nyxt-element-hint"))))

(defun remove-hints (&key (buffer (current-buffer)))
  (declare (ignore buffer))
  (remove-hint-elements)
  (remove-hintable-attribute))

(export-always 'identifier)
(defmethod identifier ((element plump:element))
  "ELEMENT's on-page identifier (constructed from `hint-alphabet' characters.)"
  (plump:attribute element "nyxt-hint"))

(export-always 'highlight-current-hint)
(define-parenscript highlight-current-hint (&key element scroll)
  "Accent the hint for the ELEMENT to be distinguishable from other hints.
If SCROLL (default to NIL), scroll the hint into view."
  (let* ((shadow (ps:@ (nyxt/ps:qs document "#nyxt-hints") shadow-root))
         (%element (nyxt/ps:qs shadow
                               (ps:lisp (str:concat "#nyxt-hint-" (identifier element))))))
    (when %element
      (unless (ps:chain %element class-list (contains "nyxt-current-hint"))
        ;; There should be, at most, a unique element with the
        ;; "nyxt-current-hint" class.
        ;; querySelectAll, unlike querySelect, handles the case when none are
        ;; found.
        (ps:dolist (current-hint (nyxt/ps:qsa shadow ".nyxt-current-hint"))
          (ps:chain current-hint class-list (remove "nyxt-current-hint"))))
      (ps:chain %element class-list (add "nyxt-current-hint"))
      (when (ps:lisp scroll)
        (ps:chain %element (scroll-into-view (ps:create block "center")))))))

(define-parenscript-async set-hint-visibility (hint state)
  "Set visibility STATE of HINT element.

Consult https://developer.mozilla.org/en-US/docs/Web/CSS/visibility."
  (let* ((shadow (ps:@ (nyxt/ps:qs document "#nyxt-hints") shadow-root))
         (el (nyxt/ps:qs shadow (ps:lisp (str:concat "#nyxt-hint-" (identifier hint))))))
    (when el (setf (ps:@ el style "visibility") (ps:lisp state)))))

(define-parenscript-async dim-hint-prefix (hint prefix-length)
  "Dim the first PREFIX-LENGTH characters of HINT element.
The badge itself stays opaque so the remaining characters stay readable."
  (let* ((shadow (ps:@ (nyxt/ps:qs document "#nyxt-hints") shadow-root))
         (el (nyxt/ps:qs shadow (ps:lisp (str:concat "#nyxt-hint-" (identifier hint))))))
    (when el
      (let ((spans (ps:chain el (query-selector-all "span"))))
        (loop for index from 0 below (ps:@ spans length)
              for span = (aref spans index)
              do (if (< index (ps:lisp prefix-length))
                     (ps:chain span class-list (add "nyxt-match-hint"))
                     (ps:chain span class-list (remove "nyxt-match-hint"))))))))

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
      (flet ((hint-string (suggestion)
               (or (plump:attribute (prompter:value suggestion) "nyxt-hint") "")))
        (multiple-value-bind (exact rest)
            (sera:partition
             (lambda (suggestion)
               (and (not (str:empty? input))
                    (string-equal input (hint-string suggestion))))
             suggestions)
          ;; A finished two-character hint is one link. Follow it instead of
          ;; keeping longer or fuzzy matches ahead of it.
          (if (and (= 2 (length input)) (sera:single exact))
              (progn
                (when-let ((prompt (current-prompt-buffer)))
                  (setf (prompter:auto-return-p prompt) t))
                exact)
              (multiple-value-bind (matching-hints other-hints)
                  (sera:partition
                   (lambda (suggestion)
                     (str:starts-with-p input (hint-string suggestion) :ignore-case t))
                   rest)
                (append exact matching-hints other-hints)))))))
   (prompter:actions-on-current-suggestion
    (when (eq :emacs (hinting-type (find-submode 'hint-mode)))
      (lambda-command highlight-current-hint* (suggestion)
        "Highlight hint."
        (highlight-current-hint :element suggestion
                                :scroll nil))))
   (prompter:actions-on-marks
    (lambda (marks)
      (let ((%marks (mapcar (lambda (mark) (str:concat "#nyxt-hint-" (identifier mark)))
                            marks)))
        (ps-eval
          (let ((shadow (ps:@ (nyxt/ps:qs document "#nyxt-hints") shadow-root)))
            (dolist (marked (nyxt/ps:qsa shadow ".nyxt-mark-hint"))
              (ps:chain marked class-list (remove "nyxt-mark-hint")))
            (dolist (mark (ps:lisp (list 'quote %marks)))
              (ps:chain (nyxt/ps:qs shadow mark) class-list (add "nyxt-mark-hint"))))))))
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
  (when-let*
      ((buffer (current-buffer))
       (mode (find-submode 'hint-mode buffer))
       (result (prompt
                :prompt prompt
                ;; TODO: No need to find the symbol if we move this code (and
                ;; the rest) to the hint-mode package.
                :extra-modes (list (sym:resolve-symbol :hint-prompt-buffer-mode :mode))
                :auto-return-p (eq :vi (hinting-type mode))
                :history nil
                :height (if (eq :vi (hinting-type mode))
                            :fit-to-prompt
                            :default)
                :hide-suggestion-count-p (eq :vi (hinting-type mode))
                :sources (make-instance 'hint-source
                                        :enable-marks-p enable-marks-p
                                        :constructor
                                        (lambda (source)
                                          (add-hints :selector selector
                                                     :buffer buffer
                                                     :source source
                                                     :css (style mode)
                                                     :alphabet (hints-alphabet mode)
                                                     :x-placement (x-placement mode)
                                                     :show-hint-scope-p (show-hint-scope-p mode))))
                :after-destructor (lambda ()
                                    (with-current-buffer buffer
                                      (remove-hints :buffer buffer))))))
    (funcall function result)))

(defmethod prompter:object-attributes :around ((element plump:element) (source hint-source))
  `(,@(when (plump:attribute element "nyxt-hint")
        `(("Hint" ,(plump:attribute element "nyxt-hint") (:width 1))))
    ;; Ensure that all of Body and URL are there, even if empty.
    ,@(loop with attributes = (call-next-method)
            for attr in '("Body" "URL")
            for (same-attr val) = (assoc attr attributes :test 'string=)
            if same-attr
              collect `(,same-attr ,val (:width 3))
            else collect `(,attr "" (:width 3)))
    ("Type" ,(str:capitalize (str:string-case
                                 (plump:tag-name element)
                               ("a" "link")
                               ("img" "image")
                               (otherwise (plump:tag-name element))))
            (:width 1))))

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
   (and-let* (((plump:has-attribute a "href"))
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
   (and-let* (((plump:has-attribute img "href"))
              (url-string (plump:attribute img "href")))
     `(("URL" ,url-string)))
   (when (nyxt/dom:body img)
     `(("Body" ,(str:shorten 80 (nyxt/dom:body img)))))))

(defmethod %follow-hint ((element plump:element))
  (nyxt/dom:click-element element))

(defun ensure-hint-url (element)
  "Return ELEMENT's URL, fetching it from the page when the prompt row does not have it yet."
  (or (url element)
      (let ((href (ps-eval
                    (let ((el (nyxt/ps:rqs-nyxt-id document
                                                   (ps:lisp (nyxt/dom:get-nyxt-id element)))))
                      (when el
                        (or (and (ps:chain el (get-attribute "href"))
                                 (or (ps:@ el href)
                                     (ps:chain el (get-attribute "href"))))
                            (ps:@ el current-src)
                            (ps:@ el src)
                            ""))))))
        (when (and (stringp href) (not (str:empty? href)))
          (if (typep element 'nyxt/dom:img-element)
              (plump:set-attribute element "src" href)
              (plump:set-attribute element "href" href))
          (url element)))))

(defmethod %follow-hint ((a nyxt/dom:a-element))
  (alex:if-let ((target (ensure-hint-url a)))
    (ffi-buffer-load (current-buffer) target)
    (nyxt/dom:click-element a)))

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
  (when (zerop (length (clss:select "option" select)))
    (when-let ((record (find (nyxt/dom:get-nyxt-id select)
                             (fetch-hint-details (current-buffer) (list select))
                             :key (lambda (record) (hint-record-field record :id))
                             :test #'string=)))
      (apply-hint-details select record)))
  (and-let* ((options (coerce (clss:select "option" select) 'list))
             (values (prompt :prompt "Value to select"
                             :sources (make-instance 'options-source
                                                     :constructor options
                                                     :enable-marks-p
                                                     (plump:attribute select "multiple")))))
    (dolist (option (mapcar (rcurry #'find options :test #'equalp) values))
      (nyxt/dom:select-option-element option select))))

(defmethod %follow-hint-new-buffer-focus ((a nyxt/dom:a-element))
  (make-buffer-focus :url (ensure-hint-url a)))

(defmethod %follow-hint-new-buffer-focus ((element plump:element))
  (%follow-hint element))

(defmethod %follow-hint-new-buffer ((a nyxt/dom:a-element))
  (make-buffer :url (ensure-hint-url a) :load-url-p t))

(defmethod %follow-hint-new-buffer ((element plump:element))
  (%follow-hint element))

(defmethod %copy-hint-url ((a nyxt/dom:a-element))
  (ffi-buffer-copy (current-buffer) (render-url (ensure-hint-url a))))

(defmethod %copy-hint-url ((img nyxt/dom:img-element))
  (ffi-buffer-copy (current-buffer) (render-url (ensure-hint-url img))))

(defmethod %copy-hint-url ((element plump:element))
  (echo "Unsupported operation for <~a> hint: can't copy hint URL."
        (plump:tag-name element)))

(define-command follow-hint ()
  "Follow the top element hint selection in the current buffer."
  (query-hints "Select elements"
               (lambda (results)
                 (%follow-hint (first results))
                 (mapcar #'%follow-hint-new-buffer (rest results)))))

(define-command follow-hint-new-buffer ()
  "Like `follow-hint', but selection is handled in background buffers."
  (query-hints "Select elements"
               (lambda (result)
                 (mapcar #'%follow-hint-new-buffer result))))

(define-command follow-hint-new-buffer-focus ()
  "Like `follow-hint-new-buffer', but switch to the top background buffer."
  (query-hints "Select elements"
               (lambda (result)
                 (%follow-hint-new-buffer-focus (first result))
                 (mapcar #'%follow-hint-new-buffer (rest result)))))

(define-command copy-hint-url ()
  "Save the element hint's URL to the clipboard."
  (query-hints "Select element"
               (lambda (result) (%copy-hint-url (first result)))
               :enable-marks-p nil
               :selector "a"))
