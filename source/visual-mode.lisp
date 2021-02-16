;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/visual-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Visual mode."))
(in-package :nyxt/visual-mode)

(define-mode visual-mode ()
  "Visual mode. For documentation on commands and keybindings, see the manual."
  ((keymap-scheme
    (define-scheme "visual"
      scheme:cua
      (list
       "up" 'backward-line
       "down" 'forward-line
       "left" 'backward-char
       "right" 'forward-char
       "escape" 'visual-mode
       "keypadend" 'end-line
       "space" 'forward-char
       "backspace" 'backward-char
       "keypadhome" 'beginning-line
       "shift-down" 'forward-line-with-selection
       "shift-up" 'backward-line-with-selection
       "shift-left" 'backward-char-with-selection
       "shift-right" 'forward-char-with-selection)
      scheme:emacs
      (list
       "C-h" 'select-paragraph
       "shift-space" 'toggle-mark
       "C-space" 'toggle-mark
       "C-g" 'visual-mode
       "C-f" 'forward-char
       "C-b" 'backward-char
       "M-f" 'forward-word
       "M-b" 'backward-word
       "C-n" 'forward-line
       "C-p" 'backward-line
       "C-a" 'beginning-line
       "C-e" 'end-line
       "M-a" 'backward-sentence
       "M-e" 'forward-sentence)
      ;; vi keybindings only enable use of vim's plain "visual" mode for now
      scheme:vi-normal
      (list
       "h" 'backward-char
       "j" 'forward-line
       "k" 'backward-line
       "l" 'forward-char
       "w" 'forward-word
       "$" 'end-line
       "0" 'beginning-line
       "v" 'toggle-mark
       "C-c" 'visual-mode)))
   (mark-set nil)
   (destructor
    (lambda (mode)
      (make-page-uneditable)
      (unlock-page-keypresses)
      (setf (mark-set mode) nil)))
   (constructor
    (lambda (mode)
      (make-page-editable)
      (block-page-keypresses)
      (select-paragraph)
      ;; imitating visual mode in vim
      (if (equal (keymap-scheme-name (buffer mode)) scheme:vi-normal)
          (setf (mark-set mode) t))))))

(define-parenscript %add-paragraph-hints (&key annotate-visible-only-p)
  (defun qs (context selector)
    "Alias of document.querySelector"
    (ps:chain context (query-selector selector)))

  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))

  (defun qsa-text-nodes ()
    "Gets all text nodes"
    (let ((elements (qsa document "body, body *"))
          child)
      (loop for element in elements
            do (setf child (ps:@ element child-nodes 0))
            if (and (ps:chain element (has-child-nodes))
                    (eql (ps:@ child node-type) 3)
                    (not (ps:chain (array "B" "I" "STRONG" "SUP" "SUB"
                                          "DEL" "S" "STRIKE" "U" "TT"
                                          "OPTION" "A")
                                   (includes (ps:@ element tag-name)))))
              collect element)))


  (defun code-char (n)
    "Alias of String.fromCharCode"
    (ps:chain -string (from-char-code n)))

  (defun add-stylesheet ()
    (unless (qs document "#nyxt-stylesheet")
      (ps:try
       (ps:let* ((style-element (ps:chain document (create-element "style")))
                 (box-style (ps:lisp (nyxt/web-mode::box-style (nyxt/web-mode::current-web-mode))))
                 (highlighted-style (ps:lisp (nyxt/web-mode::highlighted-box-style (nyxt/web-mode::current-web-mode)))))
         (setf (ps:@ style-element id) "nyxt-stylesheet")
         (ps:chain document head (append-child style-element))
         (ps:chain style-element sheet (insert-rule box-style 0))
         (ps:chain style-element sheet (insert-rule highlighted-style 1)))
       (:catch (error)))))

  (defun hint-determine-position (rect)
    "Determines the position of a hint according to the element"
    (ps:create :top  (+ (ps:@ window page-y-offset) (ps:@ rect top))
               :left (+ (ps:@ window page-x-offset) (- (ps:@ rect left) 20))))

  (defun hint-create-element (element hint)
    "Creates a DOM element to be used as a hint"
    (ps:let* ((rect (ps:chain element (get-bounding-client-rect)))
              (position (hint-determine-position rect))
              (element (ps:chain document (create-element "span"))))
      (setf (ps:@ element class-name) "nyxt-hint")
      (setf (ps:@ element style position) "absolute")
      (setf (ps:@ element style left) (+ (ps:@ position left) "px"))
      (setf (ps:@ element style top) (+ (ps:@ position top) "px"))
      (setf (ps:@ element id) (+ "nyxt-hint-" hint))
      (setf (ps:@ element text-content) hint)
      element))

  (defun hint-add (element hint)
    "Adds a hint on a single element. Additionally sets a unique
identifier for every hinted element."
    (ps:chain element (set-attribute "nyxt-identifier" hint))
    (ps:let ((hint-element (hint-create-element element hint)))
      (ps:chain document body (append-child hint-element))))

  (defun element-drawable-p (element)
    (if (or (ps:chain element offset-width)
            (ps:chain element offset-height)
            (ps:chain element (get-client-rects) length))
        t nil))

  (defun element-in-view-port-p (element)
    (ps:let* ((rect (ps:chain element (get-bounding-client-rect))))
      (if (and (>= (ps:chain rect top) 0)
               (>= (ps:chain rect left) 0)
               (<= (ps:chain rect right) (ps:chain window inner-width))
               (<= (ps:chain rect bottom) (ps:chain window inner-height)))
          t nil)))

  (defun object-create (element hint)
    (ps:create "type" "p" "hint" hint "identifier" hint "body" (ps:@ element |innerHTML|)))

  (defun hints-add (elements)
    "Adds hints on elements"
    (ps:let* ((elements-length (length elements))
              (hints (hints-generate elements-length)))
      (ps:chain |json|
                (stringify
                 (loop for i from 0 to (- elements-length 1)
                       when (and (element-drawable-p (elt elements i))
                                 (element-in-view-port-p (elt elements i)))
                         do (hint-add (elt elements i) (elt hints i))
                       when (or (and (element-drawable-p (elt elements i))
                                     (not (ps:lisp annotate-visible-only-p)))
                                (and (element-drawable-p (elt elements i))
                                     (element-in-view-port-p (elt elements i))))
                         collect (object-create (elt elements i) (elt hints i)))))))

  (defun hints-determine-chars-length (length)
    "Finds out how many chars long the hints must be"
    (floor (+ 1 (/ (log length) (log 26)))))

  (defun hints-generate (length)
    "Generates hints that will appear on the elements"
    (strings-generate length (hints-determine-chars-length length)))

  (defun strings-generate (length chars-length)
    "Generates strings of specified length"
    (ps:let ((minimum (1+ (ps:chain -math (pow 26 (- chars-length 1))))))
      (loop for i from minimum to (+ minimum length)
            collect (string-generate i))))

  (defun string-generate (n)
    "Generates a string from a number"
    (if (>= n 0)
        (+ (string-generate (floor (- (/ n 26) 1)))
           (code-char (+ 65
                         (rem n 26)))) ""))

  (add-stylesheet)
  (hints-add (qsa-text-nodes)))

(defclass paragraph-hint (nyxt/web-mode::hint) ())

(defmethod object-string ((paragraph-hint paragraph-hint))
  (nyxt/web-mode::body paragraph-hint))

(defmethod object-display ((paragraph-hint paragraph-hint))
  (format nil
          "~a  ~a  Paragraph"
          (nyxt/web-mode::hint paragraph-hint)
          (nyxt/web-mode::body paragraph-hint)))

(define-parenscript set-caret-on-start (&key nyxt-identifier)
  (defun qs (context selector)
    "Alias of document.querySelector"
    (ps:chain context (query-selector selector)))
  (let ((el (qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier))))
        (range (ps:chain document (create-range)))
        (sel (ps:chain window (get-selection))))
    (ps:chain window (focus))
    (ps:chain range (set-start (ps:@ el child-nodes 0) 0))
    (ps:chain range (collapse true))
    (ps:chain sel (remove-all-ranges))
    (ps:chain sel (add-range range))))

(defmethod %follow-hint ((paragraph-hint paragraph-hint))
  (set-caret-on-start :nyxt-identifier (nyxt/web-mode::identifier paragraph-hint)))

(defun paragraph-elements-from-json (elements-json)
  (loop for element in (cl-json:decode-json-from-string elements-json)
        collect (let ((object-type (cdr (assoc :type element))))
                  (cond ((equal "p" object-type)
                         (make-instance 'paragraph-hint
                                        :identifier (cdr (assoc :identifier element))
                                        :hint (cdr (assoc :hint element))
                                        :body (plump:text (plump:parse (cdr (assoc :body element))))))))))


(defun query-paragraph-hints (prompt function &key annotate-visible-only-p)
  (let ((buffer (current-buffer)))
    (let ((elements-json (%add-paragraph-hints :annotate-visible-only-p annotate-visible-only-p)))
      (let ((result (prompt-minibuffer
                     :input-prompt prompt
                     :default-modes '(element-hint-mode minibuffer-mode)
                     :history nil
                     :suggestion-function
                     (nyxt/web-mode::hint-suggestion-filter
                      (paragraph-elements-from-json elements-json))
                     :cleanup-function
                     (lambda ()
                       (with-current-buffer buffer
                         (nyxt/web-mode::remove-element-hints))))))
        (funcall-safely function result)))))

(define-parenscript block-page-keypresses ()
  (setf (ps:@ window block-keypresses)
        (lambda (event)
          (ps:chain event (prevent-default))))
  (ps:chain window
            (add-event-listener "keydown"
                                (ps:@ window block-keypresses)
                                false)))

(define-parenscript unlock-page-keypresses ()
  (ps:chain window
            (remove-event-listener "keydown"
                                   (ps:@ window block-keypresses)
                                   false)))

(define-parenscript make-page-editable ()
  (setf (ps:@ document body content-editable) "true"))

(define-parenscript make-page-uneditable ()
  (setf (ps:@ document body content-editable) "false"))

(define-command select-paragraph ()
  "Add hints to text elements on the page and open a minibuffer for selecting them."
  (query-paragraph-hints "Set caret on element" #'%follow-hint :annotate-visible-only-p t))

(define-parenscript is-collapsed ()
  ;; returns "true" if mark's start and end are the same value
  (defun is-collapsed ()
    (let ((sel (ps:chain window (get-selection))))
      (ps:@ sel is-collapsed)))
  (is-collapsed))

(define-command toggle-mark ()
  "Toggle the mark."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (if (string= (is-collapsed) "true")
        (progn
          (setf (mark-set mode) (not (mark-set mode)))
          (if (mark-set mode)
              (echo "Mark set")
              (echo "Mark deactivated")))
        (pflet ((collapse-to-focus ()
                                   (let ((sel (ps:chain window (get-selection))))
                                     (ps:chain sel
                                               (collapse (ps:@ sel focus-node)
                                                         (ps:@ sel focus-offset))))))
               (collapse-to-focus)
               (echo "Mark set")))))

(define-parenscript caret-move (&key action direction scale (n 1))
  (let ((sel (ps:chain window (get-selection))))
    (dotimes (i (ps:lisp n))
      (ps:chain sel (modify (ps:lisp action)
                            (ps:lisp direction)
                            (ps:lisp scale))))))

(define-command forward-char ()
  "Move caret forward by a character."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "character")))

(define-command backward-char ()
  "Move caret backward by a character."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "character")))

(define-command forward-word ()
  "Move caret forward by a word."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "word")))

(define-command backward-word ()
  "Move caret backward by a word."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "word")))

(define-command forward-line ()
  "Move caret forward by a line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "line")))

(define-command backward-line ()
  "Move caret backward by a line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "line")))

(define-command beginning-line ()
  "Move caret to the beginning of the line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "lineboundary")))

(define-command end-line ()
  "Move caret to the end of the line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "lineboundary")))

(define-command forward-sentence ()
  "Move caret forward to next end of sentence."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "forward"
                :scale "sentence")))

(define-command backward-sentence ()
  "Move caret backward to start of sentence."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (caret-move :action (if (mark-set mode)
                            "extend"
                            "move")
                :direction "backward"
                :scale "sentence")))

(define-command forward-line-with-selection ()
  "Set mark and move caret forward by a line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (forward-line)))

(define-command backward-line-with-selection ()
  "Set mark and move caret backward by a line."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (backward-line)))

(define-command forward-char-with-selection ()
  "Set mark and move caret forward by a word."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (forward-char)))

(define-command backward-char-with-selection ()
  "Set mark and move caret backward by a word."
  (let ((mode (find-submode (current-buffer) 'visual-mode)))
    (setf (mark-set mode) t)
    (backward-char)))
