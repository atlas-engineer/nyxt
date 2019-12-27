;;; link-hint.lisp --- functions to enable link hinting and navigation
;;; This file's parenscript is licensed under license documents/external/LICENSE1

(in-package :next)

(define-parenscript add-element-hints ()
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (defun code-char (n)
    "Alias of String.fromCharCode"
    (ps:chain -string (from-char-code n)))
  (defun hint-determine-position (rect)
    "Determines the position of a hint according to the element"
    (ps:create :top  (+ (ps:@ window page-y-offset) (ps:@ rect top))
               :left (+ (ps:@ window page-x-offset) (- (ps:@ rect left) 20))))
  (defun hint-create-element (element hint)
    "Creates a DOM element to be used as a hint"
    (ps:let* ((rect (ps:chain element (get-bounding-client-rect)))
              (position (hint-determine-position rect))
              (element (ps:chain document (create-element "span"))))
      (setf (ps:@ element class-name) "next-hint")
      (setf (ps:@ element style) (ps:lisp (box-style (current-buffer))))
      (setf (ps:@ element style position) "absolute")
      (setf (ps:@ element style left) (+ (ps:@ position left) "px"))
      (setf (ps:@ element style top) (+ (ps:@ position top) "px"))
      (setf (ps:@ element text-content) hint)
      element))
  (defun hint-add (element hint)
    "Adds a hint on a single element. Additionally sets a unique
identifier for every hinted element."
    (ps:chain element (set-attribute "next-identifier" hint))
    (ps:let ((hint-element (hint-create-element element hint)))
      (ps:chain document body (append-child hint-element))))
  (defun object-create (element hint)
    (cond ((equal "A" (ps:@ element tag-name))
           (ps:create "type" "link" "hint" hint "href" (ps:@ element href) "body" (ps:@ element inner-H-T-M-L)))
          ((equal "BUTTON" (ps:@ element tag-name))
           (ps:create "type" "button" "hint" hint "identifier" hint "body" (ps:@ element inner-H-T-M-L)))))
  (defun hints-add (elements)
    "Adds hints on elements"
    (ps:let* ((elements-length (length elements))
              (hints (hints-generate elements-length)))
      (ps:chain -j-s-o-n
                (stringify
                 (loop for i from 0 to (- elements-length 1)
                       do (hint-add (elt elements i) (elt hints i))
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
  (hints-add (qsa document (list "a" "button"))))

(define-parenscript remove-element-hints ()
  (defun hints-remove-all ()
    "Removes all the elements"
    (ps:dolist (element (qsa document ".next-hint"))
      (ps:chain element (remove))))
  (hints-remove-all))

(define-parenscript click-button (next-identifier)
  (defun qs (context selector)
    "Alias of document.querySelector"
    (ps:chain context (query-selector selector)))
  (ps:chain (qs document (ps:lisp (format nil "[next-identifier=\"~a\"]" next-identifier))) (click)))

(defmacro query-hints (prompt (symbol) &body body)
  `(with-result* ((elements-json (add-element-hints))
                  (,symbol (read-from-minibuffer
                            (make-minibuffer
                             :input-prompt ,prompt
                             :history nil
                             :completion-function (hint-completion-filter
                                                   (elements-from-json elements-json))
                             :cleanup-function
                             (lambda () (remove-element-hints :buffer (callback-buffer (current-minibuffer))))))))
     ,@body))

(defun hint-completion-filter (hints)
  (lambda (input)
    (let* ((matched-hints (remove-if-not (lambda (x) (str:starts-with-p input (hint x) :ignore-case t)) hints))
           (fuzzy-matched-hints (fuzzy-match input (set-difference hints matched-hints))))
      (append matched-hints fuzzy-matched-hints))))

(defun elements-from-json (elements-json)
  (loop for element in (cl-json:decode-json-from-string elements-json)
        collect (let ((object-type (cdr (assoc :type element))))
                  (cond ((equal "link" object-type)
                         (make-instance 'link-hint
                                        :hint (cdr (assoc :hint element))
                                        :url (cdr (assoc :href element))
                                        :body (plump:text (plump:parse (cdr (assoc :body element))))))
                        ((equal "button" object-type)
                         (make-instance 'button-hint
                                        :identifier (cdr (assoc :identifier element))
                                        :hint (cdr (assoc :hint element))
                                        :body (plump:text (plump:parse (cdr (assoc :body element))))))))))

(defclass hint ()
  ((hint :accessor hint :initarg :hint)
   (identifier :accessor identifier :initarg :identifier)
   (body :accessor body :initarg :body
         :documentation "The body of the anchor tag.")))

(defclass button-hint (hint) ())

(defclass link-hint (hint)
  ((url :accessor url :initarg :url)))

(defmethod object-string ((link-hint link-hint))
  (format nil "~a  ~a  ~a" (hint link-hint) (body link-hint) (url link-hint)))

(defmethod object-string ((button-hint button-hint))
  (format nil "~a  ~a  Button" (hint button-hint) (body button-hint)))

(defmethod %follow-hint ((link-hint link-hint))
  (set-url (url link-hint) :buffer (current-buffer) :raw-url-p t))

(defmethod %follow-hint ((button-hint button-hint))
  (click-button :buffer (current-buffer) :next-identifier (identifier button-hint)))

(defmethod %follow-hint-new-buffer-focus ((link-hint link-hint))
  (let ((new-buffer (make-buffer)))
    (set-url (url link-hint) :buffer new-buffer :raw-url-p t)
    (set-current-buffer new-buffer)))

(defmethod %follow-hint-new-buffer-focus ((button-hint button-hint))
  (echo "Can't open button in new buffer."))

(defmethod %follow-hint-new-buffer ((link-hint link-hint))
  (let ((new-buffer (make-buffer)))
      (set-url (url link-hint) :buffer new-buffer :raw-url-p t)))

(defmethod %follow-hint-new-buffer ((button-hint button-hint))
  (echo "Can't open button in new buffer."))

(defmethod %copy-hint-url ((link-hint link-hint))
  (trivial-clipboard:text (url link-hint)))

(defmethod %copy-hint-url ((button-hint button-hint))
  (echo "Can't copy URL from button."))

(define-command follow-hint ()
  "Show a set of element hints, and go to the user inputted one in the
currently active buffer."
  (query-hints "Go to element:" (selected-element)
    (%follow-hint selected-element)))

(define-command follow-hint-new-buffer ()
  "Show a set of element hints, and open the user inputted one in a new
buffer (not set to visible active buffer)."
  (query-hints "Open element in new buffer:" (selected-element)
    (%follow-hint-new-buffer selected-element)))

(define-command follow-hint-new-buffer-focus ()
  "Show a set of element hints, and open the user inputted one in a new
visible active buffer."
  (query-hints "Go to element in new buffer:" (selected-element)
    (%follow-hint-new-buffer-focus selected-element)))

(define-command copy-hint-url ()
  "Show a set of element hints, and copy the URL of the user inputted one."
  (query-hints "Copy element URL:" (selected-element)
    (%copy-hint-url selected-element)))

(define-deprecated-command copy-anchor-url ()
  "Deprecated by `copy-hint-url'."
  (copy-hint-url))

(define-deprecated-command go-anchor ()
  "Deprecated by `follow-hint'."
  (follow-hint))

(define-deprecated-command go-anchor-new-buffer-focus ()
  "Deprecated by `follow-hint-new-buffer-focus'."
  (follow-hint-new-buffer-focus))

(define-deprecated-command go-anchor-new-buffer ()
  "Deprecated by `follow-hint-new-buffer'."
  (follow-hint-new-buffer))
