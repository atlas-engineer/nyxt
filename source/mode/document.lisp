;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/document-mode
  (:shadow #:focus-first-input-field)
  (:documentation "Mode to interact with structured documents.
This is typically for HTML pages, but other format may be supported at some point.
It does not assume being online."))
(in-package :nyxt/document-mode)

;; TODO: Remove document-mode from special buffers (e.g. help).
;; This is required because special buffers cannot be part of a history (and it breaks it).
;; Bind C-l to set-url-new-buffer?  Wait: What if we click on a link?  url
;; changes in special buffers should open a new one.
;; Or else we require that all special-buffer-generating commands open a new buffer.

(define-mode document-mode ()
  "Base mode for interacting with documents."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (keyscheme-map
    (define-keyscheme-map "document-mode" ()
      keyscheme:default
      (list
       "C-M-Z" 'nyxt/passthrough-mode:passthrough-mode
       "M-i" 'focus-first-input-field
       "C-M-c" 'open-inspector
       "C-S-c" 'open-inspector
       "C-." 'jump-to-heading
       "C-M-." 'jump-to-heading-buffers
       "M-{" 'previous-heading
       "M-}" 'next-heading
       "C-p" 'print-buffer
       "C-R" 'reload-with-modes)
      keyscheme:cua
      (list
       "C-c" 'copy
       "C-v" 'paste
       "M-v" 'paste-from-clipboard-ring
       "C-x" 'cut
       "C-a" 'select-all
       "C-z" 'undo
       "C-Z" 'redo
       "C-+" 'zoom-page
       "C-=" 'zoom-page              ; Because + shifted = on QWERTY.
       "C-button4" 'zoom-page
       "C-hyphen" 'unzoom-page
       "C-button5" 'unzoom-page
       "C-0" 'reset-page-zoom
       "C-down" 'scroll-to-bottom
       "C-up" 'scroll-to-top
       ;; Leave SPACE and arrow keys unbound so that the renderer decides whether to
       ;; navigate textboxes (arrows), insert or scroll (space).
       ;; keypad, gtk:
       "keypadleft" 'scroll-left
       "keypadright" 'scroll-right
       "keypadup" 'scroll-up
       "keypaddown" 'scroll-down
       "keypadhome" 'scroll-to-top
       "keypadend" 'scroll-to-bottom
       "keypadpageup" 'scroll-page-up
       "keypadprior" 'scroll-page-up
       "keypadnext" 'scroll-page-down
       "end" 'maybe-scroll-to-bottom
       "home" 'maybe-scroll-to-top
       "C-u C-o" 'edit-with-external-editor)
      keyscheme:emacs
      (list
       "C-g" 'nothing              ; Emacs users may hit C-g out of habit.
       "M-w" 'copy
       "C-y" 'paste
       "M-y" 'paste-from-clipboard-ring
       "C-w" 'cut
       "C-x h" 'select-all
       "C-/" 'undo
       "C-?" 'redo ; / shifted on QWERTY
       "C-x C-+" 'zoom-page
       "C-x C-=" 'zoom-page ; Because + shifted = on QWERTY.
       "C-x C-hyphen" 'unzoom-page
       "C-x C-0" 'reset-page-zoom
       "C-p" 'scroll-up
       "C-n" 'scroll-down
       "M-<" 'scroll-to-top
       "M->" 'scroll-to-bottom
       "M-v" 'scroll-page-up
       "C-v" 'scroll-page-down
       "C-u C-x C-f" 'edit-with-external-editor)
      keyscheme:vi-normal
      (list
       "g h" 'jump-to-heading
       "g H" 'jump-to-heading-buffers
       "{" 'previous-heading
       "}" 'next-heading
       "y y" 'copy
       "p" 'paste
       ;; Debatable: means "insert after cursor" in Vi(m).
       "P" 'paste-from-clipboard-ring
       "d d" 'cut
       "u" 'undo
       "C-r" 'redo
       "+" 'zoom-page
       "z i" 'zoom-page
       "hyphen" 'unzoom-page
       "z o" 'unzoom-page
       "0" 'reset-page-zoom
       "z z" 'reset-page-zoom
       "h" 'scroll-left
       "l" 'scroll-right
       "k" 'scroll-up
       "j" 'scroll-down
       "g g" 'scroll-to-top
       "G" 'scroll-to-bottom
       "C-b" 'scroll-page-up
       "s-space" 'scroll-page-up
       "pageup" 'scroll-page-up
       "C-f" 'scroll-page-down
       "space" 'scroll-page-down
       "pagedown" 'scroll-page-down)))))

(export-always 'active-element-tag)
(defun active-element-tag (&optional (buffer (current-buffer)))
  "The name of the active element in BUFFER."
  (ps-eval :buffer buffer (ps:@ document active-element tag-name)))

(export-always 'input-tag-p)
(-> input-tag-p ((or string null)) boolean)
(defun input-tag-p (tag)
  "Whether TAG is inputtable."
  (or (string= tag "INPUT")
      (string= tag "TEXTAREA")))

(defun call-non-input-command-or-forward (command &key (buffer (current-buffer))
                                                       (window (current-window)))
  (let ((tag (active-element-tag)))
    (if (input-tag-p tag)
        (forward-to-renderer :window window :buffer buffer)
        (funcall command))))

(define-command maybe-scroll-to-bottom (&optional (buffer (current-buffer)))
  "Scroll to bottom if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-bottom :buffer buffer))

(define-command maybe-scroll-to-top (&optional (buffer (current-buffer)))
  "Scroll to top if no input element is active, forward event otherwise."
  (call-non-input-command-or-forward #'scroll-to-top :buffer buffer))

(define-command go-next ()
  "Navigate to the next element according to the HTML 'rel' attribute."
  (ps-eval (ps:chain (nyxt/ps:qs document "[rel=next]") 0 (click))))

(define-command go-previous ()
  "Navigate to the previous element according to the HTML 'rel' attribute."
  (ps-eval (ps:chain (nyxt/ps:qs document "[rel=prev]") 0 (click))))

(define-command go-to-homepage ()
  "Navigate to the homepage."
  (let* ((url (url (current-buffer)))
         (authority (quri:uri-authority url))
         (scheme (quri:uri-scheme url)))
    (buffer-load (str:concat scheme "://" authority))))

(define-command go-up ()
  "Navigate to the upper level in the URL path hierarchy."
  (let* ((url (url (current-buffer)))
         (path (quri:uri-path url))
         (path-splited (str:split "/" path :omit-nulls t))
         (new-path-splited (butlast path-splited))
         (scheme (quri:uri-scheme url))
         (authority (quri:uri-authority url))
         (new-path (reduce #'(lambda (x e) (str:concat x e "/"))
                           new-path-splited
                           :initial-value "/")))
    (buffer-load (str:concat scheme "://" authority new-path))))


(define-command paste (&optional (buffer (current-buffer)))
  "Paste from clipboard into active element."
  (ffi-buffer-paste buffer))

(define-class ring-source (prompter:source)
  ((prompter:name "Clipboard ring")
   (ring :initarg :ring :accessor ring :initform nil)
   (prompter:constructor
    (lambda (source)
      (containers:container->list (ring source))))
   (prompter:return-actions (lambda-command paste* (ring-items)
                              (ffi-buffer-paste (current-buffer) (first ring-items)))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command paste-from-clipboard-ring ()
  "Show `*browser*' clipboard ring and paste selected entry."
  (ring-insert-clipboard (clipboard-ring *browser*))
  (prompt :prompt "Paste from ring"
          :sources (make-instance 'ring-source :ring (clipboard-ring *browser*))))

(define-command copy (&optional (buffer (current-buffer)))
  "Copy selected text to clipboard."
  (ffi-buffer-copy buffer))

(define-command copy-placeholder ()
  "Copy placeholder text to clipboard."
  (let ((current-value (ps-eval (ps:@ document active-element placeholder))))
    (if (eq current-value :undefined)
        (echo "No active selected placeholder.")
        (progn (copy-to-clipboard current-value)
               (echo "Placeholder copied.")))))

(define-command cut (&optional (buffer (current-buffer)))
  "Cut the selected text in BUFFER."
  (ffi-buffer-cut buffer))

(define-command undo (&optional (buffer (current-buffer)))
  "Undo the last editing action."
  (ffi-buffer-undo buffer))

(define-command redo (&optional (buffer (current-buffer)))
  "Redo the last editing action."
  (ffi-buffer-redo buffer))

(define-command select-all (&optional (buffer (current-buffer)))
  "Select all the text in the text field."
  (ffi-buffer-select-all buffer))

(export-always 'element-focused)
(defgeneric element-focused (mode) ; TODO: Make hook instead?  Or use both, have the default method call hook.
  (:method ((mode t))
    nil)
  (:documentation "Method run when `focus-element' is called."))

(defmacro focus-element ((&optional (buffer '(current-buffer))) &body element-script)
  "Select the element pointed to by ELEMENT-SCRIPT.
ELEMENT-SCRIPT is a Parenscript script that is passed to `ps:ps'."
  (alex:with-gensyms (element)
    (alex:once-only (buffer)
      `(progn
         (ps-eval :buffer ,buffer (let ((,element (progn ,@element-script)))
                                    (ps:chain ,element (focus))
                                    (ps:chain ,element (select))))
         (dolist (mode (modes ,buffer))
           (element-focused mode))))))

(define-command focus-first-input-field (&key (type-blacklist '("hidden"
                                                                "checkbox"
                                                                "button")))
  "Move the focus to the first input field of `buffer'."
  ;; The list of input types can be found below.
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
  ;; TODO: The following results in 2 DOM traversal.  We should probably do the
  ;; whole thing in a single Parenscript instead.
  (ps-labels
    ((nth-input-type
      (i)
      (let* ((input (ps:chain document
                              (get-elements-by-tag-name "INPUT")))
             (item (when input (ps:chain input (item (ps:lisp i))))))
        (when item
          (ps:chain item type)))))
    (let ((i (do ((i 0 (1+ i)))
                 ((notany
                   (lambda (type) (equalp (nth-input-type i) type))
                   type-blacklist)
                  i))))
      (focus-element ()
        (let* ((input (ps:chain document
                                (get-elements-by-tag-name "INPUT")))
               (item (when input (ps:chain input (item (ps:lisp i))))))
          (when item
            item))))))

(defmethod nyxt:on-signal-load-committed ((mode document-mode) url)
  (declare (ignore mode url))
  nil)

(defmethod nyxt:on-signal-load-finished ((mode document-mode) url)
  (reset-page-zoom :buffer (buffer mode)
                   :ratio (current-zoom-ratio (buffer mode)))
  url)

(define-internal-page show-url-qrcode (&key url)
    (:title "*Buffer URL QR code*")
  "Display the QR code containing URL.
Warning: URL is a string."
  (let* ((stream (flex:make-in-memory-output-stream)))
    (cl-qrencode:encode-png-stream url stream)
    (spinneret:with-html-string
      (:p (:u url))
      (:p (:img :src (str:concat "data:image/png;base64,"
                                 (cl-base64:usb8-array-to-base64-string
                                  (flex:get-output-stream-sequence stream)))
                :alt url)))))

(define-command-global show-url-qrcode (&key (buffer (current-buffer)))
  "Display the QR code containing the URL of BUFFER in a new page."
  (buffer-load-internal-page-focus 'show-url-qrcode :url (quri:render-uri (url buffer))))

(export-always 'get-url-source)
(defun get-url-source (url)
  (let ((buffer (or (find (url url) (buffer-list) :test #'quri:uri= :key #'url)
                    (make-background-buffer :url (url url)))))
    (unwind-protect
         (let ((dom (if (web-buffer-p buffer)
                        (nyxt/dom:copy (document-model buffer))
                        (plump:parse (ffi-buffer-get-document buffer)))))
           (loop for e across (clss:select "[nyxt-identifier]" dom)
                 do (plump:remove-attribute e "nyxt-identifier"))
           (map nil #'plump:remove-child
                ;; FIXME: This selector hardcodes the set of identifiers/classes
                ;; we use. Update whenever we change it?
                (reverse (clss:select ".nyxt-hint, .nyxt-search-node, .nyxt-select-hint" dom)))
           (plump:serialize dom nil))
      (when (background-buffer-p buffer)
        (ffi-buffer-delete buffer)))))

(define-internal-scheme "view-source"
  (lambda (url buffer)
    (declare (ignore buffer))
    (values
     (get-url-source (quri:url-decode (quri:uri-path (quri:uri url))))
     "text/plain"))
  :no-access-p t)

(define-command-global view-source (&key (url (url (current-buffer))))
  "View source of the URL (by default current page) in a separate buffer."
  (make-buffer-focus :url (quri:make-uri :scheme "view-source"
                                         :path (quri:url-encode (quri:render-uri url)))))

(define-command scroll-to-top ()
  "Scroll to the top of the current page."
  (ps-eval (ps:chain window (scroll-by 0 (- (ps:chain document document-element scroll-height))))))

(define-command scroll-to-bottom ()
  "Scroll to the bottom of the current page."
  (ps-eval (ps:chain window (scroll-by 0 (ps:chain document document-element scroll-height)))))

(define-command scroll-down (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll down the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  ;; FIXME: This naive implementation doesn't scroll when there are several
  ;; scrollable areas on the screen.
  (ps-eval (ps:chain window (scroll-by 0 (ps:lisp scroll-distance)))))

(define-command scroll-up (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll up the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (ps-eval (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance))))))

(define-command scroll-left (&key (horizontal-scroll-distance
                                   (horizontal-scroll-distance (current-buffer))))
  "Scroll left the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (ps-eval (ps:chain window (scroll-by (ps:lisp (- horizontal-scroll-distance)) 0))))

(define-command scroll-right (&key (horizontal-scroll-distance
                                    (horizontal-scroll-distance (current-buffer))))
  "Scroll right the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (ps-eval (ps:chain window (scroll-by (ps:lisp horizontal-scroll-distance) 0))))

(define-command scroll-page-down ()
  "Scroll down by one page height."
  (ps-eval (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                          (ps:@ window inner-height))))))

(define-command scroll-page-up ()
  "Scroll up by one page height."
  (ps-eval (ps:chain window (scroll-by 0 (- (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                             (ps:@ window inner-height)))))))

(defun ensure-zoom-ratio-range (zoom &optional (buffer (current-buffer)))
  (let* ((ratio (funcall zoom (current-zoom-ratio buffer) (zoom-ratio-step buffer))))
    (setf ratio (max ratio (zoom-ratio-min buffer)))
    (setf ratio (min ratio (zoom-ratio-max buffer)))
    (setf (current-zoom-ratio buffer) ratio)))

(define-command zoom-page (&key (buffer (current-buffer)))
  "Zoom in the current page."
  (ensure-zoom-ratio-range #'+ buffer)
  (setf (ffi-buffer-zoom-level buffer) (current-zoom-ratio buffer)))

(define-command unzoom-page (&key (buffer (current-buffer)))
  "Zoom out the current page."
  (ensure-zoom-ratio-range #'- buffer)
  (setf (ffi-buffer-zoom-level buffer) (current-zoom-ratio buffer)))

(define-command reset-page-zoom (&key (buffer (current-buffer))
                                      (ratio (zoom-ratio-default buffer)))
  "Reset the page zoom to the zoom-ratio-default."
  (setf (ffi-buffer-zoom-level buffer) (setf (current-zoom-ratio buffer) ratio)))

(define-internal-page summarize-buffer (&key (summary-length 5) (id (id (current-buffer))))
    (:title "*Summary*")
  "Summarize the current buffer by creating a new summary buffer.
ID is a buffer `id'."
  (let ((buffer (nyxt::buffers-get id)))
    (let ((contents
            (serapeum:string-join
             (map 'list (lambda (e) (plump:text e))
                  (clss:select "p" (document-model buffer)))
             " ")))
      (spinneret:with-html-string
        (:h1 "Summary for: " (title buffer))
        (:ul
         (loop for point in (analysis:summarize-text contents :summary-length summary-length)
               collect (:li point)))))))

(define-command-global summarize-buffer (&key (summary-length 5) (buffer (current-buffer)))
  "Summarize the current buffer by creating a new summary buffer."
  (buffer-load-internal-page-focus 'summarize-buffer :summary-length summary-length :id (id buffer)))

(define-class heading ()
  ((inner-text "" :documentation "The inner text of the heading within the document.")
   (element nil :documentation "The header-representing element of `document-model'.")
   (buffer :documentation "The buffer to which this heading belongs.")
   (keywords :documentation "Keywords associated with this heading.")
   (scroll-position :documentation "The scroll position of the heading."))
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A heading. The inner-text must not be modified, so that we
  can jump to the anchor of the same name."))

(defmethod title ((heading heading))
  (subseq (inner-text heading) 0 (position #\[ (inner-text heading))))

(defun get-headings (&key (buffer (current-buffer)))
  (ps-labels :buffer buffer
    ((heading-scroll-position
      :buffer buffer (element)
      (ps:chain (nyxt/ps:qs-nyxt-id document (ps:lisp (nyxt/dom:get-nyxt-id element)))
                (get-bounding-client-rect) y)))
    (map 'list
         (lambda (e)
           (make-instance 'heading :inner-text (plump:text e)
                                   :element e
                                   :buffer buffer
                                   :keywords (ignore-errors
                                              (analysis:extract-keywords
                                               (plump:text (plump:next-element e))))
                                   :scroll-position (heading-scroll-position e)))
         (clss:ordered-select "h1, h2, h3, h4, h5, h6" (document-model buffer)))))

(defun current-heading (&optional (buffer (current-buffer)))
  (alex:when-let* ((scroll-position (document-scroll-position buffer))
                   (vertical-scroll-position (second scroll-position))
                   (headings (get-headings :buffer buffer)))
    (first (sort headings
                 (lambda (h1 h2)
                   (< (abs (- (scroll-position h1) vertical-scroll-position))
                      (abs (- (scroll-position h2) vertical-scroll-position))))))))

;; TODO: Make a method on plump:node? Extract to nyxt/dom?
(defun scroll-page-to-heading (heading)
  (set-current-buffer (buffer heading) :focus nil)
  (nyxt/dom:scroll-to-element (element heading)))

(defun scroll-page-to-n-headings (n &optional (buffer (current-buffer)))
  "Scroll to the N adjacent heading of the BUFFER."
  (sera:and-let* ((headings (get-headings :buffer buffer))
                  (new-position (+ n
                                   (position (element (current-heading buffer))
                                             headings
                                             :key #'element)))
                  (_ (<= 0 new-position (1- (length headings)))))
    (scroll-page-to-heading (elt headings new-position))))

(define-command next-heading (&optional (buffer (current-buffer)))
  "Scroll to the next heading of the BUFFER."
  (scroll-page-to-n-headings 1 buffer))

(define-command previous-heading (&optional (buffer (current-buffer)))
  "Scroll to the previous heading of the BUFFER."
  (scroll-page-to-n-headings -1 buffer))

(define-class heading-source (prompter:source)
  ((prompter:name "Headings")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:selection-actions-enabled-p t)
   (prompter:selection-actions (lambda-command scroll-page-to-heading* (heading)
                                 "Scroll to heading."
                                 (scroll-page-to-heading heading)))
   (prompter:constructor (lambda (source)
                           (get-headings :buffer (buffer source))))
   (prompter:return-actions (lambda-unmapped-command scroll-page-to-heading))))

(defmethod prompter:object-attributes ((heading heading) (source heading-source))
  (declare (ignore source))
  `(("Title" ,(format nil "~a ~a"
                      (make-string (typecase (element heading)
                                     (nyxt/dom:h1-element 1)
                                     (nyxt/dom:h2-element 2)
                                     (nyxt/dom:h3-element 3)
                                     (nyxt/dom:h4-element 4)
                                     (nyxt/dom:h5-element 5)
                                     (nyxt/dom:h6-element 6)
                                     (t 0))
                                   :initial-element #\*)
                      (title heading)))
    ("Keywords" ,(format nil "~:{~a~^ ~}" (keywords heading)))))

(define-command jump-to-heading (&key (buffer (current-buffer)))
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6."
  (prompt :prompt "Jump to heading"
          :sources (make-instance 'heading-source :buffer buffer)))

(define-command jump-to-heading-buffers ()
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6 across a set
of buffers."
  (let ((buffers (prompt
                  :prompt "Select headings from buffers"
                  :sources (make-instance 'buffer-source
                                          :multi-selection-p t
                                          :return-actions #'identity))))
    (prompt
     :prompt "Jump to heading"
     :sources (loop for buffer in buffers
                    collect (make-instance
                             'heading-source
                             :name (format nil "Headings: ~a" (title buffer))
                             :buffer buffer)))))

(define-panel-command-global headings-panel ()
    (panel-buffer "*Headings panel*")
  "Display a list of heading for jumping."
  (labels ((get-level (heading)
             (ignore-errors (parse-integer (plump:tag-name (element heading)) :start 1)))
           (group-headings (headings)
             (loop with min-level = (when headings
                                      (reduce #'min headings :key #'get-level))
                   with current = (list)
                   for heading in headings
                   if (= (get-level heading) min-level)
                     collect (nreverse current) into total
                     and do (setf current (list heading))
                   else
                     do (push heading current)
                   finally (return (delete nil (append total (list (nreverse current)))))))
           (headings->html (groups)
             (spinneret:with-html-string
               (:ul
                (dolist (group groups)
                  (let ((heading (first group)))
                    (:li (:a :onclick
                             (ps:ps (nyxt/ps:lisp-eval
                                     (:title "switch-buffer-scroll" :buffer panel-buffer)
                                     (switch-buffer :buffer (buffer heading))
                                     (nyxt/dom:scroll-to-element (element heading))))
                             (title heading)))
                    (when (rest group)
                      (:raw (sera:mapconcat #'headings->html (list (group-headings (rest group))) "")))))))))
    (setf (ffi-width panel-buffer) 400)
    (spinneret:with-html-string
      (:h1 "Headings")
      (:raw (headings->html (group-headings (get-headings)))))))

(export-always 'print-buffer)
(define-command print-buffer ()
  "Print the current buffer."
  (ps-eval (print)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame selection engine:

(define-class html-element ()
  ((body ""))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class link (html-element)
  ((url ""))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-class image (html-element)
  ((alt "" :documentation "Alternative text for the image.")
   (url ""))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defun frame-element-select ()
  "Allow the user to draw a frame around elements to select them."
  (let ((overlay-style (theme:themed-css (theme *browser*)
                         `("#nyxt-overlay"
                           :position "fixed"
                           :top "0"
                           :left "0"
                           :right "0"
                           :bottom "0"
                           :background ,theme:on-background
                           :z-index #.(1- (expt 2 31)))))
        (selection-rectangle-style (theme:themed-css (theme *browser*)
                                     `("#nyxt-rectangle-selection"
                                       :position "absolute"
                                       :top "0"
                                       :left "0"
                                       :border-style "dotted"
                                       :border-width "1px"
                                       :border-color ,theme:on-background
                                       :background-color ,theme:on-background
                                       :opacity 0.05
                                       :z-index ,(1- (expt 2 30))))))
    (ps-labels :async t
      ((add-overlay
        (overlay-style selection-rectangle-style)
        "Add a selectable overlay to the screen."
        (defparameter selection
          (ps:create x1 0 y1 0
                     x2 0 y2 0
                     set1 false
                     set2 false))
        (defun add-stylesheet ()
          (unless (nyxt/ps:qs document "#nyxt-stylesheet")
            (ps:let ((style-element (ps:chain document (create-element "style"))))
              (setf (ps:@ style-element id) "nyxt-stylesheet")
              (ps:chain document head (append-child style-element)))))
        (defun add-style (style)
          (ps:let ((style-element (nyxt/ps:qs document "#nyxt-stylesheet")))
            (ps:chain style-element sheet (insert-rule style 0))))
        (defun add-overlay ()
          (ps:let ((element (ps:chain document (create-element "div"))))
            (add-style (ps:lisp overlay-style))
            (setf (ps:@ element id) "nyxt-overlay")
            (ps:chain document body (append-child element))))
        (defun add-selection-rectangle ()
          (ps:let ((element (ps:chain document (create-element "div"))))
            (add-style (ps:lisp selection-rectangle-style))
            (setf (ps:@ element id) "nyxt-rectangle-selection")
            (ps:chain document body (append-child element))))
        (defun update-selection-rectangle ()
          (ps:let ((element (nyxt/ps:qs document "#nyxt-rectangle-selection")))
            (setf (ps:@ element style left) (+ (ps:chain selection x1) "px"))
            (setf (ps:@ element style top) (+ (ps:chain selection y1) "px"))
            (setf (ps:@ element style width)
                  (+ (- (ps:chain selection x2)
                        (ps:chain selection x1))
                     "px"))
            (setf (ps:@ element style height)
                  (+ (- (ps:chain selection y2)
                        (ps:chain selection y1))
                     "px"))))
        (defun add-listeners ()
          (setf (ps:@ (nyxt/ps:qs document "#nyxt-overlay") onmousemove)
                (lambda (e)
                  (when (and (ps:chain selection set1)
                             (not (ps:chain selection set2)))
                    (setf (ps:chain selection x2) (ps:chain e |pageX|))
                    (setf (ps:chain selection y2) (ps:chain e |pageY|))
                    (update-selection-rectangle))))
          (setf (ps:@ (nyxt/ps:qs document "#nyxt-overlay") onclick)
                (lambda (e)
                  (if (not (ps:chain selection set1))
                      (progn
                        (setf (ps:chain selection x1) (ps:chain e |pageX|))
                        (setf (ps:chain selection y1) (ps:chain e |pageY|))
                        (setf (ps:chain selection set1) true))
                      (progn
                        (setf (ps:chain selection x2) (ps:chain e |pageX|))
                        (setf (ps:chain selection y2) (ps:chain e |pageY|))
                        (setf (ps:chain selection set2) true))))))
        (add-stylesheet)
        (add-overlay)
        (add-selection-rectangle)
        (add-listeners)))
      (add-overlay overlay-style selection-rectangle-style))))

(defun frame-element-get-selection ()
  "Get the selected elements drawn by the user."
  (ps-labels
    ((get-selection
      ()
      (defun element-in-selection-p (selection element)
        "Determine if a element is bounded within a selection."
        (ps:let* ((element-rect (ps:chain element (get-bounding-client-rect)))
                  (offsetX (ps:chain window |pageXOffset|))
                  (offsetY (ps:chain window |pageYOffset|))
                  (element-left (+ (ps:chain element-rect left) offsetX))
                  (element-right (+ (ps:chain element-rect right) offsetX))
                  (element-top (+ (ps:chain element-rect top) offsetY))
                  (element-bottom (+ (ps:chain element-rect bottom) offsetY)))
          (if (and
               (<= element-left (ps:chain selection x2))
               (>= element-right (ps:chain selection x1))
               (<= element-top (ps:chain selection y2))
               (>= element-bottom (ps:chain selection y1)))
              t nil)))
      (defun object-create (element)
        (cond ((equal "A" (ps:@ element tag-name))
               (ps:create "type" "link" "href" (ps:@ element href) "body" (ps:@ element |innerHTML|)))
              ((equal "IMG" (ps:@ element tag-name))
               (ps:create "type" "img" "src" (ps:@ element src) "alt" (ps:@ element alt)))))
      (defun collect-selection (elements selection)
        "Collect elements within a selection"
        (loop for element in elements
              when (element-in-selection-p selection element)
                collect (object-create element)))
      (collect-selection (nyxt/ps:qsa document (list "a")) selection)))
    (loop for element in (get-selection)
          collect (str:string-case (gethash "type" element )
                    ("link"
                     (make-instance 'link
                                    :url (gethash "href" element )
                                    :body (plump:text (plump:parse (gethash "body" element )))))
                    ("img"
                     (make-instance 'image
                                    :url (gethash "src" element )
                                    :alt (gethash "alt" element )))))))

(defun frame-element-clear ()
  "Clear the selection frame created by the user."
  (ps-eval
    (ps:chain (nyxt/ps:qs document "#nyxt-rectangle-selection") (remove))
    (ps:chain (nyxt/ps:qs document "#nyxt-overlay") (remove))))

(define-class frame-source (prompter:source)
  ((prompter:name "Selection Frame")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:filter-preprocessor (lambda (initial-suggestions-copy source input)
                                   (declare (ignore initial-suggestions-copy source input))
                                   (frame-source-selection)))
   (prompter:constructor (lambda (source)
                           (declare (ignore source))
                           (frame-element-select)
                           (list)))))

(define-command select-frame-new-buffer (&key (buffer (current-buffer)))
  "Select a frame and open the links in new buffers."
  (prompt :prompt "Open selected links in new buffers"
          :sources (make-instance
                    'frame-source
                    :buffer buffer
                    :multi-selection-p t
                    :return-actions (lambda-command open-new-buffers (urls)
                                      (mapcar (lambda (i) (make-buffer :url (quri:uri i)))
                                              urls)))
          :after-destructor (lambda () (with-current-buffer buffer
                                         (frame-element-clear)))))

(defun frame-source-selection ()
  (remove-duplicates (mapcar #'url (frame-element-get-selection))
                     :test #'equal))

(pushnew 'document-mode nyxt::%default-modes)
