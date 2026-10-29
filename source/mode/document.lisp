;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/document
  (:shadow #:focus-first-input-field)
  (:documentation "Package for `document-mode', mode to interact with structured documents."))
(in-package :nyxt/mode/document)

(define-mode document-mode ()
  "Mode to interact with structured documents.
This is typically for HTML pages, but other formats could be supported too.
It does not assume being online.

Important pieces of functionality are:
- Page scrolling and zooming. A zoom level is remembered for each host, as in
Firefox, and reused for every page on that host.
- QR code generation.
- view-source: for URLs.
- Buffer content summarization.
- Heading navigation.
- Frame selection."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "document-mode" ()
      keyscheme:default
      (list
       "C-M-Z" 'nyxt/mode/passthrough:passthrough-mode
       "M-i" 'focus-first-input-field
       "C-M-c" 'open-inspector
       "C-C" 'open-inspector
       "C-p" 'print-buffer
       "C-+" 'zoom-page
       "C-=" 'zoom-page              ; Because + shifted = on QWERTY.
       "C-button4" 'zoom-page
       "C-hyphen" 'unzoom-page
       "C-button5" 'unzoom-page
       "C-0" 'reset-page-zoom)
      keyscheme:cua
      (list
       "C-M-s" 'select-frame-new-buffer
       "C-h" 'jump-to-heading
       "C-M-h" 'jump-to-heading-buffers
       "C-c" 'copy
       "C-v" 'paste
       "M-v" 'paste-from-clipboard-ring
       "C-x" 'cut
       "C-a" 'select-all
       "C-z" 'undo
       "C-Z" 'redo
       "C-down" 'scroll-to-bottom
       "C-up" 'scroll-to-top
       ;; Leave SPACE, END, HOME and arrow keys unbound for the renderer
       "keypadleft" 'scroll-left
       "keypadright" 'scroll-right
       "keypadup" 'scroll-up
       "keypaddown" 'scroll-down
       "keypadhome" 'scroll-to-top
       "keypadend" 'scroll-to-bottom
       "keypadpageup" 'scroll-page-up
       "keypadprior" 'scroll-page-up
       "keypadnext" 'scroll-page-down
       "C-u C-o" 'edit-with-external-editor)
      keyscheme:emacs
      (list
       "C-c C-s" 'select-frame-new-buffer
       "C-." 'jump-to-heading
       "C-M-." 'jump-to-heading-buffers
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
       "C-c C-e" 'nyxt/mode/input-edit:input-edit-mode
       "C-u C-x C-f" 'edit-with-external-editor)
      keyscheme:vi-normal
      (list
       "g F" 'select-frame-new-buffer
       "g h" 'jump-to-heading
       "g H" 'jump-to-heading-buffers
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
       "shift-space" 'scroll-page-up
       "pageup" 'scroll-page-up
       "C-f" 'scroll-page-down
       "space" 'scroll-page-down
       "pagedown" 'scroll-page-down)))))

(define-configuration document-buffer
  ((default-modes (cons 'document-mode %slot-value%))))

(export-always 'active-element-tag)
(defun active-element-tag (&optional (buffer (current-buffer)))
  "The name of the active element in BUFFER."
  (ps-eval :buffer buffer (ps:@ (nyxt/ps:active-element document) tag-name)))

(export-always 'input-tag-p)
(-> input-tag-p ((or string null)) boolean)
(defun input-tag-p (tag)
  "Whether TAG is inputtable."
  (or (string= tag "INPUT")
      (string= tag "TEXTAREA")))

(define-command paste (&optional (buffer (current-buffer)))
  "Paste from clipboard into active element."
  (ffi-buffer-paste buffer))

(define-class ring-source (prompter:source)
  ((prompter:name "Clipboard ring")
   (ring :initarg :ring :accessor ring :initform nil)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:constructor
    (lambda (source)
      (containers:container->list (ring source))))
   (prompter:actions-on-return (lambda-command paste* (ring-items)
                                 (ffi-buffer-paste (current-buffer) (first ring-items)))))
  (:export-class-name-p t)
  (:metaclass user-class)
  (:documentation "Source for previous clipboard contents.
Only includes the strings that were pasted/copied inside Nyxt."))

(define-command paste-from-clipboard-ring ()
  "Show `*browser*' clipboard ring and paste selected entry."
  (ring-insert-clipboard (clipboard-ring *browser*))
  (prompt :prompt "Paste from ring"
          :sources (make-instance 'ring-source :ring (clipboard-ring *browser*))))

(define-command copy (&optional (buffer (current-buffer)))
  "Copy selected text to clipboard."
  (ffi-buffer-copy buffer))

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

(define-command focus-first-input-field (&key (buffer (current-buffer)))
  "Move the focus to the first inputtable element of BUFFER."
  ;; There are two basic ways to have an editable widget on a webpage:
  ;; - Using <input>/<textarea>,
  ;; - or marking any other element as contenteditable:
  ;;   https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/contenteditable
  (loop with inputs = (clss:ordered-select "input, textarea, [contenteditable]" (document-model buffer))
        for input across inputs
        when (ps-eval :buffer buffer
               (nyxt/ps:element-editable-p
                (nyxt/ps:qs-nyxt-id document (ps:lisp (nyxt/dom:get-nyxt-id input)))))
          do (nyxt/dom:focus-select-element input)
          and do (return input)))

(defmethod nyxt:on-signal-load-finished ((mode document-mode) url title)
  (declare (ignore title))
  (with-slots (buffer) mode
    ;; The renderer resets zoom when a page loads. A host gets the ratio
    ;; remembered for it, or the buffer default. Other URLs keep the zoom
    ;; already set on this buffer.
    (setf (ffi-buffer-zoom-ratio buffer)
          (zoom-ratio-for-url url buffer)))
  url)

(define-internal-page show-url-qrcode (&key url)
    (:title "*Buffer URL QR code*")
  "Display the QR code containing URL.
Warning: URL is a string."
  (let* ((url (quri:render-uri (url url)))
         (stream (flex:make-in-memory-output-stream)))
    (cl-qrencode:encode-png-stream url stream)
    (spinneret:with-html-string
      (:nstyle
        '(h1
          :margin "0")
        '(.url
          :max-width "80ch"
          :margin "17px 0 0"
          :font-size "17px"
          :line-height "24px"
          :overflow-wrap "anywhere")
        '(img
          :display "block"
          :width "222px"
          :height "auto"
          :margin-top "37px"
          :border-radius "7px"
          :image-rendering "pixelated"))
      (:h1 "QR code")
      (:p :class "url" url)
      (:img :src (str:concat "data:image/png;base64,"
                             (cl-base64:usb8-array-to-base64-string
                              (flex:get-output-stream-sequence stream)))
            :alt url))))

(define-command-global show-url-qrcode (&key (buffer (current-buffer)))
  "Display the QR code containing the URL of BUFFER in a new page."
  (buffer-load-internal-page-focus 'show-url-qrcode :url (quri:render-uri (url buffer))))

(export-always 'get-url-source)
(defun get-url-source (url)
  "Get HTML source for URL page, as a string."
  (let ((buffer (find url (buffer-list) :test #'quri:uri= :key #'url)))
    (unless buffer
      (return-from get-url-source (echo-warning "No buffer loaded URL: ~a" url)))
    (let ((dom (if (web-buffer-p buffer)
                   (nyxt/dom:copy (document-model buffer))
                   (plump:parse (ffi-buffer-get-document buffer)))))
      (loop for e across (clss:select "[nyxt-identifier]" dom)
            do (plump:remove-attribute e "nyxt-identifier"))
      (map nil #'plump:remove-child (reverse (clss:select ".nyxt-hint" dom)))
      (plump:serialize dom nil))))

(define-internal-scheme "view-source"
    (lambda (url)
      (values (get-url-source (quri:uri-path (quri:uri url)))
              "text/plain")))

(define-command-global view-source (&key (url (url (current-buffer))))
  "View source of the URL (by default current page) in a separate buffer."
  (make-buffer-focus :url (quri:make-uri :scheme "view-source"
                                         :path (quri:render-uri url))))

(define-command scroll-to-top (&key (smooth-p (smooth-scrolling (current-buffer))))
  "Scroll to the top of the current page."
  (ps-eval
    (ps:chain window
              (scroll-by (ps:create top (- (ps:chain document document-element scroll-height))
                                    behavior (ps:lisp (if smooth-p "smooth" "instant")))))))

(define-command scroll-to-bottom (&key (smooth-p (smooth-scrolling (current-buffer))))
  "Scroll to the bottom of the current page."
  (ps-eval
    (ps:chain window
              (scroll-by (ps:create top (ps:chain document document-element scroll-height)
                                    behavior (ps:lisp (if smooth-p "smooth" "instant")))))))

(define-command scroll-down (&key (y-pixels (scroll-distance (current-buffer)))
                             (smooth-p (smooth-scrolling (current-buffer))))
  "Scroll down the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (ps-eval
    (ps:chain window
              (scroll-by (ps:create top (ps:lisp y-pixels)
                                    behavior (ps:lisp (if smooth-p "smooth" "instant")))))))

(define-command scroll-up (&key (y-pixels (scroll-distance (current-buffer)))
                           (smooth-p (smooth-scrolling (current-buffer))))
  "Scroll up the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (ps-eval
    (ps:chain window
              (scroll-by (ps:create top (ps:lisp (- y-pixels))
                                    behavior (ps:lisp (if smooth-p "smooth" "instant")))))))

(define-command scroll-left (&key (x-pixels (horizontal-scroll-distance (current-buffer)))
                             (smooth-p (smooth-scrolling (current-buffer))))
  "Scroll left the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (ps-eval
    (ps:chain window
              (scroll-by (ps:create left (ps:lisp (- x-pixels))
                                    behavior (ps:lisp (if smooth-p "smooth" "instant")))))))

(define-command scroll-right (&key (x-pixels (horizontal-scroll-distance (current-buffer)))
                              (smooth-p (smooth-scrolling (current-buffer))))
  "Scroll right the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (ps-eval
    (ps:chain window
              (scroll-by (ps:create left (ps:lisp x-pixels)
                                    behavior (ps:lisp (if smooth-p "smooth" "instant")))))))

(define-command scroll-page-down ()
  "Scroll down by one page height."
  (ps-eval (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                          (ps:@ window inner-height))))))

(define-command scroll-page-up ()
  "Scroll up by one page height."
  (ps-eval (ps:chain window (scroll-by 0 (- (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                             (ps:@ window inner-height)))))))

(defun apply-host-zoom (buffer zoom)
  "Set ZOOM on BUFFER and on every other page open on the same host.
When the page has a host, remember ZOOM for later pages on that host."
  (if (and (realp zoom) (plusp zoom))
      (let* ((zoom (float zoom 1.0))
             (host (zoom-host (url buffer))))
        (when host
          (remember-zoom (url buffer) zoom))
        (dolist (other (if host
                           (adjoin buffer (buffers-on-host host))
                           (list buffer)))
          (setf (ffi-buffer-zoom-ratio other) zoom)))
      (setf (ffi-buffer-zoom-ratio buffer) zoom)))

(define-command zoom-page (&key (buffer (current-buffer)))
  "Zoom in the current page.
The new ratio is remembered for this host and applied to every open page on
it. Later pages on the host use the same ratio."
  (apply-host-zoom buffer
                   (+ (ffi-buffer-zoom-ratio buffer)
                      (zoom-ratio-step buffer))))

(define-command unzoom-page (&key (buffer (current-buffer)))
  "Zoom out the current page.
The new ratio is remembered for this host and applied to every open page on
it. Later pages on the host use the same ratio."
  (apply-host-zoom buffer
                   (- (ffi-buffer-zoom-ratio buffer)
                      (zoom-ratio-step buffer))))

(define-command reset-page-zoom (&key (buffer (current-buffer)))
  "Reset this host to the default zoom.
The remembered ratio is forgotten, so later pages use `zoom-ratio-default'.
Other hosts are left unchanged."
  ;; Delete `zoom-ratio-default' slot when we are able to get the initform of a
  ;; slot set by `define-configuration'.
  (let ((host (zoom-host (url buffer))))
    (when host
      (remember-zoom (url buffer) nil))
    (dolist (other (if host
                       (adjoin buffer (buffers-on-host host))
                       (list buffer)))
      (setf (ffi-buffer-zoom-ratio other) (zoom-ratio-default other)))))

(define-class heading ()
  ((inner-text "" :documentation "The inner text of the heading within the document.")
   (element nil :documentation "The header-representing element of `document-model'.")
   (buffer :documentation "The buffer to which this heading belongs.")
   (keywords :documentation "Keywords associated with this heading.")
   (scroll-position :documentation "The scroll position of the heading."))
  (:documentation "A heading representation with all the attached metadata.
The inner-text must not be modified, so that we can jump to the anchor of the same name."))

(defmethod title ((heading heading))
  (subseq (inner-text heading) 0 (position #\[ (inner-text heading))))

(defun get-headings (&key (buffer (current-buffer)))
  (ps-labels :buffer buffer
    ((heading-scroll-position
      :buffer buffer (element)
      (ps:chain (nyxt/ps:rqs-nyxt-id document (ps:lisp (nyxt/dom:get-nyxt-id element)))
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
  (when-let* ((scroll-position (document-scroll-position buffer))
              (vertical-scroll-position (second scroll-position))
              (headings (get-headings :buffer buffer)))
    (first (sort headings
                 (lambda (h1 h2)
                   (< (abs (- (scroll-position h1) vertical-scroll-position))
                      (abs (- (scroll-position h2) vertical-scroll-position))))))))

(defun scroll-page-to-heading (heading)
  (set-current-buffer (buffer heading) :focus nil)
  (nyxt/dom:scroll-to-element (element heading)))

(define-class heading-source (prompter:source)
  ((prompter:name "Headings")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:actions-on-current-suggestion-enabled-p t)
   (prompter:actions-on-current-suggestion
    (lambda-command scroll-page-to-heading* (heading)
      "Scroll to heading."
      (scroll-page-to-heading heading)))
   (prompter:constructor (lambda (source)
                           (get-headings :buffer (buffer source))))
   (prompter:actions-on-return (lambda-unmapped-command scroll-page-to-heading))))

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
  "Jump to a heading (H1 to H6) among a set of buffers."
  (let ((buffers (prompt
                  :prompt "Select headings from buffers"
                  :sources (make-instance 'buffer-source
                                          :enable-marks-p t
                                          :actions-on-return #'identity))))
    (prompt
     :prompt "Jump to heading"
     :sources (loop for buffer in buffers
                    collect (make-instance
                             'heading-source
                             :name (format nil "Headings: ~a" (title buffer))
                             :buffer buffer)))))

(export-always 'print-buffer)
(define-command print-buffer ()
  "Print the current buffer."
  (ps-eval (print)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame selection engine:

(define-class html-element ()
  ((body "")))

(define-class link (html-element)
  ((url "")))

(define-class image (html-element)
  ((alt "" :documentation "Alternative text for the image.")
   (url "")))

(defun frame-element-select ()
  "Install a drag box. Drag draws it, drags inside move it, and edge drags resize it."
  (let ((overlay-style (theme:themed-css (theme *browser*)
                         `("#nyxt-overlay"
                           :position "fixed"
                           :top "0"
                           :left "0"
                           :right "0"
                           :bottom "0"
                           :opacity 0.00
                           :background ,theme:on-background-color
                           :cursor "crosshair"
                           :user-select "none"
                           :touch-action "none"
                           :z-index #.(1- (expt 2 31)))))
        (selection-rectangle-style (theme:themed-css (theme *browser*)
                                     `("#nyxt-rectangle-selection"
                                       :position "absolute"
                                       :top "0"
                                       :left "0"
                                       :width "0"
                                       :height "0"
                                       :box-sizing "border-box"
                                       :border "1px solid"
                                       :border-color ,theme:action-color
                                       :border-radius "4px"
                                       :background-color
                                       ,(let ((rgb (cl-colors-ng:as-rgb theme:action-color)))
                                          (format nil "rgb(~d ~d ~d / 0.18)"
                                                  (round (* 255 (cl-colors-ng:rgb-red rgb)))
                                                  (round (* 255 (cl-colors-ng:rgb-green rgb)))
                                                  (round (* 255 (cl-colors-ng:rgb-blue rgb)))))
                                       :box-shadow "0 0 0 0.5px rgba(0, 0, 0, 0.12)"
                                       :pointer-events "none"
                                       :z-index ,(1- (expt 2 30)))))
        (handle-style (theme:themed-css (theme *browser*)
                        `(".nyxt-frame-handle"
                          :position "absolute"
                          :width "8px"
                          :height "8px"
                          :margin-left "-4px"
                          :margin-top "-4px"
                          :box-sizing "border-box"
                          :background-color "#ffffff"
                          :border "1px solid"
                          :border-color ,theme:action-color
                          :border-radius "1px"
                          :box-shadow "0 0 0 0.5px rgba(0, 0, 0, 0.16)"
                          :pointer-events "none"
                          :z-index ,(expt 2 30)))))
    (ps-labels :async t
      ((add-overlay
        (overlay-style selection-rectangle-style handle-style)
        "Add a drag box to the page."
        (setf (ps:@ window nyxt-frame-selection)
              (ps:create x1 0 y1 0 x2 0 y2 0
                         ready nil
                         dragging nil
                         ended 0
                         mode ""
                         ox 0 oy 0
                         sx1 0 sy1 0 sx2 0 sy2 0))
        (defun frame-box ()
          (ps:@ window nyxt-frame-selection))
        (defun add-stylesheet ()
          (unless (nyxt/ps:qs document "#nyxt-stylesheet")
            (ps:let ((style-element (ps:chain document (create-element "style"))))
              (setf (ps:@ style-element id) "nyxt-stylesheet")
              (ps:chain document head (append-child style-element)))))
        (defun add-style (style)
          (ps:let ((style-element (nyxt/ps:qs document "#nyxt-stylesheet")))
            (ps:chain style-element sheet (insert-rule style 0))))
        (defun remove-existing ()
          (dolist (id (list "nyxt-overlay" "nyxt-rectangle-selection"))
            (ps:let ((element (nyxt/ps:qs document (+ "#" id))))
              (when element
                (ps:chain element (remove)))))
          (ps:dolist (handle (nyxt/ps:qsa document ".nyxt-frame-handle"))
            (ps:chain handle (remove))))
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
        (defun add-handles ()
          (add-style (ps:lisp handle-style))
          (dolist (name (list "nw" "n" "ne" "e" "se" "s" "sw" "w"))
            (ps:let ((element (ps:chain document (create-element "div"))))
              (setf (ps:@ element id) (+ "nyxt-frame-" name))
              (setf (ps:@ element class-name) "nyxt-frame-handle")
              (setf (ps:@ element style display) "none")
              (ps:chain document body (append-child element)))))
        (defun normalize ()
          (ps:let* ((frame (frame-box))
                    (left (ps:chain |Math| (min (ps:@ frame x1) (ps:@ frame x2))))
                    (right (ps:chain |Math| (max (ps:@ frame x1) (ps:@ frame x2))))
                    (top (ps:chain |Math| (min (ps:@ frame y1) (ps:@ frame y2))))
                    (bottom (ps:chain |Math| (max (ps:@ frame y1) (ps:@ frame y2)))))
            (setf (ps:@ frame x1) left
                  (ps:@ frame x2) right
                  (ps:@ frame y1) top
                  (ps:@ frame y2) bottom)))
        (defun place-handles ()
          (ps:let* ((frame (frame-box))
                    (x1 (ps:@ frame x1))
                    (y1 (ps:@ frame y1))
                    (x2 (ps:@ frame x2))
                    (y2 (ps:@ frame y2))
                    (mx (/ (+ x1 x2) 2))
                    (my (/ (+ y1 y2) 2))
                    (show (if (and (> (- x2 x1) 0) (> (- y2 y1) 0))
                              "block"
                              "none"))
                    (points (list (list "nw" x1 y1)
                                  (list "n" mx y1)
                                  (list "ne" x2 y1)
                                  (list "e" x2 my)
                                  (list "se" x2 y2)
                                  (list "s" mx y2)
                                  (list "sw" x1 y2)
                                  (list "w" x1 my))))
            (dolist (point points)
              (ps:let ((handle (nyxt/ps:qs document (+ "#nyxt-frame-" (elt point 0)))))
                (setf (ps:@ handle style left) (+ (elt point 1) "px"))
                (setf (ps:@ handle style top) (+ (elt point 2) "px"))
                (setf (ps:@ handle style display) show)))))
        (defun update-selection-rectangle ()
          (normalize)
          (ps:let* ((frame (frame-box))
                    (element (nyxt/ps:qs document "#nyxt-rectangle-selection")))
            (setf (ps:@ element style left) (+ (ps:@ frame x1) "px"))
            (setf (ps:@ element style top) (+ (ps:@ frame y1) "px"))
            (setf (ps:@ element style width)
                  (+ (- (ps:@ frame x2) (ps:@ frame x1)) "px"))
            (setf (ps:@ element style height)
                  (+ (- (ps:@ frame y2) (ps:@ frame y1)) "px"))
            (place-handles)))
        (defun zone (x y)
          (ps:let* ((frame (frame-box))
                    (pad 12)
                    (left (ps:@ frame x1))
                    (right (ps:@ frame x2))
                    (top (ps:@ frame y1))
                    (bottom (ps:@ frame y2)))
            (if (not (ps:@ frame ready))
                "draw"
                (ps:let ((near-left (<= (ps:chain |Math| (abs (- x left))) pad))
                         (near-right (<= (ps:chain |Math| (abs (- x right))) pad))
                         (near-top (<= (ps:chain |Math| (abs (- y top))) pad))
                         (near-bottom (<= (ps:chain |Math| (abs (- y bottom))) pad))
                         (in-x (and (>= x (- left pad)) (<= x (+ right pad))))
                         (in-y (and (>= y (- top pad)) (<= y (+ bottom pad)))))
                  (cond
                    ((and near-top near-left) "nw")
                    ((and near-top near-right) "ne")
                    ((and near-bottom near-left) "sw")
                    ((and near-bottom near-right) "se")
                    ((and near-top in-x) "n")
                    ((and near-bottom in-x) "s")
                    ((and near-left in-y) "w")
                    ((and near-right in-y) "e")
                    ((and (>= x left) (<= x right) (>= y top) (<= y bottom)) "move")
                    (t "draw"))))))
        (defun set-cursor (mode)
          (ps:let ((overlay (nyxt/ps:qs document "#nyxt-overlay")))
            (when overlay
              (setf (ps:@ overlay style cursor)
                    (cond
                      ((or (equal mode "nw") (equal mode "se")) "nwse-resize")
                      ((or (equal mode "ne") (equal mode "sw")) "nesw-resize")
                      ((or (equal mode "n") (equal mode "s")) "ns-resize")
                      ((or (equal mode "e") (equal mode "w")) "ew-resize")
                      ((equal mode "move") "move")
                      (t "crosshair"))))))
        (defun begin (x y mode)
          (ps:let ((frame (frame-box)))
            (setf (ps:@ frame ox) x
                  (ps:@ frame oy) y
                  (ps:@ frame sx1) (ps:@ frame x1)
                  (ps:@ frame sy1) (ps:@ frame y1)
                  (ps:@ frame sx2) (ps:@ frame x2)
                  (ps:@ frame sy2) (ps:@ frame y2)
                  (ps:@ frame mode) mode
                  (ps:@ frame dragging) t
                  (ps:@ frame ready) t)
            (when (equal mode "draw")
              (setf (ps:@ frame x1) x
                    (ps:@ frame y1) y
                    (ps:@ frame x2) x
                    (ps:@ frame y2) y))
            (update-selection-rectangle)
            (set-cursor mode)))
        (defun apply-drag (x y)
          (ps:let* ((frame (frame-box))
                    (dx (- x (ps:@ frame ox)))
                    (dy (- y (ps:@ frame oy)))
                    (mode (ps:@ frame mode)))
            (cond
              ((equal mode "move")
               (setf (ps:@ frame x1) (+ (ps:@ frame sx1) dx)
                     (ps:@ frame y1) (+ (ps:@ frame sy1) dy)
                     (ps:@ frame x2) (+ (ps:@ frame sx2) dx)
                     (ps:@ frame y2) (+ (ps:@ frame sy2) dy)))
              ((equal mode "draw")
               (setf (ps:@ frame x1) (ps:@ frame ox)
                     (ps:@ frame y1) (ps:@ frame oy)
                     (ps:@ frame x2) x
                     (ps:@ frame y2) y)
               (normalize))
              (t
               (setf (ps:@ frame x1) (ps:@ frame sx1)
                     (ps:@ frame y1) (ps:@ frame sy1)
                     (ps:@ frame x2) (ps:@ frame sx2)
                     (ps:@ frame y2) (ps:@ frame sy2))
               (when (or (equal mode "n") (equal mode "nw") (equal mode "ne"))
                 (setf (ps:@ frame y1) (+ (ps:@ frame sy1) dy)))
               (when (or (equal mode "s") (equal mode "sw") (equal mode "se"))
                 (setf (ps:@ frame y2) (+ (ps:@ frame sy2) dy)))
               (when (or (equal mode "w") (equal mode "nw") (equal mode "sw"))
                 (setf (ps:@ frame x1) (+ (ps:@ frame sx1) dx)))
               (when (or (equal mode "e") (equal mode "ne") (equal mode "se"))
                 (setf (ps:@ frame x2) (+ (ps:@ frame sx2) dx)))
               (normalize)))))
        (defun finish (e)
          (ps:let ((frame (frame-box))
                   (overlay (nyxt/ps:qs document "#nyxt-overlay")))
            (when (and frame (ps:@ frame dragging))
              (ps:chain e (prevent-default))
              (apply-drag (ps:@ e |pageX|) (ps:@ e |pageY|))
              (update-selection-rectangle)
              (setf (ps:@ frame dragging) nil)
              (setf (ps:@ frame ended) (1+ (ps:@ frame ended)))
              (set-cursor (zone (ps:@ e |pageX|) (ps:@ e |pageY|))))
            (when (and overlay
                       (ps:chain overlay (has-pointer-capture (ps:@ e pointer-id))))
              (ps:chain overlay (release-pointer-capture (ps:@ e pointer-id))))))
        (defun add-listeners ()
          (ps:let ((overlay (nyxt/ps:qs document "#nyxt-overlay")))
            (ps:chain overlay (add-event-listener
                               "pointerdown"
                               (lambda (e)
                                 (when (= (ps:@ e button) 0)
                                   (ps:chain e (prevent-default))
                                   (ps:chain e (stop-propagation))
                                   (ps:chain overlay (set-pointer-capture (ps:@ e pointer-id)))
                                   (begin (ps:@ e |pageX|)
                                          (ps:@ e |pageY|)
                                          (zone (ps:@ e |pageX|) (ps:@ e |pageY|)))))))
            (ps:chain overlay (add-event-listener
                               "pointermove"
                               (lambda (e)
                                 (ps:let ((frame (frame-box)))
                                   (if (and frame (ps:@ frame dragging))
                                       (progn
                                         (ps:chain e (prevent-default))
                                         (apply-drag (ps:@ e |pageX|) (ps:@ e |pageY|))
                                         (update-selection-rectangle))
                                       (set-cursor (zone (ps:@ e |pageX|)
                                                         (ps:@ e |pageY|))))))))
            (ps:chain overlay (add-event-listener "pointerup" (lambda (e) (finish e))))
            (ps:chain overlay (add-event-listener "pointercancel"
                                                   (lambda (e) (finish e))))
            (ps:chain overlay (add-event-listener
                               "wheel"
                               (lambda (e)
                                 (ps:chain window (scroll-by (ps:@ e delta-x) (ps:@ e delta-y)))
                                 (ps:chain e (prevent-default)))
                               (ps:create :passive nil)))))
        (remove-existing)
        (add-stylesheet)
        (add-overlay)
        (add-selection-rectangle)
        (add-handles)
        (add-listeners)))
      (add-overlay overlay-style selection-rectangle-style handle-style))))

(defun frame-element-get-selection ()
  "Get the links inside the drag box."
  (ps-labels
    ((get-selection
      ()
      (ps:let ((frame (ps:@ window nyxt-frame-selection)))
        (defun element-in-selection-p (frame element)
          "Determine if a element is bounded within a selection."
          (ps:let* ((element-rect (ps:chain element (get-bounding-client-rect)))
                    (offset-x (ps:chain window |pageXOffset|))
                    (offset-y (ps:chain window |pageYOffset|))
                    (element-left (+ (ps:chain element-rect left) offset-x))
                    (element-right (+ (ps:chain element-rect right) offset-x))
                    (element-top (+ (ps:chain element-rect top) offset-y))
                    (element-bottom (+ (ps:chain element-rect bottom) offset-y)))
            (and (<= element-left (ps:@ frame x2))
                 (>= element-right (ps:@ frame x1))
                 (<= element-top (ps:@ frame y2))
                 (>= element-bottom (ps:@ frame y1)))))
        (defun object-create (element)
          (cond ((equal "A" (ps:@ element tag-name))
                 (ps:create "type" "link"
                            "href" (ps:@ element href)
                            "body" (ps:@ element |innerHTML|)))
                ((equal "IMG" (ps:@ element tag-name))
                 (ps:create "type" "img"
                            "src" (ps:@ element src)
                            "alt" (ps:@ element alt)))))
        (defun collect-selection (elements frame)
          "Collect elements within a selection"
          (loop for element in elements
                when (element-in-selection-p frame element)
                  collect (object-create element)))
        (when (and frame (ps:@ frame ready))
          (collect-selection (nyxt/ps:qsa document (list "a")) frame)))))
    (loop for element in (get-selection)
          collect (str:string-case (cdr (assoc :type element))
                    ("link"
                     (make-instance 'link
                                    :url (cdr (assoc :href element))
                                    :body (cdr (assoc :body element))))
                    ("img"
                     (make-instance 'image
                                    :url (cdr (assoc :src element))
                                    :alt (cdr (assoc :alt element))))))))

(define-parenscript frame-element-state ()
  "Return whether the drag box is ready and whether a drag is in progress."
  (ps:let ((frame (ps:@ window nyxt-frame-selection)))
    (when frame
      (ps:create :dragging (ps:@ frame dragging)
                 :ready (ps:@ frame ready)
                 :ended (ps:@ frame ended)))))

(defun frame-element-clear ()
  "Clear the selection frame created by the user."
  (ps-eval
    (ps:let ((rectangle (nyxt/ps:qs document "#nyxt-rectangle-selection"))
             (overlay (nyxt/ps:qs document "#nyxt-overlay")))
      (when rectangle
        (ps:chain rectangle (remove)))
      (when overlay
        (ps:chain overlay (remove))))
    (ps:dolist (handle (nyxt/ps:qsa document ".nyxt-frame-handle"))
      (ps:chain handle (remove)))
    (setf (ps:@ window nyxt-frame-selection) nil)))

(defun frame-source-selection ()
  (remove-duplicates (mapcar #'url (frame-element-get-selection))
                     :test #'equal))

(-> frame-prompt (prompter:source window) (maybe prompt-buffer))
(defun frame-prompt (source window)
  "Return WINDOW's prompt when it is showing SOURCE."
  (when-let ((prompt (first (nyxt::active-prompt-buffers window))))
    (when (find source (prompter:sources prompt))
      prompt)))

(-> activate-frame-prompt (prompt-buffer) t)
(defun activate-frame-prompt (prompt)
  "Focus PROMPT and keep it fully opaque."
  (ffi-within-renderer-thread (lambda () (ffi-focus-buffer prompt)))
  (ps-eval :buffer prompt
    (setf (ps:@ (nyxt/ps:qs document "*") style opacity) "1")
    (ps:let ((input (nyxt/ps:qs document "#input")))
      (when input
        (ps:chain input (focus))))))

(-> replace-frame-suggestions (prompter:source list) t)
(defun replace-frame-suggestions (source urls)
  "Replace SOURCE's suggestions with URLS, dropping marks that left the box."
  (let ((wrapped (prompter:ensure-suggestions-list source urls)))
    (setf (slot-value source 'prompter::marks)
          (remove-if-not (lambda (mark)
                           (member mark urls :test #'equalp))
                         (prompter:marks source)))
    (bt:with-lock-held ((slot-value source 'prompter::initial-suggestions-lock))
      (setf (slot-value source 'prompter::initial-suggestions) wrapped))))

(defun keep-frame-prompt-opaque (prompt)
  "Leave PROMPT undimmed while the pointer is on the page."
  (ps-eval :async t :buffer prompt
    (setf (ps:@ (nyxt/ps:qs document "*") style opacity) "1")))

(-> watch-frame-selection (prompter:source buffer window) t)
(defun watch-frame-selection (source buffer window)
  "Update SOURCE as the drag box changes, and refocus its prompt when a drag ends."
  (let ((browser *browser*))
    (run-thread "frame-selection"
      (let ((*browser* browser)
            (seen-p nil)
            (misses 0)
            (last-urls :none)
            (last-ended 0))
        (loop
          (sleep 0.12)
          (unless (slot-value source 'alive-p)
            (return))
          (let ((prompt (frame-prompt source window)))
            (cond
              ((and prompt (not seen-p))
               (setf seen-p t)
               (sleep 0.25))
              ((not prompt)
               (when (or seen-p (> (incf misses) 50))
                 (return)))
              (t
               (handler-case
                   (with-current-buffer buffer
                     (let* ((state (frame-element-state))
                            (dragging (and state (cdr (assoc :dragging state))))
                            (ready (and state (cdr (assoc :ready state))))
                            (ended (or (and state (cdr (assoc :ended state))) 0))
                            (urls (when ready
                                    (frame-source-selection))))
                       (when dragging
                         (keep-frame-prompt-opaque prompt))
                       (when (and ready (not (equal urls last-urls)))
                         (setf last-urls urls)
                         (replace-frame-suggestions source (or urls '()))
                         (prompter:refresh prompt)
                         (prompt-render-suggestions prompt))
                       (unless (eql ended last-ended)
                         (setf last-ended ended)
                         (when (plusp ended)
                           (activate-frame-prompt prompt)))))
                 (error ()
                   nil))))))))))

(define-class frame-source (prompter:source)
  ((prompter:name "Selection Frame")
   (buffer
    nil
    :type (maybe buffer)
    :documentation "Page whose links the drag box selects.")
   (alive-p
    t
    :type boolean
    :documentation "Whether the drag box is still updating this source.")
   (prompter:constructor
    (lambda (source)
      (let ((buffer (buffer source))
            (window (window (buffer source))))
        (with-current-buffer buffer
          (frame-element-select))
        (when window
          (watch-frame-selection source buffer window)))
      nil))
   (prompter:destructor
    (lambda (prompter source)
      (declare (ignore prompter))
      (setf (slot-value source 'alive-p) nil)))))

(define-command select-frame-new-buffer (&key (buffer (current-buffer)))
  "Draw a box and open its links in new buffers. Drag to draw, move, or resize."
  (prompt :prompt "Drag a box around the links to open"
          :sources (make-instance
                    'frame-source
                    :buffer buffer
                    :enable-marks-p t
                    :actions-on-return (lambda-command open-new-buffers (urls)
                                         (mapcar (lambda (i) (make-buffer :url (quri:uri i)))
                                                 urls)))
          :after-destructor (lambda ()
                              (with-current-buffer buffer
                                (frame-element-clear)))))

(pushnew 'document-mode nyxt::%default-modes)
