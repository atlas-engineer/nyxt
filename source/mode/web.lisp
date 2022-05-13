;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/web-mode
  (:use :common-lisp :nyxt)
  (:shadow #:focus-first-input-field)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum
                #:export-always
                #:->)
  (:documentation "Mode for web pages"))
(in-package :nyxt/web-mode)
(use-nyxt-package-nicknames)

;; TODO: Remove web-mode from special buffers (e.g. help).
;; This is required because special buffers cannot be part of a history (and it breaks it).
;; Bind C-l to set-url-new-buffer?  Wait: What if we click on a link?  url
;; changes in special buffers should open a new one.
;; Or else we require that all special-buffer-generating commands open a new buffer.

(define-mode web-mode ()
  "Base mode for interacting with documents."
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "web"
      scheme:cua
      (list
       "C-M-Z" 'nyxt/passthrough-mode:passthrough-mode
       "M-i" 'focus-first-input-field
       "C-c" 'copy
       "C-v" 'paste
       "C-x" 'cut
       "C-a" 'select-all
       "C-z" 'undo
       "C-Z" 'redo
       "C-+" 'zoom-page
       "C-=" 'zoom-page              ; Because + shifted = on QWERTY.
       "C-hyphen" 'unzoom-page
       "C-0" 'reset-page-zoom
       "C-button4" 'zoom-page
       "C-button5" 'unzoom-page
       "C-M-c" 'open-inspector
       "C-." 'jump-to-heading
       "M-{" 'previous-heading
       "M-}" 'next-heading
       "end" 'maybe-scroll-to-bottom
       "home" 'maybe-scroll-to-top
       "C-down" 'scroll-to-bottom
       "C-up" 'scroll-to-top
       "C-i" 'autofill
       "C-u C-o" 'edit-with-external-editor
       ;; Leave SPACE and arrow keys unbound so that the renderer decides whether to
       ;; navigate textboxes (arrows), insert or scroll (space).
       ;; keypad, gtk:
       "keypadleft" 'scroll-left
       "keypaddown" 'scroll-down
       "keypadup" 'scroll-up
       "keypadright" 'scroll-right
       "keypadend" 'scroll-to-bottom
       "keypadhome" 'scroll-to-top
       "keypadnext" 'scroll-page-down
       "keypadpageup" 'scroll-page-up
       "keypadprior" 'scroll-page-up)
      scheme:emacs
      (list
       "C-g" 'nothing              ; Emacs users may hit C-g out of habit.
       "C-y" 'paste
       "M-w" 'copy
       "C-/" 'undo
       "C-?" 'redo ; / shifted on QWERTY
       "C-w" 'cut
       "C-x h" 'select-all
       "C-p" 'scroll-up
       "C-n" 'scroll-down
       "C-x C-+" 'zoom-page
       "C-x C-=" 'zoom-page ; Because + shifted = on QWERTY.
       "C-x C-hyphen" 'unzoom-page
       "C-x C-0" 'reset-page-zoom
       "C-." 'jump-to-heading
       "M->" 'scroll-to-bottom
       "M-<" 'scroll-to-top
       "C-v" 'scroll-page-down
       "M-v" 'scroll-page-up
       "C-u C-x C-f" 'edit-with-external-editor)

      scheme:vi-normal
      (list
       "y y" 'copy
       "p" 'paste
       "d d" 'cut
       "u" 'undo
       "C-r" 'redo
       "+" 'zoom-page
       "hyphen" 'unzoom-page
       "0" 'reset-page-zoom
       "z i" 'zoom-page
       "z o" 'unzoom-page
       "z z" 'reset-page-zoom
       "g h" 'jump-to-heading
       "g H" 'jump-to-heading-buffers
       "{" 'previous-heading
       "}" 'next-heading
       "h" 'scroll-left
       "j" 'scroll-down
       "k" 'scroll-up
       "l" 'scroll-right
       "G" 'scroll-to-bottom
       "g g" 'scroll-to-top
       "C-f" 'scroll-page-down
       "C-b" 'scroll-page-up
       "space" 'scroll-page-down
       "s-space" 'scroll-page-up
       "pageup" 'scroll-page-up
       "pagedown" 'scroll-page-down)))))

(sera:export-always '%clicked-in-input?)
(defun %clicked-in-input? (&optional (buffer (current-buffer)))
  ;; We don't use define-parenscript because we need to control over which
  ;; buffer we query.
  (ffi-buffer-evaluate-javascript buffer
                                  (ps:ps
                                    (ps:chain document active-element
                                              tag-name))))

(sera:export-always 'input-tag-p)
(-> input-tag-p ((or string null)) boolean)
(defun input-tag-p (tag)
  (or (string= tag "INPUT")
      (string= tag "TEXTAREA")))

(defun call-non-input-command-or-forward (command &key (buffer (current-buffer))
                                                       (window (current-window)))
  (let ((response (%clicked-in-input?)))
    (if (input-tag-p response)
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
  (peval (ps:chain (nyxt/ps:qsa document "rel=next") 0 (click))))

(define-command go-previous ()
  "Navigate to the previous element according to the HTML 'rel' attribute."
  (peval (ps:chain (nyxt/ps:qsa document "rel=prev") 0 (click))))

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
   (prompter:actions
    (list (lambda-command paste* (ring-items)
            (%paste :input-text (first ring-items))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command paste-from-clipboard-ring ()
  "Show `*browser*' clipboard ring and paste selected entry."
  (prompt
   :prompt "Paste from ring"
   :sources (list (make-instance 'ring-source
                                 :ring (nyxt::clipboard-ring *browser*)))))

(define-command copy (&optional (buffer (current-buffer)))
  "Copy selected text to clipboard."
  (ffi-buffer-copy buffer))

(define-command copy-placeholder ()
  "Copy placeholder text to clipboard."
  (let ((current-value (peval (ps:@ document active-element placeholder))))
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

(define-class autofill-source (prompter:source)
  ((prompter:name "Autofills")
   (prompter:constructor (autofills *browser*))
   (prompter:actions
    (list (lambda-command autofill* (autofills)
            (let ((selected-fill (first autofills)))
              (cond ((stringp (autofill-fill selected-fill))
                     (%paste :input-text (autofill-fill selected-fill)))
                    ((functionp (autofill-fill selected-fill))
                     (%paste :input-text (funcall (autofill-fill selected-fill))))))))))
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command autofill ()
  "Fill in a field with a value from a saved list."
  (prompt
   :prompt "Autofill"
   :sources (make-instance 'autofill-source)))

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
         (ffi-buffer-evaluate-javascript ,buffer
                                         (ps:ps (let ((,element (progn ,@element-script)))
                                                  (ps:chain ,element (focus))
                                                  (ps:chain ,element (select)))))
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
  (pflet ((nth-input-type (i)
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

(defmethod nyxt:on-signal-load-committed ((mode web-mode) url)
  (declare (ignore mode url))
  nil)

(defmethod nyxt:on-signal-load-finished ((mode web-mode) url)
  (reset-page-zoom :buffer (buffer mode)
                   :ratio (current-zoom-ratio (buffer mode)))
  url)

;; REVIEW: Shorten the name to e.g., `show-url-qr'? It's no longer current URL only.
(define-internal-page-command-global show-qrcode-of-current-url
    (&key (buffer-id (id (current-buffer)))
     (url (quri:render-uri (url (nyxt::buffers-get buffer-id)))))
    (buffer (format nil "*Buffer ~a (~a) QRcode*" buffer-id url) 'base-mode)
  "In a new buffer, show the QR code containing the URL for the current buffer."
  (let* ((stream (flexi-streams:make-in-memory-output-stream)))
    (cl-qrencode:encode-png-stream url stream)
    (spinneret:with-html-string
      (:p (:u url))
      (:p (:img :src (str:concat "data:image/png;base64,"
                                 (cl-base64:usb8-array-to-base64-string
                                  (flexi-streams:get-output-stream-sequence stream)))
                :alt url)))))

(define-internal-page-command-global view-source (&key (url (render-url (url (current-buffer)))))
  (source-buffer (format nil "*Source of ~a" url) 'base-mode)
  "View source of the URL (by default current page) in a separate buffer."
  (let ((buffer (or (find (quri:uri url) (buffer-list) :test #'quri:uri= :key #'url)
                    (make-background-buffer :url url))))
    (unwind-protect
         (spinneret:with-html-string
           (:pre (if (web-buffer-p buffer)
                     (plump:serialize (document-model buffer) nil)
                     (ffi-buffer-get-document buffer))))
      (when (background-buffer-p buffer)
        (ffi-buffer-delete buffer)))))

(pushnew 'web-mode nyxt::%default-modes)
