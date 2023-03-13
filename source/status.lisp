;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'mode-status)
(defgeneric mode-status (status mode)
  (:method ((status status-buffer) (mode mode))
    (if (glyph-mode-presentation-p status)
        (glyph mode)
        (princ-to-string mode)))
  (:documentation "Return a MODE `mode' string description for the STATUS `status-buffer'.
Upon returning NIL, the mode is not displayed."))

(defun sort-modes-for-status (modes)
  "Return visible modes in MODES, with `nyxt/keyscheme-mode:keyscheme-mode' placed
first."
  (multiple-value-bind (keyscheme-mode other-modes)
      (sera:partition #'nyxt/keyscheme-mode::keyscheme-mode-p
                      (sera:filter #'visible-in-status-p modes))
    (append keyscheme-mode other-modes)))

(export-always 'format-status-modes)
(defmethod format-status-modes ((status status-buffer))
  "Render the enabled modes.
Any `nyxt/keyscheme-mode:keyscheme-mode' is placed first.

This leverages `mode-status' which can be specialized for individual modes."
  (let ((buffer (current-buffer (window status))))
    (if (modable-buffer-p buffer)
        (let ((sorted-modes (sort-modes-for-status (modes buffer))))
          (spinneret:with-html-string
            (when (nosave-buffer-p buffer) (:span "⚠ nosave"))
            (:nbutton
              :buffer status
              :text "✚"
              :title (str:concat "Enabled modes: " (modes-string buffer))
              (nyxt:toggle-modes))
            (loop for mode in sorted-modes
                  collect
                  (let ((mode mode))
                    (alex:when-let ((formatted-mode (mode-status status mode)))
                      (if (html-string-p formatted-mode)
                          (:raw formatted-mode)
                          (:nbutton
                            :buffer status
                            :text formatted-mode
                            :title (format nil "Describe ~a" mode)
                            (describe-class :class (name mode)))))))))
        "")))

(defun modes-string (buffer)
  (when (modable-buffer-p buffer)
    (format nil "~{~a~^ ~}" (mapcar #'princ-to-string (modes buffer)))))

(export-always 'format-status-buttons)
(defmethod format-status-buttons ((status status-buffer))
  "Render interactive buttons."
  (spinneret:with-html-string
    ;; TODO: Firefox-like additional click actions? See
    ;; https://support.mozilla.org/en-US/kb/mouse-shortcuts-perform-common-tasks
    (:nbutton
      :buffer status
      :text "←"
      :title "Backwards"
      (nyxt/history-mode:history-backwards))
    (:nbutton
      :buffer status
      :text "↺"
      :title "Reload"
      (nyxt:reload-current-buffer))
    (:nbutton
      :buffer status
      :text "→"
      :title "Forwards"
      (nyxt/history-mode:history-forwards) )
    (:nbutton
      :buffer status
      :text "≡"
      :title "Execute"
      (nyxt:execute-command))
    (:nbutton
      :buffer status
      :text "★"
      :title "Bookmark this page"
      (funcall (read-from-string "nyxt/bookmark-mode:bookmark-current-url")))))

(export-always 'format-status-load-status)
(defmethod format-status-load-status ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (spinneret:with-html-string
      (:div :class (if (and (web-buffer-p buffer)
                            (eq (slot-value buffer 'status) :loading))
                       "loader" "")))))

(export-always 'format-status-url)
(defmethod format-status-url ((status status-buffer))
  "Formats the currently open URL for the STATUS buffer.

MODIFY AT YOUR OWN RISK! The current implementation goes a great way to make the
display safe, in particular to prevent IDN-spoofing by showing the Unicode
domains as punicode (the Unicode aesthetic domain is shown in parentheses after
the URL.)"
  ;; However this function changes, it should always:
  ;; - Prevent IDN/Unicode-spoofing.
  ;; - Clearly show subdomains (except maybe "trivial" ones) to prevent
  ;;   subdomain takeover attacks.
  ;; - Retain a clear display of which protocol/scheme is used to discourage
  ;;   e.g. trusting HTTP websites.
  (let* ((buffer (current-buffer (window status)))
         (content (multiple-value-bind (aesthetic safe)
                      (render-url (url buffer))
                    (uiop:strcat
                     (if safe
                         (format nil "~a (~a)" safe aesthetic)
                         aesthetic)
                     (when (title buffer)
                       (str:concat " — " (title buffer)))
                     (when (find (url buffer) (remove buffer (buffer-list))
                                 :test #'url-equal :key #'url)
                       (format nil " (buffer ~a)" (id buffer)))))))
    (spinneret:with-html-string
      (:nbutton
        :buffer status
        :text content
        :title content
        (nyxt:set-url)))))

(export-always 'format-status-tabs)
(defmethod format-status-tabs ((status status-buffer))
  (flet ((domains (buffers)
           (remove-duplicates
            (sera:filter-map #'quri:uri-domain
                             (remove-if #'internal-url-p
                                        (mapcar #'url (sort-by-time buffers))))
            :test #'equal)))
    (spinneret:with-html-string
      (alex:when-let ((internal-buffers (remove-if-not #'internal-url-p (sort-by-time (buffer-list)) :key #'url)))
        (:span
         :class (if (internal-url-p (url (current-buffer)))
                    "plain tab"
                    "tab")
         :onclick (ps:ps
                    (if (or (= (ps:chain window event which) 2)
                            (= (ps:chain window event which) 4))
                        (nyxt/ps:lisp-eval
                         (:title "delete-tab-group"
                          :buffer status)
                         (if-confirm ((format nil "Delete all internal buffers?"))
                                     (nyxt:delete-buffer :buffers internal-buffers)))
                        (nyxt/ps:lisp-eval
                         (:title "select-tab-group"
                          :buffer status)
                         (prompt
                          :prompt "Switch to buffer with internal page"
                          :sources (make-instance 'buffer-source
                                                  :constructor internal-buffers)))))
         (if (sera:single internal-buffers)
             (prini-to-string (parse-nyxt-url (url (first internal-buffers))))
             "internal")))
      (loop for domain in (domains (buffer-list))
            collect (:span
                     :class (if (equal (quri:uri-domain (url (current-buffer))) domain)
                                "plain tab"
                                "tab")
                     :onclick (ps:ps
                                (if (or (= (ps:chain window event which) 2)
                                        (= (ps:chain window event which) 4))
                                    (nyxt/ps:lisp-eval
                                     (:title "delete-tab-group"
                                      :buffer status)
                                     (if-confirm ((format nil "Delete all buffers with domain: ~a?" domain))
                                                 (nyxt:delete-buffer :buffers (sera:filter (match-domain domain) (buffer-list)))))
                                    (nyxt/ps:lisp-eval
                                     (:title "select-tab-group"
                                      :buffer status)
                                     (nyxt::switch-buffer-or-query-domain domain))))
                     domain)))))

(export-always 'format-status)
(defmethod format-status ((status status-buffer))
  (let* ((buffer (current-buffer (window status))))
    (spinneret:with-html-string
      (:div :id "container"
            (:div :id "controls" :class "arrow-right"
                  (:raw (format-status-buttons status)))
            (:div :id "url" :class "arrow-right"
                  (:raw
                   (format-status-load-status status)
                   (format-status-url status)))
            (:div :id "tabs"
                  (:raw
                   (format-status-tabs status)))
            (:div :id "modes" :class "arrow-left"
                  :title (modes-string buffer)
                  (:raw
                   (format-status-modes status)))))))

(defvar *setf-handlers* (sera:dict)
  "A hash-table mapping (CLASS SLOT) pairs to hash-tables mapping OBJECT to HANDLER.
OBJECT may be any value.
HANDLER is a function of argument the CLASS instance.

See `define-setf-handler'.")

(export-always 'define-setf-handler)
(defmacro define-setf-handler (class-name slot bound-object handler) ; TODO: SLOT or WRITER?
  "When (setf (SLOT-WRITER CLASS-INSTANCE) VALUE) is called,
all handlers corresponding to (CLASS SLOT) are evaluated with CLASS-INSTANCE as
argument.

There is a unique HANDLER per BOUND-OBJECT.
When BOUND-OBJECT is garbage-collected, the corresponding handler is automatically removed."
  (alex:with-gensyms (key)
    (alex:once-only (bound-object)
      `(progn
         (let ((,key (list (find-class ',class-name) ',slot)))
           (handler-bind ((warning (if (gethash ,key *setf-handlers*)
                                       #'muffle-warning ; To avoid warning on redefinition.
                                       #'identity)))
             (setf (gethash
                    ,bound-object
                    (alex:ensure-gethash ,key *setf-handlers*
                                         (tg:make-weak-hash-table :test 'equal :weakness :key)))
                   ,handler)
             (defmethod (setf ,slot) :after (value (,class-name ,class-name))
               (declare (ignorable value))
               (dolist (handler (alex:hash-table-values (gethash ,key *setf-handlers*)))
                 (funcall* handler ,class-name)))))))))

(defmethod customize-instance :after ((status-buffer status-buffer) &key)
  "Add setf-handlers calling `print-status' on:
- `buffer' `modes',
- `document-buffer' `url',
- `document-buffer' `title',
- `window' `active-buffer'.

See also `define-setf-handler'."
  ;; We need to watch both the buffer `modes' slot and the status of modes,
  ;; since the mode list does not change when a mode gets disabled.
  (define-setf-handler modable-buffer modes status-buffer
    (lambda (buffer)
      (when (window status-buffer)
        (when (eq buffer (active-buffer (window status-buffer)))
          (print-status (window status-buffer))))))
  (define-setf-handler mode enabled-p status-buffer
    (lambda (mode)
      (when (window status-buffer)
        (when (eq (buffer mode) (active-buffer (window status-buffer)))
          (print-status (window status-buffer))))))
  (define-setf-handler document-buffer url status-buffer
    (lambda (buffer)
      (when (window status-buffer)
        (when (eq buffer (active-buffer (window status-buffer)))
          (print-status (window status-buffer))))))
  (define-setf-handler document-buffer title status-buffer
    (lambda (buffer)
      (when (window status-buffer)
        (when (eq buffer (active-buffer (window status-buffer)))
          (print-status (window status-buffer))))))
  (define-setf-handler window active-buffer status-buffer
    (lambda (window)
      (when (eq window (window status-buffer))
        (print-status (window status-buffer)))))
  (define-setf-handler network-buffer status status-buffer
    (lambda (buffer)
      (when (window status-buffer)
        (when (eq buffer (active-buffer (window status-buffer)))
          (print-status (window status-buffer)))))))
