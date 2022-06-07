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
  "Return visible modes in MODES, with `nyxt/keymap-scheme-mode:scheme-mode' placed first."
  (multiple-value-bind (scheme-mode other-modes)
      (sera:partition #'nyxt/keymap-scheme-mode::keymap-scheme-mode-p
                      (sera:filter #'visible-in-status-p modes))
    (append scheme-mode other-modes)))

(export-always 'format-status-modes)
(defmethod format-status-modes ((status status-buffer))
  "Render the enabled modes.
Any `nyxt/keymap-scheme-mode:keymap-scheme-mode' is placed first.

This leverages `mode-status' which can be specialized for individual modes."
  (let ((buffer (current-buffer (window status))))
    (if (modable-buffer-p buffer)
        (let ((sorted-modes (sort-modes-for-status (modes buffer))))
          (spinneret:with-html-string
            (when (nosave-buffer-p buffer) (:span "⚠ nosave"))
            (:button :type "button" :class "button"
                     :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt:toggle-modes)))
                     :title (str:concat "Enabled modes: " (modes-string buffer)) "✚")
            (loop for mode in sorted-modes
                  collect (alex:when-let ((formatted-mode (mode-status status mode)))
                            (if (html-string-p formatted-mode)
                                (:raw formatted-mode)
                                (:button :class "button"
                                         :onclick (ps:ps (nyxt/ps:lisp-eval
                                                          `(describe-class :class ',(name mode))))
                                         :title (format nil "Describe ~a" mode)
                                         formatted-mode))))))
        "")))

(defun modes-string (buffer)
  (when (modable-buffer-p buffer)
    (format nil "~{~a~^ ~}" (mapcar #'princ-to-string (modes buffer)))))

(export-always 'format-status-buttons)
(defmethod format-status-buttons ((status status-buffer))
  "Render buttons for interactivity, like history backwards/forwards and the `execute-command' menu."
  (declare (ignore status))
  (spinneret:with-html-string
    (:button :type "button" :class "button"
             :title "Backwards"
             :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt/history-mode:history-backwards))) "«")
    (:button :type "button" :class "button"
             :title "Reload"
             :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt:reload-current-buffer))) "↺")
    (:button :type "button" :class "button"
             :title "Forwards"
             :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt/history-mode:history-forwards))) "»")
    (:button :type "button" :class "button"
             :title "Execute"
             :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt:execute-command))) "≡")))

(export-always 'format-status-load-status)
(defmethod format-status-load-status ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (spinneret:with-html-string
      (:div :class (if (and (web-buffer-p buffer)
                            (eq (slot-value buffer 'status) :loading))
                       "loader" "")))))

(export-always 'format-status-url)
(defmethod format-status-url ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (spinneret:with-html-string
      (:button :type "button" :class "button"
               :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt:set-url)))
               (format nil " ~a — ~a"
                       (render-url (url buffer))
                       (title buffer))))))

(export-always 'format-status-tabs)
(defmethod format-status-tabs ((status status-buffer))
  (declare (ignore status))
  (spinneret:with-html-string
    (loop for domain in (remove-duplicates
                         (sera:filter-map #'quri:uri-domain
                                          (mapcar #'url (sort-by-time (buffer-list))))
                         :test #'equal)
          collect (:button :type "tab" :class "button"
                           :onclick (ps:ps (nyxt/ps:lisp-eval
                                            `(nyxt::switch-buffer-or-query-domain ,domain)))
                           domain))))

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

;; TODO: Existence check:
;; (find-method #'(setf modes) '(:around) (list t (find-class 'modable-buffer)))

;; TODO: Multiple status buffer support.
;; TODO: Uninstall on destroy?

(defmethod customize-instance :after ((status-buffer status-buffer) &key)
  "XXX:?"
  (defmethod (setf modes) :after (value (buffer modable-buffer))
    (when (window status-buffer)
      (when (eq buffer (active-buffer (window status-buffer)))
        (print-status (window status-buffer)))))
  (defmethod (setf url) :after (value (buffer document-buffer))
    (when (window status-buffer)
      (when (eq buffer (active-buffer (window status-buffer)))
        (print-status (window status-buffer)))))
  (defmethod (setf title) :after (value (buffer document-buffer))
    (when (window status-buffer)
      (when (eq buffer (active-buffer (window status-buffer)))
        (print-status (window status-buffer)))))
  (defmethod (setf active-buffer) :after (value (window window))
    (when (eq window (window status-buffer))
      (print-status (window status-buffer)))))
