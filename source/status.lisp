;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmethod mode-status ((mode mode))
  (princ-to-string mode))

(export-always 'format-status-modes)
(defun format-status-modes (buffer window)
  "Format the modes for the status area.
This leverages `mode-status' which can be specialized for individual modes."
  (if (modable-buffer-p buffer)
      (spinneret:with-html-string
        (when (nosave-buffer-p buffer) (:span "⚠ nosave"))
        (:button :type "button" :class "button"
                 :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt:toggle-modes)))
                 :title (str:concat "Enabled modes: " (modes-string buffer)) "✚")
        (loop for mode in (sera:filter (alex:conjoin #'enabled-p #'visible-in-status-p)
                                       (modes buffer))
              collect (let* ((formatted-mode (if (glyph-mode-presentation-p (status-buffer window))
                                                 (glyph mode)
                                                 (mode-status mode))))
                        (if (html-string-p formatted-mode)
                            (:raw formatted-mode)
                            (:button :class "button"
                                     :onclick (ps:ps (nyxt/ps:lisp-eval
                                                      `(describe-class :class (quote ,(name mode)))))
                                     :title (format nil "Describe ~a" mode)
                                     formatted-mode)))))
      ""))

(defun modes-string (buffer)
  (when (modable-buffer-p buffer)
    (format nil "~{~a~^ ~}" (mapcar #'princ-to-string
                                    (sera:filter #'enabled-p (modes buffer))))))

(export-always 'format-status-buttons)
(defun format-status-buttons ()
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

(defun format-status-vi-mode (&optional (buffer (current-buffer)))
  (spinneret:with-html-string
    (cond ((find-submode 'nyxt/vi-mode:vi-normal-mode buffer)
           (:div
            (:button :type "button"
                     :title "vi-normal-mode"
                     :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt/vi-mode:vi-insert-mode))) "N")))
          ((find-submode 'nyxt/vi-mode:vi-insert-mode buffer)
           (:div
            (:button :type "button"
                     :title "vi-insert-mode"
                     :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt/vi-mode:vi-normal-mode))) "I")))
          (t (:span "")))))

(export-always 'format-status-load-status)
(defun format-status-load-status (buffer)
  (spinneret:with-html-string
    (:div :class (if (and (web-buffer-p buffer)
                          (eq (slot-value buffer 'status) :loading))
                     "loader" ""))))

(export-always 'format-status-url)
(defun format-status-url (buffer)
  (spinneret:with-html-string
    (:button :type "button" :class "button"
             :onclick (ps:ps (nyxt/ps:lisp-eval '(nyxt:set-url)))
             (format nil " ~a — ~a"
                     (render-url (url buffer))
                     (title buffer)))))

(export-always 'format-status-tabs)
(defun format-status-tabs ()
  (spinneret:with-html-string
    (loop for domain in (remove-duplicates
                         (sera:filter-map #'quri:uri-domain
                                          (mapcar #'url (sort-by-time (buffer-list))))
                         :test #'equal)
          collect (:button :type "tab" :class "button"
                           :onclick (ps:ps (nyxt/ps:lisp-eval
                                            `(nyxt::switch-buffer-or-query-domain ,domain)))
                           domain))))

(defun format-status (window)
  (let* ((buffer (current-buffer window))
         (vi-class (cond ((find-submode 'nyxt/vi-mode:vi-normal-mode buffer)
                          "vi-normal-mode")
                         ((find-submode 'nyxt/vi-mode:vi-insert-mode buffer)
                          "vi-insert-mode"))))
    (spinneret:with-html-string
      (:div :id (if vi-class "container-vi" "container")
            (:div :id "controls" :class "arrow-right"
                  (:raw (format-status-buttons)))
            (when vi-class
              (:div :id "vi-mode" :class (str:concat vi-class " arrow-right")
                    (:raw (format-status-vi-mode buffer))))
            (:div :id "url" :class "arrow-right"
                  (:raw
                   (format-status-load-status buffer)
                   (format-status-url buffer)))
            (:div :id "tabs"
                  (:raw
                   (format-status-tabs)))
            (:div :id "modes" :class "arrow-left"
                  :title (modes-string buffer)
                  (:raw
                   (format-status-modes buffer window)))))))
