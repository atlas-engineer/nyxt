;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'format-status-modes)
(defun format-status-modes (buffer window)
  (spinneret:with-html-string
    (when (nosave-buffer-p buffer) (:span "⚠ nosave"))
    (:button :type "button"
             :onclick (ps:ps (nyxt/ps:send-lisp-url '(nyxt:toggle-modes)))
             :title (str:concat "Enabled modes: " (list-modes buffer)) "✚")
    (loop for mode in (sera:filter #'visible-in-status-p (modes buffer))
          collect (let* ((formatted-mode (if (glyph-mode-presentation-p (status-buffer window))
                                             (glyph mode)
                                             (format-mode mode))))
                    (if (html-string-p formatted-mode)
                        (:raw formatted-mode)
                      ;; FIXME: We should ideally intercept requests coming
                        ;; from status-buffer and redirect those to the current
                        ;; web-buffer.
                        (:button :class "button"
                                 :onclick (ps:ps (nyxt/ps:send-lisp-url
                                                  `(buffer-load ,(nyxt-url 'describe-class
                                                                           :class `,(mode-name mode)))))
                                 :title (format nil "Describe ~a" (mode-name mode))
                                 formatted-mode))))))

(defun list-modes (buffer)
  (format nil "~{~a~^ ~}" (mapcar #'format-mode (modes buffer))))

(export-always 'format-status-buttons)
(defun format-status-buttons ()
  (spinneret:with-html-string
    (:button :type "button"
             :title "Backwards"
             :onclick (ps:ps (nyxt/ps:send-lisp-url '(nyxt/web-mode:history-backwards))) "«")
    (:button :type "button"
             :title "Reload"
             :onclick (ps:ps (nyxt/ps:send-lisp-url '(nyxt:reload-current-buffer))) "↺")
    (:button :type "button"
             :title "Forwards"
             :onclick (ps:ps (nyxt/ps:send-lisp-url '(nyxt/web-mode:history-forwards))) "»")
    (:button :type "button"
             :title "Execute"
             :onclick (ps:ps (nyxt/ps:send-lisp-url '(nyxt:execute-command))) "≡")))

(defun format-status-vi-mode (&optional (buffer (current-buffer)))
  (spinneret:with-html-string
    (cond ((find-submode buffer 'vi-normal-mode)
           (:div
            (:button :type "button"
                     :title "vi-normal-mode"
                     :onclick (ps:ps (nyxt/ps:send-lisp-url '(nyxt/vi-mode:vi-insert-mode))) "N")))
          ((find-submode buffer 'vi-insert-mode)
           (:div
            (:button :type "button"
                     :title "vi-insert-mode"
                     :onclick (ps:ps (nyxt/ps:send-lisp-url '(nyxt/vi-mode:vi-normal-mode))) "I")))
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
    (:button :type "button"
             :onclick (ps:ps (nyxt/ps:send-lisp-url '(nyxt:set-url)))
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
          collect (:button :type "tab"
                           :onclick (ps:ps (nyxt/ps:send-lisp-url
                                            `(nyxt::switch-buffer-or-query-domain ,domain)))
                           domain))))

(defun format-status (window)
  (let* ((buffer (current-buffer window))
         (vi-class (cond ((find-submode buffer 'vi-normal-mode)
                          "vi-normal-mode")
                         ((find-submode buffer 'vi-insert-mode)
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
                  :title (list-modes buffer)
                  (:raw
                   (format-status-modes buffer window)))))))
