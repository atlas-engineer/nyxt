;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'format-status-modes)
(defun format-status-modes (buffer window)
  (format nil "~:[~;⚠ nosave ~]~{~a~^ ~}"
          (nosave-buffer-p buffer)
          (mapcar (if (glyph-mode-presentation-p (status-buffer window))
                      #'glyph
                      #'format-mode)
                  (sera:filter #'visible-in-status-p (modes buffer)))))

(defun list-modes (buffer)
  (format nil "~{~a~^ ~}" (mapcar #'format-mode (modes buffer))))

(export-always 'format-status-buttons)
(defun format-status-buttons ()
  (markup:markup
   (:a :class "button" :title "Backwards" :href (lisp-url '(nyxt/web-mode:history-backwards)) "←")
   (:a :class "button" :title "Forwards" :href (lisp-url '(nyxt/web-mode:history-forwards)) "→")
   (:a :class "button" :title "Reload" :href (lisp-url '(nyxt:reload-current-buffer)) "↺")
   (:a :class "button" :title "Execute" :href (lisp-url '(nyxt:execute-command)) "⚙")
   (:a :class "button" :title "Buffers" :href (lisp-url '(nyxt::list-buffers)) "≡")))

(export-always 'format-status-vi-mode)
(defun format-status-vi-mode (&optional (buffer (current-buffer)))
  (cond ((find-submode buffer 'vi-normal-mode)
         (markup:markup
          (:a :class "button" :title "Vi normal mode" :href (lisp-url '(nyxt/vi-mode:vi-insert-mode)) "N")))
        ((find-submode buffer 'vi-insert-mode)
         (markup:markup
          (:a :class "button" :title "Vi insert mode" :href (lisp-url '(nyxt/vi-mode:vi-normal-mode)) "I")))
        (t (markup:markup
            (:span "")))))

(export-always 'format-status-load-status)
(defun format-status-load-status (buffer)
  (markup:markup
   (:span (if (and (web-buffer-p buffer)
                   (eq (slot-value buffer 'load-status) :loading))
              "Loading: " ""))))

(export-always 'format-status-url)
(defun format-status-url (buffer)
  (markup:markup
   (:a :class "button"
       :href (lisp-url '(nyxt:set-url))
       (format nil " ~a — ~a"
               (render-url (url buffer))
               (title buffer)))))

(export-always 'format-status-tabs)
(defun format-status-tabs ()
  (markup:markup
   (:span
    (loop for domain in (remove-duplicates
                         (sera:filter-map #'quri:uri-domain
                                          (mapcar #'url (sort-by-time (buffer-list))))
                         :test #'equal)
          collect (markup:markup
                   (:a :class "tab"
                       :href
                       (lisp-url `(nyxt::switch-buffer-or-query-domain ,domain)) domain))))))

(defun format-status (window)
  (let* ((buffer (current-buffer window))
         (vi-mode-color (cond ((find-submode buffer 'vi-normal-mode)
                               "background-color:rgb(100,100,100);")
                              ((find-submode buffer 'vi-insert-mode)
                               "background-color:rgb(105,38,38);")))
         (url-class (if vi-mode-color
                        ""
                        "")))
                        ;; "overlap")))
    (markup:markup
     (:div :id "container"
           (:div :id "controls"
                 :class url-class
                 (markup:raw (format-status-buttons)))
           (:div :class "arrow arrow-right"
                 :style "background-color:rgb(80,80,80)" "")
           (:div :id "vi-mode"
                 :style vi-mode-color
                 (markup:raw (format-status-vi-mode buffer)))
           (:div :class "arrow arrow-right"
                 :style vi-mode-color "")
           (:div :id "url"
                 (markup:raw
                  (format-status-load-status buffer)
                  (format-status-url buffer)))
           (:div :class "arrow arrow-right"
                 :style "background-color:rgb(120,120,120)" "")
           (:div :id "tabs"
                 (markup:raw
                  (format-status-tabs)))
           (:div :class "arrow arrow-left"
                 :style "background-color:rgb(120,120,120)" "")
           (:div :id "modes"
                 :title (list-modes buffer)
                 (format-status-modes buffer window))))))
