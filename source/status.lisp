;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun format-status (window)
  (let ((buffer (current-buffer window)))
    (markup:markup
     (:div :id "modes"
           (format nil "~{~a~^ ~}"
                   (mapcar (lambda (m) (str:replace-all "-mode" "" (str:downcase (mode-name m))))
                           (modes buffer))))
     (:a :class "button" :title "Backwards" :href (lisp-url '(nyxt/web-mode:history-backwards)) "←")
     (:a :class "button" :title "Forwards" :href (lisp-url '(nyxt/web-mode:history-forwards)) "→")
     (:a :class "button" :title "Reload" :href (lisp-url '(nyxt:reload-current-buffer)) "↺")
     (:a :class "button" :title "Execute" :href (lisp-url '(nyxt:execute-command)) "⚙")
     (:a :class "button" :title "Buffers" :href (lisp-url '(nyxt::list-buffers)) "≡")
     (:span (if (and (web-buffer-p buffer)
                     (eq (slot-value buffer 'load-status) :loading))
                "Loading: " ""))
     (:a :class "button"
         :href (lisp-url '(nyxt:set-url-from-current-url))
      (format nil " ~a — ~a"
              (object-display (url buffer))
              (title buffer))))))
