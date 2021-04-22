;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/element-hint-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for element hint prompt buffer"))
(in-package :nyxt/element-hint-mode)

(define-command toggle-hints-transparency (&key (buffer (current-buffer)))
  "Toggle the on-screen element hints transparency."
  (pflet ((toggle-transparent ()
            (defun qsa (context selector)
              "Alias of document.querySelectorAll"
              (ps:chain context (query-selector-all selector)))
            (ps:dolist (element (qsa document ".nyxt-hint"))
              (if (or (= (ps:chain element style opacity) "1")
                      (= (ps:chain element style opacity) ""))
                  (setf (ps:chain element style opacity) "0.2")
                  (setf (ps:chain element style opacity) "1.0")))))
    (with-current-buffer buffer
      (toggle-transparent))))

(define-mode element-hint-mode (nyxt/prompt-buffer-mode:prompt-buffer-mode)
  "Prompt buffer mode for element hinting."
  ((keymap-scheme
    (define-scheme "element-hint"
      scheme:cua
      (list
       "M-i" 'toggle-hints-transparency)))))
