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
            (ps:dolist (element (nyxt/ps:qsa document ".nyxt-hint"))
              (if (or (= (ps:chain element style opacity) "1")
                      (= (ps:chain element style opacity) ""))
                  (setf (ps:chain element style opacity) "0.2")
                  (setf (ps:chain element style opacity) "1.0")))))
    (with-current-buffer buffer
      (toggle-transparent))))

(define-command scroll-to-hint (&key (buffer (current-buffer)))
  "Show the selected hint on screen."
  (with-current-buffer buffer
    (nyxt/web-mode::highlight-selected-hint :element (current-suggestion-value) :scroll t)))

(define-mode element-hint-mode (nyxt/prompt-buffer-mode:prompt-buffer-mode)
  "Prompt buffer mode for element hinting."
  ((keymap-scheme
    (define-scheme "element-hint"
      scheme:cua
      (list
       "M-i" 'toggle-hints-transparency
       "C-l" 'scroll-to-hint)))))
