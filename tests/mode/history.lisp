;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-history-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/history:history-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/history:history-mode buffer)))))

(define-test history-blocklist-and-record ()
  (let* ((*browser* (make-instance 'browser
                                   :history-file
                                   (make-instance 'history-file
                                                  :profile (make-instance 'files:virtual-profile))))
         (buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (enable-modes* 'nyxt/mode/history:history-mode buffer)
      (let ((mode (find-submode 'nyxt/mode/history:history-mode buffer)))
        (setf (nyxt/mode/history:history-blocklist mode)
              '("https://blocked.example"))
        (assert-true (nyxt/mode/history:blocked-p
                      (quri:uri "https://blocked.example/x") mode))
        (assert-false (nyxt/mode/history:blocked-p
                       (quri:uri "https://ok.example") mode))
        (assert-equality #'quri:uri=
                         (quri:uri "https://ok.example")
                         (nyxt/mode/history::add-url-to-history
                          (quri:uri "https://ok.example") mode :title "OK"))
        (assert-false (nyxt/mode/history::add-url-to-history
                       (quri:uri "https://blocked.example/x") mode :title "No"))
        (assert-eq 1 (length (history-vector *browser*)))
        (assert-string= "OK" (title (aref (history-vector *browser*) 0)))))))

(defun history-binding (keyscheme keyspec)
  (let ((mode (make-instance 'nyxt/mode/history:history-mode
                             :buffer (make-instance 'modable-buffer))))
    (keymaps:lookup-key keyspec
                        (keymaps:get-keymap keyscheme
                                            (keyscheme-map mode)))))

(define-test history-mode-keyscheme-bindings ()
  (assert-eq 'nyxt/mode/history:history-backwards
             (history-binding keyscheme:default "M-left"))
  (assert-eq 'nyxt/mode/history:history-forwards
             (history-binding keyscheme:default "M-right"))
  (assert-eq 'nyxt/mode/history:history-backwards
             (history-binding keyscheme:default "M-["))
  (assert-eq 'nyxt/mode/history:history-forwards
             (history-binding keyscheme:default "M-]"))
  (assert-eq 'nyxt/mode/history:history-backwards
             (history-binding keyscheme:emacs "C-b"))
  (assert-eq 'nyxt/mode/history:history-forwards
             (history-binding keyscheme:emacs "C-f"))
  (assert-eq 'nyxt/mode/history:history-backwards
             (history-binding keyscheme:vi-normal "H"))
  (assert-eq 'nyxt/mode/history:history-forwards
             (history-binding keyscheme:vi-normal "L")))
