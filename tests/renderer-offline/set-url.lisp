;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(defmacro with-prompt-buffer-test (command &body body)
  (alexandria:with-gensyms (thread)
    `(let ((,thread (bt:make-thread (lambda () ,command))))
       (calispel:? (prompt-buffer-channel (current-window)))
       ,@body
       (return-selection)
       (bt:join-thread ,thread))))

;; TODO: Use `with-prompt-buffer-test'.
;; (with-prompt-buffer-test (set-url)
;;   (update-prompt-input (current-prompt-buffer) "foobar"))

(defun test-set-url (url)
  (let ((old-headless-p nyxt::*headless-p*))
    (unwind-protect
         (let ((url-channel (nyxt::make-channel 1)))
           (setf nyxt::*headless-p* t)
           (hooks:once-on nyxt:*after-startup-hook* ()
             (hooks:once-on (nyxt:prompt-buffer-ready-hook nyxt:*browser*)
                 (prompt-buffer)
               (prompter:all-ready-p prompt-buffer)
               (nyxt:set-prompt-buffer-input url prompt-buffer)
               (prompter:all-ready-p prompt-buffer)
               (hooks:once-on (nyxt:buffer-loaded-hook (nyxt:current-buffer)) buffer
                 (calispel:! url-channel (nyxt:render-url (nyxt:url buffer))))
               (nyxt/prompt-buffer-mode:return-selection prompt-buffer))
             (nyxt:run-thread "run set-url"
               ;; TODO: Test if thread returns.
               (let ((nyxt::*interactive-p* t))
                 (nyxt:set-url))))
           (nyxt:start :no-config t :no-auto-config t
                       :socket "/tmp/nyxt-test.socket"
                       :profile "test")
           (assert-string= url (calispel:? url-channel 5))
           (nyxt:quit))
      (setf nyxt::*headless-p* old-headless-p))))

(define-test set-offline-url ()
  (test-set-url "nyxt:about"))
