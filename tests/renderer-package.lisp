;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/tests/renderer
  (:use :lisp-unit2))
(in-package :nyxt/tests/renderer)

(define-class nyxt-user::test-profile (nyxt:nyxt-profile)
  ()
  (:documentation "Test profile."))

(defmethod files:write-file ((profile nyxt-user::test-profile) (file files:file) &key)
  "Don't persist test data."
  nil)

(defmethod files:resolve ((profile nyxt-user::test-profile) (file nyxt:history-file))
  "Don't use any history."
  #p"")

(defvar +test-root+ #p"/tmp/nyxt-tests/")

(defmethod files:resolve ((profile nyxt-user::test-profile) (file files:file))
  "Store all files in a temporary `+test-root+' directory."
  (files:join +test-root+  (call-next-method)))

(defmacro with-prompt-buffer-test (command &body body)
  (alexandria:with-gensyms (thread)
    `(let ((,thread (bt:make-thread (lambda () ,command))))
       (calispel:? (prompt-buffer-channel (current-window)))
       ,@body
       (run-action-on-return)
       (bt:join-thread ,thread))))

(defun test-set-url (url)
  (let ((url-channel (nyxt::make-channel 1)))
    (nyxt:start :no-config t :no-auto-config t
                :headless t
                :socket "/tmp/nyxt-test.socket"
                :profile "test")
    (hooks:once-on (nyxt:after-startup-hook *browser*) (browser)
      (hooks:once-on (nyxt:prompt-buffer-ready-hook browser)
          (prompt-buffer)
        (prompter:all-ready-p prompt-buffer)
        (nyxt:set-prompt-buffer-input url prompt-buffer)
        (prompter:all-ready-p prompt-buffer)
        (hooks:once-on (nyxt:buffer-loaded-hook (nyxt:current-buffer)) buffer
          (calispel:! url-channel (nyxt:render-url (nyxt:url buffer))))
        (nyxt/prompt-buffer-mode:run-action-on-return prompt-buffer))
      (nyxt:run-thread "run set-url"
        ;; TODO: Test if thread returns.
        (let ((nyxt::*interactive-p* t))
          (nyxt:set-url))))
    (assert-string= url (calispel:? url-channel 5))
    (nyxt:quit)))
