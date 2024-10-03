;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/tests/renderer
  (:use :lisp-unit2))
(in-package :nyxt/tests/renderer)

(defmethod files:write-file ((profile nyxt:nyxt-profile) (file files:file) &key)
  "Don't persist test data."
  nil)

(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt:history-file))
  "Don't use any history."
  #p"")

(defmacro with-prompt-buffer-test (command &body body)
  (alexandria:with-gensyms (thread)
    `(let ((,thread (bt:make-thread (lambda () ,command))))
       (calispel:? (prompt-buffer-ready-channel (current-window)))
       ,@body
       (run-action-on-return)
       (bt:join-thread ,thread))))

(defun test-set-url (url)
  (let ((url-channel (nyxt::make-channel 1)))
    (nyxt:start :no-config t
                :no-auto-config t
                :headless t
                :socket "/tmp/nyxt-test.socket")
    (hooks:once-on (nyxt:prompt-buffer-ready-hook *browser*)
        (prompt-buffer)
      (prompter:all-ready-p prompt-buffer)
      (nyxt:set-prompt-buffer-input url prompt-buffer)
      (prompter:all-ready-p prompt-buffer)
      (hooks:once-on (nyxt:buffer-loaded-hook (nyxt:current-buffer)) buffer
        (calispel:! url-channel (nyxt:render-url (nyxt:url buffer))))
      (nyxt/mode/prompt-buffer:run-action-on-return prompt-buffer))
    (nyxt:run-thread "run set-url"
      ;; TODO: Test if thread returns.
      (nyxt:set-url))
    (assert-string= url (calispel:? url-channel 5))
    (nyxt:quit)))
