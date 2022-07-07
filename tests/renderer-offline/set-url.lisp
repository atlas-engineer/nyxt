;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(plan nil)

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

(subtest "set offline url"
  (let ((url-channel (nyxt::make-channel 1))
        (url "nyxt:about"))
    (setf nyxt::*headless-p* t)
    (hooks:once-on nyxt:*after-startup-hook* ()
      (hooks:once-on (prompt-buffer-ready-hook *browser*)
          (prompt-buffer)
        (prompter:all-ready-p prompt-buffer)
        (nyxt:set-prompt-buffer-input url prompt-buffer)
        (prompter:all-ready-p prompt-buffer)
        (hooks:once-on (buffer-loaded-hook (current-buffer)) buffer
          (calispel:! url-channel (nyxt:render-url (nyxt:url buffer))))
        (nyxt/prompt-buffer-mode:return-selection prompt-buffer))
      (run-thread "run set-url"
        ;; TODO: Test if thread returns.
        (nyxt:set-url)))
    (nyxt:start :no-config t :no-auto-config t
                :socket "/tmp/nyxt-test.socket"
                :profile "test")
    (prove:is (calispel:? url-channel 5) url)
    (quit)))

(finalize)
