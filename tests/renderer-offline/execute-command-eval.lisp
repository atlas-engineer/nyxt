;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(define-test execute-command-1+ ()
  (let ((channel (nyxt::make-channel 1)))
    (nyxt:start :no-config t
                :no-auto-config t
                :headless t
                :socket "/tmp/nyxt-test.socket")
    (hooks:once-on (nyxt:prompt-buffer-ready-hook nyxt:*browser*)
        (prompt-buffer)
      (prompter:all-ready-p prompt-buffer)
      (nyxt:set-prompt-buffer-input "1+ 3" prompt-buffer)
      (prompter:all-ready-p prompt-buffer)
      (nyxt/mode/prompt-buffer:last-suggestion prompt-buffer)
      (calispel:! channel (nyxt:current-suggestion-value prompt-buffer))
      (nyxt/mode/prompt-buffer:quit-prompt-buffer prompt-buffer))
    (nyxt:run-thread "run execute-command"
      (nyxt:execute-command))
    (let ((result (calispel:? channel)))
      (assert-eq '1+ (first result))
      (assert-eq 3 (second result))))
  (sleep 1)
  (nyxt:quit))
