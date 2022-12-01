;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(define-test execute-command-1+ ()
  (let ((old-headless-p nyxt::*headless-p*))
    (unwind-protect
         (let ((channel (nyxt::make-channel 1)))
           (setf nyxt::*headless-p* t)
           (hooks:once-on nyxt:*after-startup-hook* ()
             (hooks:once-on (nyxt:prompt-buffer-ready-hook nyxt:*browser*)
                 (prompt-buffer)
               (prompter:all-ready-p prompt-buffer)
               (nyxt:set-prompt-buffer-input "1+ 3" prompt-buffer)
               (prompter:all-ready-p prompt-buffer)
               (nyxt/prompt-buffer-mode:select-last prompt-buffer)
               (nyxt/prompt-buffer-mode:select-previous prompt-buffer)
               (calispel:! channel (nyxt:current-suggestion-value prompt-buffer))
               (nyxt/prompt-buffer-mode:cancel-input prompt-buffer))
             (nyxt:run-thread "run execute-command"
               (let ((nyxt::*interactive-p* t))
                 (nyxt:execute-command))))
           (nyxt:start :no-config t :no-auto-config t
                       :socket "/tmp/nyxt-test.socket"
                       :profile "test")
           (let ((result (calispel:? channel)))
             (assert-eq '1+ (first result))
             (assert-eq 3 (second result)))
           (nyxt:quit))
      (setf nyxt::*headless-p* old-headless-p))))
