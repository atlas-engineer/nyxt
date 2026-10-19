;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(define-test load-nyxt-new ()
  (let ((ready-channel (nyxt::make-channel 1)))
    (nyxt:start :no-config t
                :no-auto-config t
                :headless t
                :socket "/tmp/nyxt-test.socket")
    (with-current-buffer (nyxt:make-buffer-focus :url "nyxt:new")
      (hooks:once-on (nyxt:buffer-loaded-hook (current-buffer)) (buffer)
        (lparallel.queue:push-queue t ready-channel))
      (lparallel.queue:pop-queue ready-channel)
      (assert-true (nyxt:internal-url-p (nyxt:url (nyxt:current-buffer)))))
    (nyxt:quit)))

(define-test reload-fires-buffer-loaded-hook ()
  (let ((ready-channel (nyxt::make-channel 1)))
    (nyxt:start :no-config t
                :no-auto-config t
                :headless t
                :socket "/tmp/nyxt-test.socket")
    (with-current-buffer (nyxt:make-buffer-focus :url "nyxt:new")
      (hooks:once-on (nyxt:buffer-loaded-hook (current-buffer)) (buffer)
        (lparallel.queue:push-queue t ready-channel))
      (lparallel.queue:pop-queue ready-channel)
      (let ((reloaded-channel (nyxt::make-channel 1)))
        (hooks:once-on (nyxt:buffer-loaded-hook (current-buffer)) (buffer)
          (lparallel.queue:push-queue t reloaded-channel))
        (nyxt:reload-current-buffer)
        (assert-true (lparallel.queue:try-pop-queue reloaded-channel :timeout 5))))
    (nyxt:quit)))
