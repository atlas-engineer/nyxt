;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(plan nil)

(class-star:define-class nyxt-user::test-data-profile (nyxt:data-profile)
  ((nyxt:name :initform "test"))
  (:documentation "Test profile."))

(defmethod nyxt:expand-data-path ((profile nyxt-user::test-data-profile) (path nyxt:data-path))
  "Don't persist data"
  nil)

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

(subtest "set-url"
  (let ((url-channel (nyxt::make-channel 1))
        (url "nyxt:about"))
    (setf nyxt::*headless-p* t)
    (on nyxt:*after-startup-hook* ()
      (let ((buffer-loaded-channel (nyxt::make-channel 1)))
        (once-on (buffer-loaded-hook (current-buffer)) buffer
          (calispel:! buffer-loaded-channel t))
        (let ((thread (run-thread "run set-url"
                        (calispel:? buffer-loaded-channel 10)
                        (nyxt:set-url))))
          (declare (ignorable thread))
          (on (prompt-buffer-ready-hook *browser*)
              (prompt-buffer)
            (prompter:all-ready-p prompt-buffer)
            (nyxt:set-prompt-buffer-input url prompt-buffer)
            (prompter:all-ready-p prompt-buffer)
            (nyxt/prompt-buffer-mode:return-selection prompt-buffer)
            (once-on (buffer-loaded-hook (current-buffer)) buffer
              (calispel:! url-channel (nyxt:render-url (nyxt:url buffer)))))
          ;; (bt:join-thread thread) ; TODO: Make sure thread always returns.
          )))
    (nyxt:start :no-init t :no-auto-config t
                :socket "/tmp/nyxt-test.socket"
                :data-profile "test")
    (prove:is (calispel:? url-channel 10) url)))

(finalize)
