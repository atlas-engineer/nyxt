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
        (url "http://example.org/"))
    (setf nyxt::*headless-p* t)
    (nhooks:add-hook
     nyxt:*after-startup-hook*
     (make-instance
      'nhooks:handler
      :fn (lambda ()
            (let ((buffer-loaded-channel (nyxt::make-channel 1)))
              (nhooks:add-hook
               (buffer-loaded-hook (current-buffer))
               (make-instance
                'nhooks:handler
                :fn (lambda (buffer)
                      (calispel:! buffer-loaded-channel t)
                      (nhooks:remove-hook (buffer-loaded-hook buffer) 'first-buffer-loaded))
                :name 'first-buffer-loaded))
              (let ((thread (bt:make-thread (lambda ()
                                              (calispel:? buffer-loaded-channel 10)
                                              (nyxt:set-url)))))
                (declare (ignorable thread))
                (calispel:? (nyxt::prompt-buffer-channel (nyxt:current-window)))
                (nyxt::update-prompt-input (nyxt:current-prompt-buffer) url)
                (prompter:all-ready-p (nyxt:current-prompt-buffer))
                (nyxt/prompt-buffer-mode:return-selection)
                (nhooks:add-hook
                 (buffer-loaded-hook (current-buffer))
                 (make-instance
                  'nhooks:handler
                  :fn (lambda (buffer)
                        (calispel:! url-channel (nyxt:render-url (nyxt:url buffer)))
                        (nhooks:remove-hook (buffer-loaded-hook buffer) 'example-org-loaded))
                  :name 'example-org-loaded))
                ;; (bt:join-thread thread) ; TODO: Make sure thread always returns.
                )))
      :name 'browser-started))
    (nyxt:start :no-init t :no-auto-config t
                :socket "/tmp/nyxt-test.socket"
                :data-profile "test")
    (prove:is (calispel:? url-channel 10) url)))

(finalize)
