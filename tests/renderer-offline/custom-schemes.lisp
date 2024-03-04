;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(nyxt:define-internal-scheme "test"
    (lambda (url)
      (declare (ignore url))
      (spinneret:with-html-string (:p "pass"))))

(define-test register-custom-scheme ()
  (let ((ready-channel (nyxt::make-channel 1)))
    (nyxt:start :no-config t :no-auto-config t :headless t
                :socket "/tmp/nyxt-test.socket" :profile "test")
    (with-current-buffer (nyxt:make-buffer-focus :url "test:test")
      (hooks:once-on (nyxt:buffer-loaded-hook (current-buffer)) (buffer)
        (calispel:! ready-channel t))
      (calispel:? ready-channel)
      (sleep 1)
      (assert-equal "pass"
                    (nyxt:ps-eval (ps:chain (nyxt/ps:qs document "p") inner-text))))
    (nyxt:quit)))

(nyxt:define-internal-scheme "iframe-embed"
    (lambda (url)
      (declare (ignore url))
      (spinneret:with-html-string
        (:script
         (ps:ps (defun check-iframe-loaded ()
                  (ps:let* ((iframe (nyxt/ps:qs document "iframe"))
                            (iframe-doc (or (ps:@ iframe content-document)
                                            (ps:@ iframe content-window document)))
                            (text-check (nyxt/ps:qs document "#text-check")))
                    (when (= (ps:@ iframe-doc ready-state) "complete")
                      (setf (ps:@ text-check inner-text) "iframe loaded")
                      (ps:return-from check-iframe-loaded t))
                    (ps:chain window (set-timeout check-iframe-loaded 100))))))
        (:body
         :onload "checkIframeLoaded"
         (:iframe :src "nyxt:new")
         (:span#text-check "")))))

(define-test nyxt-url-not-iframe-embeddable ()
  (let ((ready-channel (nyxt::make-channel 1)))
    (nyxt:start :no-config t
                :no-auto-config t
                :headless t
                :socket "/tmp/nyxt-test.socket"
                :profile "test")
    (with-current-buffer (nyxt:make-buffer-focus :url "iframe-embed:test")
      (hooks:once-on (nyxt:buffer-loaded-hook (current-buffer)) (buffer)
        (calispel:! ready-channel t))
      (calispel:? ready-channel)
      ;; More than enough time for nyxt:new to load and check-iframe-loaded to fire.
      (sleep 1)
      (assert-equal ""
                    (nyxt:ps-eval (ps:chain (nyxt/ps:qs document "#text-check") inner-text))))
    (nyxt:quit)))
