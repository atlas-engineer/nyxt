;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer/electron)

(defun wait-until (predicate &key (timeout 8.0) (interval 0.02))
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        when (funcall predicate) return t
        when (> (get-internal-real-time) deadline) return nil
        do (sleep interval)))

(defparameter *popup-test-pathname*
  (asdf:system-relative-pathname :nyxt "tests/renderer/electron/popup-parent.html"))

(defun popup-test-url ()
  (quri:uri
   (format nil "file://~a"
           (uiop:native-namestring *popup-test-pathname*))))

(define-test popup-lifecycle-preserves-opener ()
  (unwind-protect
       (progn
         (nyxt:start :no-config t
                     :no-auto-config t
                     :headless t
                     :socket "/tmp/nyxt-electron-test.socket")
         (let* ((parent (nyxt:current-buffer))
                (initial-count (length (nyxt:buffer-list))))
           (nyxt:ffi-buffer-load parent (popup-test-url))
           (assert-true
            (wait-until (lambda () (> (length (nyxt:buffer-list)) initial-count))))
           (let ((popup (find-if (lambda (buffer) (not (eq buffer parent)))
                                 (nyxt:buffer-list))))
             (assert-eq parent (nyxt/renderer/electron::popup-parent popup))
             (assert-string= "Popup" (nyxt:title popup)))
           (nyxt:ffi-buffer-evaluate-javascript-async
            parent "window.navigateFirstPopup()")
           (assert-true
            (wait-until (lambda () (string= "oauth-ok-1" (nyxt:title parent)))))
           (assert-true
            (wait-until (lambda () (= initial-count (length (nyxt:buffer-list))))))
           ;; The same opener must be able to create more than one popup.  The old
           ;; callback thread was destroyed after the first one.
           (assert-true
            (wait-until (lambda () (> (length (nyxt:buffer-list)) initial-count))))
           (assert-true
            (wait-until (lambda () (string= "oauth-ok-2" (nyxt:title parent)))))
           (assert-true
            (wait-until
             (lambda ()
               (and (= initial-count (length (nyxt:buffer-list)))
                    (eq parent (nyxt:current-buffer))))))
           ;; Deleting a live popup must close it, restore the opener, and release
           ;; the JavaScript global used to adopt the Electron view.
           (nyxt:ffi-buffer-evaluate-javascript-async
            parent "window.openPersistentPopup()")
           (assert-true
            (wait-until (lambda () (> (length (nyxt:buffer-list)) initial-count))))
           (let* ((popup (find-if (lambda (buffer) (not (eq buffer parent)))
                                  (nyxt:buffer-list)))
                  (remote-symbol (electron:remote-symbol popup)))
             (nyxt:ffi-buffer-delete popup)
             (assert-true
              (wait-until
               (lambda ()
                 (and (= initial-count (length (nyxt:buffer-list)))
                      (eq parent (nyxt:current-buffer))))))
             (assert-string=
              "undefined"
              (electron::message popup (format nil "typeof ~a" remote-symbol))))
             ;; Let Electron deliver the popup's asynchronous destroyed event.
             (sleep 0.2)))
    (when (and *browser* (slot-value *browser* 'nyxt::ready-p))
      (nyxt:quit))))
