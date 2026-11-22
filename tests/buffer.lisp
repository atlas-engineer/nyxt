;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test buffer-table ()
  (let* ((*browser* (make-instance 'browser))
         (first-buffer (make-instance 'buffer :id 1))
         (second-buffer (make-instance 'buffer :id 2)))
    (nyxt::buffer-set 1 first-buffer)
    (nyxt::buffer-set 2 second-buffer)
    (assert-eq first-buffer (nyxt::buffer-get 1))
    (assert-eq second-buffer (nyxt::buffer-get 2))
    ;; Higher ids come first.
    (assert-equal (list second-buffer first-buffer) (buffer-list))
    (nyxt::buffer-delete 1)
    (assert-false (nyxt::buffer-get 1))
    (assert-equal (list second-buffer) (buffer-list))
    (assert-true (nyxt::dead-buffer-p first-buffer))
    (assert-false (nyxt::dead-buffer-p second-buffer))))

(define-test panel-buffer-base-mode ()
  (let* ((*browser* (make-instance 'browser))
         (panel (make-instance 'panel-buffer)))
    (unwind-protect
         (progn
           (assert-equal '(panel-mode) (default-modes panel))
           (assert-true (find-mode panel 'panel-mode))
           (assert-false (find-mode panel 'base-mode))
           (assert-equal "unbound" (nyxt::binding-keys 'set-url :buffer panel))
           (assert-equal "Shift+=" (nyxt::binding-keys 'widen-panel :buffer panel))
           (assert-equal "hyphen" (nyxt::binding-keys 'narrow-panel :buffer panel))
           (assert-equal "Ctrl+w" (nyxt::binding-keys 'delete-panel-buffer :buffer panel))
           (assert-equal "Ctrl+space" (nyxt::binding-keys 'execute-command :buffer panel)))
      (nyxt::buffer-delete (id panel)))))
