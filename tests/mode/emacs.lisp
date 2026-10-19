;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-emacs-mode ()
  (let* ((buffer (make-instance 'input-and-modable-buffer))
         (default-keyscheme (nkeymaps:name (keyscheme buffer))))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/emacs:emacs-mode buffer))
      (assert-string= "emacs"
                      (nkeymaps:name (keyscheme buffer)))
      (assert-true (disable-modes* 'nyxt/mode/emacs:emacs-mode buffer))
      (assert-string= default-keyscheme
                      (nkeymaps:name (keyscheme buffer))))))

(define-test emacs-mode-uses-emacs-keyscheme ()
  (let ((mode (make-instance 'nyxt/mode/emacs:emacs-mode
                             :buffer (make-instance 'input-and-modable-buffer))))
    (assert-eq keyscheme:emacs (keyscheme mode))))
