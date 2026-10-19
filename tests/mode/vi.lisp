;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-vi-modes ()
  (let* ((buffer (make-instance 'input-and-modable-buffer))
         (default-keyscheme (nkeymaps:name (keyscheme buffer))))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/vi:vi-normal-mode buffer))
      (assert-string= "vi-normal"
                      (nkeymaps:name (keyscheme buffer)))
      (assert-true (disable-modes* 'nyxt/mode/vi:vi-normal-mode buffer))
      (assert-string= default-keyscheme
                      (nkeymaps:name (keyscheme buffer)))

      (assert-true (enable-modes* 'nyxt/mode/vi:vi-insert-mode buffer))
      (assert-string= "vi-insert"
                      (nkeymaps:name (keyscheme buffer)))
      (assert-true (disable-modes* 'nyxt/mode/vi:vi-insert-mode buffer))
      (assert-string= default-keyscheme
                      (nkeymaps:name (keyscheme buffer))))))

(defun vi-binding (mode-class keyscheme keyspec)
  (let ((mode (make-instance mode-class
                             :buffer (make-instance 'input-and-modable-buffer))))
    (keymaps:lookup-key keyspec
                        (keymaps:get-keymap keyscheme (keyscheme-map mode)))))

(define-test vi-mode-keyscheme-bindings ()
  (assert-eq 'nyxt/mode/vi:vi-insert-mode
             (vi-binding 'nyxt/mode/vi:vi-normal-mode keyscheme:vi-normal "i"))
  (assert-eq 'nyxt/mode/vi:unfocus-current-element
             (vi-binding 'nyxt/mode/vi:vi-normal-mode keyscheme:vi-normal "q"))
  (assert-eq 'nyxt/mode/visual:visual-mode
             (vi-binding 'nyxt/mode/vi:vi-normal-mode keyscheme:vi-normal "v"))
  (assert-eq 'nyxt/mode/passthrough:passthrough-mode
             (vi-binding 'nyxt/mode/vi:vi-insert-mode keyscheme:vi-insert "C-z"))
  (assert-eq 'nyxt/mode/vi:switch-to-vi-normal-mode
             (vi-binding 'nyxt/mode/vi:vi-insert-mode keyscheme:vi-insert "escape")))
