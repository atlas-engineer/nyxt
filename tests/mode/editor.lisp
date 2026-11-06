;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test editor-css-hex-accepts-named-colors ()
  (assert-string= "#000000" (nyxt/mode/editor::css-hex "black"))
  (assert-string= "#FFFFFF" (nyxt/mode/editor::css-hex "#ffffff"))
  (assert-string= "#68686866" (nyxt/mode/editor::selection-color "#686868")))

(define-test editor-language-for-path ()
  (assert-string= "lisp"
                  (nyxt/mode/editor::language-for-path #p"/tmp/foo.lisp"))
  (assert-string= "python"
                  (nyxt/mode/editor::language-for-path #p"/tmp/foo.PY"))
  (assert-string= "plaintext"
                  (nyxt/mode/editor::language-for-path #p"/tmp/foo.unknown"))
  (assert-string= "plaintext"
                  (nyxt/mode/editor::language-for-path nil)))

(define-test editor-pathname-rejects-directories ()
  (assert-false (nyxt/mode/editor::editor-pathname nil))
  (assert-false (nyxt/mode/editor::editor-pathname ""))
  (assert-false (nyxt/mode/editor::editor-pathname #p"/tmp/"))
  (assert-equal "foo"
                (pathname-name (nyxt/mode/editor::editor-pathname #p"/tmp/foo.lisp"))))

(define-test editor-boot-script-is-next-to-the-source ()
  (assert-true (uiop:file-exists-p (nyxt/mode/editor::editor-boot-path)))
  (assert-true (search "assets/editor/"
                       (namestring (nyxt/mode/editor::editor-asset-root)))))

(define-test editor-asset-stays-inside-the-tree ()
  (assert-true (nyxt/mode/editor::editor-asset "vs/loader.js"))
  (assert-false (nyxt/mode/editor::editor-asset "../secret"))
  (assert-false (nyxt/mode/editor::editor-asset "vs/../../secret"))
  (assert-false (nyxt/mode/editor::editor-asset "vs/loader.js/../../secret")))

(define-test editor-file-round-trip ()
  (uiop:with-temporary-file (:pathname path)
    (nyxt/mode/editor::write-editor-file path "hello λ")
    (assert-string= "hello λ" (nyxt/mode/editor::read-editor-file path))))

(define-test editor-rejects-binary-files ()
  (uiop:with-temporary-file (:pathname path)
    (alexandria:write-byte-vector-into-file
     (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(65 0 66))
     path :if-exists :supersede)
    (assert-error 'error (nyxt/mode/editor::read-editor-file path))))

(define-test editor-assembles-chunks ()
  (let* ((buffer (make-instance 'input-and-modable-buffer))
         (mode (make-instance 'nyxt/mode/editor:editor-mode :buffer buffer))
         (octets (babel:string-to-octets "hello" :encoding :utf-8)))
    (assert-false (nyxt/mode/editor::accept-chunk
                   mode 1 2 0 (subseq octets 2)))
    (assert-string= "hello"
                    (nyxt/mode/editor::accept-chunk
                     mode 0 2 0 (subseq octets 0 2)))))

(define-test editor-mode-toggles-keys ()
  (let ((buffer (make-instance 'input-and-modable-buffer)))
    (unwind-protect
         (with-current-buffer buffer
           (enable-modes* 'nyxt/mode/editor:editor-mode buffer)
           (assert-true (forward-input-events-p buffer))
           (nyxt/mode/editor:toggle-editor-keys buffer)
           (assert-false (forward-input-events-p buffer))
           (assert-false (nyxt/mode/editor:keys-to-editor-p
                          (find-submode 'nyxt/mode/editor:editor-mode buffer)))
           (nyxt/mode/editor:toggle-editor-keys buffer)
           (assert-true (forward-input-events-p buffer)))
      (disable-modes* 'nyxt/mode/editor:editor-mode buffer))))

(define-test editor-keyscheme-bindings ()
  (let* ((buffer (make-instance 'input-and-modable-buffer))
         (editor (make-instance 'nyxt/mode/editor:editor-mode :buffer buffer)))
    (flet ((binding (scheme keyspec)
             (setf (keyscheme buffer) scheme)
             (keymaps:lookup-key keyspec (keymap editor))))
      (assert-eq 'nyxt/mode/editor:save-editor
                 (binding keyscheme:cua "C-s"))
      (assert-eq 'nyxt/mode/editor:edit-file
                 (binding keyscheme:cua "C-o"))
      (assert-eq 'nyxt/mode/editor:save-editor
                 (binding keyscheme:emacs "C-x C-s"))
      (assert-eq 'nyxt/mode/editor:edit-file
                 (binding keyscheme:emacs "C-x C-f"))
      (assert-eq 'nyxt/mode/editor:editor-find
                 (binding keyscheme:emacs "C-s"))
      (assert-eq 'nyxt/mode/editor:editor-move-down
                 (binding keyscheme:vi-normal "j"))
      (assert-eq 'nyxt/mode/editor:editor-ex
                 (binding keyscheme:vi-normal ":"))
      (assert-eq 'nyxt/mode/editor:save-editor
                 (binding keyscheme:vi-insert "C-s"))
      (assert-false (binding keyscheme:vi-insert "j"))
      (assert-eq 'nyxt/mode/editor:toggle-editor-keys
                 (binding keyscheme:emacs "C-M-z"))
      (assert-eq 'nyxt/mode/editor:toggle-editor-keys
                 (binding keyscheme:vi-normal "C-M-z")))))

(define-test editor-vi-normal-consumes-unbound-keys ()
  (let ((buffer (make-instance 'input-and-modable-buffer)))
    (enable-modes* 'nyxt/mode/editor:editor-mode buffer)
    (setf (keyscheme buffer) keyscheme:vi-normal)
    (assert-false (forward-input-events-p buffer))
    (assert-true (nyxt::consume-unbound-editor-key-p buffer))
    (setf (keyscheme buffer) keyscheme:emacs)
    (assert-true (forward-input-events-p buffer))
    (assert-false (nyxt::consume-unbound-editor-key-p buffer))))

(define-test editor-open-bindings ()
  (let* ((buffer (make-instance 'input-and-modable-buffer))
         (base (make-instance 'base-mode :buffer buffer)))
    (flet ((binding (keyscheme keyspec)
             (setf (keyscheme buffer) keyscheme)
             (keymaps:lookup-key keyspec (keymap base))))
      (assert-eq 'nyxt/mode/editor:edit
                 (binding keyscheme:emacs "C-x e"))
      (assert-eq 'nyxt/mode/editor:edit
                 (binding keyscheme:cua "M-e"))
      (assert-eq 'nyxt/mode/editor:edit
                 (binding keyscheme:vi-normal "E")))))
