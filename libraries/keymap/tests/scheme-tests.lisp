;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :keymap/tests)

(prove:plan nil)

(prove:subtest "Make scheme"
  (let* ((scheme (keymap:define-scheme "test"
                   keymap/scheme:cua '("C-c" copy
                                "C-v" paste)))
         (keymap (keymap:make-keymap "test-cua-map")))
    (keymap:define-key keymap "C-c" 'copy)
    (keymap:define-key keymap "C-v" 'paste)
    (prove:is (fset:convert 'fset:map (keymap:keymap->map (gethash keymap/scheme:cua scheme)))
              (fset:convert 'fset:map (keymap:keymap->map keymap))
              :test #'fset:equal?)
    (prove:is (keymap:name (gethash keymap/scheme:cua scheme))
              (keymap:name keymap))))

(prove:subtest "Make scheme with LIST"
  (let* ((scheme (keymap:define-scheme "test"
                     keymap/scheme:cua (list "C-c" 'copy
                                      "C-v" 'paste)))
         (keymap (keymap:make-keymap "test-cua-map")))
    (keymap:define-key keymap
      "C-c" 'copy
      "C-v" 'paste)
    (prove:is (fset:convert 'fset:map (keymap:keymap->map (gethash keymap/scheme:cua scheme)))
              (fset:convert 'fset:map (keymap:keymap->map keymap))
              :test #'fset:equal?)))

(prove:subtest "Make scheme with multiple names"
  (let* ((scheme (keymap:define-scheme "test"
                     keymap/scheme:cua (list "C-c" 'copy
                                      "C-v" 'paste)
                   keymap/scheme:emacs (list "M-w" 'copy
                                      "M-y" 'paste)))
         (cua-keymap (keymap:make-keymap "test-cua-map"))
         (emacs-keymap (keymap:make-keymap "test-emacs-map")))
    (keymap:define-key cua-keymap
      "C-c" 'copy
      "C-v" 'paste)
    (keymap:define-key emacs-keymap
      "M-w" 'copy
      "M-y" 'paste)
    (prove:is (fset:convert 'fset:map (keymap:keymap->map (gethash keymap/scheme:cua scheme)))
              (fset:convert 'fset:map (keymap:keymap->map cua-keymap))
              :test #'fset:equal?)
    (prove:is (fset:convert 'fset:map (keymap:keymap->map (gethash keymap/scheme:emacs scheme)))
              (fset:convert 'fset:map (keymap:keymap->map emacs-keymap))
              :test #'fset:equal?)))

(prove:subtest "Test inheritance"
  (let* ((scheme (keymap:define-scheme "test"
                     keymap/scheme:cua (list "C-c" 'copy
                                      "C-v" 'paste)
                   keymap/scheme:emacs (list "M-w" 'copy
                                      "M-y" 'paste)))
         (cua-keymap (keymap:make-keymap "test-cua-map"))
         (emacs-keymap (keymap:make-keymap "test-emacs-map")))
    (keymap:define-key cua-keymap
      "C-c" 'copy
      "C-v" 'paste)
    (keymap:define-key emacs-keymap
      "M-w" 'copy
      "M-y" 'paste)
    (prove:is (list (gethash keymap/scheme:cua scheme))
              (keymap:parents (gethash keymap/scheme:emacs scheme)))))

(prove:subtest "Get keymap"
  (let* ((scheme (keymap:define-scheme "test"
                   keymap/scheme:cua (list "C-c" 'copy
                                    "C-v" 'paste)
                   keymap/scheme:emacs (list "M-w" 'copy
                                      "M-y" 'paste))))
    (prove:ok (keymap:get-keymap keymap/scheme:emacs scheme))
    (prove:ok (keymap:get-keymap keymap/scheme:cua scheme))
    (prove:isnt (keymap:get-keymap keymap/scheme:cua scheme)
                (keymap:get-keymap keymap/scheme:emacs scheme))
    (prove:is (keymap:get-keymap keymap/scheme:cua scheme)
              (keymap:get-keymap keymap/scheme:vi-normal scheme))))

(prove:subtest "Prioritize scheme over parent."
  (let* ((scheme1 (keymap:define-scheme "test1"
                    keymap/scheme:cua (list "C-c" 'do-not-hit-me)
                    keymap/scheme:emacs (list "M-w" 'copy)))
         (scheme2 (keymap:define-scheme "test2"
                    keymap/scheme:emacs (list "C-c" 'hit-me))))
    (let ((keymaps (mapcar (lambda (scheme) (keymap:get-keymap keymap/scheme:emacs  scheme))
                           (list scheme1 scheme2))                   ))
      (prove:is (keymap:lookup-key  "C-c" keymaps)
                'hit-me))))

;; (prove:subtest "Make scheme with type errors" ; TODO: How do we test macro-expansion-time error?
;;   (prove:is-error (keymap:define-scheme
;;                       keymap/scheme:cua (list "C-" 'copy))
;;                   'type-error))

(prove:finalize)
