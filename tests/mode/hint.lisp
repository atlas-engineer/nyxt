;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-hint-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/hint:hint-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/hint:hint-mode buffer)))))

(define-test hint-strings-are-prefix-free ()
  ;; Vimium's alphabet hints for three links and the alphabet "ab".
  (assert-equal '("aa" "b" "ab")
                (nyxt/mode/hint::generate-hint-strings 3 "ab"))
  (dolist (alphabet '("ab" "sadfjklewcmpgh" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (dolist (count '(0 1 2 3 8 20))
      (let ((hints (nyxt/mode/hint::generate-hint-strings count alphabet)))
        (assert-eq count (length hints))
        (dolist (left hints)
          (dolist (right hints)
            (unless (string= left right)
              (assert-false (str:starts-with-p right left)))))))))
