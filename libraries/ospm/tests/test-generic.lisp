;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :ospm/tests)

(prove:plan nil)

(defvar *test-package-name* "hello")
(defvar *test-complex-package-name* "nyxt")
(defvar *test-multi-version-package-name* "linux-libre")

(prove:subtest "Package list"
  (prove:ok (< 0 (length (ospm:list-packages))))
  (prove:ok (typep (first (ospm:list-packages)) 'ospm:os-package)))

(prove:subtest "Find package"
  (prove:is (ospm:name (first (ospm:find-os-packages *test-package-name*)))
            *test-package-name*))

(prove:subtest "Find multiple package versions"
  (prove:ok (<= 2 (length (ospm:find-os-packages *test-multi-version-package-name*)))))

(prove:subtest "Package inputs"
  (let* ((pkg (first (ospm:find-os-packages *test-complex-package-name*)))
         (all-inputs (append
                      (ospm:inputs pkg)
                      (ospm:propagated-inputs pkg)
                      (ospm:native-inputs pkg))))
    (prove:ok (mapc #'ospm:find-os-packages all-inputs))))

(prove:subtest "List profiles"
  (prove:ok (uiop:directory-exists-p (first (ospm:list-profiles)))))

(prove:finalize)
