(in-package :cl-user)

(prove:plan nil)

(defvar *test-package-name* "hello")
(defvar *test-complex-package-name* "nyxt")

(prove:subtest "Package list"
  (prove:ok (< 0 (length (ospama:list-packages))))
  (prove:ok (typep (first (ospama:list-packages)) 'ospama:os-package)))

(prove:subtest "Find package"
  (prove:is (ospama:name (ospama:find-os-package *test-package-name*))
            *test-package-name*))

(prove:subtest "Package inputs"
  (let* ((pkg (ospama:find-os-package *test-complex-package-name*))
         (all-inputs (append
                      (ospama:inputs pkg)
                      (ospama:propagated-inputs pkg)
                      (ospama:native-inputs pkg))))
    (prove:ok (mapc #'ospama:find-os-package all-inputs))))

(prove:subtest "List profiles"
  (prove:ok (uiop:directory-exists-p (first (ospama:list-profiles)))))

(prove:finalize)
