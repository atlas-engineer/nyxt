(in-package :cl-user)

(prove:plan nil)

(defvar *test-package-name* "hello")
(defvar *test-complex-package-name* "nyxt")
(defvar *multi-version-package-name* "linux-libre")

(prove:subtest "Package list"
  (prove:ok (< 0 (length (ospama:list-packages))))
  (prove:ok (typep (first (ospama:list-packages)) 'ospama:os-package)))

(prove:subtest "Find package"
  (prove:is (ospama:name (first (ospama:find-os-packages *test-package-name*)))
            *test-package-name*))

(prove:subtest "Find multiple package versions"
  (prove:ok (<= 2 (length (ospama:find-os-packages *multi-version-package-name*)))))

(prove:subtest "Package inputs"
  (let* ((pkg (first (ospama:find-os-packages *test-complex-package-name*)))
         (all-inputs (append
                      (ospama:inputs pkg)
                      (ospama:propagated-inputs pkg)
                      (ospama:native-inputs pkg))))
    (prove:ok (mapc #'ospama:find-os-packages all-inputs))))

(prove:subtest "List profiles"
  (prove:ok (uiop:directory-exists-p (first (ospama:list-profiles)))))

(prove:finalize)
