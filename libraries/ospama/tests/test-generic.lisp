(in-package :cl-user)

(prove:plan nil)

(defvar *test-package-name* "hello")

(prove:subtest "Package list"
  (prove:ok (< 0 (length (ospama:list-packages))))
  (prove:ok (typep (first (ospama:list-packages)) 'ospama:os-package)))

(prove:subtest "Find package"
  (prove:is (ospama:name (ospama:find-os-package *test-package-name*))
            *test-package-name*))

(prove:subtest "List profiles"
  (prove:ok (uiop:directory-exists-p (first (ospama:list-profiles)))))

(prove:finalize)
