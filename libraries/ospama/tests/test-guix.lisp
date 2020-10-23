(in-package :cl-user)

(prove:plan nil)

(setf ospama:*manager* (make-instance 'ospama:guix-manager :path "guix"))

(prove:subtest "Manager detection"
  (prove:is (ospama:path (ospama:manager))
            "guix"))

(prove:subtest "Package list"
  (prove:ok (< 0 (length (ospama:list-packages (ospama:manager)))))
  (prove:ok (typep (first (ospama:list-packages)) 'ospama:guix-package)))

(prove:subtest "Find package"
  (prove:is (ospama:name (ospama:find-os-package "nyxt"))
            "nyxt"))

(prove:finalize)
