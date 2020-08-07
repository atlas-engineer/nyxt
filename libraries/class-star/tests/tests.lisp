(in-package :cl-user)

(prove:plan nil)

(prove:subtest "Simple class"
  (prove:is (progn
              (class*:define-class foo ()
                ((name "fooname")))
              (let ((foo (make-instance 'foo)))
                (name-of foo)))
            "fooname"))

(prove:subtest "Simple class with custom accessors"
  (class*:define-class bar ()
    ((name "fooname")
     (age :accessor this-age)
     (address :accessor nil))
    (:accessor-name-transformer (lambda (name def) (declare (ignore def)) name)))
  (make-instance 'bar)
  (prove:is (fboundp 'name) t)
  (prove:is (fboundp 'this-age) t)
  (prove:is (fboundp 'address) nil))

(prove:subtest "Simple class default value"
  (prove:is (progn
              (class*:define-class foo-default ()
                ((name :type string)
                 (age :type number)))
              (let ((foo (make-instance 'foo-default)))
                (name-of foo)))
            ""))

;; TODO: Fix following test and try to make it portable.
#+nil
(prove:subtest "No initarg"
  (prove:is-error (let ((hu.dwim.defclass-star:*automatic-initargs-p* nil))
                    (class*:define-class foo-no-initarg ()
                      ((name :type string)))
                    (make-instance 'foo-no-initarg :name "bar"))
                  'sb-pcl::initarg-error))

(prove:subtest "No accessor"
  (prove:is (progn
              (class*:define-class foo-no-accessors ()
                ((name-no-acc :type string))
                (:automatic-accessors-p nil))
              (let ((foo (make-instance 'foo-no-accessors)))
                (fboundp 'name-no-acc-of)))
            nil))

;; TODO: These cycle tests work if run at the top-level, but not within prove:subtest.

;; (prove:subtest "Cycle"
;;   (prove:is (progn
;;               (class*:define-class zorg ()
;;                 ((zslot "z")))
;;               (closer-mop:ensure-finalized (find-class 'zorg))
;;               (class*:define-class new-zorg (zorg)
;;                 ((aslot "a")))
;;               ;; (closer-mop:ensure-finalized (find-class 'new-zorg))
;;               (setf (find-class 'zorg) (find-class 'new-zorg))
;;               (class*:define-class new-zorg (zorg)
;;                 ((bslot "b")))
;;               (setf (find-class 'zorg) (find-class 'new-zorg))
;;               (let ((z (make-instance 'zorg)))
;;                 (list (zslot-of z)
;;                       (aslot-of z)
;;                       (bslot-of z))))
;;             (list "z" "a" "b")))

;; (prove:subtest "In-place replacement"
;;   (prove:is (progn
;;               (class*:define-class borg ()
;;                 ((bslot "b")))
;;               (format t "@@ ~a ~%" (find-class 'borg nil))
;;               (class*:define-class borg (borg)
;;                 ((new-slot "n")))
;;               (let ((b (make-instance 'borg)))
;;                 (list (bslot-of b)
;;                       (new-slot-of b))))
;;             (list "b" "n")))

(prove:finalize)
