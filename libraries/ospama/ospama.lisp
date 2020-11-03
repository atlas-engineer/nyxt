(in-package :ospama)

(define-class os-package ()
  ((name "")
   (version "")
   (dependencies '())
   (synopsis "")
   (home-page "")
   (licenses '()))
  (:accessor-name-transformer #'class*:name-identity))

(export-always '*manager*)
(defvar *manager* nil
  "The currently selected package manager.")

(defun run-over-packages (argument-method package-list)
  (multiple-value-bind (pre-args post-args)
      (funcall argument-method *manager*)
    (uiop:run-program
     (append
      pre-args
      (mapcar (lambda (pkg) (name (find-os-package *manager* pkg))) package-list)
      post-args)
     :output '(:string :stripped t) )))

(defun install (package-list)
  (multiple-value-bind (pre-args post-args)
      (install-command *manager*)
    (uiop:run-program
     (append
      (list pre-args)
      (mapcar #'name package-list)
      (list post-args)))))

(defun show (package-list)
  (run-over-packages #'show-command package-list))
