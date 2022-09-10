;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defun list-dependencies (system)
  "Return SYSTEM dependencies.
If a dependency is part of the subsystem (i.e. they have the same .asd file),
they are not included but their dependencies are."
  (let ((root-location (asdf:system-source-file (asdf:find-system system))))
    (labels ((list-deps-recursively (deps)
               (when deps
                 (let* ((dep (first deps))
                        (location (asdf:system-source-file (asdf:find-system dep))))
                   (if (and location
                            (uiop:pathname-equal location root-location))
                       (append (list-deps-recursively
                                (asdf:system-depends-on (asdf:find-system dep)))
                               (list-deps-recursively (rest deps)))
                       (cons dep (list-deps-recursively (rest deps))))))))
      (let ((all-deps (asdf:system-depends-on (asdf:find-system system))))
        (delete-duplicates (list-deps-recursively all-deps) :test #'string=)))))

(defun redefinition-p (condition)       ; From Slynk.
  (and (typep condition 'style-warning)
       (every #'char-equal "redefin" (princ-to-string condition))))

#+ccl
(defun osicat-warning-p (condition)
  ;; Osicat triggers a warning on CCL because of some unimplemented chunk.
  ;; See https://github.com/osicat/osicat/issues/37.
  (and (typep condition 'style-warning)
       (search "Undefined function OSICAT::MAKE-FD-STREAM" (princ-to-string condition))))

(defun load-system-silently (system)
  (uiop:with-null-output (null-output)
    (let ((*standard-output* null-output))
      (asdf:load-system system))))

(defun compilation-conditions (system)
  (mapc #'load-system-silently (list-dependencies system))
  (let ((conditions '()))
    (handler-bind ((warning (lambda (c)
                              (unless (or (redefinition-p c)
                                          #+ccl
                                          (osicat-warning-p c))
                                (push c conditions)))))
      (asdf:load-system system :force t))
    (let ((report (mapcar (lambda (c) (format nil "~t~a~%" c))
                          (nreverse conditions))))
      (when report
        (format t "~&Found ~a warnings when loading ~s:~%~a"
                (length report) system report)
        (uiop:quit 19)))))
