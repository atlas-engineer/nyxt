(defpackage :next/build-rpm
  (:use :cl)
  (:import-from :asdf #:perform #:system)
  (:import-from :cffi-toolchain #:static-program-op)
  (:import-from :cffi
                #:foreign-library-pathname
                #:foreign-library-type
                #:list-foreign-libraries
                #:close-foreign-library)
  (:import-from :ppcre #:split)
  (:import-from :uiop
                #:run-program
                #:getenv
                #:register-image-dump-hook)
  (:import-from :next #:+version+))

(in-package :next/build-rpm)

(defun find-library-rpm-packages (ldconfig library)
  (with-input-from-string (s ldconfig)
    ;; skip the first line
    (read-line s)

    (loop for line = (read-line s nil 'eof)
       until (eq line 'eof)
       when (let ((parts (split " " (subseq line 1))))
              (string= (first parts) (namestring (foreign-library-pathname library))))
       collect (let ((parts (split " " (subseq line 1))))
                 (run-program
                  `("rpm" "-q" "--whatprovides" ,(first (last parts)) "--qf" "%{NAME}")
                  :output '(:string))))))

(defun find-rpm-packages (libraries)
  (let ((ldconfig (run-program "ldconfig -p" :output '(:string))))
    (delete-duplicates
     (reduce (lambda (packages library)
               (append
                packages
                (unless (eq (foreign-library-type library) :grovel-wrapper)
                  (find-library-rpm-packages ldconfig library))))
             libraries
             :initial-value nil)
     :test #'string=)))

;;; Make sure we close statically linked libraries.
;;; Remove when this or similar is done in cffi: https://github.com/cffi/cffi/pull/163
(register-image-dump-hook
 (lambda ()
   (loop for library in (list-foreign-libraries)
      when (eq (foreign-library-type library) :grovel-wrapper)
      do (close-foreign-library library))))

(defclass build-rpm (static-program-op)
  ())

(defmethod perform ((operation build-rpm) (system system))
  (call-next-method operation system)

  (let ((deps (delete-duplicates
               (append
                (find-rpm-packages (list-foreign-libraries))
                ;; sbcl images always depends on those
                #+sbcl '("zlib" "glibc")))))
    (run-program `("fpm" "-s" "dir"
                         "-t" "rpm"
                         "-n" "next"
                         "-v" ,+version+
                         ,@(mapcar (lambda (dep) (format nil "--depends=~a" dep)) deps)
                         "next=/usr/bin/"
                         "assets/next.desktop=/usr/share/applications/")
                 :output :interactive
                 :error-output :interactive)))

(setf (find-class 'asdf::build-rpm) (find-class 'build-rpm))
