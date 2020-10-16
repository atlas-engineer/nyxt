;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :ospama)

(defun guix-eval (form &rest more-forms)
  ;; TODO: "guix repl" is a reliable way to execute Guix code, sadly it does not
  ;; seem to support standard input.  Report upstream?  Alternatively, use Guile
  ;; the way emacs-guix.el does it, but it does not seem reliable.
  (let ((*package* (find-package :ospama)) ; Need to be in this package to avoid prefixing symbols with current package.
        (*print-case* :downcase))
    (uiop:with-temporary-file (:pathname p)
      (with-open-file (s p :direction :output :if-exists :append)
        (dolist (f (cons form more-forms))
          (write-string
           ;; Escaped symbols (e.g. '\#t) are printed as '|NAME| but should be
           ;; printed as NAME.
           (ppcre:regex-replace-all "'\\|([^|]*)\\|" (format nil "~s" f) "\\1")
           s)))
      (uiop:run-program `("guix" "repl" ,(namestring p))
                        :output '(:string :stripped t)
                        :error-output :output))))

(defvar %find-package
  ;; TODO: Use upstream's way to find packages.
  '(lamdbda (name)
    (let ((result (list)))
      (fold-packages
       (lambda (package count)
         (when (string=? (package-name package)
                         name)
           (set! result package))
         (+ 1 count))
       1)
      result)))

(defun package-output-paths (name)
  "Computing the output-paths in `generate-database' is too slow, so we do it just-in-time instead."
  (guix-eval
   '(use-modules
     (guix store)
     (guix derivations)
     (guix monads)
     (guix grafts)
     (guix gexp)
     (guix packages)
     (guix utils)
     (gnu packages))

   `(define find-package ,%find-package)

   '(define* (package-output-paths package)
     "Return store items, even if not present locally."
     (define (lower-object/no-grafts obj system) ; From (guix scripts weather)
      (mlet* %store-monad ((previous (set-grafting '\#f))
                           (drv (lower-object obj system))
                           (_ (set-grafting previous)))
       (return drv)))
     (with-store store
       (run-with-store store
        (mlet %store-monad ((drv (lower-object/no-grafts package (%current-system))))
         ;; Note: we don't try building DRV like 'guix archive' does
         ;; because we don't have to since we can instead rely on
         ;; substitute meta-data.
         (return
           (derivation->output-paths drv))))))

   `(display
     (with-output-to-string
         (lambda ()
           (format '\#t "~s" (package-output-paths (find-package ,name))))))))

(defun generate-database ()
  (guix-eval
   '(use-modules
     (guix packages)
     (guix licenses)
     (guix utils)
     (gnu packages))

   '(define (ensure-list l)
     (if (list? l)
         l
         (list l)))

   '(display
     (with-output-to-string
         (lambda ()
           (format '\#t "(~&")
           (fold-packages
            (lambda (package count)
              (let ((location (package-location package)))
                (format '\#t "(~s (:version ~s :outputs ~s :supported-systems ~s :inputs ~s :propagated-inputs ~s :native-inputs ~s :location ~s :home-page ~s :licenses ~s :synopsis ~s :description ~s))~&"
                        (package-name package)
                        (package-version package)
                        (package-outputs package)
                        (package-supported-systems package)
                        (map car (package-inputs package))
                        (map car (package-propagated-inputs package))
                        (map car (package-native-inputs package))
                        (string-join (list (location-file location)
                                           (number->string (location-line location))
                                           (number->string (location-column location)))
                                     ":")
                        (or (package-home-page package) 'nil) ; #f must be turned to NIL for Common Lisp.
                        (map license-name (ensure-list (package-license package)))
                        (package-synopsis package)
                        (package-description package)))
              (+ 1 count))
            1)
           (format '\#t "~&)~&"))))))

(defun package-dependents (name)
  ""
  (guix-eval
   '(use-modules
     (guix graph)
     (guix scripts graph)
     (guix store)
     (guix monads)
     (guix packages)
     (gnu packages))

   '(define (all-packages)                  ; From refresh.scm.
     "Return the list of all the distro's packages."
     (fold-packages (lambda (package result)
                      ;; Ignore deprecated packages.
                      (if (package-superseded package)
                          result
                          (cons package result)))
      (list)
      #:select? (const '\#t)))

   '(define (list-dependents packages)
     "List all the things that would need to be rebuilt if PACKAGES are changed."
     ;; Using %BAG-NODE-TYPE is more accurate than using %PACKAGE-NODE-TYPE
     ;; because it includes implicit dependencies.
     (define (full-name package)
      (string-append (package-name package) "@"
       (package-version package)))
     (mlet %store-monad ((edges (node-back-edges %bag-node-type
                                 (package-closure (all-packages)))))
      (let* ((dependents (node-transitive-edges packages edges))
             (covering   (filter (lambda (node)
                                   (null? (edges node)))
                                 dependents)))
        (return dependents))))

   `(define find-package ,%find-package)

   `(format '\#t "~s"
            (map package-name
                 (with-store store
                   (run-with-store
                    store
                    (mbegin %store-monad
                            (list-dependents (list (find-package ,name))))))))))

(define-class guix-package (os-package)
  ((outputs '())
   (supported-systems '())
   (inputs '())
   (propagated-inputs '())
   (native-inputs '())
   (location "")
   (description "")
   (output-paths '()
                 :accessor nil)))

(defmethod output-paths ((pkg guix-package))
  (unless (slot-value pkg 'output-paths)
    (setf (slot-value pkg 'output-paths)
          (package-output-paths (name pkg))))
  (slot-value pkg 'output-paths))

(defvar *guix-database* nil)

(defun guix-database ()
  (unless *guix-database*
    (setf *guix-database* (read-from-string (generate-database))))
  *guix-database*)

(defun make-guix-package (name &optional (pkg (second (assoc name (guix-database) :test #'string=))))
  (apply #'make-instance 'guix-package
         :name name
         (alexandria:mappend
          (lambda (kw)
            (list kw (getf pkg kw)))
          '(:version :outputs :supported-systems :inputs :propagated-inputs
            :native-inputs :location :home-page :licenses :synopsis
            :description))))

(defun database-entry->guix-package (entry)
  (make-guix-package (first entry) (second entry)))

(defmethod find-os-package ((manager (eql :guix)) name) ; TODO: Useless?
  (make-guix-package name))

(defmethod list-packages ((manager (eql :guix))) ; TODO: Rename `all-packages'?
  (mapcar #'database-entry->guix-package (guix-database)))

(defmethod refresh ((manager (eql :guix))) ; TODO: Unused?
  (declare (ignore manager))
  (setf *guix-database* nil))

(defmethod install-command ((manager (eql :guix)))
  (declare (ignore manager))
  '("guix" "install"))

(defmethod uninstall-command ((manager (eql :guix)))
  (declare (ignore manager))
  '("guix" "remove"))

(defmethod show-command ((manager (eql :guix))) ; TODO: Remove once tests are OK.
  (declare (ignore manager))
  '("guix" "show"))

(defmethod list-files ((manager (eql :guix)) package output)
  (flet (list-files-recursively (dir))
    (let ((result '())) (uiop:collect-sub*directories
                         dir (constantly t) (constantly t)
                         (lambda (dir)
                           (setf result (append (uiop:directory-files dir)
                                                result))))
      result)
    (list-files-recursively (assoc output (output-paths package) :test #'string=))))

(defmethod profile-install ((manager (eql :guix)) profile)
  (declare (ignore manager))
  (list "guix" "install" (str:concat "--profile=" profile)))

(defmethod size-command ((manager (eql :guix)))
  '("guix" "size"))

(defmethod size ((manager (eql :guix)) package)
  (run-over-packages #'size-command (list package)))

(defmethod list-profiles ((manager (eql :guix))) ; TODO: Rename `all-profiles'?
  (delete (namestring (uiop:xdg-config-home "guix/current"))
          (str:split
           (string #\newline)
           (uiop:run-program
            '("guix" "package" "--list-profiles")
            :output '(:string :stripped t)))
          :test #'string=))

;; TODO: Guix special commands:
;; - build
;; - edit
