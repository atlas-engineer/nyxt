;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :ospama)

(define-class guix-manager (manager)
  ()
  (:export-class-name-p t))

(define-class guix-package-output (os-package-output)
  ((parent-package nil
                   :type (or null guix-package))
   (path ""
         :type (or string pathname)
         :documentation "The path is not automatically filled in `make-guix-package'.
Call `expand-outputs' to fill this field for all the outputs of a package.
Also see `expand-output-p' and `expand-outputs-p'.")
   (size 0
         :accessor nil
         :documentation "Apparent size in bytes of outputs, in order.
This can only be derived if `path' has been derived."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "OS package outputs are meaningful mostly for functional
package managers like Nix or Guix."))

(detect-manager "guix" 'guix-manager)

;; TODO: Call read-from-string in `guix-eval'?
(declaim (ftype (function (t) string)))
(defun cl->scheme-syntax (form)
  #+ccl
  ;; Escape symbols are printed as \\NAME while they should be printed as NAME.
  (str:replace-all "\\" "" (format nil "~s" form))
  #+(not ccl)
  ;; Escaped symbols (e.g. '\#t) are printed as '|NAME| but should be
  ;; printed as NAME.
  (ppcre:regex-replace-all "'\\|([^|]*)\\|" (format nil "~s" form) "\\1"))

(defun guix-eval (form &rest more-forms)
  ;; TODO: "guix repl" is a reliable way to execute Guix code, sadly it does not
  ;; seem to support standard input.  Report upstream?  Alternatively, use Guile
  ;; the way emacs-guix.el does it, but it does not seem reliable.
  "Evaluate forms in Guix REPL.
Return the REPL output (including the error output) as a string."
  (let ((*package* (find-package :ospama)) ; Need to be in this package to avoid prefixing symbols with current package.
        (*print-case* :downcase))
    (uiop:with-temporary-file (:pathname p)
      (with-open-file (s p :direction :output :if-exists :append)
        (dolist (f (cons form more-forms))
          (write-string
           ;; Backslashes in Common Lisp are doubled, unlike Guile.
           (str:replace-all
            "\\\\" "\\"
            (cl->scheme-syntax f))
           s)))
      (uiop:run-program `(,(path *manager*) "repl" ,(namestring p))
                        :output '(:string :stripped t)
                        :error-output :output))))

(defvar %find-package
  ;; TODO: Use upstream's way to find packages.
  '(lambda (name)
    (let ((result (list)))
      (fold-packages
       (lambda (package count)
         (when (string=? (package-name package)
                         name)
           (set! result package))
         (+ 1 count))
       1)
      result)))

;; TODO: Find a fast way to compute the output-paths.
(defun package-output-paths (name)
  "Computing the output-paths in `generate-database' is too slow, so we do it
just-in-time instead."
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

   `(write (package-output-paths (find-package ,name)))))

(defun package-output-size (output-path)
  (guix-eval
   '(use-modules
     (guix store)
     (guix monads))

   `(write
     (let ((path-info (with-store store
                        (run-with-store store
                                        (mbegin %store-monad
                                                (query-path-info* ,output-path))))))
       (if path-info
           (path-info-nar-size path-info)
           0)))))

(defun generate-database ()
  (guix-eval
   '(use-modules
     (guix packages)
     (guix licenses)
     (guix utils)
     (guix build utils)                 ; For `string-replace-substring'.
     (gnu packages))

   '(define (ensure-list l)
     (if (list? l)
         l
         (list l)))

   '(format '\#t "(~&")
   '(fold-packages
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
                 (or (package-home-page package) "") ; #f must be turned to NIL for Common Lisp.
                 (map license-name (ensure-list (package-license package)))
                 (package-synopsis package)
                 (string-replace-substring (package-description package) "\\n" " ")))
       (+ 1 count))
     1)
   '(format '\#t "~&)~&")))

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

   `(write
     (map package-name
          (with-store store
            (run-with-store
             store
             (mbegin %store-monad
                     (list-dependents (list (find-package ,name))))))))))

(declaim (ftype (function (&optional (or symbol string pathname))) list-installed))
(defun list-installed (&optional (profile '%current-profile))
  "Return the installed package outputs in PROFILE as a list of (NAME OUTPUT).
PROFILE is a full path to a profile."
  (guix-eval
   '(use-modules
     (guix profiles))
   `(write
     (map (lambda (entry)
            (list (manifest-entry-name entry)
                  (manifest-entry-output entry)))
          (manifest-entries
           (profile-manifest ,(namestring profile)))))))

(defun generation-list (&optional (profile '%current-profile))
  "Return the generations in PROFILE as a list of
(NUMBER CURRENT? PACKAGES TIME FILENAME).
PROFILE is a full path to a profile.
Date is in the form 'Oct 22 2020 18:38:42'."
  (let ((profile (if (pathnamep profile)
                     (namestring profile)
                     profile)))
    (guix-eval
     '(use-modules
       (ice-9 match)
       (srfi srfi-19)
       (guix profiles))
     `(write
       (let loop ((generations (profile-generations ,profile)))
         (match generations
           ((number . rest)
            (cons (list number
                        (if (= (generation-number ,profile) number)
                            't
                            'nil)
                        (length (manifest-entries
                                 (profile-manifest
                                  (generation-file-name ,profile number))))
                        (date->string
                         (time-utc->date
                          (generation-time ,profile number))
                         ;; ISO-8601 date/time
                         "~5")
                        (generation-file-name ,profile number))
                  (loop rest)))
           (_ '())))))))

(define-class guix-package (os-package)
  ((outputs '())
   (supported-systems '())
   (inputs '())
   (propagated-inputs '())
   (native-inputs '())
   (location "")
   (description ""))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'class*:name-identity))

(export-always 'expanded-output-p)
(defun expanded-output-p (output)
  "Return nil if package OUTPUT location hasn't been computed."
  (not (uiop:emptyp (path output))))

(export-always 'expanded-outputs-p)
(defun expanded-outputs-p (pkg)
  "Return nil if PKG outputs haven't been computed."
  (expanded-output-p (first (outputs pkg))))

(export-always 'expand-outputs)
(defun expand-outputs (pkg)
  "Compute the output locations of PKG."
  (dolist (pair (read-from-string (package-output-paths (name pkg))))
    (setf (path (find (first pair) (outputs pkg) :key #'name :test #'string=))
          (rest pair))))

(export-always 'size)
(defmethod size ((output guix-package-output))
  "Size can only be computed once the OUTPUT has been expanded."
  (when (and (= 0 (slot-value output 'size))
             (expanded-output-p output))
    (setf (slot-value output 'size)
          (read-from-string (package-output-size (path output)))))
  (slot-value output 'size))

(defun make-guix-package (entry)
  (let* ((name (first entry))
         (properties (second entry))
         (result (apply #'make-instance 'guix-package
                       :name name
                       (alexandria:mappend
                        (lambda (kw)
                          (list kw (getf properties kw)))
                        '(:version :supported-systems :inputs :propagated-inputs
                          :native-inputs :location :home-page :licenses :synopsis
                          :description)))))
    (setf (outputs result)
          (mapcar (lambda (output-name) (make-instance 'guix-package-output
                                                       :name output-name
                                                       :parent-package result))
                  (getf properties :outputs)))
    result))

(defvar *guix-database* nil)

(defun guix-database ()
  (unless *guix-database*
    (setf *guix-database*
          (mapcar #'make-guix-package (read-from-string (generate-database)))))
  *guix-database*)

(defmethod manager-find-os-package ((manager guix-manager) name)
  (find name (guix-database) :key #'name :test #'string=))

(defmethod manager-list-packages ((manager guix-manager) &optional profile)
  (if profile
      (delete nil
              (mapcar
               (lambda (name+output)
                 ;; name+output may be that of a channel, e.g. when profile is a Guix checkout.
                 ;; In this case, `find-os-package' may return nil.
                 ;; TODO: Should we return channel derivations as first class objects?
                 (serapeum:and-let* ((pkg (find-os-package (first name+output))))
                   (find (second name+output)
                         (outputs pkg)
                         :key #'name
                         :test #'string=)))
               (read-from-string (list-installed profile))))
      (guix-database)))

(defmethod manager-list-package-outputs ((manager guix-manager))
  (alexandria:mappend #'outputs (list-packages)))

(defmethod manager-list-profiles ((manager guix-manager) &key include-manager-p)
  (let ((all-profiles (str:split
                       (string #\newline)
                       (uiop:run-program
                        (list (path manager) "package" "--list-profiles")
                        :output '(:string :stripped t)))))
    (if include-manager-p
        all-profiles
        (delete (namestring (uiop:xdg-config-home "guix/current"))
                all-profiles
                :test #'string=))))

(defun make-generation (id current? package-count date path)
  (make-instance 'os-generation :id id
                                :current? current?
                                :package-count package-count
                                :date (local-time:parse-timestring date)
                                :path path))

(defun guix-expand-profile-symlink (profile)
  "Return the path the PROFILE link points too.
This function is mostly useful for the standard profile and the Guix checkout profile.

If the result is not absolute, return PROFILE untouched.  This is what we want
for non-standard profiles."
  (let ((result (osicat:read-link profile)))
    (if (uiop:relative-pathname-p result)
        profile
        result)))

(defmethod manager-list-generations ((manager guix-manager) &optional profile)
  (mapcar (lambda (args) (apply #'make-generation args))
          (read-from-string (if profile
                                ;; We need to read the symlink for
                                ;; ~/.guix-profile and the Guix checkout profile
                                ;; otherwise `generation-numbers' won't work.
                                (generation-list (read-symlink profile))
                                (generation-list)))))

(defmethod manager-switch-generation ((manager guix-manager) (generation os-generation)
                                      &optional profile)
  (run (append (list (path manager) "package"
                     (format nil "--switch-generation=~a" (id generation)))
               (when profile
                 (list (str:concat "--profile=" (namestring profile)))))))

(defmethod manager-delete-generations ((manager guix-manager) generations
                                       &optional profile)
  (run (append (list (path manager) "package"
                     (format nil "--delete-generations=~{~a~^,~}"
                             (mapcar #'id generations)))
               (when profile
                 (list (str:concat "--profile=" (namestring profile)))))))

(defmethod refresh ((manager guix-manager)) ; TODO: Unused?
  (declare (ignore manager))
  (setf *guix-database* nil))

(defmethod install-command ((manager guix-manager) profile)
  (append (list (path manager) "install")
          (when profile
            (list (str:concat "--profile=" (namestring profile))))))

(defmethod manager-install-manifest ((manager guix-manager) manifest &optional profile)
  (run (append (list (path manager) "package"
                     (str:concat "--manifest=" (namestring manifest)))
               (when profile
                 (list (str:concat "--profile=" (namestring profile)))))))

(defmethod uninstall-command ((manager guix-manager) profile)
  (append (list (path manager) "remove")
          (when profile
            (list (str:concat "--profile=" (namestring profile))))))

(defmethod manager-list-files ((manager guix-manager) outputs)
  (alexandria:mappend
   (lambda (output)
     (unless (expanded-output-p output)
       (expand-outputs (parent-package output)))
     (when (uiop:directory-exists-p (path output))
       (list-files-recursively (path output))))
   outputs))

;; TODO: Write tests.
;; TODO: Guix special commands:
;; - build
;; - edit
;; - list-generations (guix pull -l + guix package -l)
;;   guix package  -p /home/ambrevar/.config/guix/current -l
;;   ui.scm: display-profile-content
;; - list-installed from profile?
