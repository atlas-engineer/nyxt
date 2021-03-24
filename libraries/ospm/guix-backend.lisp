;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :ospm)

(named-readtables:in-readtable scheme-writer-syntax)

;; TODO: Add support for the '() syntax.
;; Workaround: use `(list)`.

;; TODO: With CCL keywords cannot have the same name as interned symbols.

(defvar %find-package
  ;; TODO: Use upstream's way to find packages.
  '(lambda* (name #:optional version)
    (let ((result (list)))
      (fold-packages
       (lambda (package count)
         (when (and (string=? (package-name package)
                              name)
                    (or (not version)
                        (string=? (package-version package)
                                  version)))
           (set! result package))
         (+ 1 count))
       1)
      result)))

(defvar %make-package
  '(lambda (package)
    (let ((loc (package-location package))
          (inputs->names (lambda (inputs)
                           (map package-name
                                ;; Input may be an `origin', not necessarily a package.
                                (filter package? (map cadr inputs))))))
      (list
       (package-name package)
       (list
        #:version (package-version package)
        #:outputs (package-outputs package)
        #:supported-systems (package-supported-systems package)

        #:inputs (inputs->names (package-inputs package))
        #:propagated-inputs (inputs->names (package-propagated-inputs package))
        #:native-inputs (inputs->names (package-native-inputs package))
        #:location (string-join (list (location-file loc)
                                      (number->string (location-line loc))
                                      (number->string (location-column loc)))
                                ":")
        ;; In Guix, an empty home-page is #f, but we want a string.
        #:home-page (or (package-home-page package) "")
        #:licenses (map license-name (ensure-list (package-license package)))
        #:synopsis (package-synopsis package)
        #:description (string-replace-substring (package-description package) "\\n" " "))))))

(defvar %make-output
  '(lambda (entry)
    (list
     (manifest-entry-name entry)
     (list
      #:version (manifest-entry-version entry)
      #:output (manifest-entry-output entry)
      ;; REVIEW: Would any of these ever be interesting to report to the user?
      ;; #:item (manifest-entry-item entry)
      ;; #:dependencies (manifest-entry-dependencies entry)
      ;; #:search-paths (manifest-entry-search-paths entry)
      ;; #:properties (manifest-entry-properties entry)
      ))))

;; TODO: Find a fast way to compute the output-paths.
(defun package-output-paths (name version)
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
      (mlet* %store-monad ((previous (set-grafting #f))
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

   `(package-output-paths (find-package ,name ,version))))

(defun package-output-size (output-path)
  (guix-eval
   '(use-modules
     (guix store)
     (guix monads))

   `(let ((path-info (with-store store
                       (run-with-store store
                                       (mbegin %store-monad
                                               (query-path-info* ,output-path))))))
      (if path-info
          (path-info-nar-size path-info)
          0))))

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

   `(define make-package ,%make-package)

   '(fold-packages
     (lambda (package result)
       (cons
        (make-package package)
        result))
     (list))))

(defun package-dependents (name)        ; TODO: Unused?
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
      #:select? (const #t)))

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

   `(map package-name
         (with-store store
           (run-with-store
            store
            (mbegin %store-monad
                    (list-dependents (list (find-package ,name)))))))))

(declaim (ftype (function (&optional (or symbol string pathname))) list-installed))
(defun list-installed (&optional (profile '%current-profile))
  "Return the installed package outputs in PROFILE as a list of (NAME (:VERSION :OUTPUT)).
PROFILE is a full path to a profile."
  (guix-eval
   '(use-modules
     (guix profiles))

   `(define make-output ,%make-output)

   `(map make-output
         (manifest-entries
          (profile-manifest ,(namestring profile))))))

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

     `(let loop ((generations (profile-generations ,profile)))
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
          (_ (list)))))))

