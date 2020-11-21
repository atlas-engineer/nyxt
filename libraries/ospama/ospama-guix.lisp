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

(defmethod print-object ((obj (eql 'ospama::\#t)) stream)
  "Specialized printing of Scheme's #t for `cl->scheme-syntax'."
  (write-string "#t" stream))

(defmethod print-object ((obj (eql 'ospama::\#f)) stream)
  "Specialized printing of Scheme's #f for `cl->scheme-syntax'."
  (write-string "#f" stream))

(declaim (ftype (function (t) string)))
(defun cl->scheme-syntax (form)
  (str:replace-all
   ;; Backslashes in Common Lisp are doubled, unlike Guile.
   "\\\\" "\\"
   (write-to-string form)))

(defvar *guix-repl-idle-timeout* 60
  "Time in seconds of being idle before the `guix repl` is exited.")

(defvar *guix-command* "guix"
  "Name or path to the `guix' executable.")

(defvar %guix-listener-channel nil)
(defvar %guix-result-channel nil)

;; TODO: Make sure Guix process is closed when Lisp shuts down.

(defun guix-listener ()
  "Automatically start and quit the Guix REPL after
`*guix-repl-idle-timeout*'.
For each inputs on `%guix-listener-channel' a result is returned on
`%guix-result-channel'."
  (flet ((maybe-start-guix (guix-process)
           (unless (and guix-process
                        (uiop:process-alive-p guix-process))
             (setf guix-process
                   (uiop:launch-program `(,*guix-command* "repl" "--type=machine")
                                        :input :stream :output :stream))
             ;; Skip REPL header.
             ;; We could use `read' but CCL won't swallow the linebreak, while
             ;; SBCL will.
             (read-line (uiop:process-info-output guix-process) nil :eof))
           guix-process))
    (do ((guix-process nil)) (nil)
      (handler-case
          (do () (nil)
            (let ((input (bt:with-timeout (*guix-repl-idle-timeout*)
                           (chanl:recv %guix-listener-channel))))
              (setf guix-process (maybe-start-guix guix-process))
              ;; Append a newline so that REPL proceeds.
              (format (uiop:process-info-input guix-process) "~a~%" input)
              (finish-output (uiop:process-info-input guix-process))
              ;; We set the package to current so that symbols in
              ;;   (values (value ...) (value ...) ...)
              ;; do not get returned with a package prefix.
              (let* ((*package* (find-package :ospama))
                     (output (read-line (uiop:process-info-output guix-process)
                                        nil :eof)))
                ;; Use `read-line' to ensure we empty the output stream.
                ;; `read' errors could leave a truncated s-expression behind
                ;; which would prefix the next evaluation result.
                ;; TODO: Report read errors.
                (chanl:send %guix-result-channel
                            (ignore-errors
                              (named-readtables:in-readtable scheme-reader-syntax)
                              (prog1 (read-from-string output)
                                (named-readtables:in-readtable nil)))))))
        (t ()
          (close (uiop:process-info-input guix-process))
          (unless (uiop:process-alive-p guix-process)
            (uiop:terminate-process guix-process)))))))

(defun guix-eval (form &rest more-forms)
  "Evaluate forms in Guix REPL.
Return the REPL result of the last form.
On REPL exception (or unexpected form), return NIL and the form as second
value.
#<unspecified> objects are returns as NIL."
  (unless (and %guix-listener-channel %guix-result-channel)
    (setf %guix-listener-channel (make-instance 'chanl:channel))
    (setf %guix-result-channel (make-instance 'chanl:channel))
    (bt:make-thread #'guix-listener))
  ;; Need to be in this package to avoid prefixing symbols with current package.
  (let* ((*package* (find-package :ospama))
         (*print-case* :downcase)
         (all-forms (cons form more-forms))
         (ignore-result-forms (butlast all-forms))
         (final-form (first (last all-forms))))
    (dolist (form ignore-result-forms)
      (let ((input (cl->scheme-syntax form)))
        (chanl:send %guix-listener-channel input)
        ;; Discard result:
        (chanl:recv %guix-result-channel)))
    ;; Final form:
    (let ((input (cl->scheme-syntax final-form)))
      (chanl:send %guix-listener-channel input))
    (let ((repl-result (chanl:recv %guix-result-channel)))
      (match repl-result
        ((list* 'values values)
         (apply #'values
                (mapcar (lambda-match
                          ;; Calling `(use-packages (ice-9 match))` returns:
                          ;;   (values (non-self-quoting 2052 "#<unspecified>"))
                          ((list 'non-self-quoting _ desc)
                           ;; (values nil desc)
                           desc)
                          ;; Calling `(values 'a 'b)` returns:
                          ;;   (values (value a) (value b))
                          ((list 'value value)
                           value)
                          (other
                           ;; (values nil other)
                           other))
                        values)))
        (_
          ;; Error, or unexpected REPL result formatting.
         (values nil repl-result))))))

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
  (dolist (pair (package-output-paths (name pkg)))
    (setf (path (find (first pair) (outputs pkg) :key #'name :test #'string=))
          (rest pair))))

(export-always 'size)
(defmethod size ((output guix-package-output))
  "Size can only be computed once the OUTPUT has been expanded."
  (when (and (= 0 (slot-value output 'size))
             (expanded-output-p output))
    (setf (slot-value output 'size)
          (package-output-size (path output))))
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
          (mapcar #'make-guix-package (generate-database))))
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
               (list-installed profile)))
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
          (if profile
              ;; We need to read the symlink for
              ;; ~/.guix-profile and the Guix checkout profile
              ;; otherwise `generation-numbers' won't work.
              (generation-list (guix-expand-profile-symlink profile))
              (generation-list))))

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

(defmethod manager-uninstall ((manager guix-manager) package-output-list &optional profile)
  (run (append (uninstall-command manager profile)
               (mapcar (lambda (output)
                         (format nil "~a:~a"
                                 (name (parent-package output))
                                 (name output)))
                       package-output-list))))

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

;; TODO: Implement more Guix-specific commands:
;; - build
;; - edit
