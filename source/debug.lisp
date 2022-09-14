;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/debug ; TODO: Rename to `nyxt/develop' to avoid confusion with `debugger'?
  (:use :common-lisp)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum #:export-always #:->)
  (:documentation "Development helpers"))
(in-package :nyxt/debug)
(nyxt:use-nyxt-package-nicknames)

(defvar *all-nyxt-objects* '()
  "Weak list of all Nyxt objects.
This list is populated on new object instantiation when object tracking is
enabled with `toggle-object-tracking'.
Also see `cleanup-all-nyxt-objects'.")

(defun toggle-object-tracking ()
  "When enabled, collect all newly instantiated Nyxt objects into `*all-nyxt-objects*'."
  (alex:if-let ((method (find-method #'initialize-instance '(:after) (list t) nil)))
    (progn
      (remove-method #'initialize-instance method)
      (log:info "Nyxt object are no longer tracked."))
    (progn
      (defmethod initialize-instance :after ((object t) &key)
        (when (member (symbol-package (sera:class-name-of object))
                      (list (find-package :nyxt)
                            (find-package :nyxt-user)))
          (push (tg:make-weak-pointer object) *all-nyxt-objects*)))
      (log:info "New Nyxt objects are now tracked in `*all-nyxt-objects*'."))))

(defun cleanup-all-nyxt-objects ()
  "Remove broken pointers from `*all-nyxt-objects*'.
You may want to run `tg:gc' beforehand, or even:

  (tg:gc :full t)

If we were to use a weak hash table for `*all-nyxt-objects*', the clean up would
be done automatically, but then how would we access the weak pointers?


`*all-nyxt-objects*' must not be altered when this runs." ; TODO: Lock?
  (let ((all-nyxt-objects *all-nyxt-objects*)) ; Just make sure we are handling a consistent object.
    (maplist (lambda (l)
               (when (and (second l)
                          (null (tg:weak-pointer-value (second l))))
                 (setf (rest l) (rest (rest l)))))
             all-nyxt-objects)
    (setf *all-nyxt-objects*
          (if (tg:weak-pointer-value (first all-nyxt-objects))
              (rest all-nyxt-objects)
              all-nyxt-objects))))

(defun find-nyxt-objects (class-sym)
  "Return the list of all Nyxt object pointers of type CLASS-SYM.
Example:

  (sb-ext:search-roots (find-nyxt-objects 'web-buffer) :print :verbose)

Use `:print :verbose' is you want a human-readable overview.
See also `find-object-by-address' (SBCL only)."
  (sera:filter (lambda (object-pointer)
                 (eq class-sym
                     (sera:class-name-of (tg:weak-pointer-value object-pointer))))
               *all-nyxt-objects*))

#+sbcl
(defun find-object-by-address (address)
  "Return the object with ADDRESS.
As a second value, return the symbol bound to it, it any.
Prefix the ADDRESS number with the #x reader macro to provide an hexadecimal
address.
This is useful to inspect the objects reported by `sb-ext:search-roots'."
  (values
   (sb-kernel:make-lisp-obj address)
   (block sym
     (do-all-symbols (s)
       (when (and (boundp s)
                  (not (find s '(* ** *** - -- --- + ++ +++ / // ///)))
                  (= (sb-kernel:get-lisp-obj-address (symbol-value s))
                     address))
         (return-from sym s))))))

(-> system-depends-on-all ((or string asdf:system)) (cons string *))
(defun system-depends-on-all (system)
  "List SYSTEM dependencies recursively, even if SYSTEM is an inferred system.
Inspired by https://gitlab.common-lisp.net/asdf/asdf/issues/10#note_5018."
  (let (depends)
    (labels ((deps (system)
               "Return the list of system dependencies as strings."
               (mapcar (trivia:lambda-match
                         ((list _ s _)  ; e.g. (:VERSION "asdf" "3.1.2")
                          (princ-to-string s))
                         (s s))
                       (ignore-errors
                        (asdf:system-depends-on (asdf:find-system system nil)))))
             (subsystem? (system parent-system)
               "Whether PARENT-SYSTEM is a parent of SYSTEM following the naming convention.
For instance FOO is a parent of FOO/BAR."
               (alexandria:when-let ((match? (search system parent-system)))
                 (zerop match?)))
             (iter (systems)
               (cond
                 ((null systems)
                  depends)
                 ((subsystem? (first systems) system)
                  (iter (append (deps (first systems)) (rest systems))))
                 ((find (first systems) depends :test 'equalp)
                  (iter (rest systems)))
                 (t
                  (when (asdf:find-system (first systems) nil)
                    (push (first systems) depends))
                  (iter (union (rest systems) (deps (first systems))))))))
      (iter (list (if (typep system 'asdf:system)
                      (asdf:coerce-name system)
                      system))))))
