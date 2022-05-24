;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :keymap)

(defclass scheme-name ()
  ((name :initarg :name
         :accessor name
         :type string
         :documentation "A scheme name.")
   ;; TODO: Remove `parents' and use superclasses instead.
   (parents :initarg :parents
            :accessor parents
            :initform '()
            :type list-of-scheme-names
            :documentation "The list of parents.  When a scheme is defined, the
keymap parents are automatically set to the keymaps corresponding to the given
schemes.  See `define-scheme'.")
   (bound-type :accessor bound-type
               :initarg :bound-type
               :initform *default-bound-type*
               :documentation
               "Type of the bound-value.
The type is enforced in `define-scheme' at macro-expansion time.
Type should allow `keymap's, so it should probably be in the form
\(or keymap NON-KEYMAP-BOUND-TYPE).")))

(defmethod print-object ((scheme-name scheme-name) stream)
  (print-unreadable-object (scheme-name stream :type t :identity t)
    (format stream "~a" (name scheme-name))))

(declaim (ftype (function (string &rest scheme-name) (values scheme-name &optional))
                make-scheme-name))
(defun make-scheme-name (name &rest parents)
  "Return a new `scheme-name' object.
The scheme name inherits from the optional PARENTS, ordered by priority.

Example:

  (defvar emacs (make-scheme-name \"emacs\" cua))

In the above, we define a new scheme name called `emacs` which inherits from the
existing name `cua`."
  (the (values scheme-name &optional)
       (make-instance 'scheme-name
                      :name name
                      :parents parents)))

(defun scheme-name-p (name)
  (typep name 'scheme-name))

(declaim (ftype (function (hash-table) boolean) scheme-p))
(defun scheme-p (scheme)
  (loop :for name :being :the hash-keys :in scheme :using (:hash-value keymap)
        :always (and (scheme-name-p name)
                     (keymap-p keymap))))

(deftype scheme ()
  `(and hash-table
        (satisfies scheme-p)))

(defun copy-scheme (scheme)
  (let ((new-scheme (make-hash-table :test #'equal)))
    (maphash (lambda (scheme-name keymap)
               (setf (gethash scheme-name new-scheme)
                     (copy-keymap keymap)))
             scheme)
    new-scheme))

(declaim (ftype (function (string (or null scheme) scheme-name list &rest (or scheme-name list)) scheme) define-scheme*))
(defun define-scheme* (name-prefix imported-scheme name bindings &rest more-name+bindings-pairs)
  "Define scheme.
See `define-scheme' for the user-facing function."
  (let ((name+bindings-pairs (append (list name bindings) more-name+bindings-pairs))
        (scheme (if imported-scheme
                    (copy-scheme imported-scheme)
                    (make-hash-table :test #'equal))))
    (unless imported-scheme
      (loop :for (name nil . nil) :on name+bindings-pairs :by #'cddr
            :do (setf (gethash name scheme)
                      (let ((new-keymap (make-keymap (format nil "~a-~a-map" name-prefix (name name)))))
                        (setf (bound-type new-keymap) (bound-type name))
                        new-keymap))))
    ;; Set parents now that all keymaps exist.
    (maphash (lambda (name keymap)
               (setf (parents keymap)
                     (delete nil
                             (mapcar (lambda (parent-name) (gethash parent-name scheme))
                                     (parents name)))))
             scheme)
    ;; Set bindings.
    (loop :for (name bindings . nil) :on name+bindings-pairs :by #'cddr
          :for keymap = (gethash name scheme)
          :do (loop :for (keyspecs bound-value . nil) :on bindings :by #'cddr
                    :do (define-key* keymap keyspecs bound-value))) ; TODO: Can we use define-key?
    scheme))

(defun check-plist (plist &rest keys)
  "Raise error if PLIST has keys not in KEYS."
  (let ((extra-keys)
        (all-keys))
    (alexandria:doplist (k v plist)
      (push k all-keys)
      (unless (member k keys)
        (push k extra-keys)))
    (if extra-keys
        (error "Allowed keys are ~a, got ~a." keys all-keys)
        t)))

(defun quoted-symbol-p (arg)
  (and (listp arg)
       (eq (first arg) 'quote)
       (= 2 (length arg))))

(defmacro define-scheme (scheme-specifier name bindings &rest more-name+bindings-pairs)
  "Return a scheme, a hash table with scheme NAMEs as key and their BINDINGS as value.
SCHEME-SPECIFIER is either a string or a plist in the form

  (:name-prefix NAME-PREFIX :import IMPORTED-SCHEME)

The keymap names are prefixed with NAME-PREFIX or SCHEME-SPECIFIER (if a string)
and suffixed with \"-map\".

This is a macro like `define-key' so that it can type-check the BINDINGS
keyspecs at compile-time.

Example:

  (define-scheme \"my-mode\"
    scheme:cua (list
                \"C-c\" 'copy
                \"C-v\" 'paste)

    scheme:emacs '(\"M-w\" copy
                   \"M-y\" paste))

`scheme:cua' and `scheme:emacs' are pre-defined scheme names.  To define a new
scheme name, see `make-scheme-name'.

`scheme:cua' is a parent of `scheme:emacs'; thus, in the above example, the Emacs keymap
will have the CUA keymap as parent.
The scheme keymaps are named \"my-mode-cua-map\" and \"my-mode-emacs-map\"."
  (let ((name+bindings-pairs (append (list name bindings) more-name+bindings-pairs))
        (name-prefix (if (stringp scheme-specifier)
                         scheme-specifier
                         (getf scheme-specifier :name-prefix)))
        (imported-scheme (unless (stringp scheme-specifier)
                           (getf scheme-specifier :import))))
    (unless (stringp scheme-specifier)
      (check-plist scheme-specifier :name-prefix :import))
    (loop :for (name quoted-bindings . nil) :on name+bindings-pairs :by #'cddr
          :for bindings = (rest quoted-bindings)
          :do (check-type (symbol-value name) scheme-name)
          :do (check-type bindings list)
          :do (loop :for (keyspecs bound-value . nil) :on bindings :by #'cddr
                    :do (check-type keyspecs (or keyspecs-type list))
                    :when (quoted-symbol-p bound-value)
                      :do (assert (typep (second bound-value) (bound-type (symbol-value name))) (bound-value)
                                  'type-error :datum (second bound-value) :expected-type (bound-type (symbol-value name)))))
    `(progn
       (define-scheme* ,name-prefix ,imported-scheme ,name ,bindings ,@more-name+bindings-pairs))))

(declaim (ftype (function (scheme-name scheme) (or keymap null)) get-keymap))
(defun get-keymap (name scheme)
  "Return keymap corresponding to NAME in SCHEME.
If no keymap is associated to NAME in SCHEME, try with NAME's `parents'.
For instance, if SCHEME has a `scheme:cua' keymap and no `scheme:emacs' keymap,
this function returns the `scheme:cua' keymap when NAME is `scheme:emacs'.
Return nil if nothing is found."
  (or (gethash name scheme)
      (when (parents name)
        (some (alexandria:rcurry #'get-keymap scheme) (parents name)))))

(declaim (ftype (function (scheme-name keymap &rest (or scheme-name keymap)) scheme) make-scheme))
(defun make-scheme (name keymap &rest more-name+keymap-pairs)
  "Return a new scheme associating NAME to KEYMAP.
With MORE-NAME+KEYMAP-PAIRS, include those names and keymaps as well.  This is
useful in complement to `define-scheme' to make a scheme with pre-existing
keymaps."
  (let ((scheme (make-hash-table :test #'equal))
        (name+keymap-pairs (append (list name keymap) more-name+keymap-pairs)))
    (loop :for (name keymap) :on name+keymap-pairs :by #'cddr
          :do (setf (gethash name scheme) keymap))
    scheme))
