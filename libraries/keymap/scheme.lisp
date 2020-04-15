(in-package :keymap)

(defclass scheme-name ()
  ((name :initarg :name
         :accessor name
         :type string
         :documentation "A scheme name.")
   (parents :initarg :parents
            :accessor parents
            :initform '()
            :type list-of-scheme-names
            :documentation "The list of parents.  When a scheme is defined, the
keymap parents are automatically set to the keymaps corresponding to the given
schemes.  See `define-scheme'.")))

(declaim (ftype (function (string &rest scheme-name) (values scheme-name &optional))
                make-scheme-name))
(defun make-scheme-name (name &rest parents)
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

(declaim (ftype (function (string scheme-name list &rest (or scheme list)) scheme) define-scheme*))
(defun define-scheme* (name-prefix name bindings &rest more-name+bindings-pairs)
  "Define scheme.
See `define-scheme' for the user-facing function."
  (let ((name+bindings-pairs (append (list name bindings) more-name+bindings-pairs))
        (scheme (make-hash-table :test #'equal)))
    (loop :for (name _ . rest) :on name+bindings-pairs :by #'cddr
          :do (setf (gethash name scheme) (make-keymap (format nil "~a-~a-map" name-prefix (name name)))))
    ;; Set parents now that all keymaps exist.
    (maphash (lambda (name keymap)
               (setf (parents keymap)
                     (mapcar (lambda (parent-name) (gethash parent-name scheme))
                             (parents name))))
             scheme)
    ;; Set bindings.
    (loop :for (name bindings . rest) :on name+bindings-pairs :by #'cddr
          :for keymap = (gethash name scheme)
          :do (loop :for (keyspecs bound-value . rest) :on bindings :by #'cddr
                    :do (define-key* keymap keyspecs bound-value))) ; TODO: Can we use define-key?
    scheme))

(defmacro define-scheme (name-prefix name bindings &rest more-name+bindings-pairs)
  "Return a scheme, a hash table with scheme NAMEs as key and their BINDINGS as value.
The keymap names are prefixed with NAME-PREFIX and suffixed with \"-map\".

This is a macro like `define-key' so that it can type-check the BINDINGS
keyspecs at compile-time.

Example:

  (define-scheme \"my-mode\"
    scheme:cua (list
                \"C-c\" 'copy
                \"C-v\" 'paste)

    scheme:emacs '(\"M-w\" copy
                   \"M-y\" paste))

`scheme:cua' is a parent of `scheme:emacs'; thus, in the above example, the Emacs keymap
will have the CUA keymap as parent.
The scheme keymaps are named \"my-mode-cua-map\" and \"my-mode-emacs-map\"."
  (let ((name+bindings-pairs (append (list name bindings) more-name+bindings-pairs)))
    (loop :for (_ quoted-bindings . rest) :on name+bindings-pairs :by #'cddr
          :for bindings = (rest quoted-bindings)
          :do (check-type bindings list)
          :do (loop :for (keyspecs _ . rest) :on bindings :by #'cddr
                    :do (check-type keyspecs (or keyspecs-type list))))
    `(progn
       (define-scheme* ,name-prefix ,name ,bindings ,@more-name+bindings-pairs))))

(declaim (ftype (function (scheme-name scheme) (or keymap null)) get-keymap))
(defun get-keymap (name scheme)
  "Return keymap corresponding to NAME in SCHEME.
Return nil if not found."
  (gethash name scheme))

(declaim (ftype (function (scheme-name keymap &rest list) scheme) make-scheme))
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
