;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'funcall-safely)
(defun funcall-safely (f &rest args)
  "Like `funcall' except that if `*keep-alive*' is nil (e.g. the program is run
from a binary) then any condition is logged instead of triggering the debugger."
  (if *keep-alive*
      (apply f args)
      (handler-case
          (apply f args)
        (error (c)
          (log:error "In ~a: ~a" f c)
          nil))))

(defun REPLACEME-class-name (class-sym)
  (intern (str:concat "REPLACEME-" (string class-sym))
          (symbol-package class-sym)))

(defmacro define-REPLACEME-class (name &optional superclasses)
  "Define the REPLACEME class of NAME.
This helper function is useful to compose the customizations of a class.

This may be called multiple times.
NAME must be an existing class.
NAME is automatically append to SUPERCLASSES, so that REPLACEME-name inherits
from NAME last."
  (let ((REPLACEME-name (REPLACEME-class-name name))
        (superclasses-with-original (remove-duplicates
                                     (append superclasses (list name)))))
    `(progn
       (export-always ',REPLACEME-name (symbol-package ',REPLACEME-name))
       ;; Probably no need to call the defclass macro if we just need to
       ;; set the superclasses.
       (closer-mop:ensure-class ',REPLACEME-name
                                :direct-superclasses ',superclasses-with-original))))

(export-always '%slot-default)
(export-always 'define-configuration)
(defmacro define-configuration (name &body slots)
  "Helper macro to customize NAME class slots.

Classes can be modes or a core class like `browser', `buffer', `minibuffer',
`window'.  Note that the classes must _not_ be prefixed by 'REPLACEME-'.

The `%slot-default' variable is replaced by the slot initform.

Example that sets some defaults for all buffers:

\(define-configuration buffer
  ((status-buffer-height 24)
   (default-modes (append '(vi-normal-mode) %slot-default))))

In the above, `%slot-default' will be substituted with the default value of
`default-modes'.

To discover the default value of a slot or all slots of a class, use the
`describe-slot' or `describe-class' commands respectively.

Example to get the `blocker-mode' command to use a new default hostlists:

\(define-configuration nyxt/blocker-mode:REPLACEME-blocker-mode
  ((nyxt/blocker-mode:hostlists (append (list *my-blocked-hosts*) %slot-default))))

The above defines `nyxt/blocker-mode:REPLACEME-blocker-mode' to inherit from a
generated class containing the specialized hostlists and the original
`blocker-mode'."

  (let* ((final-name (REPLACEME-class-name name))
         (temp-name (gentemp (string final-name) (symbol-package name))))
    (dolist (name (list name final-name))
      (unless (find-class name nil)
        (error "define-configuration argument ~a is not a known class." name)))
    `(progn
       (define-class ,temp-name ()
         ,(loop with super-class = (closer-mop:ensure-finalized (find-class final-name))
                for slot in (first slots)
                for known-slot? = (find (first slot) (mopu:slot-names super-class))
                for initform = (and known-slot?
                                    (getf (mopu:slot-properties super-class (first slot))
                                          :initform))
                if known-slot?
                  collect (list (first slot)
                                :initform `(funcall (lambda (%slot-default)
                                                      (declare (ignorable %slot-default))
                                                      ,(cadr slot))
                                                    ,initform))
                else do
                  (log:warn "Undefined slot ~a in ~a" (first slot) final-name))
         (:accessor-name-transformer #'class*:name-identity))
       (define-REPLACEME-class ,name ,(cons temp-name
                                            (mapcar #'class-name
                                                    (mopu:direct-superclasses final-name)))))))

(export-always 'load-system)
(defun load-system (system)
  "Load Common Lisp SYSTEM.
Use Quicklisp if possible.
Return NIL if system could not be loaded and return the condition as a second value.

Initialization file use case:

(when (load-system :foo)
  (defun function-if-foo-is-found () ...))"
  (ignore-errors
   #+quicklisp
   (ql:quickload system :silent t)
   #-quicklisp
   (asdf:load-system system)))

(defun make-ring (&key (size 1000))
  "Return a new ring buffer."
  (containers:make-ring-buffer size :last-in-first-out))

(export-always 'trim-list)
(defun trim-list (list &optional (limit 100))
  (handler-case
      (if (< limit (length list))
          (nconc (sera:nsubseq list 0 (1- limit)) (list "…"))
          list)
    (error ()
      ;; Improper list.
      list)))

(export-always 'expand-path)
(declaim (ftype (function ((or null data-path)) (or string null)) expand-path))
(defun expand-path (data-path)
  "Return the expanded path of DATA-PATH or nil if there is none.
`expand-data-path' is dispatched against `data-path' and `*browser*'s
`data-profile' if `*browser*' is instantiated, `+default-data-profile+'
otherwise.
This function can be used on browser-less globals like `*init-file-path*'."
  (when data-path
    (the (values (or string null) &optional)
         (match (if *browser*
                    (expand-data-path (data-profile *browser*) data-path)
                    (expand-data-path +default-data-profile+ data-path))
           ("" nil)
           (m (uiop:native-namestring m))))))

(export-always 'with-data-file)
(defmacro with-data-file ((stream data-path &rest options) &body body)
  "Evaluate BODY with STREAM bound to DATA-PATH.
DATA-PATH can be a GPG-encrypted file if it ends with a .gpg extension.
If DATA-PATH expands to NIL or the empty string, do nothing.
OPTIONS are as for `open'.
Parent directories are created if necessary."
  `(let ((path (expand-path ,data-path)))
     (when path
       (with-maybe-gpg-file (,stream path ,@options)
         ,@body))))
