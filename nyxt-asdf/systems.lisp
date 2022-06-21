;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt-asdf)

(export-always 'nyxt-system)
(defclass nyxt-system (asdf:system) ()
  (:documentation "Specialized systems for Nyxt."))
;; TODO: This is how `prove' does it, not very clean.
;; Alternatively, we could switch package in nyxt.asd, but this seems cumbersome too.
(import 'nyxt-system  :asdf-user)

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c nyxt-system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression (when (getenv "NYXT_COMPRESS")
                                  (parse-integer (getenv "NYXT_COMPRESS")))))

(defmethod asdf:perform :before ((o asdf:image-op) (c nyxt-system))
  "Perform some last minute tweaks to the final image.

- Register immutable systems to prevent compiled images of Nyxt from
trying to recompile dependencies.
See `asdf::*immutable-systems*'.

- If on SBCL, include `sb-sprof', the statistical profiler, since it's one of
the few modules that's not automatically included in the image."
  #+sbcl
  (require :sb-sprof)
  (map () 'asdf:register-immutable-system
       (remove-if (lambda (system) (uiop:string-prefix-p "nyxt" system))
                  (asdf:already-loaded-systems))))


(defclass nyxt-renderer-system (asdf:system) ()
  (:documentation "Specialized systems for Nyxt."))
(import 'nyxt-renderer-system  :asdf-user)

(export-always '*nyxt-renderer*)
(defvar *nyxt-renderer* (or (getenv "NYXT_RENDERER")
                            "gi-gtk"))

(defmethod asdf:component-depends-on ((o asdf:prepare-op) (c nyxt-renderer-system))
  `((load-op ,(format nil "nyxt/~a-application" *nyxt-renderer*))
    ,@(call-next-method)))
