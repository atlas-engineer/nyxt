;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/tests
  (:use :lisp-unit2))

;; KLUDGE Temporary workarounds to deal with the absence of data profiles.

(defmethod nfiles:resolve ((profile nyxt:nyxt-profile) (file nyxt:history-file))
  (serapeum:path-join (uiop:ensure-directory-pathname (nfiles:name profile))
                      (call-next-method)))

(uiop:delete-directory-tree (nfiles:expand (make-instance 'nyxt:nyxt-data-directory))
                            :validate t :if-does-not-exist :ignore)
