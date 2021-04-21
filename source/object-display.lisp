;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'object-string)
(defmethod object-string ((object t))
  (princ-to-string object))

(defmethod object-string ((package package))
  (if (eq (package-name package) (find-package :nyxt))
      ""
      (str:replace-all "nyxt/" "" (str:downcase (package-name package)))))
