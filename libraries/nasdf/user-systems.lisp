;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nasdf)

(export-always 'nyxt-user-system)
(defclass nyxt-user-system (asdf:system) ()
  (:documentation "Specialized systems for Nyxt users."))
(import 'nyxt-user-system :asdf-user)
