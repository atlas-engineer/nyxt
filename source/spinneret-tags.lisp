;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :spinneret)

(deftag :mayberaw (body attrs &key &allow-other-keys)
  attrs
  `(:raw (if (nyxt:html-string-p (progn ,@body))
             (progn ,@body)
             (escape-string (progn ,@body)))))
