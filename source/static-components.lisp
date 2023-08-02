;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/static
  (:use :cl)
  (:export #:collect-static-file-components))

(in-package :nyxt/static)

(defmethod collect-static-file-components ((system symbol))
  (collect-static-file-components (asdf:find-system system)))

(defmethod collect-static-file-components ((system string))
  (collect-static-file-components (asdf:find-system system)))

(defmethod collect-static-file-components ((component t))
  '())

(defmethod collect-static-file-components ((component asdf:static-file))
  (list component))

(defmethod collect-static-file-components ((module asdf:module))
  (mapcan (function collect-static-file-components)
          (asdf:component-children module)))
