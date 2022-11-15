;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package :history-tree
  (:nicknames :htree)
  (:use :cl)
  (:import-from :class-star #:define-class #:make-name-transformer))

(trivial-package-local-nicknames:add-package-local-nickname :class* :hu.dwim.defclass-star :history-tree)
