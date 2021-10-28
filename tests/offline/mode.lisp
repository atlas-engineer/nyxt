;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(plan nil)

(subtest "Mode API"
  (is (mode-name 'root-mode)
      'root-mode)
  (is (mode-name 'base-mode)
      'base-mode)
  (is (mode-name 'nyxt/web-mode:web-mode)
      'nyxt/web-mode:web-mode)
  (is (mode-name 'nyxt/web-mode:user-web-mode)
      'nyxt/web-mode:web-mode)
  (is (mode-name 'web-mode)
      'nyxt/web-mode:web-mode)
  (is (mode-name 'user-web-mode)
      nil)
  (is (mode-name (make-instance 'base-mode))
      'base-mode))

(finalize)
