;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)
(use-nyxt-package-nicknames)

(plan nil)

(let* ((code "// ==UserScript==
// @name          Script Name
// @namespace     Script
// @description	  A simple testing script
// @version       1.23.456
// @author        https://github.com/atlas-engineer
// @homepageURL   https://github.com/atlas-engineer/nyxt
// @run-at        document-start
// @include       http://*/*
// @include       https://*/*
// @grant         none
// @noframes
// ==/UserScript==")
       (file-backed-script (make-instance
                            'nyxt/user-script-mode:user-script
                            :code code :base-path #p"testing-script.user.js"))
       (virtual-script (make-instance 'nyxt/user-script-mode:user-script :code code)))
  (subtest "Virtual user script code equality"
    (is (nyxt/user-script-mode:code virtual-script) code))
  (subtest "Virtual user script noframes"
    (is (nyxt/user-script-mode:all-frames-p virtual-script) nil))
  (subtest "Virtual user script document start"
    (is (nyxt/user-script-mode:run-at virtual-script) :document-start))
  (subtest "Virtual user script @include"
    (is (nyxt/user-script-mode:include virtual-script) '("http://*/*" "https://*/*")))
  ;; TODO:  Check the file serialization.
  (subtest "Virtual user script code equality"
    (is (nyxt/user-script-mode:code virtual-script)
        (nyxt/user-script-mode:code file-backed-script)))
  (uiop:delete-file-if-exists (nfiles:expand file-backed-script)))
