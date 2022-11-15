;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)
(use-nyxt-package-nicknames)

(define-test user-scripts ()
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
    ;; Virtual user script code equality
    (assert-string= code
                    (nyxt/user-script-mode:code virtual-script))
    ;; Virtual user script noframes
    (assert-false (nyxt/user-script-mode:all-frames-p virtual-script))
    ;; Virtual user script document start
    (assert-eq :document-start (nyxt/user-script-mode:run-at virtual-script))
    ;; Virtual user script @include
    (assert-equal '("http://*/*" "https://*/*")
                  (nyxt/user-script-mode:include virtual-script))
    ;; TODO:  Check the file serialization.
    ;; Virtual user script code equality
    (assert-equal (nyxt/user-script-mode:code virtual-script)
                  (nyxt/user-script-mode:code file-backed-script))
    (uiop:delete-file-if-exists (nfiles:expand file-backed-script))))
