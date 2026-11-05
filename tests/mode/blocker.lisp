;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-blocker-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/blocker:blocker-mode buffer))
      (assert-true (disable-modes* 'nyxt/mode/blocker:blocker-mode buffer)))))

(define-test blocked-host-match ()
  (let ((table (nyxt/mode/blocker:blocked-host-table
                '("Ads.Example" "example.com." "example.com:443" "[::1]"))))
    (assert-true (nyxt/mode/blocker:host-blocked-p table "ads.example"))
    (assert-true (nyxt/mode/blocker:host-blocked-p table "EXAMPLE.COM"))
    (assert-true (nyxt/mode/blocker:request-blocked-p
                  table "https://a:b@Ads.Example:443/banner.js"))
    (assert-true (nyxt/mode/blocker:request-blocked-p table "http://[::1]/"))
    (assert-false (nyxt/mode/blocker:host-blocked-p table "notads.example"))
    (assert-false (nyxt/mode/blocker:request-blocked-p table "https://ads.example.evil/a"))
    (assert-false (nyxt/mode/blocker:request-blocked-p table "https://ads.example.com/a"))
    (assert-false (nyxt/mode/blocker:request-blocked-p table "nyxt:help"))))
