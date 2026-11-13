;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-authenticator-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/mode/authenticator:authenticator-mode
                                  buffer))
      (assert-true (disable-modes* 'nyxt/mode/authenticator:authenticator-mode
                                   buffer)))))

(defun authenticator-entry (secret &rest args)
  (apply #'make-instance 'nyxt/mode/authenticator:authenticator-entry
         :secret secret
         args))

(define-test totp-rfc6238-sha1 ()
  (let ((entry (authenticator-entry
                "gezd gnbv gy3t qojq gezd gnbv gy3t qojq"
                :digits 8)))
    (assert-string= "94287082"
                    (nyxt/mode/authenticator:one-time-code entry :unix-time 59))
    (assert-string= "07081804"
                    (nyxt/mode/authenticator:one-time-code entry
                                                           :unix-time 1111111109))
    (assert-string= "14050471"
                    (nyxt/mode/authenticator:one-time-code entry
                                                           :unix-time 1111111111))
    (assert-string= "89005924"
                    (nyxt/mode/authenticator:one-time-code entry
                                                           :unix-time 1234567890))
    (assert-string= "69279037"
                    (nyxt/mode/authenticator:one-time-code entry
                                                           :unix-time 2000000000))
    (assert-string= "65353130"
                    (nyxt/mode/authenticator:one-time-code entry
                                                           :unix-time 20000000000))))

(define-test totp-rfc6238-sha256-and-sha512 ()
  (let ((sha256 (authenticator-entry
                 "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQGEZA===="
                 :algorithm :sha256
                 :digits 8))
        (sha512 (authenticator-entry
                 (concatenate
                  'string
                  "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQGEZDGNBV"
                  "GY3TQOJQGEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
                  "GEZDGNBVGY3TQOJQGEZDGNA=")
                 :algorithm :sha512
                 :digits 8)))
    (assert-string= "46119246"
                    (nyxt/mode/authenticator:one-time-code sha256 :unix-time 59))
    (assert-string= "68084774"
                    (nyxt/mode/authenticator:one-time-code sha256
                                                           :unix-time 1111111109))
    (assert-string= "90698825"
                    (nyxt/mode/authenticator:one-time-code sha256
                                                           :unix-time 2000000000))
    (assert-string= "90693936"
                    (nyxt/mode/authenticator:one-time-code sha512 :unix-time 59))
    (assert-string= "99943326"
                    (nyxt/mode/authenticator:one-time-code sha512
                                                           :unix-time 1111111111))
    (assert-string= "47863826"
                    (nyxt/mode/authenticator:one-time-code sha512
                                                           :unix-time 20000000000))))

(define-test hotp-rfc4226 ()
  (let ((entry (authenticator-entry "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
                                    :otp-type :hotp))
        (codes '("755224" "287082" "359152" "969429" "338314"
                 "254676" "287922" "162583" "399871" "520489")))
    (loop for code in codes
          for counter from 0
          do (setf (nyxt/mode/authenticator:counter entry) counter)
             (assert-string= code (nyxt/mode/authenticator:one-time-code entry)))))

(define-test steam-and-battle-codes ()
  (let ((steam (authenticator-entry "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
                                    :otp-type :steam))
        (battle (authenticator-entry "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
                                     :otp-type :battle
                                     :digits 6)))
    (assert-string= "PV9M4"
                    (nyxt/mode/authenticator:one-time-code steam :unix-time 59))
    (assert-string= "5PP3V"
                    (nyxt/mode/authenticator:one-time-code steam
                                                           :unix-time 1111111111))
    (assert-string= "94287082"
                    (nyxt/mode/authenticator:one-time-code battle :unix-time 59))))

(define-test otpauth-uri-and-invalid-secret ()
  (let ((entry (nyxt/mode/authenticator:authenticator-from-otpauth
                (concatenate
                 'string
                 "otpauth://totp/GitHub%3Aalice%40example.com"
                 "?secret=GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
                 "&issuer=GitHub&algorithm=SHA1&digits=8&period=30"))))
    (assert-string= "GitHub (alice@example.com)"
                    (nyxt/mode/authenticator:label entry))
    (assert-string= "alice@example.com"
                    (nyxt/mode/authenticator:account entry))
    (assert-eq :sha1 (nyxt/mode/authenticator:algorithm entry))
    (assert-string= "94287082"
                    (nyxt/mode/authenticator:one-time-code entry :unix-time 59)))
  (assert-error 'nyxt/mode/authenticator:authenticator-error
                (nyxt/mode/authenticator:authenticator-from-otpauth
                 "otpauth://totp/GitHub:alice?secret=!!!!"))
  (assert-error 'nyxt/mode/authenticator:authenticator-error
                (nyxt/mode/authenticator:one-time-code
                 (authenticator-entry ""))))

(define-test persist-authenticator-replaces-same-label ()
  (let* ((*browser* (make-instance 'browser))
         (buffer (make-instance 'modable-buffer))
         (file (make-instance 'nyxt/mode/authenticator:authenticators-file
                              :profile (make-instance 'files:virtual-profile))))
    (with-current-buffer buffer
      (enable-modes* 'nyxt/mode/authenticator:authenticator-mode buffer)
      (setf (nyxt/mode/authenticator:authenticators-file
             (find-submode 'nyxt/mode/authenticator:authenticator-mode buffer))
            file)
      (nyxt/mode/authenticator:persist-authenticator
       (authenticator-entry "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
                            :label "GitHub"
                            :otp-type :hotp))
      (assert-string= "755224"
                      (nyxt/mode/authenticator::use-code
                       (first (files:content file))))
      (assert-eq 1 (nyxt/mode/authenticator:counter
                    (first (files:content file))))
      (nyxt/mode/authenticator:persist-authenticator
       (authenticator-entry "MFRGGZDFMZTWQ2LK" :label "github"))
      (let ((entries (files:content file)))
        (assert-eq 1 (length entries))
        (assert-string= "MFRGGZDFMZTWQ2LK"
                        (nyxt/mode/authenticator:secret (first entries)))))))

(define-test authenticators-file-sits-beside-bookmarks ()
  (let ((authenticators (files:expand
                         (make-instance 'nyxt/mode/authenticator:authenticators-file)))
        (bookmarks (files:expand
                    (make-instance 'nyxt/mode/bookmark:bookmarks-file))))
    (assert-equal (pathname-directory bookmarks)
                  (pathname-directory authenticators))
    (assert-true (search "authenticators"
                         (uiop:native-namestring authenticators)))))
