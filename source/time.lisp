;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; Convenience function for time manipulation.
;; This can be useful for user configs.

(-> asctime->timestamp (string) time:timestamp)
(export-always 'asctime->timestamp)
(defun asctime->timestamp (asc-timestring)
  "Convert ASC-TIMESTRING to a timestamp.
An ASC-TIMESTRING is in the form of `Tue Oct 1 15:55:09 2019'."
  (let ((timestamp nil))
    (handler-case
        (let* ((timestring (str:split " " asc-timestring :omit-nulls t))
               (time (str:split ":" (nth 3 timestring)))
               (second (parse-integer (nth 2 time)))
               (minute (parse-integer (nth 1 time)))
               (hour (parse-integer (nth 0 time)))
               (day (parse-integer (nth 2 timestring)))
               (month (position (nth 1 timestring) time:+short-month-names+
                                :test #'string-equal))
               (year (parse-integer (nth 4 timestring))))
          (setf timestamp
                (time:universal-to-timestamp
                 (encode-universal-time second minute hour day month year))))
      (error (c)
        (log:debug "Error creating timestamp: ~a" c)
        nil))
    timestamp))

(defun sort-by-time (sequence &key (key #'last-access))
  "Return a timely ordered SEQUENCE by KEY.  More recent elements come first."
  (sort sequence #'time:timestamp> :key key))
