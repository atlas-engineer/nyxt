;; Convenience function for time manipulation.
;; This can be useful for user configs.

(in-package :next)

(declaim (ftype (function (string) local-time:timestamp) asctime->timestamp))
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
               (month (position (nth 1 timestring) local-time:+short-month-names+
                                :test #'string-equal))
               (year (parse-integer (nth 4 timestring))))
          (setf timestamp
                (local-time:universal-to-timestamp
                 (encode-universal-time second minute hour day month year))))
      (error (c)
        (log:debug "Error creating timestamp: ~a" c)
        nil))
    timestamp))
