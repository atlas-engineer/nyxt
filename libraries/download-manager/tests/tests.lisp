(in-package :cl-user)

(let ((uris '("https://abcl.org"
              "http://slack.net"
              "https://duckduckgo.com"
              "https://atlas.engineer")))
  (prove:plan (length uris))
  (dolist (uri uris)
    (prove:ok
     (download-manager:resolve uri)
     (format nil "Able to download <~a>~%" uri))))

(prove:finalize)
