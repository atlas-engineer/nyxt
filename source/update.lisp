;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(-> release-version ((or null string)) (values (or null string) &optional))
(defun release-version (text)
  "Return TEXT when it is a major.minor.patch version, otherwise NIL.
Surrounding whitespace is ignored. A development version such as \"4\" is NIL."
  (when text
    (let ((line (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
      (when (and (plusp (length line))
                 (every (lambda (character)
                          (or (digit-char-p character)
                              (char= character #\.)))
                        line))
        (let ((components (uiop:parse-version line)))
          (when (and components
                     (= 3 (length components))
                     (string= line (format nil "~{~d~^.~}" components)))
            line))))))

(-> version-newer-p (string string) (values boolean &optional))
(defun version-newer-p (latest current)
  "Return T when LATEST is a newer numeric version than CURRENT.
A string that `uiop:parse-version' rejects yields NIL."
  (let ((latest-components (uiop:parse-version latest))
        (current-components (uiop:parse-version current)))
    (and latest-components
         current-components
         (uiop:version< current latest))))

(-> fetch-latest-version () (values (or null string) &optional))
(defun fetch-latest-version ()
  "Return the latest stable version at `+latest-version-url+', or NIL.
A network failure or a body that is not a release version returns NIL."
  (handler-case
      (release-version
       (dex:get +latest-version-url+
                :connect-timeout 5
                :read-timeout 5
                :force-string t))
    (dex:http-request-failed (c)
      (log:debug "Version check failed: ~a returned ~a."
                 +latest-version-url+ (dex:response-status c))
      nil)
    (error (c)
      (let* ((message (princ-to-string c))
             (newline (position #\Newline message)))
        (log:debug "Version check failed: ~a"
                   (subseq message 0 (min 160 (or newline (length message))))))
      nil)))

(-> report-latest-version ((or null string) &key (:always boolean)) *)
(defun report-latest-version (latest &key always)
  "Echo LATEST when it is newer than `+version+'.
A development version is reported only when ALWAYS is true.
With ALWAYS, also echo when this Nyxt is current or the request failed."
  (let ((newer (and latest (version-newer-p latest +version+))))
    (cond
      ((and newer (or always (release-version +version+)))
       (echo "Nyxt ~a is available." latest))
      ((not always)
       nil)
      ((null latest)
       (echo "Could not check for a new Nyxt version."))
      ((uiop:parse-version +version+)
       (echo "Nyxt ~a is up to date." +version+))
      (t
       (echo "The latest Nyxt release is ~a." latest)))))

(define-command-global check-for-updates ()
  "Check whether a newer version of Nyxt is published."
  (echo "Checking for a new Nyxt version.")
  (run-thread "check for updates"
    (report-latest-version (fetch-latest-version) :always t)))

(defun check-for-updates-handler (browser)
  "Look for a newer Nyxt release when `check-for-updates-p' is set."
  (when (check-for-updates-p browser)
    (run-thread "check for updates"
      (report-latest-version (fetch-latest-version)))))

(defmethod customize-instance :after ((browser browser) &key)
  (hooks:add-hook (after-startup-hook browser) 'check-for-updates-handler))
