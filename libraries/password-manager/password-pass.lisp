;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(define-class password-store-interface (password-interface)
  ((executable (pathname->string (sera:resolve-executable "pass")))
   (sleep-timer (or (uiop:getenv "PASSWORD_STORE_CLIP_TIME") 45))
   (password-directory
    (or (uiop:getenv "PASSWORD_STORE_DIR")
        (format nil "~a/.password-store" (uiop:getenv "HOME")))
    :type string
    :reader password-directory)
   (scan-for-username-entries
    t
    :type boolean
    :documentation "When non-nil, a two-step process determines the username:
1. Scan credential content for an entry with a key matching `*username-keys*'.
2. If no match could be found, use the credential filename as a fallback.

When nil, Nyxt always immediately uses the fallback strategy.
If your store doesn't utilize username keys, this skips credential decryption."))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(push 'password-store-interface *interfaces*)

(defmethod list-passwords ((password-interface password-store-interface))
  (let ((directory (uiop:truename* (uiop:parse-native-namestring
                                    (password-directory password-interface)))))
    (when directory
      ;; Special care must be taken for symlinks. Say `~/.password-store/work`
      ;; points to `~/work/pass`, would we follow symlinks, we would not be able to
      ;; truncate `~/.password-store/` in `~/work/pass/some/password.gpg`.  Because
      ;; of this, we don't follow symlinks.
      (let* ((raw-list (uiop:directory*
                        ;; We truncate the root directory so that the password list
                        ;; resembles the output from `pass list`. To do so, we
                        ;; truncate `~/.password-store/` in the pathname strings of
                        ;; the passwords.
                        (format nil "~a/**/*.gpg" directory)))
             (dir-length (length (namestring directory))))
        (mapcar #'(lambda (x)
                    (subseq (namestring x) dir-length (- (length (namestring x)) (length ".gpg"))))
                raw-list)))))

(defmethod clip-password ((password-interface password-store-interface) &key password-name service)
  (declare (ignore service))
  (execute password-interface (list "show" "--clip" password-name)
    ;; Outputting to string blocks `pass'.
    :output 'nil))

(defvar *multiline-separator* ": *"
  "A regular expression to separate keys from values in the `pass' multiline format.")

(defun parse-multiline (content)
  "Return an alist of the multiple entries.
An entry is a sequence of
- a key string,
- a colon,
- optional spaces,
- a value string.

This is meant to handle the organization suggestion from
http://www.passwordstore.org/#organization.

Lines that don't match the format are ignored.
The first line (the password) is skipped."
  (unless (uiop:emptyp content)
    (let ((lines (str:split (string #\newline)
                            content)))
      (delete nil (mapcar (lambda (line)
                            (let ((entry (ppcre:split *multiline-separator* line :limit 2)))
                              (when (= 2 (length entry))
                                entry)))
                          ;; Skip first line to ignore password:
                          (rest lines))))))

(defvar *username-keys* '("login" "user" "username")
  "A list of string keys used to find the `pass' username in `clip-username'.")

(defun username-from-name (password-name)
  "Select a username using the path of the credential file.
The strategy for deriving the username is context-dependent:
- If credential exists in a subdirectory (e.g.: x.example/me@y.example),
  username is taken as-is from the filename (me@y.example)
- Otherwise, if no subdirectory is used (e.g.: me@x.example@y.example),
  username is taken from the first half of the filename (me@x.example)"
  (multiple-value-bind (_ parent-dirs credential-name)
      (uiop/pathname:split-unix-namestring-directory-components password-name)
    (declare (ignore _))
    (if parent-dirs
        credential-name
        (subseq credential-name 0 (position #\@ credential-name :from-end t)))))

(defun username-from-content (password-interface password-name)
  "Select username from first entry in credential matching `*username-keys*'"
  (let* ((content (execute password-interface (list "show" password-name)
                           :output '(:string :stripped t)))
         (entries (parse-multiline content))
         (username-entry (when entries
                           (some (lambda (key)
                                   (find key entries :test #'string-equal :key #'first))
                                 *username-keys*))))
    (when username-entry (second username-entry))))

(defmethod clip-username ((password-interface password-store-interface)
                          &key password-name service)
  "Save username of the `password-name' credential to clipboard.
See the `scan-for-usename-entries' slot for details.
Case is ignored.
The resulting username is also returned."
  (declare (ignore service))
  (when password-name
    (let ((username
           (or (when (scan-for-username-entries password-interface)
                 (username-from-content password-interface password-name))
               (username-from-name password-name))))
      (when username
        (trivial-clipboard:text username)
        username))))

(defmethod save-password ((password-interface password-store-interface)
                          &key password-name username password service)
  (declare (ignore service))
  (with-open-stream (st (make-string-input-stream (format nil "~a~%username:~a"
                                                          password
                                                          username)))
    (execute password-interface (list "insert" "--multiline" password-name)
      :input st))
  (when (str:emptyp password)
    (execute password-interface (list "generate" "--in-place" password-name))))

(defmethod password-correct-p ((password-interface password-store-interface))
  t)
