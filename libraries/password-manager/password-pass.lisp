;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(define-class password-store-interface (password-interface)
  ((executable (pathname->string (sera:resolve-executable "pass")))
   (sleep-timer (or (uiop:getenv "PASSWORD_STORE_CLIP_TIME") 45))
   (password-directory (or (uiop:getenv "PASSWORD_STORE_DIR")
                           (format nil "~a/.password-store" (uiop:getenv "HOME")))
                       :type string
                       :reader password-directory))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

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

(defmethod clip-username ((password-interface password-store-interface) &key password-name service)
  "Save the multiline entry that's prefixed with on of the `*username-keys*' to clipboard.
Case is ignored.
The prefix is discarded from the result and returned."
  (declare (ignore service))
  (when password-name
    (let* ((content (execute password-interface (list "show" password-name)
                      :output '(:string :stripped t)))
           (entries (parse-multiline content))
           (username-entry (when entries
                             (some (lambda (key)
                                     (find key entries :test #'string-equal :key #'first))
                                   *username-keys*))))
      (when username-entry
        (trivial-clipboard:text (second username-entry))
        (second username-entry)))))

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
