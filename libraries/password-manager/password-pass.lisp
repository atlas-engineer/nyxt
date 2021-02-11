;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(define-class password-store-interface (password-interface)
  ((executable (executable-find "pass"))
   (sleep-timer (or (uiop:getenv "PASSWORD_STORE_CLIP_TIME") 45))
   (password-directory (or (uiop:getenv "PASSWORD_STORE_DIR")
                           (namestring (format nil "~a/.password-store"
                                               (uiop:getenv "HOME"))))
                       :reader password-directory))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(push 'password-store-interface *interfaces*)

(defmethod list-passwords ((password-interface password-store-interface))
  ;; Special care must be taken for symlinks. Say `~/.password-store/work`
  ;; points to `~/work/pass`, would we follow symlinks, we would not be able to
  ;; truncate `~/.password-store/` in `~/work/pass/some/password.gpg`.  Because
  ;; of this, we don't follow symlinks.
  (let ((raw-list (uiop:directory*
                   ;; We truncate the root directory so that the password list
                   ;; resembles the output from `pass list`. To do so, we
                   ;; truncate `~/.password-store/` in the pathname strings of
                   ;; the passwords.
                   (format nil "~a/**/*.gpg"
                           (password-directory password-interface))))
        (dir-length (length (namestring
                             (truename (password-directory password-interface))))))
    (mapcar #'(lambda (x)
                (subseq (namestring x) dir-length (- (length (namestring x)) 4)))
            raw-list)))

(defmethod clip-password ((password-interface password-store-interface) &key password-name service)
  (declare (ignore service))
  (execute password-interface (list "show" "--clip" password-name)
    :output '(:string :stripped t)))

(defmethod save-password ((password-interface password-store-interface)
                          &key password-name password service)
  (declare (ignore service))
  (if (str:emptyp password)
      (execute password-interface (list "generate" password-name))
      (with-open-stream (st (make-string-input-stream password))
        (execute password-interface (list "insert" "--echo" password-name)
          :input st))))

(defmethod password-correct-p ((password-interface password-store-interface))
  t)
