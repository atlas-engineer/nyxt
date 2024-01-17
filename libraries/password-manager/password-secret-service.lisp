;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; Search for password, get password, write password, using Secret Service API:
;; https://specifications.freedesktop.org/secret-service
;; Password entries are retrieved using python package SecretStorage: https://pypi.org/project/SecretStorage
;; This package may be provided by your linux distribution. In Ubuntu python3-secretstorage.
;; Password entries are written using python package keyring: https://pypi.org/project/keyring
;; This package may be provided by your linux distribution. In Ubuntu python3-keyring.
;; The default collection (service) in Secret Service is used, this is normally the login collection.
;; On at least Ubuntu this collection is by default open when a user is logged in. So there is no
;; need to enter a master password to unlock the collection, when logging in at a web page.
;;
;; It would have been preferable to only use keyring, and not secretstorage, but keyring does not
;; provide facilities to search the Secret Service collection.
;;
;; This interface relies on a number of command line scripts:
;; keyring: Provided by python package keyring
;; secret-service-keys, secret-service-show-password, secret-service-show-username: Implementation shown below,
;; also available from https://github.com/johanwiden/secret-service-scripts
;;
;; Limitations:
;; - Currently this interface does not generate a new password, if an empty password is saved.
;;   The user must therefore use some other facility to generate a new password.]

(in-package :password)

(define-class password-secret-service-interface (password-interface)
  ((executable (pathname->string (sera:resolve-executable "keyring"))))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(push 'password-secret-service-interface *interfaces*)

;; Return something like ("pypi.org" "foo.org")
;; If executable "secret-service-list-keys" is available, return result from that, otherwise nil.
;; https://stackoverflow.com/questions/72020628/how-do-i-list-and-access-the-secrets-stored-in-ubuntus-keyring-from-the-command
;; In ubuntu: sudo apt get python3-secretstorage
;; Example implementation of "secret-service-list-keys":
;; #!/usr/bin/env python3
;; import secretstorage
;; conn = secretstorage.dbus_init()
;; collection = secretstorage.get_default_collection(conn)
;; services = []
;; for item in collection.get_all_items():
;;     attributes = item.get_attributes()
;;     if 'service' in attributes:
;;         services.append(attributes['service'])
;; print(' '.join(e for e in sorted(set(services))))
(defmethod list-passwords ((password-interface password-secret-service-interface))
  (let ((secret-service-keys (pathname->string (serapeum:resolve-executable "secret-service-list-keys"))))
    (when secret-service-keys
      (let* ((key-string (uiop:run-program (list secret-service-keys) :output '(:string :stripped t)))
             (key-list (split-sequence:split-sequence #\Space key-string :remove-empty-subseqs t)))
        key-list))))

;; If executable "secret-service-show-password" is available, and returns result, then copy result to clipboard, return t.
;; Otherwise return nil.
;; In ubuntu: sudo apt get python3-secretstorage
;; Example implementation of "secret-service-show-password":
;; #!/usr/bin/env python3
;; import argparse
;; import secretstorage
;; parser = argparse.ArgumentParser(
;;     prog = 'secret-service-show-password',
;;     description = 'Return password for password entry password_name')
;; parser.add_argument('password_name', type=str)
;; args = parser.parse_args()
;; conn = secretstorage.dbus_init()
;; collection = secretstorage.get_default_collection(conn)
;; for item in collection.get_all_items():
;;     attributes = item.get_attributes()
;;     if 'service' in attributes and attributes['service'] == args.password_name:
;;         print(item.get_secret().decode('utf-8'))
;;         break
(defmethod clip-password ((password-interface password-secret-service-interface) &key password-name service)
  (declare (ignore service))
  (let ((secret-service-show-password (pathname->string (serapeum:resolve-executable "secret-service-show-password"))))
    (when secret-service-show-password
      (let* ((password-str (uiop:run-program (list secret-service-show-password password-name) :output '(:string :stripped t)))
             (password (if (uiop:emptyp password-str) nil password-str)))
        (when password
          (trivial-clipboard:text password)
          t)))))

;; If executable "secret-service-show-username" is available, and returns result, then copy result to clipboard, return t.
;; Otherwise return nil.
;; In ubuntu: sudo apt get python3-secretstorage
;; Example implementation of "secret-service-show-username":
;; #!/usr/bin/env python3
;; import argparse
;; import secretstorage
;; parser = argparse.ArgumentParser(
;;     prog = 'secret-service-show-username',
;;     description = 'Return field "username" for password entry password_name')
;; parser.add_argument('password_name', type=str)
;; args = parser.parse_args()
;; conn = secretstorage.dbus_init()
;; collection = secretstorage.get_default_collection(conn)
;; for item in collection.get_all_items():
;;     attributes = item.get_attributes()
;;     if 'service' in attributes and attributes['service'] == args.password_name and 'username' in attributes:
;;         print(attributes['username'])
;;         break
(defmethod clip-username ((password-interface password-secret-service-interface) &key password-name service)
  (declare (ignore service))
  (let ((secret-service-show-username (pathname->string (serapeum:resolve-executable "secret-service-show-username"))))
    (when secret-service-show-username
      (let* ((username-str (uiop:run-program (list secret-service-show-username password-name) :output '(:string :stripped t)))
             (username (if (uiop:emptyp username-str) nil username-str)))
        (when username
          (trivial-clipboard:text username)
          t)))))

;; Generate new password is not supported.
(defmethod save-password ((password-interface password-secret-service-interface)
                          &key password-name username password service)
  (declare (ignore service))
  (with-input-from-string (st (format nil "~a~C" password #\newline))
    (execute password-interface (list "set" password-name username) :input st)))

(defmethod password-correct-p ((password-interface password-secret-service-interface))
  t)
