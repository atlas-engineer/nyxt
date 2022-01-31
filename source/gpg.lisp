;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; TODO: Handle condition when GPG recipient is not found and prompt the user.

(define-class gpg-key-source (prompter:source)
  ((prompter:name "GPG Private Keys")
   (prompter:constructor (nfiles:gpg-private-keys))))

(defmethod prompter:object-attributes ((gpg-key gpg-key))
  `(("ID" ,(nfiles:gpg-key-key-id gpg-key))
    ("Additional" ,(str:join ", " (mapcar #'nfiles:gpg-uid-user-id (nfiles:gpg-key-uids gpg-key))))))
