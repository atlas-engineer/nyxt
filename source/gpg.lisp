;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; TODO: Handle condition when GPG recipient is not found and prompt the user.

(define-class gpg-key-source (prompter:source)
  ((prompter:name "GPG Private Keys")
   (prompter:constructor (nfiles/gpg:gpg-private-keys))))

(defmethod prompter:object-attributes ((gpg-key nfiles/gpg:gpg-key) (source gpg-key-source))
  (declare (ignore source))
  `(("ID" ,(nfiles/gpg:key-id gpg-key))
    ("Additional" ,(str:join ", " (mapcar #'nfiles/gpg:user-id (nfiles/gpg:uids gpg-key))))))
