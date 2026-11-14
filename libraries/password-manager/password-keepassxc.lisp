;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :password)

(define-class keepassxc-interface (password-interface)
  ((executable (pathname->string (sera:resolve-executable "keepassxc-cli")))
   (password-file
    :documentation "The path to the KeePass password database.")
   (key-file
    nil
    :type (or null string pathname)
    :documentation "The key file for `password-file'.")
   (master-password
    ""
    :type string
    :documentation "The password to the `password-file'.")
   (yubikey-slot
    nil
    :documentation "Yubikey slot to unlock the `password-file'.")
   (entries-cache
    nil
    :type list
    :export nil
    :documentation "The cache to speed the entry listing up.")
   (entries-cached-p
    nil
    :type boolean
    :export nil
    :documentation "Whether `entries-cache' has been filled.
An empty database caches NIL, so the listing must be remembered separately."))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(pushnew 'keepassxc-interface *interfaces*)

(defun keepassxc-lines (password-interface &rest lines)
  "Stdin for KeePassXC: the master password, then LINES."
  (with-output-to-string (stream)
    (write-line (master-password password-interface) stream)
    (dolist (line lines)
      (write-line line stream))))

(defun keepassxc-database-arguments (password-interface)
  "Unlock options and the database path for PASSWORD-INTERFACE."
  (append (when (key-file password-interface)
            (list "-k" (uiop:native-namestring (key-file password-interface))))
          (when (yubikey-slot password-interface)
            (list "-y" (yubikey-slot password-interface)))
          (list (password-file password-interface))))

(defun keepassxc-run (password-interface arguments &key lines output ignore-error-status)
  "Run KeePassXC with ARGUMENTS. LINES follow the master password on stdin."
  (with-input-from-string (stream (apply #'keepassxc-lines password-interface lines))
    (execute password-interface arguments
             :input stream
             :output output
             :ignore-error-status ignore-error-status)))

(defun keepassxc-entry-lines (output)
  "Entry paths from KeePassXC `ls' output.
Group lines and the `[empty]' placeholder are dropped."
  (when output
    (remove-if (lambda (line)
                 (or (str:emptyp line)
                     (string= "[empty]" line)
                     (str:ends-with-p "/" line)))
               (sera:lines output))))

(defun keepassxc-ensure-groups (password-interface password-name)
  "Create the groups named by the slash-separated parents of PASSWORD-NAME.
A group that already exists is left in place."
  (let ((parts (str:split "/" password-name :omit-nulls t)))
    (when (rest parts)
      (loop for count from 1 below (length parts)
            do (keepassxc-run
                password-interface
                (append (list "mkdir" "-q")
                        (keepassxc-database-arguments password-interface)
                        (list (str:join "/" (subseq parts 0 count))))
                :output '(:string :stripped t)
                :ignore-error-status t)))))

(defun keepassxc-attribute (password-interface password-name attribute &key protected)
  "Return ATTRIBUTE of the entry PASSWORD-NAME.
PROTECTED includes a concealed field such as the password."
  (keepassxc-run
   password-interface
   (append (list "show" "-q")
           (when protected
             (list "--show-protected"))
           (list "--attributes" attribute)
           (keepassxc-database-arguments password-interface)
           (list password-name))
   :output '(:string :stripped t)))

(defmethod list-passwords ((password-interface keepassxc-interface))
  (unless (entries-cached-p password-interface)
    (setf (entries-cache password-interface)
          (keepassxc-entry-lines
           (keepassxc-run
            password-interface
            (append (list "ls" "-q" "-Rf") ; Recursive flattened.
                    (keepassxc-database-arguments password-interface))
            :output '(:string :stripped t)))
          (entries-cached-p password-interface) t))
  (entries-cache password-interface))

(defmethod clip-password ((password-interface keepassxc-interface) &key password-name service)
  (declare (ignore service))
  (clip-password-string
   password-interface
   (keepassxc-attribute password-interface password-name "Password" :protected t)))

(defmethod clip-username ((password-interface keepassxc-interface) &key password-name service)
  (declare (ignore service))
  (let ((username (keepassxc-attribute password-interface password-name "UserName")))
    (unless (str:emptyp username)
      (clipboard-copy username)
      username)))

(defmethod save-password ((password-interface keepassxc-interface)
                          &key password-name username password service)
  "Save an entry named PASSWORD-NAME.
An empty PASSWORD asks KeePassXC to generate one. Slashes in PASSWORD-NAME
name groups, which are created when they do not exist."
  (declare (ignore service))
  (when (str:emptyp password-name)
    (error "The KeePassXC entry name is empty."))
  ;; This is to force entries re-fetching the next time we need passwords.
  (setf (entries-cache password-interface) nil
        (entries-cached-p password-interface) nil)
  (keepassxc-ensure-groups password-interface password-name)
  (let ((generate-p (str:emptyp password)))
    (keepassxc-run
     password-interface
     (append (list "add" "-q"
                   "--username" (or username "")
                   (if generate-p "--generate" "--password-prompt"))
             (keepassxc-database-arguments password-interface)
             (list password-name))
     :lines (unless generate-p
              (list password))
     :output '(:string :stripped t))))

(defmethod password-correct-p ((password-interface keepassxc-interface))
  (handler-case
      (progn
        (list-passwords password-interface)
        t)
    (uiop/run-program:subprocess-error ()
      nil)))
