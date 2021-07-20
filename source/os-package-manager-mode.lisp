;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/os-package-manager-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Universal interface to various operating system package managers."))
(in-package :nyxt/os-package-manager-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

;; TODO: Prompt for password?  Use this:
;; (with-input-from-string (i "password")
;;   (uiop:run-program '("sudo" "-S" "ls" "-la" "/root")
;;                     :output '(:string) :input i))

(define-mode os-package-manager-mode ()
  "Mode for package management."
  ((current-process-info nil
                         :type (or null uiop/launch-program::process-info))
   (keymap-scheme
    (define-scheme "web"
      scheme:cua
      (list
       "C-d" 'cancel-package-operation)
      scheme:emacs
      (list
       "C-d" 'cancel-package-operation)
      scheme:vi-normal
      (list
       "C-d" 'cancel-package-operation)))))

(define-command cancel-package-operation ()
  "Terminate the package manager process in the current buffer."
  (serapeum:and-let* ((process-info (current-process-info
                                     (find-submode (current-buffer) 'os-package-manager-mode))))
    (uiop:terminate-process process-info)
    (ffi-buffer-evaluate-javascript-async
     (current-buffer)
     (ps:ps (ps:chain document
                      (write (ps:lisp (spinneret:with-html-string
                                        (:p "Operation cancelled.")))))))))

(defmethod prompter:object-attributes ((pkg ospm:os-package))
  `(("Name" ,(ospm:name pkg))
    ("Version" ,(ospm:version pkg))
    ("Synopsis" ,(ospm:synopsis pkg))))

(defmethod prompter:object-attributes ((output ospm:os-package-output))
  (let* ((pkg (ospm:parent-package output))
         (name (format nil "~a~a"
                       (ospm:name pkg)
                       ;; TODO: Make this specializable.
                       (if (string= (ospm:name output) "out")
                           ""
                           (str:concat ":" (ospm:name output))))))
    `(("Name" ,name)
      ("Version" ,(ospm:version pkg))
      ("Synopsis" ,(ospm:synopsis pkg)))))

(defmethod prompter:object-attributes ((gen ospm:os-generation))
  `(("ID" ,(princ-to-string (ospm:id gen)))
    ("Date" ,(local-time:format-timestring nil (ospm:date gen)
                                           :format local-time:+asctime-format+))
    ("Package count" ,(princ-to-string (ospm:package-count gen)))
    ("Current?" ,(if (ospm:current? gen) "yes" ""))))

(defmethod prompter:object-attributes ((pkg ospm:guix-package))
  ;; We could have called `call-next-method', then modify the result, but it's
  ;; too costly for thousands of packages.
  `(("Name" ,(ospm:name pkg))
    ("Version" ,(ospm:version pkg))
    ("Synopsis" ,(ospm:synopsis pkg))
    ("Description" ,(ospm:description pkg))
    ("Home page" ,(ospm:home-page pkg))
    ("Location" ,(ospm:location pkg))
    ("Outputs" ,(sera:string-join (mapcar #'ospm:name (ospm:outputs pkg)) " "))
    ("Supported systems" ,(sera:string-join (ospm:supported-systems pkg) " "))
    ("Inputs" ,(sera:string-join (ospm:inputs pkg) " "))
    ("Native inputs" ,(sera:string-join (ospm:native-inputs pkg) " "))
    ("Propagated inputs" ,(sera:string-join (ospm:propagated-inputs pkg) " "))
    ("Licenses" ,(format nil "~{~a~^, ~}" (ospm:licenses pkg)))))

(define-class os-package-source (prompter:source)
  ((prompter:name "Packages")
   (prompter:multi-selection-p t)
   (prompter:constructor (ospm:list-packages))
   (prompter:active-attributes-keys '("Name" "Version" "Synopsis"))))

(define-class os-manifest-source (prompter:source)
  ((prompter:name "Manifests")
   (prompter:constructor (mapcar #'namestring (ospm:list-manifests)))))

(define-class os-package-output-source (prompter:source)
  ((prompter:name "Package Outputs")
   (prompter:multi-selection-p t)
   (prompter:constructor (ospm:list-package-outputs))))

(define-class os-installed-package-source (prompter:source)
  ((prompter:name "Installed Packages")
   (profile)
   (prompter:constructor
    (lambda (source)
      (ospm:list-packages (profile source)))))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class os-profile-source (prompter:source)
  ((prompter:name "Profiles")
   (include-manager-p)
   (prompter:constructor
    (lambda (source) (ospm:list-profiles
                      :include-manager-p (include-manager-p source)))))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class os-generation-source (prompter:source)
  ((prompter:name "Packages")
   (profile (error "Profile required."))
   (prompter:constructor
    (lambda (source) 
      (ospm:list-generations (profile source)))))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defun %describe-os-package (packages)
  (let* ((buffer (or (find-buffer 'os-package-manager-mode)
                     (nyxt/os-package-manager-mode:os-package-manager-mode
                      :activate t
                      :buffer (make-internal-buffer :title "*OS packages*")))))
    (flet ((format-inputs (inputs)
             (spinneret:with-html
               (dolist (input inputs)
                 (:a :href (lisp-url
                            '(%describe-os-package
                              (ospm:find-os-packages input)))
                     input))))
           (format-outputs (outputs)
             (spinneret:with-html
               (:div
                 (:table
                  (dolist (output outputs)
                    (:tr
                     (:td (ospm:name output))
                     (when (ospm:expanded-output-p output)
                       (:td
                        (sera:format-file-size-human-readable
                         nil
                         (ospm:size output)))
                       (:td (ospm:path output))))))
                 (when (and (<= 2 (length outputs))
                            (ospm:expanded-output-p (first outputs)))
                   (:li "Total size: " (sera:format-file-size-human-readable
                                        nil
                                        (reduce #'+ (mapcar #'ospm:size outputs)))))))))
      (nyxt::html-set
       (spinneret:with-html-string
         (:style (style buffer))
         (:h1 "Packages")
         (:ul (dolist (package packages)
                (:li (ospm:name package) " " (ospm:version package)
                     (:ul
                      (when (typep package 'ospm:guix-package)
                        (:li "Outputs: "
                             (unless (ospm:expanded-outputs-p package)
                               (:a :class "button"
                                   :href (lisp-url '(echo "Computing path & size...")
                                                   `(ospm:expand-outputs (first (ospm:find-os-packages
                                                                                 ,(ospm:name package)
                                                                                 :version ,(ospm:version package))))
                                                   `(%describe-os-package
                                                     (ospm:find-os-packages ,(ospm:name package)
                                                                            :version ,(ospm:version package))))
                                   "Compute path & size"))
                             (format-outputs (ospm:outputs package)))
                        (:li "Supported systems: " (str:join " " (ospm:supported-systems package)))
                        (:li "Inputs: " (format-inputs (ospm:inputs package)))
                        (:li "Propagated inputs: " (format-inputs (ospm:propagated-inputs package)))
                        (:li "Native inputs: " (format-inputs (ospm:native-inputs package)))
                        (:li "Location: " (ospm:location package)))
                      (:li "Home-page: " (:a :href (ospm:home-page package)
                                             (ospm:home-page package)))
                      (:li "Licenses: " (str:join ", " (ospm:licenses package)))
                      (:li "Synopsis: " (ospm:synopsis package))
                      (when (typep package 'ospm:guix-package)
                        (:li "Description: " (ospm:description package))))))))
       buffer))
    (set-current-buffer buffer)
    buffer))

(defun assert-package-manager ()
  (unless (ospm:manager)
    (let ((message "No supported package manager detected."))
      (echo message)
      (error message))))

(define-command-global describe-os-package ()
  "Show description of select packages."
  (assert-package-manager)
  (let* ((packages (prompt
                    :sources '(os-package-source)
                    :prompt "Describe OS package(s)")))
    (%describe-os-package packages)))

(defun viewable-file-type-p (path)
  (let ((path-suffix (string-downcase (namestring path))))
    (some (lambda (suffix)
            (str:ends-with? path-suffix suffix) )
          '(".html" ".htm") )))

;; TODO: open in editor, with select program, leverage file-manager
(define-command-global list-os-package-files ()
  "List files of select packages."
  (assert-package-manager)
  (let* ((packages-or-outputs (if (typep (ospm:manager) 'ospm:guix-manager)
                                  (prompt
                                   :sources '(os-package-output-source)
                                   :prompt "List files of OS package outputs(s)")
                                  (prompt
                                   :sources '(os-package-source)
                                   :prompt "List files of OS package(s)")))
         (buffer (or (find-buffer 'os-package-manager-mode)
                     (nyxt/os-package-manager-mode:os-package-manager-mode
                      :activate t
                      :buffer (make-internal-buffer :title "*OS packages*")))))
    (echo "Computing file list...")
    (nyxt::html-set
     (spinneret:with-html-string
       (:style (style buffer))
       (:h1 "Package files")
       (:ul
        (dolist (package-or-output packages-or-outputs)
          (:li (prompter:attributes-default package-or-output)
               (:ul
                (let ((files (mapcar #'namestring (ospm:list-files (list package-or-output)))))
                  (if files
                      (dolist (file files)
                        (:li (if (viewable-file-type-p file)
                                 (:a :href file file)
                                 file)))
                      "Could not compute file list.  Build the package first.")))))))
     buffer)
    (echo "")
    (set-current-buffer buffer)
    buffer))

(defun format-command-stream (process-info callback)
  (loop for object = (read-line (uiop:process-info-output process-info) nil :eof)
        until (eq object :eof)
        do (funcall callback object)))

(defun operate-os-package (title command profile objects)
  "Run COMMAND over OBJECTS in PROFILE.
OBJECTS can be a list of packages, a generation, etc."
  (let* ((buffer (or (find-buffer 'os-package-manager-mode)
                     (nyxt/os-package-manager-mode:os-package-manager-mode
                      :activate t
                      :buffer (make-internal-buffer :title "*OS packages*")))))
    (if (sera:and-let* ((process-info (nyxt/os-package-manager-mode:current-process-info
                                       (find-submode buffer 'os-package-manager-mode))))
          (uiop:process-alive-p process-info))
        (echo "An package operation is already running.  You can cancel it with `cancel-package-operation'.")
        (progn
          (run-thread
            (let ((process-info (funcall command objects profile))
                  (mode (find-submode buffer 'os-package-manager-mode)))
              (setf (nyxt/os-package-manager-mode:current-process-info mode) process-info)
              (nyxt::html-set "" buffer) ; Reset content between operations.
              (nyxt::html-write
               (spinneret:with-html-string
                 (:style (style buffer))
                 (:h1 title)
                 (:p
                  (:a :class "button"
                      :href (lisp-url '(nyxt/os-package-manager-mode:cancel-package-operation))
                      "Cancel")))
               buffer)
              (format-command-stream
               process-info
               (lambda (s)
                 ;; TODO: Make shell formating function and add support for
                 ;; special characters, e.g. progress bars.
                 (nyxt::html-write
                  (spinneret:with-html-string
                    (:code (str:replace-all " " " " s))
                    (:br))
                  buffer)))
              (nyxt::html-write
               (spinneret:with-html-string (:p "Done."))
               buffer)))
          (set-current-buffer buffer)
          buffer))))

(define-command-global install-os-package ()
  "Install select packages."
  (assert-package-manager)
  ;; TODO: Allow profile creation.  Need multi-source support for that?
  (let* ((profile (first
                   (prompt
                    :sources '(os-profile-source)
                    :prompt "Target profile")))
         (packages (prompt
                    :sources '(os-package-output-source)
                    :prompt "Install OS package(s)")))
    (operate-os-package "Installing packages..." #'ospm:install profile packages)))

(define-command-global uninstall-os-package ()
  "Uninstall select packages."
  (assert-package-manager)
  (let* ((profile (first
                   (prompt
                    :sources '(os-profile-source)
                    :prompt "Target profile")))
         (packages (prompt
                    :sources (list (make-instance 'os-installed-package-source
                                                  :profile profile))
                    :prompt "Uninstall OS package(s)")))
    (operate-os-package "Uninstalling packages..." #'ospm:uninstall profile packages)))

(define-command-global install-package-manifest ()
  "Install select manifest to a profile."
  (assert-package-manager)
  (let* ((profile (first
                   (prompt
                    :sources '(os-profile-source)
                    :prompt "Target profile")))
         (manifest (first
                    (prompt
                     :sources '(os-manifest-source)
                     :prompt "Manifest"))))
    (operate-os-package "Installing package manifest..." #'ospm:install-manifest profile manifest)))

(define-command-global edit-package-manifest ()
  "Edit select manifest."
  (assert-package-manager)
  (let ((manifest (first
                   (prompt
                    :sources '(os-manifest-source)
                    :prompt "Manifest"))))
    (echo "Opening ~s with ~a" manifest (external-editor-program *browser*))
    (uiop:launch-program (append (external-editor-program *browser*) (list manifest)))))

(define-command-global describe-os-generation ()
  "Show the packages of a given profile generation."
  (assert-package-manager)
  (let* ((profile (first
                   (prompt
                    :sources (list (make-instance 'os-profile-source
                                                  :include-manager-p t))
                    :prompt "Profile")))
         (generation (first
                      (prompt
                       :sources (list (make-instance 'os-generation-source
                                                     :profile profile))
                       :prompt "Generation")))
         (buffer (or (find-buffer 'os-package-manager-mode)
                     (nyxt/os-package-manager-mode:os-package-manager-mode
                      :activate t
                      :buffer (make-internal-buffer :title "*OS packages*")))))
    (echo "Loading package database...")
    (nyxt::html-set
     (spinneret:with-html-string
       (:style (style buffer))
       (:h2 (format nil "Packages for generation ~a" (ospm:id generation)))
       (:p "Profile " profile)
       (:ul
        (dolist (package-output (ospm:list-packages (ospm:path generation)))
          (let ((package (ospm:parent-package package-output)))
            (:li (:a :class "button"
                     :href (lisp-url `(%describe-os-package
                                       (or (ospm:find-os-packages
                                            ,(ospm:name package)
                                            :version ,(ospm:version package))
                                           (ospm:find-os-packages
                                            ,(ospm:name package)))))
                     (prompter:attributes-default package-output))
                 " " (ospm:version package))))))
     buffer)
    (echo "")
    (set-current-buffer buffer)
    buffer))

(define-command-global switch-os-generation ()
  "Switch generation of selected profile."
  (assert-package-manager)
  (let* ((profile (first
                   (prompt
                    :sources (list (make-instance 'os-profile-source
                                                  :include-manager-p t))
                    :prompt "Target profile")))
         (generation (first
                      (prompt
                       :sources (list (make-instance 'os-generation-source
                                                     :profile profile))
                       :prompt "Switch to generation"))))
    (operate-os-package "Switching to generation..." #'ospm:switch-generation
                        profile generation)))

(define-command-global delete-os-generations ()
  "Delete generations of selected profile."
  (assert-package-manager)
  (let* ((profile (first
                   (prompt
                    :sources (list (make-instance 'os-profile-source
                                                  :include-manager-p t))
                    :prompt "Target profile")))
         (generations (prompt
                       :sources (list (make-instance 'os-generation-source
                                                     :multi-selection-p t
                                                     :profile profile))
                       :prompt "Delete generations")))
    (operate-os-package "Deleting generations..." #'ospm:delete-generations
                        profile generations)))

;; TODO: Parse Texinfo for Guix descriptions.
;; TODO: Add commands:
;; - show-deps, show-reverse-deps (when prompt-buffer has actions)
