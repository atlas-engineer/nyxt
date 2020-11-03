;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/os-package-manager-mode
  (:use :common-lisp :trivia :nyxt)
  ;; (:documentation "")
  )
(in-package :nyxt/os-package-manager-mode)

;; TODO: Prompt for password?  Use this:
;; (with-input-from-string (i "password")
;;   (uiop:run-program '("sudo" "-S" "ls" "-la" "/root")
;;                     :output '(:string) :input i))

(define-mode os-package-manager-mode ()
  "Mode for package management."
  ((current-process-info nil

                         :type (or null uiop/launch-program::process-info))))

(define-command cancel-package-operation ()
  (let ((process-info (current-process-info
                       (find-submode (current-buffer) 'os-package-manager-mode))))
    (uiop:terminate-process process-info)
    (ffi-buffer-evaluate-javascript-async
     (current-buffer)
     (ps:ps (ps:chain document
                      (write (ps:lisp (markup:markup (:p "Operation cancelled.")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :nyxt)

(defmethod object-string ((pkg ospama:os-package))
  (ospama:name pkg))
(defmethod object-display ((pkg ospama:os-package))
  (format nil "~a~a~a"
          (ospama:name pkg)
          (make-string (max 1 (- 40 (length (ospama:name pkg)))) :initial-element #\ )
          (ospama:synopsis pkg)))

(defmethod object-string ((output ospama:os-package-output))
  (format nil "~a:~a"
          (ospama:name (ospama:parent-package output))
          (ospama:name output)))
(defmethod object-display ((output ospama:os-package-output))
  (let ((pkg (ospama:parent-package output)))
    (format nil "~a:~a~a~a"
            (ospama:name pkg)
            (ospama:name output)
            (make-string (max 1 (- 40 (length (ospama:name pkg)))) :initial-element #\ )
            (ospama:synopsis pkg))))

(defun os-package-suggestion-filter ()
  (echo "Loading package database...")
  (let* ((all-packages (ospama:list-packages)))
    (echo "")
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) all-packages))))

(defun os-package-output-suggestion-filter ()
  (echo "Loading package database...")
  (let* ((all-outputs (ospama:list-package-outputs)))
    (echo "")
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) all-outputs))))

(defun os-installed-package-suggestion-filter (profile)
  (let* ((installed-packages (ospama:list-packages profile)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) installed-packages))))

(defun os-profile-suggestion-filter ()
  (let* ((all-profiles (ospama:list-profiles)))
    (lambda (minibuffer)
      ;; TODO: Don't prompt when there is just 1 profile.
      (fuzzy-match (input-buffer minibuffer) all-profiles))))

(defun %describe-os-package (packages)
  (let* ((buffer (or (find-buffer 'os-package-manager-mode)
                     (nyxt/os-package-manager-mode:os-package-manager-mode
                      :activate t
                      :buffer (make-internal-buffer :title "*OS packages*"))))
         (content
           (flet ((format-inputs (inputs)
                    (alex:mappend
                     (lambda (input)
                       `((:a :href (lisp-url
                                    '(%describe-os-package
                                      (list (ospama:find-os-package ,input))))
                             ,input)
                         " "))
                     inputs))
                  (format-outputs (outputs)
                    `(:div
                      (:table
                       ,@(alex:mappend
                          (lambda (output)
                            `((:tr
                               (:td ,(ospama:name output))
                               ,@(when (ospama:expanded-output-p output)
                                   `((:td
                                      ,(sera:format-file-size-human-readable
                                        nil
                                        (ospama:size output)))
                                     (:td ,(ospama:path output)))))))
                          outputs))
                      ,@(when (and (<= 2 (length outputs))
                                   (ospama:expanded-output-p (first outputs)))
                          `((:li "Total size: " ,(sera:format-file-size-human-readable
                                                  nil
                                                  (reduce #'+ (mapcar #'ospama:size outputs)))))))))
             (markup:markup
              (:style (style buffer))
              (:h1 "Packages")
              (:ul
               (loop for package in packages
                     collect (markup:markup*
                              `(:li ,(ospama:name package) " " ,(ospama:version package)
                                    (:ul
                                     ,@(when (typep package 'ospama:guix-package)
                                         `((:li "Outputs: "
                                                ,@(unless (ospama:expanded-outputs-p package)
                                                    `((:a :class "button"
                                                          :href ,(lisp-url '(echo "Computing path & size...")
                                                                           `(ospama:expand-outputs (ospama:find-os-package ,(ospama:name package)))
                                                                           `(%describe-os-package
                                                                             (list (ospama:find-os-package ,(ospama:name package)))))
                                                          "Compute path & size")))
                                                ,(format-outputs (ospama:outputs package)))
                                           (:li "Supported systems: " ,(str:join " " (ospama:supported-systems package)))
                                           (:li "Inputs: " ,@(format-inputs (ospama:inputs package)))
                                           (:li "Propagated inputs: " ,@(format-inputs (ospama:propagated-inputs package)))
                                           (:li "Native inputs: " ,@(format-inputs (ospama:native-inputs package)))))
                                     (:li "Home-page: " (:a :href ,(ospama:home-page package)
                                                            ,(ospama:home-page package)))
                                     (:li "Licenses: " ,(str:join ", " (ospama:licenses package)))
                                     (:li "Synopsis: " ,(ospama:synopsis package))
                                     ,(when (typep package 'ospama:guix-package)
                                        `(:li "Description: " ,(ospama:description package)))))))))))
         (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                      (ps:lisp content)))))
    (ffi-buffer-evaluate-javascript-async buffer insert-content)
    (set-current-buffer buffer)
    buffer))

(define-command describe-os-package ()
  "Show description of select packages."
  (let* ((packages (prompt-minibuffer
                    :suggestion-function (os-package-suggestion-filter)
                    :input-prompt "Describe OS package(s)"
                    :multi-selection-p t)))
    (%describe-os-package packages)))

(defun viewable-file-type-p (path)
  (let ((path-suffix (string-downcase (namestring path))))
    (some (lambda (suffix)
            (str:ends-with? path-suffix suffix) )
          '(".html" ".htm") )))

;; TODO: open in editor, with select program, leverage file-manager
(define-command list-os-package-files ()
  "List files of select packages."
  (let* ((packages-or-outputs (if (typep (ospama:manager) 'ospama:guix-manager)
                                  (prompt-minibuffer
                                   :suggestion-function (os-package-output-suggestion-filter)
                                   :input-prompt "List files of OS package outputs(s)"
                                   :multi-selection-p t)
                                  (prompt-minibuffer
                                   :suggestion-function (os-package-suggestion-filter)
                                   :input-prompt "List files of OS package(s)"
                                   :multi-selection-p t)))
         (buffer (or (find-buffer 'os-package-manager-mode)
                     (nyxt/os-package-manager-mode:os-package-manager-mode
                      :activate t
                      :buffer (make-internal-buffer :title "*OS packages*"))))
         (content
           (markup:markup
            (:style (style buffer))
            (:h1 "Package files")
            (:ul
             (loop for package-or-output in packages-or-outputs
                   collect (markup:markup*
                            `(:li ,(object-string package-or-output)
                                  (:ul
                                   ,@(mapcar (lambda (file)
                                               `(:li ,(if (viewable-file-type-p file)
                                                          `(:a :href ,file ,file)
                                                          file)))
                                             (ospama:list-files (list package-or-output))))))))))
         (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                      (ps:lisp content)))))
    (ffi-buffer-evaluate-javascript-async buffer insert-content)
    (set-current-buffer buffer)
    buffer))

(defun format-command-stream (process-info callback)
  (loop for object = (read-line (uiop:process-info-output process-info) nil :eof)
        until (eq object :eof)
        do (funcall callback object)))

(defun operate-os-package (title command profile packages)
  (let* (;; TODO: Check for running process.
         (buffer (or (find-buffer 'os-package-manager-mode)
                     (nyxt/os-package-manager-mode:os-package-manager-mode
                      :activate t
                      :buffer (make-internal-buffer :title "*OS packages*"))))
         (content
          (markup:markup
           (:style (style buffer))
           (:h1 title)))
         (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                      (ps:lisp content)))))
    (ffi-buffer-evaluate-javascript-async buffer insert-content)
    (chanl:pexec ()
      (let ((process-info (funcall command packages profile))
            (mode (find-submode buffer 'os-package-manager-mode)))
        (setf (nyxt/os-package-manager-mode:current-process-info mode) process-info)
        (ffi-buffer-evaluate-javascript-async
         buffer
         (ps:ps (ps:chain document
                          (write (ps:lisp (markup:markup
                                           (:p
                                            (:a :class "button"
                                                :href (lisp-url '(nyxt/os-package-manager-mode:cancel-package-operation))
                                                "Cancel"))))))))
        (format-command-stream process-info
                               (lambda (s)
                                 (ffi-buffer-evaluate-javascript-async
                                  buffer
                                  (ps:ps (ps:chain document
                                                   ;; TODO: Make shell formating
                                                   ;; function and add support
                                                   ;; for special characters,
                                                   ;; e.g. progress bars.
                                                   (write (ps:lisp (markup:markup
                                                                    (:code (str:replace-all " " " " s))
                                                                    (:br)))))))))
        (ffi-buffer-evaluate-javascript-async
         buffer
         (ps:ps (ps:chain document
                          (write (ps:lisp (markup:markup (:p "Done.")))))))))
    (set-current-buffer buffer)
    buffer))

(define-command install-os-package ()
  "Install select packages."
  ;; TODO: Allow profile creation.  Need multi-source support for that?
  (let* ((profile (prompt-minibuffer
                   :suggestion-function (os-profile-suggestion-filter)
                   :input-prompt "Target profile"))
         (packages (prompt-minibuffer
                    :suggestion-function (os-package-suggestion-filter)
                    :input-prompt "Install OS package(s)"
                    :multi-selection-p t)))
    (operate-os-package "Installing packages..." #'ospama:install profile packages)))

(define-command uninstall-os-package ()
  "Uninstall select packages."
  (let* ((profile (prompt-minibuffer
                   :suggestion-function (os-profile-suggestion-filter)
                   :input-prompt "Target profile"))
         (packages (prompt-minibuffer
                    :suggestion-function (os-installed-package-suggestion-filter profile)
                    :input-prompt "Uninstall OS package(s)"
                    :multi-selection-p t)))
    (operate-os-package "Uninstalling packages..." #'ospama:uninstall profile packages)))

;; TODO: Parse Texinfo for Guix descriptions.
;; TODO: Add commands:
;; - show-deps, show-reverse-deps (when minibuffer has actions)
