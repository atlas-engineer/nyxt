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
  ())

;; TODO: Add command to interrupt operation?
;; (uiop:terminate-process  process-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :nyxt)

(defmethod object-string ((pkg ospama:os-package))
  (ospama:name pkg))
(defmethod object-display ((pkg ospama:os-package))
  (format nil "~a~a~a"
          (ospama:name pkg)
          (make-string (max 1 (- 40 (length (ospama:name pkg)))) :initial-element #\ )
          (ospama:synopsis pkg)))

(defun os-package-suggestion-filter ()
  (echo "Loading package database...")
  (let* ((all-packages (ospama:list-packages)))
    (echo "")
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) all-packages))))

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
                    (alex:mappend
                     (lambda (output)
                       `((:tr
                          (:td ,(ospama:name output))
                          (:td ,(if (= 0 (ospama:size output))
                                    "Unknown size"
                                    (format nil "~aB" (ospama:size output))))
                          (:td ,(ospama:path output)))))
                     outputs)))
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
                                                (:table  ,@(format-outputs (ospama:outputs package))))
                                           ;; TODO: Print output size + total size.
                                           (:li "Total size: " ,(sera:format-file-size-human-readable
                                                                 nil
                                                                 (reduce #'+
                                                                         (mapcar #'ospama:size
                                                                                 (ospama:outputs package)))) )
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
      (let ((process-info (funcall command packages profile)))
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
                                                                    (:br)))))))))))
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
;; - Report store item path.
;; - Report package size.
;; - find-files (open in editor, with select program) -- leverage file-manager
;; - show-deps, show-reverse-deps (when minibuffer has actions)
