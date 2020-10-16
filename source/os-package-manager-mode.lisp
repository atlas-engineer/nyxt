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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :nyxt)

(defmethod object-string ((pkg ospama:os-package))
  (ospama:name pkg))
(defmethod object-display ((pkg ospama:os-package))
  (format nil "~a ~a" (ospama:name pkg) (ospama:synopsis pkg)))

(defun os-package-suggestion-filter ()
  (let* ((all-packages (ospama:list-packages)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) all-packages))))

(define-command describe-os-package ()
  ""
  (let* ((packages (prompt-minibuffer
                    :suggestion-function (os-package-suggestion-filter)
                    :input-prompt "Describe OS package"
                    :multi-selection-p t))
         (buffer (or (find-buffer 'os-package-manager-mode)
                     (nyxt/os-package-manager-mode:os-package-manager-mode
                      :activate t
                      :buffer (make-internal-buffer :title "*OS packages*"))))
         (content
           (markup:markup
            (:style (style buffer))
            (:h1 "Packages")
            (:ul
             (loop for package in packages
                   collect (markup:markup*
                            `(:li ,(ospama:name package) " " ,(ospama:version package)
                                  (:ul
                                   ,@(when (typep package 'ospama:guix-package)
                                       `((:li "Outputs: " ,(str:join " " (ospama:outputs package)))
                                         (:li "Supported systems: " ,(str:join " " (ospama:supported-systems package)))
                                         (:li "Inputs: " ,(str:join " " (ospama:inputs package)))
                                         (:li "Propagated inputs: " ,(str:join " " (ospama:propagated-inputs package)))
                                         (:li "Native inputs: " ,(str:join " " (ospama:native-inputs package)))))
                                   (:li "Home-page: " ,(ospama:home-page package))
                                   (:li "Licenses: " ,(str:join ", " (ospama:licenses package)))
                                   (:li "Synopsis: " ,(ospama:synopsis package))
                                   ,(when (typep package 'ospama:guix-package)
                                      `(:li "Description: " ,(ospama:description package))))))))))
         (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                      (ps:lisp content)))))
    (ffi-buffer-evaluate-javascript-async buffer insert-content)
    (set-current-buffer buffer)
    buffer))
