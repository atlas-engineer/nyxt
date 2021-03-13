;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;;
;; Emacs development helpers for Nyxt.
;;
;; To load it, simply add something like the following to your Emacs
;; initialization file:
;;
;; (load "/PATH/TO/NYXT/CHECKOUT/build-scripts/nyxt-guix.el" :noerror)
;;
;; See `nyxt-make-guix-sbcl-for-nyxt' for an example.

;;; Code:

(require 'cl-lib)

(defun nyxt--pure-env (&rest preserve-vars)
  "Return a pure `env' command as a list of string."
  (append '("env" "-i")
          (mapcar (lambda (var) (concat var "=" (getenv var)))
                  (append
                   '("DISPLAY"
                     "HOME"
                     "LOGNAME"
                     "TERM"
                     "USER")
                   preserve-vars))))

(defun nyxt--guix-preserve-vars (&rest preserve-vars)
  "Return the arguments to pass to `guix environment' to preserve PRESERVE-VARS."
  (mapcar (lambda (var) (format "--preserve=^%s$" var))
          preserve-vars))

(defun nyxt-mtime (file)
  "Return modification time of FILE."
  (when (file-exists-p file)
    (file-attribute-modification-time (file-attributes file))))

(defun nyxt-cache-dir ()
  "Return directory where image is saved, as per lisp-repl-core-dumper."
  (or (getenv "LISP_REPL_CORE_PATH")
      (concat
       (or (getenv "XDG_CACHE_HOME") "~/.cache")
       "/lisp-repl-core-directory")))

(defvar nyxt-guix-profile-directory "~/.guix-temp-profiles/nyxt"
  "Default directory where to dump auto-generated Guix profiles for Nyxt development.")

(cl-defun nyxt-guix-lazy-environment-command (root &key
                                                   ;; expression
                                                   load
                                                   ;; manifest
                                                   ad-hoc
                                                   preserve
                                                   container
                                                   network
                                                   share
                                                   expose
                                                   no-grafts
                                                   extra-args
                                                   command-args)
  "Return the command to load a Guix environment.
If the environment already exists, don't regenerate it.

If CONTAINER is non nil, the environment is containerized, otherwise it's pure.

PRESERVE is a list of environment variables (list of strings) to preserve.

EXTRA-ARGS is passed to `guix environment', before \"--\".
When CONTAINER is nil, EXTRA-ARGS are shell-quoted with `shell-quote-argument'.

COMMAND-ARGS as passed after EXTRA-ARGS and \"--\", or, if the environment
already exists and CONTAINER is nil, after sourcing \"etc/profile\"."
  (setq root (expand-file-name (or root
                                   (concat nyxt-guix-profile-directory "/nyxt"))))
  (make-directory (file-name-directory root) :parents)
  (cl-flet ((guix-environment-command
             (root-exists)
             (mapconcat
              #'shell-quote-argument
              (append
               '("guix" "environment")
               `(,@(when no-grafts '("--no-grafts"))
                 ,@(if container
                       `("--container"
                         ,(mapcar (lambda (dir) (concat "--share=" dir)) share)
                         ,(mapcar (lambda (dir) (concat "--expose=" dir)) expose)
                         ,@(when network '("--network")))
                     '("--pure"))

                 ,@(if root-exists
                       `("-p" ,root)
                     `("-r" ,root
                       ,@(when load `(,(concat "--load=" load)))))
                 ,@(apply #'nyxt--guix-preserve-vars preserve)
                 ,@extra-args
                 "--"
                 ,@command-args))
              " ")))
    (let ((root-env (concat root "/etc/profile")))
      (list (executable-find "bash") "--norc" "--noprofile" "-c"
            (format "if [ -e '%s' ]; then %s ; else %s ; fi"
                    root-env
                    (if container
                        (guix-environment-command t)
                      (format "source '%s' && %s" (shell-quote-argument root-env)
                              (mapconcat #'shell-quote-argument command-args " ")))
                    (guix-environment-command nil))))))

(cl-defun nyxt-make-guix-sbcl-for-nyxt (nyxt-checkout
                                        &key
                                        ;; Core dumper options:
                                        image-path
                                        force
                                        ;; Guix environment options:
                                        root
                                        container no-grafts preserve
                                        (ad-hoc '("gnupg")))
  "Run an SBCL executable image with all Nyxt dependencies pre-loaded.

The image is generated as needed and cached as IMAGE-PATH.
It's generated if it does not exist, if FORCE is non-nil, or if the \"guix.scm\"
file in NYXT-CHECKOUT is more recent.

When CONTAINER is non-nil, generate the image in a Guix container.
When no-grafts is non-nil, disable Guix grafting when generating the image.

PRESERVE is a list of environmental variable to preserve when running the image.

This function is a suitable candidate as a SLIME or SLY Lisp
implementation.  Example:

 (setq sly-lisp-implementations
  `((sbcl-nyxt (lambda () (nyxt-make-guix-sbcl-for-nyxt
                           \"~/projects/nyxt\"
                           :preserve '(\"PERSONAL\"))))))"
  (setq nyxt-checkout (expand-file-name nyxt-checkout))
  (setq image-path (expand-file-name
                    (or image-path
                        (concat (nyxt-cache-dir) "/sbcl-nyxt.image"))))
  (let ((guix-def (concat nyxt-checkout "/build-scripts/guix.scm")))
    (when (and (file-exists-p image-path)
               (or force
                   (time-less-p (nyxt-mtime image-path) (nyxt-mtime guix-def))))
      (message "Rebuilding Lisp image %S..." image-path)
      (ignore-errors (delete-file image-path)))
    (nyxt-guix-lazy-environment-command
     root
     :load guix-def
     :ad-hoc (cons "lisp-repl-core-dumper" ad-hoc)
     :preserve preserve
     :container container
     :network t
     :share (list (file-name-directory image-path)
                  (concat nyxt-checkout "=/nyxt"))
     :no-grafts no-grafts
     :command-args `("lisp-repl-core-dumper"
                     "-o" ,image-path
                     "-d" "nyxt/gtk"
                     "sbcl"))))

(provide 'nyxt-guix)
