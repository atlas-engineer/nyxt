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
;; See `nyxt-make-guix-cl-for-nyxt' for an example.

;;; Code:

(require 'cl-lib)

(defvar nyxt-guix-profile-directory "~/.guix-temp-profiles/nyxt"
  "Default directory where to dump auto-generated Guix profiles for Nyxt development.")

(defun nyxt--pure-env (&rest preserve-vars)
  "Return a pure `env' command as a list of string."
  (append '("env" "-i")
          (mapcar (lambda (var) (concat var "=" (getenv var)))
                  (append
                   '("DISPLAY"
                     "EDITOR"
                     "HOME"
                     "LOGNAME"
                     "TERM"
                     "USER"
                     "VISUAL"
                     ;; Used to share files (such as communication sockets) in
                     ;; some applications like `emacsclient':
                     "XDG_RUNTIME_DIR")
                   preserve-vars))))

(defun nyxt--guix-preserve-vars (&rest preserve-vars)
  "Return the arguments to pass to `guix shell' to preserve PRESERVE-VARS."
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

(cl-defun nyxt-guix-lazy-shell-command (root &key
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
  "Return the command to load a Guix shell, persisted at ROOT.
If the ROOT shell already exists, don't regenerate it.

If CONTAINER is non nil, the shell is containerized,
otherwise it's pure (that is, it does not inherit from the
current environment variables.)

PRESERVE is a list of environment variables (list of strings) to preserve.

EXTRA-ARGS is passed to `guix shell', before \"--\".

COMMAND-ARGS as passed after EXTRA-ARGS and \"--\", or, if the shell
already exists and CONTAINER is nil, after sourcing \"etc/profile\"."
  (let ((root-env (concat root "/etc/profile")))
    (if (and (not container)
             (file-exists-p root-env))
        (append
         (apply #'nyxt--pure-env preserve)
         (list (executable-find "bash")    ; `executable-find' needed because of the pure-env.
               "--norc" "--noprofile" "-c"
               (format "source %s && %s" (shell-quote-argument root-env)
                       (mapconcat #'shell-quote-argument command-args " "))))
      (append
       '("guix" "shell")
       `(,@(when no-grafts '("--no-grafts"))
         ,@(if container
               `("--container"
                 ,(mapcar (lambda (dir) (concat "--share=" dir)) share)
                 ,(mapcar (lambda (dir) (concat "--expose=" dir)) expose)
                 ,@(when network '("--network")))
             '("--pure"))
         ,@(if (file-exists-p root-env)
               `("-p" ,root)
             `("-r" ,root
               ,@(when load `("-D" ,(concat "--file=" load)))))
         ,@(when ad-hoc
             `(,@ad-hoc))

         ,@(apply #'nyxt--guix-preserve-vars preserve)
         ,@extra-args
         "--"
         ,@command-args)))))

(cl-defun nyxt-make-guix-cl-for-nyxt (nyxt-checkout
                                      &key
                                      force
                                      ;; Core dumper options:
                                      (cl-implementation "sbcl")
                                      (cl-system "nyxt/gi-gtk")
                                      image-path
                                      ;; Guix shell options:
                                      root
                                      container
                                      preserve
                                      no-grafts
                                      (ad-hoc '("guix" "gnupg")))
  "Run a CL-IMPLEMENTATION executable image with all dependencies of CL-SYSTEM
pre-loaded.

The image is generated as needed and cached as IMAGE-PATH.
It's generated if it does not exist, if FORCE is non-nil, or if the \"nyxt.scm\"
file in NYXT-CHECKOUT is more recent.

When CONTAINER is non-nil, generate the image in a Guix container.
When no-grafts is non-nil, disable Guix grafting when generating the image.

PRESERVE is a list of environmental variable to preserve when running the image.

This function is a suitable candidate as a SLIME or SLY Lisp
implementation.  Example:

 (setq sly-lisp-implementations
  `((nyxt-ccl-tests (lambda () (nyxt-make-guix-cl-for-nyxt
                                \"~/projects/nyxt\"
                                :cl-implementation \"ccl\"
                                :cl-system-dependencies \"nyxt/tests\"
                                :preserve '(\"PERSONAL\"))))))"
  (setq nyxt-checkout (expand-file-name nyxt-checkout))
  (setq image-path (expand-file-name
                    (or image-path
                        (concat (nyxt-cache-dir) "/"
                                cl-implementation "/"
                                (string-replace "/" "-" cl-system)
                                ".image"))))
  (setq root (expand-file-name (or root
                                   (concat nyxt-guix-profile-directory "/nyxt"))))
  (let ((cl-source-registry (format "%s:%s"
                                    nyxt-checkout
                                    (or (getenv "CL_SOURCE_REGISTRY") "")))
        (guix-def (concat nyxt-checkout "/build-scripts/nyxt.scm")))
    (setenv "CL_SOURCE_REGISTRY" cl-source-registry)
    (cl-flet ((guix-shell (&rest command-args)
                          (nyxt-guix-lazy-shell-command
                           root
                           :load guix-def
                           :ad-hoc (cons "lisp-repl-core-dumper" ad-hoc)
                           :preserve (cons "CL_SOURCE_REGISTRY" preserve)
                           :container container
                           :network t
                           :share (list (file-name-directory image-path)
                                        (concat nyxt-checkout "=/nyxt"))
                           :no-grafts no-grafts
                           :command-args command-args)))
      (when (or force
                (not (file-exists-p root))
                (not (file-exists-p image-path))
                (and (file-exists-p image-path)
                     (time-less-p (nyxt-mtime image-path) (nyxt-mtime guix-def))))
        (message "Rebuilding environment %S\nand Lisp image %S..." root image-path)
        (ignore-errors (delete-file image-path))
        (ignore-errors (delete-file root))
        (make-directory (file-name-directory root) :parents)
        (let ((output (get-buffer-create "*Nyxt Guix shell compilation*"))
              (command (guix-shell "lisp-repl-core-dumper"
                                   "-o" image-path
                                   "-d" cl-system
                                   cl-implementation
                                   "--eval" "'(uiop:quit)'")))
          (let ((status (apply #'call-process
                               (car command)
                               nil (list output t) nil
                               (cdr command))))
            (if (= status 0)
                (kill-buffer output)
              (error "Nyxt Guix shell creation failed, see %S." output)
              (switch-to-buffer-other-window output)))))
      (list (guix-shell "lisp-repl-core-dumper"
                        "-o" image-path
                        "-d" cl-system
                        cl-implementation)))))

(provide 'nyxt-guix)
