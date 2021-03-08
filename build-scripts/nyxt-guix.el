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

(defun nyxt--cl-dump-call (image-path)
  `(uiop:run-program
    `("lisp-repl-core-dumper"
      "-o" ,,image-path
      "-p" ,(format nil "~{ ~a~}~%"
                    (delete-if
                     (lambda (s) (string= "nyxt" (subseq s 0 (min (length s) 4))))
                     (append (asdf:system-depends-on (asdf:find-system :nyxt))
                             (asdf:system-depends-on (asdf:find-system :nyxt/gtk)))))
      "sbcl")))

(defvar nyxt-guix-profile-directory "~/.guix-temp-profiles/nyxt"
  "Default directory where to dump auto-generated Guix profiles for Nyxt development.")

(cl-defun nyxt-guix-lazy-environment (root &key preserve guix-env-args run-args)
  "Return the command to load a Guix pure environment.
If the environment already exists, don't regenerate it.

PRESERVE is a list of environment variables to preserve.

GUIX-ENV-ARGS is passed to the `guix environment', before \"--\".
GUIX-ENV-ARGS must be shell-quoted (for instance with `shell-quote-argument').

RUN-ARGS are passed after GUIX-ENV-ARGS and \"--\", or, if the
environment already exists, after sourcing \"etc/profile\"."
  (setq root (expand-file-name (or root
                                   (concat nyxt-guix-profile-directory "/nyxt"))))
  (make-directory (file-name-directory root) :parents)
  (let ((root-env (concat root "/etc/profile")))
    (list (executable-find "bash") "--norc" "--noprofile" "-c"
          (format "if [ -e '%s' ]; then %s ; else %s ; fi"
                  root-env
                  (format "source '%s' && %s" (shell-quote-argument root-env)
                          (mapconcat #'shell-quote-argument run-args " "))
                  (mapconcat #'identity (append (list "guix" "environment" "-r" (shell-quote-argument root) "--pure")
                                                (apply #'nyxt--guix-preserve-vars preserve)
                                                guix-env-args
                                                (list "--")
                                                (mapcar #'shell-quote-argument run-args)) " ")))))

(cl-defun nyxt-make-guix-sbcl-for-nyxt (nyxt-checkout
                                        &key image-path container no-grafts preserve force
                                        guix-profile-root)
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
    (when (or force
              (not (file-exists-p image-path))
              (time-less-p (nyxt-mtime image-path) (nyxt-mtime guix-def)))
      (message "Dumping Nyxt-optimized SBCL at %S..." image-path)
      (ignore-errors (delete-file image-path))
      (make-directory (file-name-directory image-path) :parents)
      ;; TODO: Can we reuse the same Guix environment?  Would probably be
      ;; faster, but then containerizing is harder.
      (let* ((output (get-buffer-create " *Guix SBCL for Nyxt*"))
             (status (apply #'call-process
                            "guix"
                            nil (list output t) nil
                            "environment"
                            `(,@(when no-grafts
                                  '("--no-grafts"))
                              "-l" ,guix-def
                              ,@(if container
                                    (list
                                     "--container"
                                     (concat "--share=" (file-name-directory image-path))
                                     (concat "--share=" nyxt-checkout "=/nyxt"))
                                  '("--pure"))
                              "--ad-hoc" "lisp-repl-core-dumper"
                              "--"
                              "sbcl" "--noinform" "--no-userinit" "--eval" "(require :asdf)"
                              "--eval" ,(prin1-to-string (nyxt--cl-dump-call image-path))
                              "--quit"))))
        (if (= status)
            (kill-buffer output)
          (error "Guix environment failed, see %s" output)
          (switch-to-buffer-other-window output))))
    (list (nyxt-guix-lazy-environment
           guix-profile-root
           :preserve preserve
           :guix-env-args (list "-l" (shell-quote-argument guix-def) "--ad-hoc" "gnupg")
           :run-args (list image-path)))))

(provide 'nyxt-guix)
