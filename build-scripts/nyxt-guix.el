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

(defun nyxt-pure-env (&rest preserve-vars)
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

(cl-defun nyxt-make-guix-sbcl-for-nyxt (nyxt-checkout
                                        &key image-path container no-grafts preserve force)
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
  (let ((guix-def (concat nyxt-checkout "/build-scripts/guix.scm"))
        (output (get-buffer-create " *Guix SBCL for Nyxt*")))
    (when (or force
              (not (file-exists-p image-path))
              (time-less-p (nyxt-mtime image-path) (nyxt-mtime guix-def)))
      (ignore-errors (delete-file image-path))
      (make-directory (file-name-directory image-path) :parents)
      (let ((status (apply #'call-process
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
    `((,@(apply #'nyxt-pure-env preserve) ,image-path))))

(provide 'nyxt-guix)
