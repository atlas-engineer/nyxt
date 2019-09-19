(uiop:define-package :next/vcs
    (:use :common-lisp :trivia :next)
  (:export :*vcs-projects-roots*)
  (:documentation "Interact with Git repositories.

New command: vcs-clone (alias git-clone), to clone a VCS repository on
disk (Git only at the moment). rst rst rts rst rst rst rst rt rst rt
rtr tr strs t rst rs t

Change the `*vcs-projects-roots*' list to define where to look for
existing repositories on disk.

The clone command is run asynchronously.

***********************************************************************
*Disclaimer*: this feature is meant to grow with Next 1.4 and onwards!
***********************************************************************

We could clone on Github/Gitlab, be notified if we have unpushed
changes, browse files in a text editor, use hooks...
"))

(in-package :next/vcs)

(defparameter *vcs-projects-roots* '("~/projects" "~/src" "~/work" "~/common-lisp" "~/quicklisp/local-projects")
  "A list of directories to look for VCS repositories into.")
;; Possible improvement: specify the depth to look for projects alongside the directory.
;; See magit-list-repositories.

(defparameter *git-projects* '()
  "Currently registered Git projects.")

(defun search-git-directories (dir)
  "Search all directories that contain a .git/ subdirectory, one level deep inside DIR."
  (when (uiop:directory-pathname-p (uiop:ensure-directory-pathname dir))
    (loop for dir in (uiop:subdirectories dir)
       for git-dir = (merge-pathnames dir ".git/")
       when (uiop:directory-exists-p git-dir)
       collect dir)))

(defun parse-projects ()
  "Scan `*vcs-projects-roots*'."
  (mapcan #'search-git-directories *vcs-projects-roots*))

(defun find-project-directory (name &key exit)
  "Return the directory pathname of the project named NAME.
If EXIT is non-nil and the project was not found, don't parse the project roots again."
  (unless *git-projects*
    (setf *git-projects* (parse-projects)))
  (let ((result (find name *git-projects*
                      :key (lambda (dir)
                              (alexandria:last-elt (str:split "/" (namestring dir) :omit-nulls t)))
                      :test #'string=)))
    (unless (or result
                exit)
      (setf *git-projects* (parse-projects))
      (setf result (find-project-directory name :exit t)))
    result))

;XXX: add ftype declaration.
(defun ensure-directory (base)
  "Create this directory if it doesn't exist.
Needed to call `truename' to exand a tilde after it.
BASE: directory name (string)."
  ;; ensure a trailing slash.
  (setf base
        (str:concat (string-right-trim (list #\/) base)
                    "/"))
  (ensure-directories-exist base))

(defun concat-filenames (base dir)
  "Concat filenames. Expand a tilde in BASE.
Create BASE if it doesn't exist."
  (ensure-directory base)
  ;; truename expands the tilde, but fails if the directory doesn't actually exist.
  ;; The tilde must be expanded for the following git clone command, that otherwise creates
  ;; ./~/my/foo instead of /home/user/my/foo.
  (let* ((base-truename-path (truename base))
         (base-truename-string (namestring base-truename-path))
         (dir-string (string-trim (list #\/) (namestring dir)))
         (joined (str:concat base-truename-string dir-string)))
    joined))

(defun projects-roots-completion-function (input)
  "Fuzzy-match local project roots."
  (fuzzy-match input *vcs-projects-roots*))

(defun clone (project-name root-name target-dir clone-uri)
  "Do the (Git) clone.
PROJECT-NAME: string
ROOT-NAME: string
TARGET-DIR: full path directory name (string)
CLONE-URI: quri:uri object."
  (log:debug "Cloning ~a into ~a" project-name target-dir)
  (echo "Cloning ~a into ~a" project-name target-dir)
  (setf (quri:uri-path clone-uri)
        (str:concat "/" root-name "/" project-name))
  (handler-case (progn
                  (uiop:launch-program
                   (list "git" "clone"
                         (quri:render-uri clone-uri)
                         (next/vcs::concat-filenames target-dir project-name))))
    (error (c)
      (log:warn "Error cloning ~a: ~a" project-name c)
      (echo "There was an error cloning ~a." project-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :next)

(define-command vcs-clone ()
  "Clone the repository of the current url to disk (if any). Git only at the moment.
Ask for which directory to clone to, expect if there is one single choice."
  (with-result (url (buffer-get-url))
    (let* ((uri (quri:uri url))
           (root-name (first (str:split "/" (quri:uri-path uri) :omit-nulls t)))
           (project-name (second (str:split "/" (quri:uri-path uri) :omit-nulls t)))
           (clone-uri (quri:copy-uri uri))
           (existing-repo (next/vcs::find-project-directory project-name))
           target-dir)
      (if project-name
          (if existing-repo
              (echo "This repository exists in ~a" existing-repo)
              (progn
                (if (= 1 (length next/vcs:*vcs-projects-roots*))
                    (progn
                      (setf target-dir (first next/vcs:*vcs-projects-roots*))
                      (next/vcs::clone project-name root-name target-dir clone-uri))
                    (with-result (target-dir (read-from-minibuffer
                                              (make-instance 'minibuffer
                                                             :input-prompt "Target directory:"
                                                             :completion-function #'next/vcs::projects-roots-completion-function)))
                      (next/vcs::clone project-name root-name target-dir clone-uri)))))
          (echo "Could not find the project name.")))))

(define-command git-clone ()
  "Alias of `vcs-clone'."
  (vcs-clone))

(define-command vcs-update-local-projects ()
  "Scan the project roots and update the list of existing repositories."
  (setf next/vcs::*git-projects* (next/vcs::parse-projects))
  (echo "VCS projects updated."))
