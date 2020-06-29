(uiop:define-package :nyxt/vcs
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Interact with Git repositories.

New command: vcs-clone (alias git-clone), to clone a VCS repository on
disk (Git only at the moment).

Change the `*vcs-projects-roots*' list to define where to look for
existing repositories on disk.

The clone command is run asynchronously.

One can set their username for GitHub and other forges.  It helps the
clone command in doing The Right Thing©, such as using a Git remote
URL instead of HTTPS.

See `nyxt/vcs:*vcs-username*' (default username) and `*vcs-username-alist*'.


***********************************************************************
*Disclaimer*: this feature is meant to grow with Next 1.4 and onwards!
***********************************************************************

We could clone on GitHub/GitLab, be notified if we have unpushed
changes, browse files in a text editor, use hooks...
"))
(in-package :nyxt/vcs)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(declaim (type (or null list-of-strings) *vcs-projects-roots*))
(sera:export-always '*vcs-projects-roots*)
(defparameter *vcs-projects-roots* '("~/projects" "~/src" "~/work" "~/common-lisp" "~/quicklisp/local-projects")
  "A list of directories to look for VCS repositories into.")
;; Possible improvement: specify the depth to look for projects alongside the directory.
;; See magit-list-repositories.

(declaim (type (or null alist-of-strings) *vcs-usernames-alist*))
(sera:export-always '*vcs-usernames-alist*)
(defvar *vcs-usernames-alist* '(("github.com" . "")
                                ("gitlab.com" . "")
                                ("bitbucket.org" . ""))
  "Your VCS usernames on different forges.  It helps some commands to do things
right, such as cloning over SSH instead of HTTPS.  The forge name should be a
domain, such as 'github.com'.")

(declaim (type (or null string) *vcs-username*))
(sera:export-always '*vcs-username*)
(defvar *vcs-username* ""
  "Default username to use for forges if none is found in `*vcs-usernames-alist*'.")

(defparameter *git-projects* '()
  "Currently registered Git projects (internal variable).")

(declaim (ftype (function (string)) vcs-username))
(defun vcs-username (forge)
  "Find the username for this forge name. Look up into `*vcs-usernames-alist*' and fallback to `*vcs-username*'."
  (let* ((forge/username (assoc forge *vcs-usernames-alist* :test #'string-equal)))
    (cond
      ((null forge/username)
       (log:info "VCS clone: no configuration found for ~a." forge)
       nil)
      ((not (str:blankp (cdr forge/username)))
       (cdr forge/username))
      ((not (str:blankp *vcs-username*))
       *vcs-username*)
      (t
       nil))))

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

(defun find-project-directory (name &key exit-recursive-scan)
  "Return the directory pathname of the project named NAME by searching the local projects.
If EXIT-RECURSIVE-SCAN is non-nil, avoid recursive scan of local projects. By default, it will re-scan the local projects to avoid false negatives and false positives."
  (unless *git-projects*
    (setf *git-projects* (parse-projects)))
  (let ((result (find name *git-projects*
                      :key (lambda (dir)
                              (alex:last-elt (str:split "/" (namestring dir) :omit-nulls t)))
                      :test #'string=)))
    ;; If null, re-parse the local projects.
    ;; It could have been cloned manually.
    (unless (or result
                exit-recursive-scan)
      (setf *git-projects* (parse-projects))
      (setf result (find-project-directory name :exit-recursive-scan t)))
    ;; Avoid false positives: check the directory exists.
    ;; It could have been deleted.
    (unless (or exit-recursive-scan
                (uiop:file-exists-p result))
      (setf *git-projects* (parse-projects))
      (setf result (find-project-directory name :exit-recursive-scan t)))
    result))

(declaim (ftype (function (string)) ensure-directory))
(defun ensure-directory (base)
  "Create this directory if it doesn't exist.
Needed to call `truename' to exand a tilde after it."
  ;; ensure a trailing slash.
  (ensure-directories-exist (str:concat (string-right-trim (list #\/) base)
                                        "/")))

(defun concat-filenames (base dir)
  "Concat filenames. Expand a tilde in BASE.
Create BASE if it doesn't exist."
  (ensure-directory base)
  ;; truename expands the tilde, but fails if the directory doesn't actually exist.
  ;; The tilde must be expanded for the following git clone command, that otherwise creates
  ;; ./~/my/foo instead of /home/user/my/foo.
  (namestring (merge-pathnames (uiop:truename* base) dir)))

(defun projects-roots-suggestion-filter (minibuffer)
  "Fuzzy-match local project roots."
  (fuzzy-match (input-buffer minibuffer) *vcs-projects-roots*))

(defun choose-clone-url (root-name project-name clone-uri)
  "If we are cloning one repository of ours (ROOT-NAME equals `vcs-username'), then use a git remote url instead of https."
  (let ((username (nyxt/vcs::vcs-username (quri:uri-domain clone-uri))))
    (if (and username
             (string= root-name username))
        (progn
          (log:info "Let's clone ~a with a git remote url." project-name)
          (format nil "git@~a:~a/~a.git"
                  (quri:uri-domain clone-uri)
                  root-name
                  project-name))
        (progn
          (setf (quri:uri-path clone-uri)
                (str:concat "/" root-name "/" project-name))
          (quri:render-uri clone-uri)))))

(defun clone (project-name root-name target-dir clone-uri)
  "Do the (Git) clone.
PROJECT-NAME: string
ROOT-NAME: string
TARGET-DIR: full path directory name (string)
CLONE-URI: quri:uri object."
  (log:debug "Cloning ~a into ~a" project-name target-dir)
  (echo "Cloning ~a into ~a" project-name target-dir)
  (handler-case (progn
                  (launch-and-notify
                   (list "git" "clone"
                         (choose-clone-url root-name project-name clone-uri)
                         (nyxt/vcs::concat-filenames target-dir project-name))
                   :success-msg (format nil "Repository ~a cloned." project-name)
                   :error-msg (format nil "Repository ~a was NOT cloned." project-name)))
    (error (c)
      (echo-warning "Error cloning ~a: ~a" project-name c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :nyxt)

(define-command vcs-clone ()
  "Clone the repository of the current URL to disk.  Only Git is supported at
the moment.  Set the list of preferred destinations in the `*vcs-projects-roots*' variable.
The default username can be set in `*vcs-username*' or `*vcs-username-alist*'."
  (let* ((uri (url (current-buffer)))
         (root-name (first (str:split "/" (quri:uri-path uri) :omit-nulls t)))
         (project-name (second (str:split "/" (quri:uri-path uri) :omit-nulls t)))
         (clone-uri (quri:copy-uri uri))
         (existing-repo (nyxt/vcs::find-project-directory project-name))
         target-dir)
    (cond
      ((not project-name)
       (echo "Could not find the project name."))
      (existing-repo
       (echo "This repository exists in ~a" existing-repo))
      ((= 1 (length nyxt/vcs::*vcs-projects-roots*))
       (setf target-dir (first nyxt/vcs::*vcs-projects-roots*))
       (nyxt/vcs::clone project-name root-name target-dir clone-uri))
      (t (with-result (target-dir (read-from-minibuffer
                                   (make-minibuffer
                                    :input-prompt "Target directory"
                                    :suggestion-function #'nyxt/vcs::projects-roots-suggestion-filter)))
           (nyxt/vcs::clone project-name root-name target-dir clone-uri))))))

(define-command git-clone ()
  "Alias of `vcs-clone'."
  (vcs-clone))

(define-command vcs-update-local-projects ()
  "Scan the project roots and update the list of existing repositories."
  (setf nyxt/vcs::*git-projects* (nyxt/vcs::parse-projects))
  (echo "VCS projects updated."))
