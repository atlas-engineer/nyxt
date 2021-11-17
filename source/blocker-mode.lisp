;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/blocker-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum #:->)
  (:documentation "Block resource queries for listed hosts."))
(in-package :nyxt/blocker-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)
  (trivial-package-local-nicknames:add-package-local-nickname :hooks :serapeum/contrib/hooks))

;; TODO: Add convenient interface to block hosts depending on the current URL.

(define-class hostlist (data-path)
  ((url
    (quri:uri "")
    :type quri:uri
    :documentation "URL where to download the list from.  If empty, no attempt
will be made at updating it.")
   (hosts
    '()
    :type (or (cons string *) null)
    :documentation "List of hosts to ignore.
This is useful to reference hosts manually instead of via `url'.")
   (content
    nil
    :type (or null string)
    :export nil)
   (update-interval
    (* 60 60 24)
    :type integer
    :documentation "If URL is provided, update the list after this amount of
seconds.")
   (force-update-p
    nil
    :type boolean
    :documentation "If non-nil, the list is re-downloaded before it's (re-)read,
ignoring the `update-interval'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "A hostlist `blocker-mode' can use for its `hostlists' slot.
See `*default-hostlist*' for an example."))

(serapeum:export-always 'make-hostlist)
(defun make-hostlist (&rest args)
  "Return a new `hostlist'.
See the `hostlist' class documentation."
  (apply #'make-instance 'hostlist args))

(defmethod store ((profile data-profile) (path hostlist) &key &allow-other-keys)
  (with-data-file (file path :direction :output)
    (write-string (get-data path) file)))

(defmethod restore ((profile data-profile) (path hostlist) &key &allow-other-keys)
  "Restore a hostlist."
  (handler-case
      (let ((data (with-data-file (file path)
                    (when file
                      (uiop:read-file-string file)))))
        (when data
          (nyxt::%set-data path data)))
    (error (c)
      (echo-warning "Failed to load hostlist from ~s: ~a"
                    (expand-path path) c))))

(-> fetch (hostlist) string)
(defun fetch (hostlist)
  "Fetch HOSTLIST and return its content.
If HOSTLIST yields a path with `expand-path', persist it locally.
Auto-update file if older than UPDATE-INTERVAL seconds and AUTO-UPDATE-P is non-nil.
The hostlist is downloaded in the background."
  (let ((path (expand-path hostlist)))
    (if (and (or (force-update-p hostlist)
                 (not (uiop:file-exists-p path))
                 (< (update-interval hostlist)
                    (- (get-universal-time) (uiop:safe-file-write-date path))))
             (not (url-empty-p (url hostlist))))
        (let ((url-string (render-url (url hostlist))))
          (log:debug "Downloading hostlist from ~a to ~a." url-string path)
          (or (with-protect ("Could not download hostlist ~a: ~a" url-string :condition)
                (let ((hostlist-string (dex:get url-string)))
                  (with-data-access (h hostlist)
                    (setf h hostlist-string))
                  (echo "Hostlist ~a updated." (render-url (url hostlist)))
                  hostlist-string))
              ""))
        (progn
          (log:debug "Restoring hostlist from ~a" path)
          (the (values string &optional)
               (get-data hostlist))))))

(-> load-hostlist (hostlist blocker-mode) boolean)
(defun load-hostlist (hostlist mode)
  "Load HOSTLIST into MODE's `blocked-hosts'.
Return NIL on failure, T on success."
  (cond
    ((hosts hostlist)
     (dolist (host (hosts hostlist))
       (setf (gethash host (blocked-hosts mode)) host)))
    ((url hostlist)
     (let ((hostlist-string (fetch hostlist)))
       (unless (uiop:emptyp hostlist-string)
         (with-input-from-string (stream hostlist-string)
           (flet ((empty-line? (line)
                    (< (length line) 2))
                  (comment? (line)
                    (string= (subseq line 0 1) "#"))
                  (custom-hosts? (line)
                    (not (str:starts-with? "0.0.0.0" line))))
             (loop as line = (read-line stream nil)
                   while line
                   unless (or (empty-line? line)
                              (comment? line)
                              (custom-hosts? line))
                     do (let ((host (second (str:split " " line))))
                          (setf (gethash host (blocked-hosts mode)) host)))))
         t)))
    (t nil)))

(-> load-hostlists (blocker-mode &key (:force-update-p boolean)) t)
(defun load-hostlists (blocker-mode &key force-update-p)
  "Load BLOCKER-MODE's hostlists into `blocked-hosts' in the background."
  (dolist (hostlist (hostlists blocker-mode))
    (when force-update-p
      ;; TODO: Move and export `copy-object' to a separate library.
      (setf hostlist (prompter::copy-object hostlist))
      (setf (force-update-p hostlist) t))
    (calispel:! (worker-channel blocker-mode) hostlist)))

(defun worker (mode)
  (do () (nil)
    (alex:when-let ((hostlist (calispel:? (worker-channel mode))))
      (load-hostlist hostlist mode))))

(serapeum:export-always '*default-hostlist*)
(defparameter *default-hostlist*
  (make-instance 'hostlist
                 :url (quri:uri "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts")
                 :basename "hostlist-stevenblack.txt")
  "Default hostlist for `blocker-mode'.")

(define-mode blocker-mode ()
  "Enable blocking of listed hosts.
To customize the list of blocked hosts, set the `hostlists' slot.
See the `hostlist' class documentation.

Example:

\(defvar *my-blocked-hosts*
  (nyxt/blocker-mode:make-hostlist
   :hosts '(\"platform.twitter.com\"
            \"syndication.twitter.com\"
            \"m.media-amazon.com\")))

\(define-mode my-blocker-mode (nyxt/blocker-mode:blocker-mode)
  \"Blocker mode with custom hosts from `*my-blocked-hosts*'.\"
  ((nyxt/blocker-mode:hostlists (list *my-blocked-hosts* nyxt/blocker-mode:*default-hostlist*))))

\(define-configuration buffer
  ((default-modes (append '(my-blocker-mode) %slot-default%))))"
  ((hostlists (list *default-hostlist*))
   (thread
    nil
    :type (or bt:thread null)
    :export nil
    :documentation "This thread is used to update the hostlists in the background.")
   (worker-channel
    nil
    :type (or calispel:channel null)
    :export nil
    :documentation "Communication channel between `thread' and `blocker-mode'.")
   (blocked-hosts
    (make-hash-table :test 'equal)
    :export nil
    :documentation "The set of domain names to block.")
   (destructor
    (lambda (mode)
      (bt:destroy-thread (thread mode))
      (when (web-buffer-p (buffer mode))
        (hooks:remove-hook (request-resource-hook (buffer mode))
                           'request-resource-block))))
   (constructor
    (lambda (mode)
      (setf (worker-channel mode) (nyxt::make-channel))
      (setf (thread mode)
            (run-thread "async-data-path worker"
              (worker mode)
              :name "Blocker-mode hostlist update worker"))
      (load-hostlists mode)
      (when (web-buffer-p (buffer mode))
        (if (request-resource-hook (buffer mode))
            (hooks:add-hook (request-resource-hook (buffer mode))
                            (make-handler-resource #'request-resource-block))
            (make-hook-resource
             :combination #'combine-composed-hook-until-nil
             :handlers (list #'request-resource-block))))))))

(defmethod blocklisted-host-p ((mode blocker-mode) host)
  "Return non-nil of HOST if found in the hostlists of MODE.
Return nil if MODE's hostlist cannot be parsed."
  (gethash host (blocked-hosts mode)))

(defun request-resource-block (request-data)
  "Block resource queries from blocklisted hosts.
This is an acceptable handler for `request-resource-hook'."
  (let ((mode (find-submode (buffer request-data) 'blocker-mode)))
    (if (and mode
             (blocklisted-host-p
              mode
              (quri:uri-host (url request-data))))
        (progn
          (log:debug "Dropping ~a for ~a (~a)"
                     (render-url (url request-data))
                     (buffer request-data)
                     (render-url (url (buffer request-data))))
          nil)
        ;; Pass request to the other handlers.
        request-data)))

(defmethod s-serialization:serializable-slots ((object blocker-mode))
  "Discard hostlists which can get pretty big."
  (delete 'nyxt/blocker-mode::hostlists
          (mapcar #'closer-mop:slot-definition-name
                  (closer-mop:class-slots (class-of object)))))

(define-command update-hostlists (&optional (blocker-mode (find-mode (current-buffer) 'blocker-mode)))
  "Forces update for all the hostlists of `blocker-mode'."
  (load-hostlists blocker-mode :force-update-p t)
  (echo "Hostlists updating..."))
