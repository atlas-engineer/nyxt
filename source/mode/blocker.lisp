;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/blocker-mode
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum #:->)
  (:documentation "Block resource queries for listed hosts."))
(in-package :nyxt/blocker-mode)
(use-nyxt-package-nicknames)

;; TODO: Add convenient interface to block hosts depending on the current URL.

(define-class hostlist-data-path (data-path) ())

(define-class hostlist ()
  ((url
    (quri:uri "")
    :type quri:uri
    :documentation "URL where to download the list from.  If empty, no attempt
will be made at updating it.")
   (path
    (make-instance 'hostlist-data-path)
    :type hostlist-data-path
    :documentation "Where to find the list locally.
If nil, the list won't be persisted.
If path is relative, it will be set to (xdg-data-home path).")
   (hosts
    '()
    :documentation "Deprecated, see `load-hostlist' and `blocked-hosts' instead.")
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
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A hostlist `blocker-mode' can use for its `hostlists' slot.
See `*default-hostlist*' for an example."))

(serapeum:export-always 'make-hostlist)
(defun make-hostlist (&rest args)
  "Return a new `hostlist'.
See the `hostlist' class documentation."
  (apply #'make-instance 'hostlist args))

(-> download (hostlist) string)
(defun download (hostlist)
  "Fetch HOSTLIST and return it.
If HOSTLIST has a `path', persist it locally.
Return nil if hostlist file is not found.
Auto-update file if older than UPDATE-INTERVAL seconds and AUTO-UPDATE-P is non-nil.

The hostlist is downloaded in the background."
  (let ((path (expand-path (path hostlist))))
    (if (and (or (force-update-p hostlist)
                 (not (uiop:file-exists-p path))
                 (< (update-interval hostlist)
                    (- (get-universal-time) (uiop:safe-file-write-date path))))
             (not (uiop:emptyp (url hostlist))))
        (let ((url-string (render-url (url hostlist))))
          (handler-case
              (let ((hostlist-string (dex:get url-string)))
                (if path
                    (progn
                      (log:info "Updating hostlist ~s from ~s." path url-string)
                      (handler-case
                          (alex:write-string-into-file hostlist-string
                                                       (ensure-parent-exists path)
                                                       :if-exists :supersede
                                                       :if-does-not-exist :create)
                        (t (c)
                          (log:error "Could not persist hostlist ~a: ~a" url-string c))))
                    (progn
                      (log:info "Updating hostlist from ~s." url-string))))
            (t ()
              (log:error "Could not download hostlist ~a: ~a" url-string c))))
        (uiop:read-file-string path))))

(-> load-hostlist (hostlist) t)
(defun load-hostlist (hostlist mode)
  "Load HOSTLIST into MODE's `blocked-hosts'."
  (let ((hostlist-string (download hostlist)))
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
                       (setf (gethash host (blocked-hosts mode)) host))))))))

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
      (load-hostlist hostlist mode)
      (echo "Hostlist ~a updated." (render-url (url hostlist))))))

(serapeum:export-always '*default-hostlist*)
(defparameter *default-hostlist*
  (make-instance 'hostlist
                 :url (quri:uri "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts")
                 :path (make-instance 'hostlist-data-path :basename "hostlist-stevenblack.txt"))
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
