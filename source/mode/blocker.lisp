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

(define-class hostlist (nfiles:data-file nyxt-file)
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
   (update-interval
    (* 60 60 24)
    :type integer
    :documentation "If URL is provided, update the list after this amount of
seconds."))
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


(serapeum:export-always '*default-hostlist*)
(defparameter *default-hostlist*
  (make-instance 'hostlist
                 :url (quri:uri "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts")
                 :base-path #p"hostlist-stevenblack.txt")
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
   (blocked-hosts
    (make-hash-table :test 'equal)
    :export nil
    :documentation "The set of host names to block.")
   (destructor
    (lambda (mode)
      (when (web-buffer-p (buffer mode))
        (hooks:remove-hook (request-resource-hook (buffer mode))
                           'request-resource-block))))
   (constructor
    (lambda (mode)
      (load-hostlists mode)
      (when (web-buffer-p (buffer mode))
        (if (request-resource-hook (buffer mode))
            (hooks:add-hook (request-resource-hook (buffer mode))
                            'request-resource-block)
            (make-instance 'hook-resource
             :combination #'combine-composed-hook-until-nil
             :handlers '(request-resource-block))))))))

(defmethod nfiles:write-file ((profile application-profile) (hostlist hostlist) &key)
  "Download the hostlist file if it has a URL."
  (when (url hostlist)
    (let ((url-string (render-url (url hostlist)))
          (path (nfiles:expand hostlist)))
      (unless (nfiles:nil-pathname-p path)
        (log:debug "Downloading hostlist from ~a to ~a." url-string path)
        (with-protect ("Could not download hostlist ~a: ~a" url-string :condition)
          (uiop:with-staging-pathname (destination path)
            (with-open-file (stream destination :direction :output :if-exists :supersede)
              (dex:get url-string :stream stream)))

          (echo "Hostlist ~a updated." url-string))))))

(defmethod nfiles:read-file ((profile application-profile) (hostlist hostlist) &key)
  "If file does not exist or if `update-interval' is less than the last write date of the file,
fetch HOSTLIST file in the background from its `url' and save it on disk.
When the download is done, `nfiles:content' should automatically retrieve the
new list."
  (let ((path (nfiles:expand hostlist)))
    (if (and (not (url-empty-p (url hostlist)))
             (or (not (uiop:file-exists-p path))
                 (< (update-interval hostlist)
                    (- (get-universal-time) (uiop:safe-file-write-date path)))))
        (run-thread "blocker-mode hostlist updater"
          (nfiles:write-file profile hostlist))
        (when (uiop:file-exists-p path)
          (log:debug "Restoring hostlist from ~a" path)
          (call-next-method)))))

(defmethod nfiles:deserialize ((profile application-profile) (hostlist hostlist) raw-content &key)
  (flet ((empty-line? (line)
           (< (length line) 2))
         (comment? (line)
           (string= (subseq line 0 1) "#"))
         (custom-hosts? (line)
           (not (str:starts-with? "0.0.0.0" line))))
    (loop as line = (read-line raw-content nil)
          while line
          unless (or (empty-line? line)
                     (comment? line)
                     (custom-hosts? line))
            collect (second (str:split " " line)))))

(-> load-hostlists (blocker-mode &key (:force-update-p boolean)) t)
(defun load-hostlists (blocker-mode &key force-update-p)
  "Load BLOCKER-MODE's hostlists into `blocked-hosts' (in the background)."
  (dolist (hostlist (hostlists blocker-mode))
    (dolist (host (or (hosts hostlist)
                      (nfiles:content hostlist force-update-p)))
      (setf (gethash host (blocked-hosts blocker-mode)) host))))

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
