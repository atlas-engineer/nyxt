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

(define-class hostlist (nfiles:data-file nyxt-remote-file)
  ((hosts
    '()
    :type (or (cons string *) null)
    :documentation "List of hosts to ignore.
This is useful to reference hosts manually instead of via `nfiles:url'.")
   (nfiles:update-interval
    #.(* 60 60 24)
    :type unsigned-byte
    :documentation "If URL is provided, update the list after this amount of
seconds."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A hostlist `blocker-mode' can use for its `hostlists' slot.
See `*default-hostlist*' for an example."))

(sera:export-always 'make-hostlist)
(defun make-hostlist (&rest args)
  "Return a new `hostlist'.
See the `hostlist' class documentation."
  (apply #'make-instance 'hostlist args))


(sera:export-always '*default-hostlist*)
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

\(defmethod customize-instance ((buffer buffer) &key)
  (my-blocker-mode :buffer buffer))"
  ((hostlists (list *default-hostlist*))
   (blocked-hosts
    (make-hash-table :test 'equal)
    :export nil
    :documentation "The set of host names to block.")))

(defmethod enable ((mode blocker-mode) &key)
  (when (web-buffer-p (buffer mode))
    (if (request-resource-hook (buffer mode))
        (hooks:add-hook (request-resource-hook (buffer mode))
                        'request-resource-block)
        (make-instance 'hook-resource
                       :combination #'combine-composed-hook-until-nil
                       :handlers '(request-resource-block)))))

(defmethod disable ((mode blocker-mode) &key)
  (when (web-buffer-p (buffer mode))
    (hooks:remove-hook (request-resource-hook (buffer mode))
                       'request-resource-block)))

(defmethod nfiles:write-file ((profile nyxt-profile) (hostlist hostlist) &key destination)
  "Write the downloaded hostlist to disk.
This is the raw downloaded content and not the serialized parsed content.
This gives more integrity guaranteees to the user and allows external manipulation."
  (unless (uiop:emptyp (nfiles:url-content hostlist))
    (alex:write-string-into-file (nfiles:url-content hostlist) destination :if-exists :supersede)))

(defmethod nfiles:deserialize ((profile nyxt-profile) (hostlist hostlist) raw-content &key)
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
  (clrhash (blocked-hosts blocker-mode))
  (dolist (hostlist (hostlists blocker-mode))
    ;; TODO: Allow running in the background, but warning, it could leak
    ;; personal information to undesired third-party.
    (dolist (host (or (hosts hostlist)
                      (let ((path (nfiles:expand hostlist)))
                        (unless (uiop:file-exists-p path)
                          (echo "Updating hostlist ~s..." path))
                        (nfiles:content hostlist
                                        :force-update force-update-p))))
      (setf (gethash host (blocked-hosts blocker-mode)) host))))

(defmethod blocklisted-host-p ((mode blocker-mode) host)
  "Return non-nil of HOST if found in the hostlists of MODE.
Return nil if MODE's hostlist cannot be parsed."
  (gethash host (blocked-hosts mode)))

(defun request-resource-block (request-data)
  "Block resource queries from blocklisted hosts.
This is an acceptable handler for `request-resource-hook'."
  (let ((mode (find-submode 'blocker-mode (buffer request-data))))
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

(define-command update-hostlists (&optional (blocker-mode (find-submode 'nyxt/blocker-mode:blocker-mode (current-buffer))))
  "Forces update for all the hostlists of `blocker-mode'."
  (load-hostlists blocker-mode :force-update-p t))
