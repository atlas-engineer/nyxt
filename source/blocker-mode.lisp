(uiop:define-package :next/blocker-mode
    (:use :common-lisp :trivia :next :annot.class)
  (:export
   :hostlist
   :make-hostlist)
  (:documentation "Block resource queries blacklisted hosts."))
(in-package :next/blocker-mode)
(annot:enable-annot-syntax)

;; TODO: Add convenient interface to block hosts depending on the current URL.

@export
@export-accessors
(defclass hostlist ()
  ((url :accessor url :initarg :url
        :initform nil
        :documentation "URL where to download the list from.  If nil, no attempt
will be made at updating it.")
   (path :accessor path :initarg :path
         :initform nil
         :documentation "Where to find the list locally.
If nil, the list won't be persisted.
If path is relative, it will be set to (xdg-data-home path).")
   (hosts :accessor hosts :initarg :hosts
          :initform '()
          :documentation "The list of domain name.")
   (update-interval :accessor update-interval :initarg :update-interval
                    :initform (* 60 60 24)
                    :documentation "If URL is provided, update the list after
this amount of seconds.")))

@export
(defmethod path ((hostlist hostlist))
  (with-slots (path) hostlist
    (if (uiop:absolute-pathname-p path)
        path
        (xdg-data-home path))))

@export
(defun make-hostlist (&rest args)
  (apply #'make-instance 'hostlist args))

(defmethod update ((hostlist hostlist))
  "Fetch HOSTLIST and return it.
If HOSTLIST has a `path', persist it locally."
  (when (url hostlist)
    (echo "Updating hostlist ~a from ~a" (path hostlist) (url hostlist))
    (let ((hosts (dex:get (url hostlist))))
      (when (path hostlist)
        ;; TODO: In general, we should do more error checking when writing to disk.
        (alexandria:write-string-into-file hosts (path hostlist)
                                           :if-exists :overwrite
                                           :if-does-not-exist :create))
      hosts)))

(defmethod load-to-memory ((hostlist hostlist))
  "Load hostlist.
Auto-update file if older than UPDATE-INTERVAL seconds."
  (if (and (uiop:file-exists-p (path hostlist))
           (< (- (get-universal-time) (uiop:safe-file-write-date (path hostlist)))
              (update-interval hostlist)))
      (uiop:read-file-string (path hostlist))
      (update hostlist)))

(defmethod parse ((hostlist hostlist))
  "Return the hostlist as a list of strings."
  ;; We could use a hash table instead, but a simple benchmark shows little difference.
  (unless (hosts hostlist)
    (setf (hosts hostlist)
          (with-input-from-string (stream (load-to-memory hostlist))
            (loop as line = (read-line stream nil)
                  while line
                  unless (or (< (length line) 2) (string= (subseq line 0 1) "#"))
                    collect (second (str:split " " line))))))
  (hosts hostlist))

@export
(defparameter *default-host-list*
  (make-instance 'hostlist
                 :url "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
                 :path "hostlist-stevenblack"))

(define-mode blocker-mode ()
    "Enable blocking of blacklisted hosts."
    ((hostlists :accessor hostlists :initarg :hostlists
                :initform (list *default-host-list*))
     (previous-blocker :initform nil
                       :documentation "Save the previous blocker to be restored
when disabling the mode.")
     (destructor
      :initform
      (lambda (mode)
        (setf (resource-query-function (buffer mode))
              (slot-value mode 'previous-blocker))))
     (constructor
      :initform
      (lambda (mode)
        (let ((active-buffer (buffer mode)))
          (setf (slot-value mode 'previous-blocker) (resource-query-function active-buffer))
          (setf (resource-query-function active-buffer) #'resource-query-block))))))

(defmethod blacklisted-host-p ((mode blocker-mode) host)
  "Return non-nil of HOST if found in the hostlists of MODE."
  (when host
    (not (loop for hostlist in (hostlists mode)
               never (member-string host (parse hostlist))))))

(defmethod resource-query-block ((buffer buffer)
                                 &key url
                                   cookies
                                   event-type
                                   (is-new-window nil)
                                   (is-known-type t)
                                   (mouse-button "")
                                   (modifiers '())
                                 &allow-other-keys)
  "Block resource queries from blacklisted hosts.
Fall back on `resource-query-default'."
  ;; TODO: Use quri:uri-domain?
  (if (blacklisted-host-p (find-mode buffer 'blocker-mode)
                          (ignore-errors (quri:uri-host (quri:uri url))))
      (progn
        (log:info "Dropping ~a" url)
        nil)
      (resource-query-default buffer
                              :url url
                              :cookies cookies
                              :event-type event-type
                              :is-new-window is-new-window
                              :is-known-type is-known-type
                              :mouse-button mouse-button
                              :modifiers modifiers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :s-serialization)

(defmethod serializable-slots ((object next/blocker-mode::blocker-mode))
  "Discard hostlists which can get pretty big."
  (delete 'next/blocker-mode::hostlists
          (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))))
