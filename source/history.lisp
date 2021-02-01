;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class history-entry ()          ; TODO: Export?
  ((url (quri:uri "")
        :type (or quri:uri string))
   (title "")
   (last-access "" ; TODO: Remove with Nyxt 2.0?
                :type (or string local-time:timestamp)
                :export nil
                :documentation "This slot is only kept for backward
compatibility to import the old flat history.")
   ;; TODO: For now we never increment the explicit-visits count.  Maybe we
   ;; could use a new buffer slot to signal that the last load came from an
   ;; explicit request?
   (explicit-visits 0
                    :type integer
                    :documentation "
Number of times the URL was visited by a minibuffer request.  This does not
include implicit visits.")
   (implicit-visits 0
                    :type integer
                    :documentation "
Number of times the URL was visited by following a link on a page.  This does
not include explicit visits."))
  (:accessor-name-transformer #'class*:name-identity)
  (:documentation "
Entry for the global history.
The total number of visit for a given URL is (+ explicit-visits implicit-visits)."))

(defmethod object-string ((entry history-entry))
  (object-string (url entry)))

(defmethod object-display ((entry history-entry))
  (format nil "~a  ~a" (object-display (url entry)) (title entry)))

(export-always 'equals)
(defmethod equals ((e1 history-entry) (e2 history-entry))
  (quri:uri= (url e1) (url e2)))

(defmethod url ((he history-entry))
  "This accessor ensures we always return a `quri:uri'.
This is useful in cases the URL is originally stored as a string (for instance
when deserializing a `history-entry').

We can't use `initialize-instance :after' to convert the URL because
`s-serialization:deserialize-sexp' sets the slots manually after making the
class."
  (unless (quri:uri-p (slot-value he 'url))
    (setf (slot-value he 'url) (ensure-url (slot-value he 'url))))
  (slot-value he 'url))

(defmethod s-serialization::serialize-sexp-internal ((he history-entry)
                                                     stream
                                                     serialization-state)
  "Serialize `history-entry' by turning the URL into strings."
  (let ((new-he (make-instance 'history-entry
                               :title (title he)
                               :explicit-visits (explicit-visits he)
                               :implicit-visits (implicit-visits he))))
    (setf (url new-he) (object-string (url he)))
    (call-next-method new-he stream serialization-state)))

(defmethod s-serialization::serialize-sexp-internal ((binding htree:binding)
                                                     stream
                                                     serialization-state)
  "Serialize `history-entry' by turning the URL into strings."
  (let ((new-binding (make-instance 'htree:binding
                                    :forward-child (htree:forward-child binding))))
    (setf (slot-value new-binding 'htree:last-access)
          (local-time:format-timestring nil (htree:last-access binding)
                                        :timezone local-time:+utc-zone+))
    (call-next-method new-binding stream serialization-state)))

(defmethod s-serialization::serialize-sexp-internal ((accessors htree:entry-accessors)
                                                     stream
                                                     serialization-state)
  "Serialize `history-entry' by turning the URL into strings."
  (let ((new-accessors (make-instance 'htree:entry-accessors
                                      :nodes (htree:nodes accessors))))
    (setf (htree:last-access new-accessors)
          (local-time:format-timestring nil (htree:last-access accessors)
                                        :timezone local-time:+utc-zone+))
    (call-next-method new-accessors stream serialization-state)))

(defun make-history-tree (&optional (buffer (current-buffer)))
  "Return a new global history tree for `history-entry' data."
  (htree:make :key 'url :current-owner-identifier (id buffer)))

(declaim (ftype (function (quri:uri &key (:title string)) t) history-add))
(defun history-add (uri &key (title ""))
  "Add URL to the global/buffer-local history.
The `implicit-visits' count is incremented."
  (with-data-access (history (history-path (current-buffer))
                     :default (make-history-tree))
    (unless (url-empty-p uri)
      (htree:add-child (make-instance 'history-entry
                                      :url uri
                                      :title title)
                       history)
      (let* ((entry (htree:value (htree:current-owner-node history))))
        (incf (implicit-visits entry))))))

(define-command delete-history-entry ()
  "Delete queried history entries."
  (with-data-access (history (history-path (current-buffer)))
    (let ((entries (prompt-minibuffer
                    :input-prompt "Delete entries"
                    :suggestion-function (history-suggestion-filter)
                    :history (minibuffer-set-url-history *browser*)
                    :multi-selection-p t)))
      (dolist (entry entries)
        (htree:delete-data history entry)))))

(defun score-history-entry (entry history)
  "Return history ENTRY score.
The score gets higher for more recent entries and if they've been visited a
lot."
  ;; TODO: Or use current owner last access?  Or both?
  (let ((last-access (htree:data-last-access history entry)))
    (+ (* 0.1
          ;; Total number of visits.
          (+ (implicit-visits entry)
             (explicit-visits entry)))
       (if last-access
           (* 1.0
              ;; Inverse number of hours since the last access.
              (/ 1
                 (1+ (/ (local-time:timestamp-difference (local-time:now)
                                                         last-access)
                        (* 60 60)))))
           0))))

(defun history-suggestion-filter (&key prefix-urls)
  "Include prefix-urls in front of the history.
This can be useful to, say, prefix the history with the current URL.  At the
moment the PREFIX-URLS are inserted as is, not a `history-entry' objects since
it would not be very useful."
  (with-data-lookup (hist (history-path (current-buffer)))
    (let* ((all-history-entries (when hist
                                  (sort (htree:all-data hist)
                                        (lambda (x y)
                                          (> (score-history-entry x hist)
                                             (score-history-entry y hist))))))
           (prefix-urls (delete-if #'uiop:emptyp prefix-urls)))
      (when prefix-urls
        (setf all-history-entries (append (mapcar #'quri:url-decode prefix-urls)
                                          all-history-entries)))
      (lambda (minibuffer)
        (fuzzy-match (input-buffer minibuffer) all-history-entries)))))

(defun history-stored-data (path)
  "Return the history data that needs to be serialized.
This data can be used to restore the session later, e.g. when starting a new
instance of Nyxt."
  (list +version+ (get-data path)))

(defmethod store ((profile data-profile) (path history-data-path) &key &allow-other-keys)
  "Store the global/buffer-local history to the PATH."
  (with-data-file (file path
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    ;; We READ the output of serialize-sexp to make it more human-readable.
    (let ((*package* (find-package :nyxt))
          (*print-length* nil))
      ;; We need to make sure current package is :nyxt so that
      ;; symbols are printed with consistent namespaces.
      (format file
              "~s"
              (with-input-from-string (in (with-output-to-string (out)
                                            (s-serialization:serialize-sexp
                                             (history-stored-data path)
                                             out)))
                (read in))))))

;; REVIEW: This works around the issue of cl-prevalence to deserialize structs
;; with custom constructors: https://github.com/40ants/cl-prevalence/issues/16.
(setf (fdefinition 'quri.uri::make-uri) #'quri.uri::%make-uri)

;; Hack of cl-prevalence to support the history-tree custom hash tables:
(defun history-deserialize-sexp (stream &optional (serialization-state (s-serialization::make-serialization-state)))
  "Read and return an s-expression serialized version of a lisp object from stream, optionally reusing a serialization state"
  (s-serialization::reset serialization-state)
  (let ((sexp (read stream nil :eof)))
    (if (eq sexp :eof)
        nil
        (history-deserialize-sexp-internal sexp (s-serialization::get-hashtable serialization-state)))))

;; Hack of cl-prevalence to support the history-tree custom hash tables:
(defun history-deserialize-sexp-internal (sexp deserialized-objects)
  (if (atom sexp)
      sexp
      (ecase (first sexp)
        (:sequence (destructuring-bind (id &key class size elements) (rest sexp)
                     (cond
                       ((not class)
                        (error "Unknown sequence class"))
                       ((not size)
                        (error "Unknown sequence size"))
                       (t
                        (let ((sequence (make-sequence class size)))
                          (declare (ignorable sequence))
                          (setf (gethash id deserialized-objects) sequence)
                          (map-into sequence
                                    #'(lambda (x) (history-deserialize-sexp-internal x deserialized-objects))
                                    elements))))))
        (:hash-table (destructuring-bind (id &key test size rehash-size rehash-threshold entries) (rest sexp)
                       (cond
                         ((not test)
                          (error "Test function is unknown"))
                         ((not size)
                          (error "Hash table size is unknown"))
                         ((not rehash-size)
                          (error "Hash table's rehash-size is unknown"))
                         ((not rehash-threshold)
                          (error "Hash table's rehash-threshold is unknown"))
                         (t
                          (if (member test '(eq eql equal equalp))
                              (let ((hash-table (make-hash-table :size size
                                                                 :test test
                                                                 :rehash-size rehash-size
                                                                 :rehash-threshold rehash-threshold)))
                                (setf (gethash id deserialized-objects) hash-table)
                                (dolist (entry entries)
                                  (setf (gethash (history-deserialize-sexp-internal (first entry) deserialized-objects) hash-table)
                                        (history-deserialize-sexp-internal (rest entry) deserialized-objects)))
                                hash-table)
                              (let ((hash-table (htree::make-entry-hash-table)))
                                (cl-custom-hash-table:with-custom-hash-table
                                  (setf (gethash id deserialized-objects) hash-table)
                                  (dolist (entry entries)
                                    (setf (gethash (history-deserialize-sexp-internal (first entry) deserialized-objects) hash-table)
                                          (history-deserialize-sexp-internal (rest entry) deserialized-objects))))
                                hash-table))))))

        (:object (destructuring-bind (id &key class slots) (rest sexp)
                   (let ((object (s-serialization::deserialize-class class slots deserialized-objects)))
                     (setf (gethash id deserialized-objects) object)
                     (dolist (slot slots)
                       (when (slot-exists-p object (first slot))
                         (setf (slot-value object (first slot))
                               (history-deserialize-sexp-internal (rest slot) deserialized-objects))))
                     object)))
        (:struct (destructuring-bind (id &key class slots) (rest sexp)
                   (let ((object (s-serialization::deserialize-struct class slots deserialized-objects)))
                     (setf (gethash id deserialized-objects) object)
                     (dolist (slot slots)
                       (when (slot-exists-p object (first slot))
                         (setf (slot-value object (first slot))
                               (history-deserialize-sexp-internal (rest slot) deserialized-objects))))
                     object)))
        (:cons (destructuring-bind (id cons-car cons-cdr) (rest sexp)
                 (let ((conspair (cons nil nil)))
                   (setf (gethash id deserialized-objects)
                         conspair)
                   (rplaca conspair (history-deserialize-sexp-internal cons-car deserialized-objects))
                   (rplacd conspair (history-deserialize-sexp-internal cons-cdr deserialized-objects)))))
        (:ref (gethash (rest sexp) deserialized-objects)))))

(defmethod s-serialization::deserialize-class ((history (eql 'htree:history-tree)) slots deserialized-objects)
  ;; We need this specialization because 'history-tree cannot be make-instance'd
  ;; without specifying some slots like `owners'.
  (let ((history (htree:make)))
    history))

(defvar flat-history-path (make-instance 'history-data-path :basename "history") ; TODO: Move to global.lisp?
  "Global flat history that was used before the introduction of the global history tree.
This is deprecated.
We keep this variable as a means to import the old format to the new one.")

(defmethod restore ((profile data-profile) (path history-data-path)
                    &key &allow-other-keys)
  "Restore the global/buffer-local history and session from the PATH."
  (labels ((restore-buffers (history)
             "For each owner, make buffer, swap owner identifier for buffer id.
             Keep table of old-id -> new-id, then go through all the owners and update their creator."
             (let ((old-id->new-id (make-hash-table :test 'equalp))
                   (new-owners (make-hash-table :test #'equalp)))
               (maphash (lambda (owner-id owner)
                          ;; `htree:+default-owner+' may be present if the
                          ;; history is created (e.g. restored) while no web
                          ;; buffer exists.  In all cases, this owner is
                          ;; uninteresting.
                          (unless (equal owner-id htree:+default-owner+)
                            (let ((current-node (htree:current
                                                 (htree:owner history owner-id))))
                              ;; Node-less owners can safely be ignored.
                              (when current-node
                                (let ((new-buffer (make-buffer :title (title (htree:value current-node))
                                                               :url (url (htree:value current-node))
                                                               :load-url-p nil)))
                                  (setf (gethash owner-id old-id->new-id) (id new-buffer))
                                  (setf (gethash (id new-buffer) new-owners) owner))))))
                        (htree:owners history))
               (maphash (lambda (_ owner)
                          (declare (ignore _))
                          (setf (htree:creator owner)
                                (gethash (htree:creator owner) old-id->new-id)))
                        (htree:owners history))
               (setf (htree:owners history) new-owners))
             ;; TODO: Focus last buffer.
             ;; (let ((latest-id (id (htree:current
             ;;                       (first (sort (alex:hash-table-values (htree:owners history))
             ;;                                    #'local-time:timestamp>
             ;;                                    :key #'htree:last-access))))
             ;;                  ;; (sort (mapcar #'id (buffer-list))
             ;;                  ;;       (lambda ()))
             ;;                  ))
             ;;   (switch-buffer :id latest-id))
             )

           (restore-history-tree (history)
             (echo "Loading history of ~a URLs from ~s."
                   (hash-table-count (htree:entries history))
                   (expand-path path))
             (setf (get-data path) history)
             (match (session-restore-prompt *browser*)
               ;; Need `funcall-safely' so we continue if the user exits the
               ;; minibuffer (which raises a condition).
               (:always-ask (if-confirm ("Restore session?")
                                        (restore-buffers history)))
               (:always-restore (restore-buffers history))
               (:never-restore (log:info "Not restoring session."))))

           (restore-flat-history (old-history old-path)
             (echo "Importing deprecated global history of ~a URLs from ~s."
                   (hash-table-count old-history)
                   (expand-path old-path))
             (with-data-access (history path
                                :default (make-history-tree)) ; TODO: What shall the default owner be here?
               (maphash (lambda (key data)
                          (declare (ignore key))
                          (let ((last-access (last-access data)))
                            ;; Remove last-access from DATA to avoid storing it
                            ;; twice.
                            (setf (last-access data) "")
                            (htree:add-entry history data last-access)))
                        old-history)))

           (%restore ()
             (let* ((path (if (uiop:file-exists-p (expand-path path))
                              path
                              flat-history-path))
                    (data (with-data-file (file path
                                               :direction :input
                                               :if-does-not-exist nil)
                           (when file
                             ;; We need to make sure current package is :nyxt so that
                             ;; symbols are printed with consistent namespaces.
                             (let ((*package* (find-package :nyxt)))
                               (history-deserialize-sexp file))))))
               (match data
                 (nil nil)
                 ((guard (list version history) t)
                  (unless (string= version +version+)
                    (log:warn "History version ~s differs from current version ~s"
                              version +version+))
                  (ctypecase history
                    (htree:history-tree
                     (restore-history-tree history))

                    (hash-table
                     (restore-flat-history history path))))
                 (_ (error "Expected (list version history) structure."))))))

    (if *keep-alive*
        (%restore)
        (handler-case (%restore)
          (error (c)
            (echo-warning "Failed to restore history from ~a: ~a"
                          (expand-path path) c))))))
