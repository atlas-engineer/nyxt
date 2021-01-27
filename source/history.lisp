;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class history-entry ()          ; TODO: Export?
  ((url (quri:uri "")
        :type (or quri:uri string))
   (title "")
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

;; TODO: Define serialization method for global-history-tree's last-access.

(declaim (ftype (function (quri:uri &key (:title string)) t) history-add))
(defun history-add (uri &key (title ""))
  "Add URL to the global/buffer-local history.
The `implicit-visits' count is incremented."
  ;; Warning: This should only be called from `web-mode''s `on-signal-notify-uri'.
  ;; `buffer-load' has its own data syncronization, so we assume that
  ;; history is up-to-date there.  Using `with-data-access' here is not
  ;; an option -- it will cause the new thread and the thread from
  ;; `buffer-load' to mutually deadlock.
  (let ((history (or (get-data (history-path (current-buffer)))
                     (htree:make))))
    (unless (url-empty-p uri)
      (htree:go-to-child (make-instance 'history-entry
                                        :url uri
                                        :title title)
                         history)
      (let* ((entry (htree:value (htree:current-owner-node history))))
        (incf (implicit-visits entry))))
    (setf (get-data (history-path (current-buffer))) history)))

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
  (+ (* 0.1
        ;; Total number of visits.
        (+ (implicit-visits entry)
           (explicit-visits entry)))
     (* 1.0
        ;; Inverse number of hours since the last access.
        (/ 1
           (1+ (/ (local-time:timestamp-difference (local-time:now)
                                                   ;; TODO: Or use current buffer last access?  Or both?
                                                   (htree:data-last-access history entry))
                  (* 60 60)))))))

(defun history-suggestion-filter (&key prefix-urls)
  "Include prefix-urls in front of the history.
This can be useful to, say, prefix the history with the current URL.  At the
moment the PREFIX-URLS are inserted as is, not a `history-entry' objects since
it would not be very useful."
  (with-data-access (hist (history-path (current-buffer)))
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

(defmethod restore ((profile data-profile) (path history-data-path)
                    ;; &key restore-session-p
                    &key
                    &allow-other-keys)
  "Restore the global/buffer-local history and session from the PATH."
  (handler-case
      (let ((data (with-data-file (file path
                                        :direction :input
                                        :if-does-not-exist nil)
                    (when file
                      ;; We need to make sure current package is :nyxt so that
                      ;; symbols are printed with consistent namespaces.
                      (let ((*package* (find-package :nyxt)))
                        (s-serialization:deserialize-sexp file))))))
        (match data
          (nil nil)
          ((guard (list version history) t)
           (ctypecase history
             (htree:history-tree
              (unless (string= version +version+)
                (log:warn "History version ~s differs from current version ~s"
                          version +version+))
              (echo "Loading history of ~a URLs from ~s."
                    (htree:size history)
                    (expand-path path))
              (setf (get-data path) history)
              ;; (when restore-session-p
              ;;   (let ((buffer-histories (buffer-local-histories-table history)))
              ;;     (when (< 0 (hash-table-count buffer-histories))
              ;;       ;; Make the new buffers.
              ;;       (dolist (root (alex:hash-table-values buffer-histories))
              ;;         ;; TODO: Restore buffer!
              ;;         (make-buffer-from-history root history))
              ;;       ;; Switch to the last active buffer.
              ;;       (let* ((current-history-nodes (remove-if #'null (mapcar #'current-history-node
              ;;                                                               (buffer-list))))
              ;;              (latest-id (id (first (sort (mapcar #'htree:value current-history-nodes)
              ;;                                          #'local-time:timestamp>
              ;;                                          :key #'last-access)))))
              ;;         (when latest-id (switch-buffer :id latest-id))))))
              )
             (hash-table
              (echo "Importing deprecated global history of ~a URLs from ~s."
                    (hash-table-count history)
                    (expand-path path))
              ;; TODO: Convert this `entry' to an htree:entry.
              (with-data-access (history path
                                 :default (htree:make))
                (dolist (entry (alex:hash-table-values history))
                  (htree:add-entry history entry))))))
          (_ (error "Expected (list version history) structure."))))
    (error (c)
      (echo-warning "Failed to restore history from ~a: ~a"
                    (expand-path path) c))))
