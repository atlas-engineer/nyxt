;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class history-entry ()
  ((url (quri:uri "")
        :type quri:uri)
   (title "")
   (id ""
       :documentation "The `id' of the buffer that this entry belongs to the branch of.")
   (last-access (local-time:now)
                :type local-time:timestamp)
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

(defmethod equals ((e1 history-entry) (e2 history-entry))
  ;; We need to compare IDs to preserve history entries of different buffers.
  ;; TODO: Should we?
  (and (string= (id e1) (id e2))
       (quri:uri= (url e1) (url e2))))

(declaim (ftype (function (quri:uri &key (:title string) (:explicit t)) t) history-add))
(export-always 'history-add)
(defun history-add (uri &key title explicit)
  "Add URL to the global/buffer-local history.
The `implicit-visits' count is incremented unless EXPLICIT is non-nil, in which
case `explicit-visits'.
The history is sorted by last access."
  (with-data-access (history (history-path (current-buffer))
                             :default (htree:make))
      (unless (url-empty-p uri)
        (let* ((maybe-entry (make-instance 'history-entry
                                           :url uri :id (id (current-buffer)) :title title))
               (node (htree:find-data maybe-entry history :ensure-p t :test #'equals))
               (entry (htree:data node)))
          (if explicit
              (incf (explicit-visits entry))
              (incf (implicit-visits entry)))
          (setf (last-access entry) (local-time:now))
          (when title
            ;; Always update the title since it may have changed since last visit.
            (setf (title entry) title))
          (setf (htree:data node) entry)))
    (setf (current-history-node (current-buffer)) (htree:current history))))

(define-command delete-history-entry ()
  "Delete queried history entries."
  (with-data-access (history (history-path (current-buffer)))
    (let ((entries (prompt-minibuffer
                    :input-prompt "Delete entries"
                    :suggestion-function (history-suggestion-filter)
                    :history (minibuffer-set-url-history *browser*)
                    :multi-selection-p t)))
      (dolist (entry entries)
        (htree:delete-node entry history :test #'equals :rebind-children-p t)))))


(defun score-history-entry (entry)
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
                                                   (last-access entry))
                  (* 60 60)))))))

(defun history-suggestion-filter (&key prefix-urls)
  "Include prefix-urls in front of the history.
This can be useful to, say, prefix the history with the current URL.  At the
moment the PREFIX-URLS are inserted as is, not a `history-entry' objects since
it would not be very useful."
  (with-data-access (hist (history-path (current-buffer)))
      (let* ((history (when hist
                        (sort (htree:all-nodes-data hist)
                              (lambda (x y)
                                (> (score-history-entry x)
                                   (score-history-entry y))))))
          (prefix-urls (delete-if #'uiop:emptyp prefix-urls)))
     (when prefix-urls
       (setf history (append (mapcar #'quri:url-decode prefix-urls) history)))
     (lambda (minibuffer)
       (fuzzy-match (input-buffer minibuffer) history)))))

(defun history-stored-data ()
  "Return the history data that needs to be serialized.
This data can be used to restore the session later, e.g. when starting a new
instance of Nyxt."
  (list +version+
        (get-data (history-path (current-buffer)))))

(defmethod store ((profile data-profile) (path history-data-path))
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
                                             (history-stored-data)
                                             out)))
                (read in))))))

;; REVIEW: This works around the issue of cl-prevalence to deserialize structs
;; with custom constructors: https://github.com/40ants/cl-prevalence/issues/16.
(setf (fdefinition 'quri.uri::make-uri) #'quri.uri::%make-uri)

(defmethod restore ((profile data-profile) (path history-data-path))
  "Restore the global/buffer-local history from the PATH."
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
          ((guard (list version history)
                  (typep 'htree:history-tree history))
           (unless (string= version +version+)
             (log:warn "History version ~s differs from current version ~s"
                       version +version+))
           (echo "Loading history of ~a URLs from ~s."
                 (htree:size history)
                 (expand-path path))
           (setf (get-data path) history))
          (_ (error "Expected (list version history) structure."))))
    (error (c)
      (echo-warning "Failed to restore history from ~a: ~a"
                    (expand-path path) c))))
