;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defclass history-entry ()
  ((url :initarg :url
        :accessor url
        :type quri:uri
        :initform (quri:uri ""))
   (title :initarg :title
          :accessor title
          :type string
          :initform "")
   (last-access :initarg :last-access
                :accessor last-access
                :type local-time:timestamp
                :initform (local-time:now))
   ;; TODO: For now we never increment the explicit-visits count.  Maybe we
   ;; could use a new buffer slot to signal that the last load came from an
   ;; explicit request?
   (explicit-visits :initarg :explicit-visits
                    :accessor explicit-visits
                    :type integer
                    :initform 0
                    :documentation "
Number of times the URL was visited by a minibuffer request.  This does not
include implicit visits.")
   (implicit-visits :initarg :implicit-visits
                    :accessor implicit-visits
                    :type integer
                    :initform 0
                    :documentation "
Number of times the URL was visited by following a link on a page.  This does
not include explicit visits."))
  (:documentation "
Entry for the global history.
The total number of visit for a given URL is (+ explicit-visits implicit-visits)."))

(defmethod object-string ((entry history-entry))
  (object-string (url entry)))

(defmethod object-display ((entry history-entry))
  (format nil "~a  ~a" (object-display (url entry)) (title entry)))

(defmethod equals ((e1 history-entry) (e2 history-entry))
  (quri:uri= (url e1) (url e2)))

(declaim (ftype (function (quri:uri &key (:title string) (:explicit t)) t) history-add))
(export-always 'history-add)
(defun history-add (uri &key title explicit)
  "Add URL to the global/buffer-local history.
The `implicit-visits' count is incremented unless EXPLICIT is non-nil, in which
case `explicit-visits'.
The history is sorted by last access."
  (with-data-access history (history-path (current-buffer))
    (unless (url-empty-p uri)
      (unless history
        (setf history (make-hash-table :test #'equal)))
      (let ((entry (gethash (object-string uri) history)))
        (unless entry
          (setf entry (make-instance 'history-entry :url uri)))
        (if explicit
            (incf (explicit-visits entry))
            (incf (implicit-visits entry)))
        (setf (last-access entry) (local-time:now))
        ;; Always update the title since it may have changed since last visit.
        (setf (title entry) title)
        (setf (gethash (object-string uri) history) entry)))))

(define-command delete-history-entry ()
  "Delete queried history entries."
  (with-data-access history (history-path (current-buffer))
    (with-result (entries (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Delete entries"
                            :suggestion-function (history-suggestion-filter)
                            :history (minibuffer-set-url-history *browser*)
                            :multi-selection-p t)))
      (dolist (entry entries)
        (remhash (object-string (url entry)) history)))))



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
  (let* ((path (history-path (current-buffer)))
         (history (when (get-data path)
                       (sort (alex:hash-table-values
                              (get-data path))
                             (lambda (x y)
                               (> (score-history-entry x)
                                  (score-history-entry y))))))
        (prefix-urls (delete-if #'uiop:emptyp prefix-urls)))
    (when prefix-urls
      (setf history (append (mapcar #'quri:url-decode prefix-urls) history)))
    (lambda (minibuffer)
      (fuzzy-match (input-buffer minibuffer) history))))

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
                  (hash-table-p history))
           (unless (string= version +version+)
             (log:warn "History version ~s differs from current version ~s"
                       version +version+))
           (echo "Loading global history of ~a URLs from ~s."
                 (hash-table-count history)
                 (expand-path path))
           (setf (get-data path) history))
          (_ (error "Expected (list version history) structure."))))
    (error (c)
      (echo-warning "Failed to restore history from ~a: ~a"
                    (expand-path path) c))))
