;;; history.lisp --- manage and create bookmarks

(in-package :next)

(defclass history-entry ()
  ((url :initarg :url
        :accessor url
        :type string
        :initform "")
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
  (url entry))

(defmethod object-display ((entry history-entry))
  (format nil "~a  ~a" (url entry) (title entry)))

(defmethod equals ((e1 history-entry) (e2 history-entry))
  (string= (url e1) (url e2)))

(serapeum:export-always 'history-add)
(defun history-add (url &key title explicit)
  "Add URL to the global history.
The `implicit-visits' count is incremented unless EXPLICIT is non-nil, in which
case `explicit-visits'.
The history is sorted by last access."
  (unless (str:emptyp url)
    (unless (history-data *browser*)
      (setf (slot-value *browser* 'history-data)
            (make-hash-table :test #'equal)))
    (let ((entry (gethash url (history-data *browser*))))
      (unless entry
        (setf entry (make-instance 'history-entry
                                   :url url)))
      (if explicit
          (incf (explicit-visits entry))
          (incf (implicit-visits entry)))
      (setf (last-access entry) (local-time:now))
      (unless (str:emptyp title)
        (setf (title entry) title))
      (setf (gethash url (history-data *browser*)) entry)
      ;; Use accessor to ensure store function is called.
      (setf (history-data *browser*) (history-data *browser*)))))

(define-command delete-history-entry ()
  "Delete queried history entries."
  (with-result (entries (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Delete entries"
                          :completion-function (history-completion-filter)
                          :history (minibuffer-set-url-history *browser*)
                          :multi-selection-p t)))
    (dolist (entry entries)
      (remhash (url entry) (history-data *browser*)))
    ;; Use accessor to ensure store function is called.
    (setf (history-data *browser*) (history-data *browser*))))



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

(defun history-completion-filter ()
  (let ((history (sort (alex:hash-table-values
                        (history-data *browser*))
                       (lambda (x y)
                         (> (score-history-entry x)
                            (score-history-entry y))))))
    (lambda (input)
      (fuzzy-match input history))))

(defun history-stored-data ()
  "Return the history data that needs to be serialized.
This data can be used to restore the session later, e.g. when starting a new
instance of Next."
  (list +version+
        (history-data *browser*)))

(defun store-sexp-history ()            ; TODO: Factor with `store-sexp-session'.
  "Store the global history to the browser `history-path'."
  (with-open-file (file (history-path *browser*)
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    ;; We READ the output of serialize-sexp to make it more human-readable.
    (let ((*package* *package*))
      ;; We need to make sure current package is :next so that
      ;; symbols a printed with consistent namespaces.
      (in-package :next)
      (format file
              "~s"
              (with-input-from-string (in (with-output-to-string (out)
                                            (s-serialization:serialize-sexp
                                             (history-stored-data)
                                             out)))
                (read in))))))

(defun restore-sexp-history ()
  "Restore the global history from the browser `history-path'."
  (let ((path (history-path *browser*)))
    (handler-case
        (let ((data (with-open-file (file path
                                          :direction :input
                                          :if-does-not-exist nil)
                      (when file
                        ;; We need to make sure current package is :next so that
                        ;; symbols a printed with consistent namespaces.
                        (let ((*package* *package*))
                          (in-package :next)
                          (s-serialization:deserialize-sexp file))))))
          (match data
            ((guard (list version history)
               (hash-table-p history))
             (unless (string= version +version+)
               (log:warn "Session version ~s differs from current version ~s"
                         version +version+))
             (echo "Loading global history of ~a URLs."
                   (hash-table-count history))
             (setf (slot-value *browser* 'history-data) history))

            ((guard history (listp history))
             (let ((new-history (make-hash-table :test #'equal)))
               (dolist (e history)
                 (if (nth-value 1 (gethash (url e) new-history))
                     (incf (implicit-visits (gethash (url e) new-history)) (implicit-visits e))
                     (setf (gethash (url e) new-history) e)))
               (log:info "Imported 1.3.4 history from ~a entries to ~a entries (~a duplicates)."
                         (length history)
                         (hash-table-count new-history)
                         (- (length history)
                            (hash-table-count new-history)))
               (setf (slot-value *browser* 'history-data) new-history)))))
      (error (c)
        (echo-warning "Failed to restore history from ~a: ~a" path c)))))
