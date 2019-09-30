;;; history.lisp --- manage and create bookmarks

(in-package :next)
(annot:enable-annot-syntax)

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
  (format nil "~a  ~a" (url entry) (title entry)))

(defmethod equals ((e1 history-entry) (e2 history-entry))
  (string= (url e1) (url e2)))

@export
(defun history-add (url &key title explicit)
  "Add URL to the global history.
The `implicit-visits' count is incremented unless EXPLICIT is non-nil, in which
case `explicit-visits'.
The history is sorted by last access."
  (unless (str:emptyp url)
    (let ((entry nil)
          (global-history (history-data *interface*)))
      (setf global-history
            (delete-if (lambda (e)
                         (when (string= url (url e))
                           (setf entry e)))
                       global-history))
      (unless entry
        (setf entry (make-instance 'history-entry
                                   :url url)))
      (if explicit
          (incf (explicit-visits entry))
          (incf (implicit-visits entry)))
      (setf (last-access entry) (local-time:now))
      (unless (str:emptyp title)
        (setf (title entry) title))
      ;; Use accessor to ensure store function is called.
      (push entry (history-data *interface*)))))

(define-command delete-history-entry ()
  "Delete chosen history entries."
  (with-result (entries (read-from-minibuffer
                         (make-instance 'minibuffer
                                        :input-prompt "Delete entries:"
                                        :completion-function (history-completion-filter)
                                        :history (minibuffer-set-url-history *interface*)
                                        :multi-selection-p t)))
    (setf (history-data *interface*)
          (set-difference (history-data *interface*) entries :test #'equals))))



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
  (let ((history (sort (history-data *interface*)
                       (lambda (x y)
                         (> (score-history-entry x)
                            (score-history-entry y))))))
    (lambda (input)
      (fuzzy-match input history))))

(defun store-sexp-history ()            ; TODO: Factor with `store-sexp-session'.
  "Store the global history to the last interface `history-path'."
  (with-open-file (file (history-path *interface*)
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
                                            (s-serialization:serialize-sexp (history-data *interface*) out)))
                (read in))))))

(defun restore-sexp-history ()
  "Restore the global history session from the interface `history-path'."
  (let ((path (history-path *interface*)))
    (if (not (uiop:file-exists-p path))
        ;; TODO: Stop importing the SQLite history after 1.3.3?
        (dolist (url-visit (import-sqlite-history))
          (unless (str:emptyp (first url-visit))
            (let ((entry (make-instance 'history-entry
                                        :url (first url-visit)
                                        :implicit-visits (second url-visit))))
              ;; Calling (history-data *interface*) call the restore function if
              ;; empty, so we need to use SLOT-VALUE here.
              (pushnew entry (slot-value *interface* 'history-data) :test #'equals) ; TODO: Does this persist the data on each call?
              ))))
    (handler-case
        (let ((data (with-open-file (file path
                                          :direction :input
                                          :if-does-not-exist nil)
                      (when file
                        ;; We need to make sure current package is :next so that
                        ;; symbols a printed with consistent namespaces.
                        (let ((*package* *package*))
                          (in-package :next)
                          (s-serialization:deserialize-sexp
                           file))))))

          (when data
            (log:info "Restoring global history of ~a URLs."
                      (length data))
            (setf (slot-value *interface* 'history-data) data)))
      (error (c)
        (log:warn "Failed to restore history from ~a: ~a" path c)))))




;; SQLite importer.
(defun import-sqlite-history ()
  (let ((database-path (cl-ppcre:regex-replace "\\.lisp$"
                                               (namestring (history-path *interface*))
                                               ".db")))
    (when (uiop:file-exists-p database-path)
      (log:info "Importing global history from ~a" database-path)
      (let* ((db (sqlite:connect database-path))
             (urls
               (sqlite:execute-to-list
                db "select url, visits from typed")))
        (sqlite:disconnect db)
        urls))))
