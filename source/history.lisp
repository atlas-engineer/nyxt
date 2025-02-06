;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class history-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"history/default")
   (files:name "history"))
  (:export-class-name-p t))

(export-always 'buffer-history)
(defun buffer-history (&optional (buffer (current-buffer)))
  "Get the history of BUFFER.
Not modifiable."
  (files:content (history-file buffer)))

(define-class history-entry ()
  ((url
    (quri:uri "")
    :writer nil
    :type (or quri:uri string))
   (title "")
   (implicit-visits
    0
    :type integer
    :documentation "Number of times the URL was visited.")
   (scroll-position
    '()
    :type (list-of number)
    :documentation "The scroll position user was at when last visiting the page.
It's a list of a form (Y &OPTIONAL X)."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Entry for the global history."))

(defmethod (setf url) (value (he history-entry))
  (setf (slot-value he 'url) (url value)))

(defmethod url ((node htree:node))
  (url (htree:data node)))

(defmethod prompter:object-attributes ((entry history-entry) (source prompter:source))
  (declare (ignore source))
  `(("Title" ,(title entry) (:width 3))
    ("URL" ,(render-url (url entry)) (:width 2))
    ("Visits" ,(implicit-visits entry) (:width 1))))

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
    (setf (slot-value he 'url) (url (slot-value he 'url))))
  (slot-value he 'url))

(defmethod s-serialization::serialize-sexp-internal ((uri quri:uri)
                                                     stream
                                                     serialization-state)
  "Serialize `history-entry' by turning the URL and last access into strings."
  (declare (ignore serialization-state))
  (prin1 (quri:render-uri uri) stream))

(defmethod s-serialization::serialize-sexp-internal ((timestamp time:timestamp)
                                                     stream
                                                     serialization-state)
  "Serialize `history-entry' by turning the URL and last access into strings."
  (declare (ignore serialization-state))
  (prin1 (time:format-timestring nil timestamp :timezone time:+utc-zone+)
         stream))

(defun history-tree-key (history-entry)
  (url history-entry))

(defun make-history-tree (&optional (buffer (current-buffer)))
  "Return a new global history tree for `history-entry' data."
  (htree:make :key 'history-tree-key :initial-owners (when buffer (list (id buffer)))))

(define-command delete-history-entry (&key (buffer (current-buffer)))
  "Delete queried history entries.
Only deletes the disowned entries (= the ones not belonging to a buffer)."
  (let ((entries (prompt
                  :prompt "Delete entries"
                  :sources (make-instance 'history-disowned-source
                                          :buffer buffer))))
    (files:with-file-content (history (history-file buffer))
      (dolist (entry entries)
        (htree:delete-data history entry)))))

(define-command reset-buffer-history (&key (buffers (prompt :prompt "Reset histories of buffer(s)"
                                                            :sources (make-instance
                                                                      'buffer-source
                                                                      :actions-on-return #'identity))))
  "Set selected BUFFER's history to the current URL only.
This removes the parenthood with the parent buffer, if there was any.

When called over many or all buffers, it may free many history entries which
then become available for deletion with `delete-history-entry'."
  (files:with-file-content (history (history-file (current-buffer)))
    (dolist (buffer buffers)
      (htree:reset-owner history (id buffer)))))

(defun score-history-entry (htree-entry)
  "Return history ENTRY score.
The score gets higher for more recent entries and if they've been visited a
lot."
  (let* ((entry (htree:data htree-entry))
         (last-access (htree:last-access htree-entry)))
    (+ (* 0.1
          ;; Total number of visits.
          (implicit-visits entry))
       (if last-access
           (* 1.0
              ;; Inverse number of hours since the last access.
              (/ 1
                 (1+ (/ (time:timestamp-difference (time:now) last-access)
                        (* 60 60)))))
           0))))

(define-class history-disowned-source (prompter:source)
  ((prompter:name "Disowned History")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:enable-marks-p t)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:constructor
    (lambda (source)
      (let* ((history (buffer-history (buffer source)))
             (owner-less-history-entries
               (when history
                 (mapcar #'htree:data
                         (sort
                          (delete-if (lambda (entry) (htree:nodes entry))
                                     (alex:hash-table-keys (htree:entries history)))
                          (lambda (x y)
                            (> (score-history-entry x)
                               (score-history-entry y))))))))
        owner-less-history-entries)))))

(defun history-html-list (&key (limit 100))
  (let* ((history (buffer-history))
         (history-entries
           (sort-by-time (alex:hash-table-keys (htree:entries history))
                         :key #'htree:last-access)))
    (spinneret:with-html-string
      (loop for entry in (sera:take limit (the list history-entries))
            for data = (htree:data entry)
            collect (:tr (:td (title data))
                         (:td (:a :href (render-url (url data))
                                  (render-url (url data)))))))))

(defmethod files:serialize ((profile nyxt-profile) (file history-file) stream &key)
  (let ((*package* (find-package :nyxt))
        (*print-length* nil))
    ;; We need to make sure current package is :nyxt so that symbols are printed
    ;; with consistent namespaces.
    (write
     (with-input-from-string (in (with-output-to-string (out)
                                   (s-serialization:serialize-sexp
                                    (list +version+ (files:content file))
                                    out)))
       ;; We READ the output of serialize-sexp to make it more
       ;; human-readable.
       (safe-read in))
     :stream stream)))

;; REVIEW: This works around the issue of cl-prevalence to deserialize structs
;; with custom constructors: https://github.com/40ants/cl-prevalence/issues/16.
(setf (fdefinition 'quri.uri::make-uri) #'quri.uri::%make-uri)

;; Hack of cl-prevalence to support the history-tree custom hash tables:
(defun history-deserialize-sexp (stream &optional (serialization-state (s-serialization::make-serialization-state)))
  "Read and return an s-expression serialized version of a lisp object from stream, optionally reusing a serialization state"
  (s-serialization::reset serialization-state)
  (handler-bind ((reader-error (lambda (c)
                                 (log:warn "~a" c)
                                 (continue)))
                 ;; CCL unintuitively raises simple-errors...
                 #+ccl
                 (simple-error (lambda (c)
                                 (log:warn "~a" c)
                                 (continue))))
    (let* ((*package* (find-package :nyxt))
           (sexp (safe-read stream nil stream)))
      (if (eq sexp stream)
          nil
          (history-deserialize-sexp-internal sexp (s-serialization::get-hashtable serialization-state))))))

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
  ;; We need this specialization because
  ;; - `history-tree' cannot be make-instance'd without specifying some slots like `owners'.
  ;; - We need a history tree
  (declare (ignore slots deserialized-objects))
  ;; WARNING: At this stage, there may be no current-buffer, we make sure to
  ;; pass NIL to `make-history-tree' to avoid potential dead-locks.
  (let ((history (make-history-tree nil)))
    history))

(defmethod files:deserialize ((profile nyxt-profile) (file history-file) raw-content &key)
  "Restore the global/buffer-local history and session from the PATH."
  (let ((data (let ((*package* (find-package :nyxt)))
                ;; We need to make sure current package is :nyxt so that
                ;; symbols are printed with consistent namespaces.
                (history-deserialize-sexp raw-content))))
    (match data
      (nil nil)
      ((guard (list version history) t)
       ;; The equality is exclusively established on the first return value,
       ;; i.e. the major version.
       (unless (= (parse-version version) (version))
         (log:warn "History major version ~s differs from current major version ~s"
                   (parse-version version) (version)))
       history)
      (_ (progn
           (error "Expected (list version history) structure.")
           nil)))))

(defun histories-directory (&optional (buffer (current-buffer)))
  "Get the directory where history files are stored, based on `history-file' of BUFFER."
  (when (context-buffer-p buffer)
    (files:parent (files:expand (history-file buffer)))))

(defun histories-list (&optional (buffer (current-buffer)))
  "List all the files with persisted history.
Uses `histories-directory' of the BUFFER to get files."
  (when-let ((dir (histories-directory buffer)))
    (sera:keep "lisp" (uiop:directory-files dir)
               :test 'string-equal
               :key #'files:pathname-type*)))

(define-class history-name-source (prompter:source)
  ((prompter:name "Histories")
   (prompter:constructor (mapcar #'pathname-name (histories-list)))))
