;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class history-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"history/default")
   (files:name "history"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(export-always 'buffer-history)
(defun buffer-history (&optional (buffer (current-buffer)))
  (files:content (history-file buffer)))

(define-class history-entry ()          ; TODO: Export?
  ((url (quri:uri "")
        :accessor nil
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
Number of times the URL was visited by a prompt-buffer request.  This does not
include implicit visits.")
   (implicit-visits 0
                    :type integer
                    :documentation "
Number of times the URL was visited by following a link on a page.  This does
not include explicit visits.")
   (scroll-position '()
                    :type (list-of numbers)
                    :documentation "The scroll position user was at when last visiting the page.
It's a list of a form (Y &OPTIONAL X)."))
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "
Entry for the global history.
The total number of visit for a given URL is (+ explicit-visits implicit-visits)."))

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

(defmethod s-serialization::serialize-sexp-internal ((uri quri:uri)
                                                     stream
                                                     serialization-state)
  "Serialize `history-entry' by turning the URL and last access into strings."
  (declare (ignore serialization-state))
  (prin1 (quri:render-uri uri) stream))

(defmethod s-serialization::serialize-sexp-internal ((timestamp local-time:timestamp)
                                                     stream
                                                     serialization-state)
  "Serialize `history-entry' by turning the URL and last access into strings."
  (declare (ignore serialization-state))
  (prin1 (local-time:format-timestring nil timestamp
                                       :timezone local-time:+utc-zone+)
         stream))

(defun history-tree-key (history-entry)
  (render-url (url history-entry)))

(defun make-history-tree (&optional (buffer (current-buffer)))
  "Return a new global history tree for `history-entry' data."
  (htree:make :key 'history-tree-key :initial-owners (when buffer (list (id buffer)))))

(-> history-add (quri:uri &key (:title string) (:buffer buffer)) *)
(defun history-add (url &key (title "") (buffer (current-buffer)))
  "Add URL to the global/buffer-local history.
The `implicit-visits' count is incremented."
  (files:with-file-content (history (history-file (current-buffer))
                            :default (make-history-tree))
    (unless (or (url-empty-p url)
                ;; If buffer was not registered in the global history, don't
                ;; proceed.  See `buffer's `customize-instance' `:after' method..
                (not (htree:owner history (id buffer))))
      (htree:add-child (make-instance 'history-entry
                                      :url url
                                      :title title)
                       history
                       (id buffer))
      (let* ((entry (htree:data (htree:current (htree:owner history (id buffer))))))
        (setf (title entry) title)
        (incf (implicit-visits entry))))))

(define-command delete-history-entry (&key (buffer (current-buffer)))
  "Delete queried history entries."
  (let ((entries (prompt
                  :prompt "Delete entries"
                  :sources (make-instance 'history-disowned-source
                                          :buffer buffer))))
    (files:with-file-content (history (history-file buffer))
      (dolist (entry entries)
        (htree:delete-data history entry)))))

(define-command reset-buffer-history (&optional buffer)
  "Set selected buffers history to the current URL only.
This removes the parenthood with the parent buffer, if there was any.

When called over many or all buffers, it may free many history entries which
then become available for deletion with `delete-history-entry'."
  (let ((buffers (or (alex:ensure-list buffer)
                     (prompt :prompt "Reset histories of buffer(s)"
                             :sources (make-instance 'buffer-source
                                                     :return-actions '())))))
    (files:with-file-content (history (history-file (current-buffer)))
      (dolist (buffer buffers)
        (htree:reset-owner history (id buffer))))))

(defun score-history-entry (htree-entry)
  "Return history ENTRY score.
The score gets higher for more recent entries and if they've been visited a
lot."
  ;; TODO: Or use current buffer last access?  Or both?
  ;; WARNING: `htree:data-last-access' is slow, which is why we take a
  ;; htree-entry instead which has much faster access to the last access.
  (let* ((entry (htree:data htree-entry))
         (last-access (htree:last-access htree-entry)))
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

(defun history-initial-suggestions (&key prefix-urls) ; TODO: Rename?  Make this a preprocessor so that it runs in the background?
  "Return all history entries, with PREFIX-URLS prepended to the result."
  (let* ((history (buffer-history))
         (all-history-entries (when history
                                (mapcar #'htree:data
                                        (sort (alex:hash-table-keys (htree:entries history))
                                              (lambda (x y)
                                                (> (score-history-entry x)
                                                   (score-history-entry y)))))))
         (prefix-urls (delete-if #'uiop:emptyp prefix-urls)))
    (when prefix-urls
      (setf all-history-entries (append (mapcar #'quri:url-decode prefix-urls)
                                        all-history-entries)))
    all-history-entries))

(define-class history-disowned-source (prompter:source)
  ((prompter:name "Disowned History")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:multi-selection-p t)
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

(defmethod prompter:object-attributes ((entry history-entry) (source history-disowned-source))
  (declare (ignore source))
  `(("URL" ,(render-url (url entry)))
    ("Title" ,(title entry))))

(defun history-html-list (&key (limit 100) (separator " → "))
  (let* ((history (buffer-history))
         (history-entries
           (sort-by-time (alex:hash-table-keys (htree:entries history))
                         :key #'htree:last-access)))
    (spinneret:with-html-string
      (loop for entry in (sera:take limit (the list history-entries))
            for data = (htree:data entry)
            collect (:li (title data) (unless (str:emptyp (title data)) separator)
                         (:a :href (render-url (url data))
                             (render-url (url data))))))))

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
  (let* ((*package* (find-package :nyxt))
         (sexp (safe-read stream nil stream)))
    (if (eq sexp stream)
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
  ;; We need this specialization because
  ;; - `history-tree' cannot be make-instance'd without specifying some slots like `owners'.
  ;; - We need a history tree
  (declare (ignore slots deserialized-objects))
  ;; WARNING: At this stage, there may be no current-buffer, we make sure to
  ;; pass NIL to `make-history-tree' to avoid potential dead-locks.
  (let ((history (make-history-tree nil)))
    history))

(defun restore-history-buffers (history history-file)
  "Restore buffers corresponding to the HISTORY owners.

This modifies the history owners as follows.
For each owner, make a buffer, swap old owner identifier for the new buffer ID
and maintain a table of (old-id -> new-id).
Finally go through all the owners and update their creator.

Return non-NIL of history was restored, NIL otherwise."
  (when history
    (log:info "Restoring ~a buffer~:p from history."
              (hash-table-count (htree:owners history)))
    (let ((old-id->new-id (make-hash-table :test #'equalp))
          (new-owners (make-hash-table :test #'equalp)))
      ;; We can't `maphash' over (htree:owners history) because
      ;; `make-buffer' modifies the owners hash table.
      (mapc (lambda-match
              ((cons owner-id owner)
               (let ((current-node (htree:current
                                    (htree:owner history owner-id))))
                 ;; Node-less owners can safely be ignored.
                 (when current-node
                   (let ((new-buffer (make-buffer :title (title (htree:data current-node))
                                                  :history-file history-file
                                                  :url (url (htree:data current-node))
                                                  :load-url-p nil)))
                     (setf (gethash owner-id old-id->new-id) (id new-buffer))
                     (setf (gethash (id new-buffer) new-owners) owner))))))
            (alex:hash-table-alist (htree:owners history)))
      (maphash (lambda (_ owner)
                 (declare (ignore _))
                 (setf (htree:creator-id owner)
                       (gethash (htree:creator-id owner) old-id->new-id)))
               (htree:owners history))
      (setf (htree:owners history) new-owners))
    (alex:when-let ((latest-id (first
                                (first
                                 (sort-by-time (alex:hash-table-alist (htree:owners history))
                                               :key (compose #'htree:last-access #'rest))))))
      (switch-buffer :buffer (buffers-get latest-id)))))

(defmethod files:deserialize ((profile nyxt-profile) (file history-file) raw-content &key)
  "Restore the global/buffer-local history and session from the PATH."
  (let ((data (let ((*package* (find-package :nyxt)))
                ;; We need to make sure current package is :nyxt so that
                ;; symbols are printed with consistent namespaces.
                (history-deserialize-sexp raw-content))))
    (match data
      (nil nil)
      ((guard (list version history) t)
       (unless (string= version +version+)
         (log:warn "History version ~s differs from current version ~s"
                   version +version+))
       history)
      (_ (progn
           (error "Expected (list version history) structure.")
           nil)))))

(defun histories-directory (&optional (buffer (current-buffer)))
  (when (context-buffer-p buffer)
    (files:parent (nfiles:expand (history-file buffer)))))

(defun histories-list (&optional (buffer (current-buffer)))
  (alex:when-let ((dir (histories-directory buffer)))
    (sera:keep "lisp" (uiop:directory-files dir)
               :test 'string-equal
               :key #'files:pathname-type*)))

(define-class history-name-source (prompter:source)
  ((prompter:name "Histories")
   (prompter:constructor (mapcar #'pathname-name (histories-list)))
   (prompter:hide-attribute-header-p :single)))

(define-command store-history-by-name ()
  "Store the history data in the file named by user input.
Useful for session snapshots, as `restore-history-by-name' will restore opened buffers."
  (sera:and-let* ((name (prompt1
                         :prompt "The name to store history with"
                         :sources (list 'prompter:raw-source
                                        (make-instance 'history-name-source))))
                  (new-file (make-instance 'history-file
                                           :base-path (make-pathname
                                                       :name name
                                                       :directory (pathname-directory (histories-directory))))))
    (when (or (not (uiop:file-exists-p (files:expand new-file)))
              (if-confirm ((format nil "Overwrite ~s?" (files:expand new-file)))))
      (setf (files:content new-file) (buffer-history))
      (echo "History stored to ~s." (files:expand new-file)))))

(define-command restore-history-by-name ()
  "Delete all the buffers of the current session/history and import the history chosen by user.
The imported history file is untouched while the current one is overwritten.
If you want to save the current history file beforehand, call
`store-history-by-name' to save it under a new name."
  ;; TODO: backup current history?
  (sera:and-let* ((name (prompt1 :prompt "The name of the history to restore"
                                 :sources 'history-name-source))
                  (new-file (make-instance 'history-file
                                           :base-path (make-pathname
                                                       :name name
                                                       :directory (pathname-directory (histories-directory))))))
    (let ((old-buffers (buffer-list))
          (new-history (files:content new-file)))
      (restore-history-buffers new-history (history-file (current-buffer)))
      (dolist (buffer old-buffers)
        (buffer-delete buffer)))))
