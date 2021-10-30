;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-parenscript %get-input-data ()
  (let* ((inputs (ps:chain document (query-selector-all "input")))
         (len-inputs (ps:chain inputs length))
         (text-areas (ps:chain document (query-selector-all "textArea")))
         (len-text-areas (ps:chain text-areas length))
         (output ""))
    (dotimes (i len-inputs)
      (setf output (+ output "|new-field|" "~" ":input" "~" "t" "~"))
      (unless (equal (ps:chain (aref inputs i) id) "")
        (setf output (+ output "~" ":id" "~" (ps:chain  (aref inputs i) id) "~")))
      (unless (equal (ps:chain (aref inputs i) name) "")
        (setf output (+ output "~" ":name" "~" (ps:chain  (aref inputs i) name) "~"))) 
      (unless (equal (ps:chain (aref inputs i) value) "")
        (setf output (+ output "~" ":value" "~" (ps:chain  (aref inputs i) value) "~"))) 
      (unless (equal (ps:chain (aref inputs i) type) "")
        (setf output (+ output "~" ":type" "~" (ps:chain  (aref inputs i) type) "~"))))
    (dotimes (i len-text-areas)
      (setf output (+ output "|new-field|" "~" ":text-area" "~" "t" "~"))
      (unless (equal (ps:chain (aref text-areas i) id) "")
        (setf output (+ output "~" ":id" "~" (ps:chain  (aref text-areas i) id) "~")))
      (unless (equal (ps:chain (aref text-areas i) name) "")
        (setf output (+ output "~" ":name" "~" (ps:chain  (aref text-areas i) name) "~"))) 
      (unless (equal (ps:chain (aref text-areas i) value) "")
        (setf output (+ output "~" ":value" "~" (ps:chain  (aref text-areas i) value) "~"))) 
      (unless (equal (ps:chain (aref text-areas i) placeholder) "")
        (setf output (+ output "~" ":placeholder" "~" (ps:chain  (aref text-areas i) type) "~"))))
    output))

(define-class input-entry ()            
  ((url (quri:uri ""))
   (title "")
   (date (local-time:now))
   (input-data '()))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(define-command save-input-data ()
  "Save HTML input data in a local file."
  (labels ((split-input-fields (str)
             (str:split "|new-field|" str :omit-nulls t))
           (split-input-attributes (str)
             (str:split "~" str :omit-nulls t))
           (list-input-data-as-string (str)
             (mapcar #'(lambda (e) (split-input-attributes e)) (split-input-fields str)))
           (convert-string-to-keyword (string
                                       &key
                                         (upcase t)
                                         (max-string-length 100))
             (and (<= 2 (length string) max-string-length)
                  (char= (char string 0) #\:)
                  (let ((string1 (subseq string 1)))
                    (when upcase
                      (setf string1 (string-upcase string1)))
                    (values (intern string1 "KEYWORD")))))
           (convert-key-value-lists (lists)
             (loop for list in lists
                   collect (loop for (key value) on list by #'cddr
                                 collect (convert-string-to-keyword key)
                                 collect value)))
           (input-field-add (input-field) 
             (with-data-access (input-fields (inputs-path (current-buffer)))
               (push (make-instance 'input-entry :url (render-url (url (current-buffer)))
                                                 :title (title (current-buffer))
                                                 :date (local-time:now)
                                                 :input-data input-field)
                     input-fields))))
    (input-field-add (convert-key-value-lists (list-input-data-as-string (%get-input-data))))))

(defun list-values (fields)
  (local-time:enable-read-macros)
  (let* ((input-fields (remove-if-not #'(lambda (e) (member :input e)) fields))
         (text-area-fields (remove-if-not #'(lambda (e) (member :text-area e)) fields))
         (input-values (mapcar #'(lambda (e) (getf e :value "NIL")) input-fields))
         (text-area-values (mapcar #'(lambda (e) (getf e :value "NIL"))
                                   text-area-fields)))
    (list input-values text-area-values)))

(define-parenscript ps-write-input-data (fields)
  (let* ((inputs (ps:chain document (query-selector-all "input")))
         (len-inputs (ps:chain inputs length))
         (saved-input-values (ps:lisp
                              (cons 'ps:array(first (list-values fields)))))
         (text-areas (ps:chain document (query-selector-all
                                         "textArea")))
         (len-text-areas (ps:chain text-areas length))
         (saved-text-areas-values (ps:lisp (cons 'ps:array (second (list-values fields))))))
    (dotimes (i len-inputs)
      (setf (ps:chain (aref inputs i) value)
            (ps:chain (aref saved-input-values i))))
    (dotimes (i len-text-areas)
      (setf (ps:chain (aref text-areas i) value)
            (ps:chain (aref saved-text-areas-values i))))))

(define-class input-data-source (prompter:source)
  ((prompter:name "Inputs")
   (prompter:constructor (get-data (inputs-path (current-buffer))))))

(define-class filtered-domain-input-data-source (prompter:source)
  ((prompter:name "Inputs")
   (prompter:constructor (remove-if-not #'(lambda (input-entry)
                                            (equal (quri:uri-domain (quri:uri (url input-entry)))
                                                   (quri:uri-domain (url (current-buffer)))))
                                        (get-data (inputs-path (current-buffer)))))))

(define-command set-input-data-from-saved 
    (&key (actions (list (make-command buffer-load* (suggestion-values)
                           "Load selected input-entry in current buffer's input fields."
                           (ps-write-input-data (input-data (first suggestion-values)))))))
  "Set the input data from a list of saved data into the current buffer."
  (prompt
   :prompt "Write input data from:"
   :sources (make-instance 'input-data-source
                           :actions actions)))

(define-command set-input-data-from-saved-domain
    (&key (actions (list (make-command buffer-load* (suggestion-values)
                           "Load selected input-entry in current buffer's input fields."
                           (ps-write-input-data (input-data (first suggestion-values)))))))
  "Set the input data from a list of saved data filtered by current domain into
the current buffer."
  (prompt
   :prompt "Write input data from:"
   :sources (make-instance 'filtered-domain-input-data-source
                           :actions actions)))

(defmethod store ((profile data-profile) (path inputs-data-path) &key &allow-other-keys)
  "Store the input data to the buffer `inputs-path'."
  (with-data-file (file path :direction :output)
    (%set-data path (get-data path))
    (s-serialization:serialize-sexp (get-data path) file))
  t)

(defmethod restore ((profile data-profile) (path inputs-data-path) &key &allow-other-keys)
  "Restore the input data from the buffer `inputs-path'."
  (handler-case
      (let ((data (with-data-file (file path)
                    (when file
                      (s-serialization:deserialize-sexp file)))))
        (when data
          (%set-data path data)))
    (error (c)
      (echo-warning "Failed to load inputs from ~s: ~a" (expand-path path) c))))

(defun input-fields ()
  "Lists all input entries objects saved in the local file."
  (with-data-access (input-fields (inputs-path (current-buffer)))
    input-fields))
