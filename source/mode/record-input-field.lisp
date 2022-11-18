;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/record-input-field-mode
    (:documentation "Record input fields to be refilled later."))
(in-package :nyxt/record-input-field-mode)

;;; FIXME: Use "submit-form" WebkitWebView signal instead.
(define-mode record-input-field-mode ()
  "Record input fields to be refilled later.
See `save-input-data' and `set-input-data-from-saved'."
  ((visible-in-status-p nil)
   (inputs-file
    (make-instance 'inputs-file)
    :type inputs-file
    :documentation "File where input date is saved.")))

(defmethod inputs-file ((buffer buffer))
  (inputs-file (find-submode 'record-input-field-mode buffer)))

(define-class inputs-file (files:data-file nyxt-lisp-file)
  ((files:base-path #p"inputs")
   (files:name "inputs"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

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
   (date (time:now))
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
             (mapcar #'split-input-attributes (split-input-fields str)))
           (convert-string-to-keyword (string &key (max-string-length 100))
             (and (<= 2 (length string) max-string-length)
                  (str:starts-with-p ":" string)
                  (alex:make-keyword (string-upcase string :start 1))))
           (convert-key-value-lists (lists)
             (loop for list in lists
                   collect (loop for (key value) on list by #'cddr
                                 collect (convert-string-to-keyword key)
                                 collect value)))
           (input-field-add (input-field)
             (files:with-file-content (input-fields (inputs-file (current-buffer)))
               (push (make-instance 'input-entry :url (url (current-buffer))
                                                 :title (title (current-buffer))
                                                 :date (time:now)
                                                 :input-data input-field)
                     input-fields))))
    (input-field-add (convert-key-value-lists (list-input-data-as-string (%get-input-data))))))

(defun list-values (fields)
  (time:enable-read-macros)
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
   (prompter:constructor (files:content (inputs-file (current-buffer))))))

(define-class filtered-domain-input-data-source (prompter:source)
  ((prompter:name "Inputs")
   (prompter:constructor (remove-if-not #'(lambda (input-entry)
                                            (equal (quri:uri-domain (quri:uri (url input-entry)))
                                                   (quri:uri-domain (url (current-buffer)))))
                                        (files:content (inputs-file (current-buffer)))))))

(define-command set-input-data-from-saved
    (&key (return-actions (list (lambda-command set-input-data* (suggestion-values)
                                  "Load selected input-entry in current buffer's input fields."
                                  (ps-write-input-data (input-data (first suggestion-values)))))))
  "Set the input data from a list of saved data into the current buffer.
See also `set-input-data-from-saved-domain'."
  (prompt :prompt "Write input data from"
          :sources (make-instance 'input-data-source
                                  :return-actions return-actions)))

(define-command set-input-data-from-saved-domain
    (&key (return-actions (lambda-command buffer-load* (suggestion-values)
                            "Load selected input-entry in current buffer's input fields."
                            (ps-write-input-data (input-data (first suggestion-values))))))
  "Set the input data from a list of saved data filtered by current domain into
the current buffer.

See also `set-input-data-from-saved'."
  (prompt :prompt "Write input data from"
          :sources (make-instance 'filtered-domain-input-data-source
                                  :return-actions return-actions)))

(defun input-fields ()
  "List all input entries objects saved in the local file."
  (files:content (inputs-file (current-buffer))))
