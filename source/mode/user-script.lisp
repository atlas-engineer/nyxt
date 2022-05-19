;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(export-always 'renderer-user-script)
(defclass renderer-user-script ()
  ()
  (:metaclass mixin-class))

(define-class user-script (renderer-user-script nfiles:data-file nyxt-remote-file)
  ((code "" :type string)
   (version "")
   (description "")
   (namespace "")
   (world-name
    nil
    :type (maybe string)
    :documentation "The JavaScript world to run the `code' in.")
   (requires
    nil
    :type (maybe hash-table))
   (include
    '()
    :type list-of-strings)
   (exclude
    '()
    :type list-of-strings)
   (all-frames-p
    t
    :type boolean
    :documentation "Whether to run on both top-level frame and all the subframes.
If false, runs on the toplevel frame only.")
   (run-at
    :document-end
    :type (member :document-start :document-end :document-idle)
    :documentation "When to run the script.
Possible values:
- :document-start (page started loading).
- :document-end (page loaded, resources aren't).
- :document-idle (page and resources are loaded)."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "The Nyxt-internal representation of user scripts to bridge with the renderer.")
  (:metaclass user-class))

(sera:-> get-script-url
         (string (maybe nyxt::url-designator pathname))
         (values (maybe quri:uri) boolean))
(defun get-script-url (script original-url)
  "A helper to get the URL to a SCRIPT string.
Return:
- a final URL;
- a boolean when it's a file URL."
  (cond
    ((valid-url-p script)
     (let ((script (quri:uri script)))
       (if (quri:uri-file-p script)
           (values script t)
           (values script nil))))
    ((and (uiop:file-pathname-p script)
          (uiop:file-exists-p script)
          (uiop:absolute-pathname-p script))
     (values (quri.uri.file:make-uri-file :path script) t))
    ((and (uiop:file-pathname-p script) original-url)
     (let ((full-url (quri:merge-uris (quri:uri script)
                                      (typecase original-url
                                        (pathname (quri.uri.file:make-uri-file :path original-url))
                                        (nyxt::url-designator (url original-url))))))
       (if (and (quri:uri-file-p full-url)
                (uiop:file-exists-p (quri:uri-path full-url)))
           (values full-url t)
           (values full-url nil))))
    (t (values nil nil))))

(sera:-> get-script-text
         ((or string nyxt::url-designator pathname)
          &optional (maybe nyxt::url-designator pathname))
         (values (maybe string) &optional))
(defun get-script-text (script &optional original-url)
  (etypecase script
    (pathname
     (nfiles:content (make-instance 'user-script :base-path script)))
    (quri:uri
     (nfiles:content
      (if (quri:uri-file-p script)
          (make-instance 'user-script :base-path (quri:uri-path script))
          (make-instance 'user-script :url script))))
    (string
     (multiple-value-bind (url file-p)
         (get-script-url script original-url)
       (cond
         ((and url file-p)
          (nfiles:content (make-instance 'user-script :base-path (quri:uri-path url))))
         ((and url (not file-p))
          (nfiles:content (make-instance 'user-script :url script)))
         ;; No URL. No need to download anything.
         ;; It's just code (hopefully).
         (t script))))))

(defmethod nfiles:write-file ((profile nyxt-profile) (script user-script) &key destination)
  "Persist the script body if it has a URL and associated content."
  (unless (uiop:emptyp (nfiles:url-content script))
    (alex:write-string-into-file (nfiles:url-content script) destination :if-exists :supersede)))

(defmethod nfiles:deserialize ((profile nyxt-profile) (script user-script) raw-content &key)
  ;; TODO: Parse the stream directly?
  (sera:and-let* ((code (alex:read-stream-content-into-string raw-content))
                  (start-position (search "// ==UserScript==" code))
                  (end-position (search "// ==/UserScript==" code))
                  (meta (subseq code
                                (+ (1+ (length "// ==UserScript==")) start-position)
                                end-position)))
    (flet ((getprop (prop)
             (alex:when-let* ((regex (str:concat "// @" prop "\\s*(.*)"))
                              (raw-props (ppcre:all-matches-as-strings regex meta)))
               (mapcar (lambda (raw-prop)
                         (multiple-value-bind (begin end reg-starts reg-ends)
                             (ppcre:scan regex raw-prop)
                           (declare (ignore end))
                           (when begin
                             (subseq raw-prop (elt reg-starts 0) (elt reg-ends 0)))))
                       raw-props))))

      (let ((code-with-requires (format nil "~{~a;~&~}~a"
                                        (mapcar (lambda (require)
                                                  (get-script-text
                                                   require
                                                   (get-script-url script nil)))
                                                (getprop "require"))
                                        code)))
        (setf
         (nfiles:name script) (or (first (getprop "name")) (alex:required-argument 'name))
         (version script) (first (getprop "version"))
         (description script) (first (getprop "description"))
         (namespace script) (first (getprop "namespace"))
         (all-frames-p script) (not (first (getprop "noframes")))
         (code script) code-with-requires
         (include script) (append (getprop "include") (getprop "match"))
         (exclude script) (getprop "exclude")
         (run-at script) (str:string-case (first (getprop "run-at"))
                           ("document-start" :document-start)
                           ("document-end" :document-end)
                           ("document-idle" :document-idle)
                           (otherwise :document-end)))
        code-with-requires))))
