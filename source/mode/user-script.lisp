;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/user-script-mode
    (:documentation "Mode to load 'user scripts', such as GreaseMonkey scripts."))
(in-package :nyxt/user-script-mode)

(defun inject-user-scripts (scripts buffer)
  (mapcar (lambda (script) (ffi-buffer-add-user-script buffer script)) scripts))

(defun inject-user-styles (styles buffer)
  (mapcar (rcurry #'ffi-buffer-add-user-style buffer) styles))

(define-mode user-script-mode ()
  "Load user scripts such as GreaseMonkey scripts."
  ((rememberable-p nil)
   (user-scripts
    nil
    :reader user-scripts
    :type list
    :documentation "List of `user-script'-s to attach via renderer-specific mechanisms.")
   (user-styles
    nil
    :reader user-styles
    :type list
    :documentation "List of `user-style'-s to attach via renderer-specific mechanisms.")))

(defmethod enable ((mode user-script-mode) &key)
  (inject-user-scripts (user-scripts mode) (buffer mode))
  (inject-user-styles (user-scripts mode) (buffer mode)))

(export-always 'user-scripts)
(defmethod (setf user-scripts) (new-value (mode user-script-mode))
  (inject-user-scripts new-value (buffer mode))
  (setf (slot-value mode 'user-scripts) new-value))

(export-always 'user-styles)
(defmethod (setf user-styles) (new-value (mode user-script-mode))
  (inject-user-styles new-value (buffer mode))
  (setf (slot-value mode 'user-styles) new-value))

(export-always 'renderer-user-script)
(defclass renderer-user-script ()
  ()
  (:metaclass interface-class))

(define-class user-script (renderer-user-script nfiles:data-file nyxt-remote-file)
  ((code
    :unbound
    :type (maybe string))
   (version "")
   (description "")
   (namespace "")
   (world-name
    :unbound
    :type (maybe string)
    :documentation "The JavaScript world to run the `code' in.")
   (requires
    nil
    :type (maybe hash-table))
   (include
    :unbound
    :type list-of-strings)
   (exclude
    :unbound
    :type list-of-strings)
   (all-frames-p
    :unbound
    :type boolean
    :documentation "Whether to run on both top-level frame and all the subframes.
If false, runs on the toplevel frame only.")
   (run-at
    :unbound
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
          (make-instance 'user-script :url script :base-path #p""))))
    (string
     (multiple-value-bind (url file-p)
         (get-script-url script original-url)
       (cond
         ((and url file-p)
          (nfiles:content (make-instance 'user-script :base-path (quri:uri-path url))))
         ((and url (not file-p))
          (nfiles:content (make-instance 'user-script :url (quri:uri script) :base-path #p"")))
         ;; No URL. No need to download anything.
         ;; It's just code (hopefully).
         (t script))))))

(defmethod nfiles:write-file ((profile nyxt-profile) (script user-script) &key destination)
  "Persist the script body if it has a URL and associated content."
  (unless (uiop:emptyp (nfiles:url-content script))
    (alex:write-string-into-file (nfiles:url-content script) destination :if-exists :supersede)))

(defmethod parse-user-script ((script user-script))
  (let ((code (files:content script)))
    (or
     (sera:and-let* ((start-position (search "// ==UserScript==" code))
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
                                                      (get-script-url require nil)))
                                                   (getprop "require"))
                                           code)))
           (setf
            (nfiles:name script) (or (first (getprop "name")) (alex:required-argument 'name))
            (version script) (first (getprop "version"))
            (description script) (first (getprop "description"))
            (namespace script) (first (getprop "namespace"))
            (all-frames-p script) (not (first (getprop "noframes")))
            (code script) code-with-requires
            (include script) (let ((includes (append (getprop "include") (getprop "match"))))
                               (cond
                                 ((and (sera:single includes)
                                       (equal "http*" (first includes)))
                                  '("http://*/*" "https://*/*"))
                                 ((and (sera:single includes)
                                       (equal "https*" (first includes)))
                                  '("https://*/*"))
                                 ((and (sera:single includes)
                                       (equal "*" (first includes)))
                                  '("*://*/*"))
                                 (t includes)))
            (exclude script) (getprop "exclude")
            (run-at script) (str:string-case (first (getprop "run-at"))
                              ("document-start" :document-start)
                              ("document-end" :document-end)
                              ("document-idle" :document-idle)
                              (otherwise :document-end)))
           code-with-requires)))
     (setf (code script) code
           (include script) '("http://*/*" "https://*/*")
           (exclude script) '()
           (all-frames-p script) t
           (run-at script) :document-end
           (world-name script) nil)
     code)))

(defmethod slot-unbound (class (instance user-script) slot-name)
  (declare (ignore class))
  (parse-user-script instance)
  (slot-value instance slot-name))

(defmethod nfiles:deserialize ((profile nyxt-profile) (script user-script) raw-content &key)
  "If the script is not in the UserScript format, the raw content is used as is
and only the `code' slot is set."
  (declare (ignorable profile))
  (setf (files:content script) (alex:read-stream-content-into-string raw-content))
  (parse-user-script script))
