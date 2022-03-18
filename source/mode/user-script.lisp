;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/user-script-mode
    (:documentation "Mode to load 'user scripts', such as GreaseMonkey scripts."))
(in-package :nyxt/user-script-mode)

(defun inject-user-scripts (scripts buffer)
  (mapcar (alex:rcurry #'ffi-buffer-add-user-script buffer) scripts))

(defun inject-user-styles (styles buffer)
  (mapcar (alex:rcurry #'ffi-buffer-add-user-style buffer) styles))

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
    :documentation "List of `user-style'-s to attach via renderer-specific mechanisms.")
   (constructor
    (lambda (mode)
      (inject-user-scripts (user-scripts mode) (buffer mode))))
   (destructor
    (lambda (mode)
      (inject-user-styles (user-scripts mode) (buffer mode))))))

(export-always 'user-scripts)
(defmethod (setf user-scripts) (new-value (mode web-mode))
  (inject-user-scripts new-value (buffer mode))
  (setf (slot-value mode 'user-scripts) new-value))

(export-always 'user-styles)
(defmethod (setf user-styles) (new-value (mode web-mode))
  (inject-user-styles new-value (buffer mode))
  (setf (slot-value mode 'user-styles) new-value))

(define-class user-script ()
  ((name nil :type (maybe string))
   (code "" :type string)
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
  (:documentation "The Nyxt-internal representation of user scripts to bridge with the renderer."))
(define-user-class user-script)

(sera:-> get-script-url
         (string (maybe nyxt::url-designator pathname))
         (values (maybe quri:uri) boolean))
(defun get-script-url (script original-url)
  "A helper to get the URL to a SCRIPT string.
Returns:
- A final URL.
- A boolean for whether it's a file URL."
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
         (values string &optional))
(defun get-script-text (script &optional original-url)
  (etypecase script
    (pathname
     (alex:read-file-into-string script))
    (quri:uri
     (if (quri:uri-file-p script)
         (alex:read-file-into-string (quri:uri-path script))
         (dex:get (quri:render-uri script))))
    (string
     (multiple-value-bind (url file-p)
         (get-script-url script original-url)
       (cond
         ((and url file-p)
          (alex:read-file-into-string (quri:uri-path url)))
         ((and url (not file-p))
          (dex:get (quri:render-uri url)))
         ;; No URL. No need to download anything.
         ;; It's just code (hopefully).
         (t script))))))

(in-package :nyxt)

(export 'make-greasemonkey-script)
(defun make-greasemonkey-script (greasemonkey-script)
  (sera:and-let* ((code (nyxt/web-mode::get-script-text greasemonkey-script))
                  (start-position (search "// ==UserScript==" code))
                  (end-position (search "// ==/UserScript==" code))
                  (meta (subseq code
                                (+ (1+ (length "// ==UserScript==")) start-position)
                                end-position)))
    (flet ((getprop (prop)
             (alex:when-let* ((regex (str:concat "// @" prop "\\s*(.*)"))
                              (raw-props (ppcre:all-matches-as-strings regex meta)))
               (mapcar (lambda (raw-prop)
                         (multiple-value-bind (start end reg-starts reg-ends)
                             (ppcre:scan regex raw-prop)
                           (declare (ignore end))
                           (when start
                             (subseq raw-prop (elt reg-starts 0) (elt reg-ends 0)))))
                       raw-props))))
      (make-instance 'nyxt/web-mode:user-script
                     :name (or (first (getprop "name")) (alex:required-argument 'name))
                     :version (first (getprop "version"))
                     :description (first (getprop "description"))
                     :namespace (first (getprop "namespace"))
                     :all-frames-p (not (first (getprop "noframes")))
                     :code (format nil "~{~a;~&~}~a"
                                   (mapcar (lambda (require)
                                             (nyxt/web-mode::get-script-text
                                              require (nyxt/web-mode::get-script-url
                                                       greasemonkey-script nil)))
                                           (getprop "require"))
                                   code)
                     :include (append (getprop "include") (getprop "match"))
                     :exclude (getprop "exclude")
                     :run-at (str:string-case (first (getprop "run-at"))
                               ("document-start" :document-start)
                               ("document-end" :document-end)
                               ("document-idle" :document-idle)
                               (otherwise :document-end))))))
