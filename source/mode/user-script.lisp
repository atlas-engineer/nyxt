;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/user-script
    (:documentation "Mode to load 'user scripts', such as GreaseMonkey scripts."))
(in-package :nyxt/mode/user-script)

(defun inject-user-scripts (scripts buffer)
  (mapcar (lambda (script) (ffi-buffer-add-user-script buffer script)) scripts))

(defun de-inject-user-scripts (scripts buffer)
  (mapcar (lambda (script) (ffi-buffer-remove-user-script buffer script)) scripts))

(defun inject-user-styles (styles buffer)
  (mapcar (lambda (style) (ffi-buffer-add-user-style buffer style)) styles))

(defun de-inject-user-styles (styles buffer)
  (mapcar (lambda (style) (ffi-buffer-remove-user-style buffer style)) styles))

(sera:eval-always
  (define-class user-script (renderer-user-script files:data-file nyxt-remote-file)
    ((code "" :type (maybe string))
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
      '("http://*/*" "https://*/*")
      :type (list-of string))
     (exclude
      '()
      :type (list-of string))
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
    (:documentation "The Nyxt-internal representation of user scripts to bridge with the renderer.")
    (:metaclass user-class)))

(-> get-script-url
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

(-> get-script-text
    ((or string nyxt::url-designator pathname)
     &optional (maybe nyxt::url-designator pathname))
    (values (maybe string) &optional))
(defun get-script-text (script &optional original-url)
  (etypecase script
    (pathname
     (files:content (make-instance 'user-script :base-path script)))
    (quri:uri
     (files:content
      (if (quri:uri-file-p script)
          (make-instance 'user-script :base-path (quri:uri-path script))
          (make-instance 'user-script :url script :base-path #p""))))
    (string
     (multiple-value-bind (url file-p)
         (get-script-url script original-url)
       (cond
         ((and url file-p)
          (files:content (make-instance 'user-script :base-path (quri:uri-path url))))
         ((and url (not file-p))
          (files:content (make-instance 'user-script :url (quri:uri script) :base-path #p"")))
         ;; No URL. No need to download anything.
         ;; It's just code (hopefully).
         (t script))))))

(defmethod files:write-file ((profile nyxt-profile) (script user-script) &key destination)
  "Persist the script body if it has a URL and associated content."
  (unless (uiop:emptyp (files:url-content script))
    (alex:write-string-into-file (files:url-content script) destination :if-exists :supersede)))

(defmethod parse-user-script ((script user-script))
  (let ((code (if (uiop:emptyp (code script))
                  (files:content script)
                  (code script))))
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
            (files:name script) (or (first (getprop "name")) (alex:required-argument 'name))
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
     (setf (code script) code))))

(defmethod customize-instance :after ((script user-script) &key)
  (parse-user-script script))

(export-always 'renderer-user-style)
(defclass renderer-user-style ()
  ()
  (:metaclass interface-class))

(sera:eval-always
  (define-class user-style (renderer-user-style files:data-file nyxt-remote-file)
    ((code "" :type (maybe string))
     (world-name
      nil
      :type (maybe string)
      :documentation "The JavaScript world to inject the style in.")
     (include
      '("http://*/*" "https://*/*")
      :type (list-of string))
     (exclude
      '()
      :type (list-of string))
     (all-frames-p
      t
      :type boolean
      :documentation "Whether to run on both top-level frame and all the subframes.
If false, runs on the toplevel frame only.")
     (level
      :user
      :type (member :user :author)
      :documentation "The level of authority (:USER > :AUTHOR) with which to inject the style.
:USER styles override everything else."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:documentation "The Nyxt-internal representation of user styles to bridge with the renderer.")
    (:metaclass user-class)))

(defmethod files:write-file ((profile nyxt-profile) (style user-style) &key destination)
  "Persist the script body if it has a URL and associated content."
  (unless (uiop:emptyp (files:url-content style))
    (alex:write-string-into-file (files:url-content style) destination :if-exists :supersede)))

(defmethod customize-instance :after ((style user-style) &key)
  ;; TODO: Somehow parse @-moz-document patterns?
  (when (uiop:emptyp (code style))
    (setf (code style) (files:content style))))

(define-mode user-script-mode ()
  "Load user scripts such as GreaseMonkey scripts."
  ((rememberable-p nil)
   (user-scripts
    nil
    :reader user-scripts
    :type (list-of user-script)
    :documentation "List of `user-script'-s to attach via renderer-specific mechanisms.")
   (user-styles
    nil
    :reader user-styles
    :type (list-of user-style)
    :documentation "List of `user-style'-s to attach via renderer-specific mechanisms.")))

(defmethod enable ((mode user-script-mode) &key)
  (inject-user-scripts (user-scripts mode) (buffer mode))
  (inject-user-styles (user-styles mode) (buffer mode)))

(defmethod disable ((mode user-script-mode) &key)
  (de-inject-user-scripts (user-scripts mode) (buffer mode))
  (de-inject-user-styles (user-styles mode) (buffer mode)))

(export-always 'user-scripts)
(defmethod (setf user-scripts) (new-value (mode user-script-mode))
  (inject-user-scripts (slot-value mode 'user-scripts) (buffer mode))
  (inject-user-scripts new-value (buffer mode))
  (setf (slot-value mode 'user-scripts) new-value))

(export-always 'user-styles)
(defmethod (setf user-styles) (new-value (mode user-script-mode))
  (de-inject-user-styles (slot-value mode 'user-styles) (buffer mode))
  (inject-user-styles new-value (buffer mode))
  (setf (slot-value mode 'user-styles) new-value))

(export-always 'renderer-user-script)
(defclass renderer-user-script ()
  ()
  (:metaclass interface-class))
