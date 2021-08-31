;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/web-extensions
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum #:export-always)
  (:documentation "WebExtensions API conformance code."))
(in-package :nyxt/web-extensions)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-nyxt-package-nicknames))

(define-class content-script ()
  ((match-patterns (error "Match pattern is required.")
                   :type list)
   (files nil
          :type list)
   (user-styles nil
                :type list)
   (user-scripts nil
                 :type list))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defmethod remove-content-script (buffer extension (script content-script))
  (dolist (s (user-scripts script))
    (ffi-buffer-remove-user-script buffer s))
  (dolist (s (user-styles script))
    (ffi-buffer-remove-user-style buffer s)))

(defmethod inject-content-script (buffer extension (script content-script))
  (remove-content-script buffer extension script)
  (dolist (file (files script))
    (if (equal (pathname-type file) "css")
        (push (ffi-buffer-add-user-style buffer (uiop:read-file-string
                                                  (merge-extension-path extension file))
                                          :inject-as-author-p t
                                          :all-frames-p t
                                          :world-name (name extension)
                                          :allow-list (match-patterns script))
              (user-styles script))
        (push
         (ffi-buffer-add-user-script buffer (uiop:read-file-string
                                             (merge-extension-path extension file))
                                     :world-name (name extension)
                                     :all-frames-p t
                                     :run-now-p t
                                     :at-document-start-p t
                                     :allow-list (match-patterns script))
         (user-scripts script)))))

(defun make-content-script (&key (matches (error "Matches key is mandatory.")) js css)
  (let ((sanitize-mozilla-regex (alex:curry #'str:replace-using '("*." "*"
                                                                  "?" "*"
                                                                  "<all_urls>" "*://*/*"))))
    (make-instance
     'content-script
     :match-pattern (mapcar sanitize-mozilla-regex (uiop:ensure-list matches))
     :files (append (uiop:ensure-list js) (uiop:ensure-list css)))))


(defun make-data-url (file &optional mime-type)
  (let* ((type (or mime-type (mimes:mime file)))
         (binary-p (not (str:starts-with-p "text" type))))
    (format nil "data:~a~@[;base64~*~],~a"
            type binary-p (if binary-p
                              (base64:usb8-array-to-base64-string
                               (alex:read-file-into-byte-vector file))
                              (quri:url-encode (uiop:read-file-string file))))))

(defun default-browser-action-icon (json optimal-height)
  (let* ((browser-action (alex:assoc-value json :browser--action))
         (default-icon (alex:assoc-value browser-action :default--icon)))
    (if (stringp default-icon)
        default-icon
        (rest (first (sort (append (alex:assoc-value browser-action :default--icon)
                                   (alex:assoc-value json :icons))
                           (lambda (a b)
                             (< (abs (- optimal-height a))
                                (abs (- optimal-height b))))
                           :key (alex:compose #'parse-integer #'symbol-name #'first)))))))

(defun encode-browser-action-icon (json extension-directory)
  (let* ((status-buffer-height (nyxt:height (status-buffer (current-window))))
         (padded-height (- status-buffer-height 10))
         (best-icon
           (default-browser-action-icon json padded-height)))
    (format nil "<img src=\"~a\" alt=\"~a\"
height=~a/>"
            (make-data-url (uiop:merge-pathnames* best-icon extension-directory))
            (alex:assoc-value json :name)
            padded-height)))

(defun make-browser-action (json)
  (let* ((browser-action (alex:assoc-value json :browser--action))
         (sorted-icons (sort (alex:assoc-value browser-action :theme--icons)
                             #'> :key (alex:rcurry #'alex:assoc-value :size)))
         (max-icon (first sorted-icons))
         (default-icon (default-browser-action-icon json 1000)))
    (make-instance 'browser-action
                   :default-popup (alex:assoc-value browser-action :default--popup)
                   :default-title (alex:assoc-value browser-action :default--title)
                   :default-icon default-icon
                   :default-dark-icon (or (alex:assoc-value max-icon :dark)
                                          (alex:assoc-value max-icon :light))
                   :default-light-icon (or (alex:assoc-value max-icon :light)
                                           (alex:assoc-value max-icon :dark)))))

(define-class browser-action ()
  ((default-popup nil
                  :type (or null string pathname)
                  :documentation "An HTML file for the popup to open when its icon is clicked.")
   (default-title nil
                  :type (or null string)
                  :documentation "The title to call the popup with.")
   (default-icon nil
                 :type (or null string)
                 :documentation "The extension icon to use in mode line.")
   (default-light-icon nil
                       :type (or null string)
                       :documentation "The extension icon for use in mode line in the light theme.")
   (default-dark-icon nil
                       :type (or null string)
                       :documentation "The extension icon for use in mode line in the dark theme."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class extension-storage-data-path (nyxt:data-path)
  ((ref "extension-storage"))
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod store ((profile data-profile) (path extension-storage-data-path) &key &allow-other-keys)
  "Store the data to the extension's `extension-storage-data-path'."
  (with-data-file (file path :direction :output)
    (format file "~s" (get-data path)))
  t)

(defmethod restore ((profile data-profile) (path extension-storage-data-path) &key &allow-other-keys)
  "Restore the bookmarks from the buffer `bookmarks-path'."
  (handler-case
      (let ((data (with-data-file (file path)
                    (when file
                      (read file)))))
        (when data
          (nyxt::%set-data path data)))
    (error (c)
      (echo-warning "Failed to load extension data from ~s: ~a"
                    (expand-path path) c))))

(define-mode extension ()
  "The base mode for any extension to inherit from."
  ((name (error "Extension should have a name")
         :type string)
   (version (error "Extension should have a version")
            :type string)
   (manifest nil
             :type (or null string)
             :documentation "Original contents of the manifest.json file.")
   (id nil
       :type (or null string)
       :documentation "A unique ID of the extension.
Is shared between all the instances of the same extension.")
   (background-buffer nil
                      :documentation "The buffer to host background page of the extension in.
Is shared between all the instances of the same extension.")
   (popup-buffer nil
                 :type (or null panel-buffer)
                 :documentation "The panel-buffer to host extension popup in.
Is only set when popup is active.")
   (description nil
                :type (or null string))
   (homepage-url nil
                 :type (or null quri:uri))
   (extension-directory nil
              :type (or null pathname)
              :documentation "The directory that the extension resides in.")
   (extension-files (make-hash-table)
                    :type hash-table
                    :documentation "All the files extension packages.
Key is the relative path to the file.
Value is the loadable URL of that file.")
   (permissions nil
                :type list-of-strings
                :documentation "List of API permissions extension requires.")
   (content-scripts nil
                    :type list
                    :documentation "A list of `content-script's used by this extension.")
   (browser-action nil
                   :type (or null browser-action)
                   :documentation "Configuration for popup opening on extension icon click.")
   (storage-path nil
                 :type (or null extension-storage-data-path)
                 :documentation "The path that the storage API stores data in.")
   (destructor (lambda (mode)
                 (dolist (script (content-scripts mode))
                    (remove-content-script (buffer mode) mode script))
                 ;; Destroy the view when there are no more instances of this extension.
                 (when (null (sera:filter (alex:rcurry #'typep (type-of mode))
                                          (alex:mappend #'modes (buffer-list))))
                   (nyxt::buffer-delete (background-buffer mode)))))
   (constructor (lambda (mode)
                  (dolist (script (content-scripts mode))
                    (inject-content-script (buffer mode) mode script))
                  (unless (background-buffer mode)
                    ;; Need to set it to something to not trigger this in other instances.
                    (setf (background-buffer mode) t)
                    (setf (background-buffer mode) (make-background-buffer)))
                  (setf (extension-files mode)
                        (alex:alist-hash-table
                         (mapcar (lambda (file)
                                   (let ((relative-path
                                           (str:replace-all
                                            (namestring (extension-directory mode))
                                            "/" (namestring file))))
                                     (cons relative-path
                                           (if (equal (mimes:mime file) "text/html")
                                               (format nil "file://~a" file)
                                               (make-data-url file)))))
                                 (recursive-directory-elements (extension-directory mode)))))))))

(export-always 'has-permission-p)
(defmethod has-permission-p ((extension extension) (permission string))
  (str:s-member permission (permissions extension)))

(export-always 'merge-extension-path)
(defmethod merge-extension-path ((extension extension) path)
  (uiop:merge-pathnames* (string-left-trim "/" (namestring path))
                         (extension-directory extension)))

(defmethod nyxt::format-mode ((extension extension))
  (name extension))

(defun open-popup (extension-class &optional (buffer (current-buffer)))
  (with-current-buffer buffer
    ;;TODO: Send click message to background script if there's no popup.
    (sera:and-let* ((extension (nyxt:find-submode (nyxt:current-buffer) extension-class))
                    (browser-action (browser-action extension))
                    (default-popup (default-popup browser-action))
                    (popup (make-instance 'user-panel-buffer
                                          :title (default-title (browser-action extension))
                                          :id (nyxt::get-unique-identifier *browser*))))
      (setf (popup-buffer extension) popup)
      (enable-modes (list extension-class) popup)
      (nyxt::window-add-panel-buffer
       (current-window) popup
       :right)
      (setf (popup-buffer extension) popup)
      (buffer-load (quri.uri.file:make-uri-file :path (merge-extension-path extension default-popup))
                   :buffer popup))))

(export-always 'load-web-extension)
(defmacro load-web-extension (lispy-name directory)
  "Make an extension from DIRECTORY accessible as Nyxt mode (under LISPY-NAME).
DIRECTORY should be the one containing manifest.json file for the extension in question."
  (let* ((directory (uiop:parse-native-namestring directory))
         (manifest-text (uiop:read-file-string (uiop:merge-pathnames* "manifest.json" directory)))
         (json (json:decode-json-from-string manifest-text))
         (name (alex:assoc-value json :name)))
    `(progn
       (define-mode ,lispy-name (extension)
         ,(alex:assoc-value json :description)
         ((name ,name)
          (version ,(alex:assoc-value json :version))
          (manifest ,manifest-text)
          (id (or (symbol-name (gensym ,name)))
              :allocation :class)
          (background-buffer nil
                             :allocation :class)
          (popup-buffer nil
                        :allocation :class)
          (storage-path (make-instance
                         'extension-storage-data-path
                         :dirname (uiop:xdg-data-home nyxt::+data-root+ "extension-storage")
                         :basename (format nil "~a.txt" ,name))
                        :allocation :class)
          (description ,(alex:assoc-value json :description))
          (extension-directory ,directory)
          (homepage-url ,(alex:assoc-value json :homepage--url))
          (browser-action ,(make-browser-action json))
          (content-scripts (list ,@(mapcar (lambda (content-script-alist)
                                             (apply #'make-content-script
                                                    (alex:alist-plist content-script-alist)))
                                           (alex:assoc-value json :content--scripts))))))
       (defmethod initialize-instance :after ((extension ,lispy-name) &key)
         (setf (nyxt:glyph extension)
               (spinneret:with-html-string
                 (:a :class "button" :href (lisp-url `(open-popup ',(mode-name extension)))
                     :title (format nil "Open the browser action of ~a" (mode-name extension))
                     (:raw (setf (default-icon (browser-action extension))
                                 (encode-browser-action-icon (quote ,json) ,directory))))))))))
