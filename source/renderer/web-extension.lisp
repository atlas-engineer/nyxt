;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/renderer/cef)

(define-class web-extensions-directory (files:data-file nyxt-file)
  ((files:base-path #p"web-extensions/")
   (files:name "web-extensions"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Data directory whose subdirectories are unpacked Chromium web extensions."))

(export-always '*web-extensions-directory*)
(defvar *web-extensions-directory* (make-instance 'web-extensions-directory)
  "Directory of unpacked Chromium web extensions.
Each subdirectory that contains a manifest.json is loaded at startup.")

(define-class web-extension ()
  ((extension-name
    ""
    :type string
    :documentation "Name from the manifest.")
   (version
    ""
    :type string
    :documentation "Version from the manifest.")
   (description
    ""
    :type string
    :documentation "Description from the manifest.")
   (manifest-version
    0
    :type integer
    :documentation "Manifest version, 2 or 3.")
   (path
    #p""
    :type pathname
    :documentation "Absolute directory of the unpacked extension.")
   (extension-id
    ""
    :type string
    :documentation "Chromium extension id, from the manifest key or the directory path.")
   (page
    ""
    :type string
    :documentation "Relative popup or options page, when the manifest names one."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "An unpacked Chromium web extension."))

(-> web-extensions-directory-path () (or null pathname))
(defun web-extensions-directory-path ()
  "Return the expanded web-extensions data directory, or NIL when it is unset."
  (let ((path (files:expand *web-extensions-directory*)))
    (when (and (pathnamep path) (not (files:nil-pathname-p path)))
      path)))

(-> ensure-web-extensions-directory () (or null pathname))
(defun ensure-web-extensions-directory ()
  "Create the web-extensions data directory and return it."
  (let ((path (web-extensions-directory-path)))
    (when (and path (not (uiop:directory-exists-p path)))
      (handler-case
          (ensure-directories-exist path)
        (file-error (condition)
          (log:warn "Could not create the web extensions directory ~a: ~a." path condition)
          (return-from ensure-web-extensions-directory nil))))
    path))

(-> manifest-file (pathname) pathname)
(defun manifest-file (directory)
  (uiop:merge-pathnames* #p"manifest.json"
                         (uiop:ensure-directory-pathname directory)))

(-> extension-roots ((or pathname string)) list)
(defun extension-roots (path)
  "Return extension directories under PATH.
PATH itself is the extension when it contains manifest.json. Otherwise each
child directory that contains manifest.json is an extension."
  (let ((directory (uiop:ensure-directory-pathname path)))
    (cond
      ((not (uiop:directory-exists-p directory))
       '())
      ((uiop:file-exists-p (manifest-file directory))
       (list directory))
      (t
       (remove-if-not (lambda (child)
                        (uiop:file-exists-p (manifest-file child)))
                      (uiop:subdirectories directory))))))

(-> extension-directories (cef-browser) list)
(defun extension-directories (browser)
  (let ((roots '()))
    (dolist (path (append (list (ensure-web-extensions-directory))
                          (web-extension-paths browser)))
      (when path
        (dolist (root (extension-roots path))
          (handler-case
              (pushnew (truename root) roots :test #'uiop:pathname-equal)
            (file-error ()
              nil)))))
    (nreverse roots)))

(-> json-text (t string) (or null string))
(defun json-text (object key)
  (when (hash-table-p object)
    (let ((value (j:get key object)))
      (cond
        ((and (stringp value) (not (uiop:emptyp value)))
         value)
        ((integerp value)
         (princ-to-string value))
        (t nil)))))

(-> manifest-version-number (t) (or null integer))
(defun manifest-version-number (value)
  (let ((version (typecase value
                   (integer value)
                   (float (when (= value (float (round value) 1.0))
                            (round value)))
                   (t nil))))
    (when (member version '(2 3))
      version)))

(-> read-manifest (pathname) (or null hash-table))
(defun read-manifest (directory)
  (let ((file (manifest-file directory)))
    (handler-case
        (let ((value (j:decode (uiop:read-file-string file))))
          (if (hash-table-p value)
              value
              (progn
                (log:warn "Skipping web extension ~a: manifest.json is not an object." file)
                nil)))
      (error (condition)
        (log:warn "Skipping web extension ~a: ~a." file condition)
        nil))))

(-> nested-text (t string string) (or null string))
(defun nested-text (object key nested-key)
  (when (hash-table-p object)
    (let ((nested (j:get key object)))
      (when (hash-table-p nested)
        (json-text nested nested-key)))))

(-> manifest-page (hash-table) (or null string))
(defun manifest-page (manifest)
  (or (nested-text manifest "action" "default_popup")
      (nested-text manifest "browser_action" "default_popup")
      (nested-text manifest "page_action" "default_popup")
      (nested-text manifest "options_ui" "page")
      (json-text manifest "options_page")))

(-> u32 (integer) (unsigned-byte 32))
(defun u32 (n)
  (logand n #xffffffff))

(-> rotr ((integer 1 31) (unsigned-byte 32)) (unsigned-byte 32))
(defun rotr (count value)
  (u32 (logior (ash value (- count))
               (ash value (- 32 count)))))

(defparameter +sha256-k+
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(-> sha256-pad ((vector (unsigned-byte 8))) (vector (unsigned-byte 8)))
(defun sha256-pad (octets)
  (let* ((length (length octets))
         (bit-length (* length 8))
         (padded (make-array (* 64 (ceiling (+ length 1 8) 64))
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (replace padded octets)
    (setf (aref padded length) #x80)
    (loop for i from 0 below 8
          do (setf (aref padded (+ (- (length padded) 8) i))
                   (ldb (byte 8 (* (- 7 i) 8)) bit-length)))
    padded))

(-> sha256-hex ((vector (unsigned-byte 8))) string)
(defun sha256-hex (octets)
  "Return the lowercase hex SHA-256 of OCTETS."
  (let ((hash (make-array 8 :element-type '(unsigned-byte 32)
                          :initial-contents #(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                                              #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
        (words (make-array 64 :element-type '(unsigned-byte 32)))
        (padded (sha256-pad octets)))
    (loop for chunk from 0 below (length padded) by 64
          do (loop for i from 0 below 16
                   do (setf (aref words i)
                            (logior (ash (aref padded (+ chunk (* i 4))) 24)
                                    (ash (aref padded (+ chunk (* i 4) 1)) 16)
                                    (ash (aref padded (+ chunk (* i 4) 2)) 8)
                                    (aref padded (+ chunk (* i 4) 3)))))
             (loop for i from 16 below 64
                   do (let* ((earlier (aref words (- i 15)))
                             (recent (aref words (- i 2))))
                        (setf (aref words i)
                              (u32 (+ (aref words (- i 16))
                                      (logxor (rotr 7 earlier)
                                              (rotr 18 earlier)
                                              (ash earlier -3))
                                      (aref words (- i 7))
                                      (logxor (rotr 17 recent)
                                              (rotr 19 recent)
                                              (ash recent -10)))))))
             (let ((a (aref hash 0)) (b (aref hash 1)) (c (aref hash 2)) (d (aref hash 3))
                   (e (aref hash 4)) (f (aref hash 5)) (g (aref hash 6)) (h (aref hash 7)))
               (loop for i from 0 below 64
                     do (let* ((s1 (logxor (rotr 6 e) (rotr 11 e) (rotr 25 e)))
                               (ch (logxor (logand e f) (logand (lognot e) g)))
                               (temp1 (u32 (+ h s1 ch (aref +sha256-k+ i) (aref words i))))
                               (s0 (logxor (rotr 2 a) (rotr 13 a) (rotr 22 a)))
                               (maj (logxor (logand a b) (logand a c) (logand b c)))
                               (temp2 (u32 (+ s0 maj))))
                          (setf h g g f f e e (u32 (+ d temp1))
                                d c c b b a a (u32 (+ temp1 temp2)))))
               (setf (aref hash 0) (u32 (+ (aref hash 0) a))
                     (aref hash 1) (u32 (+ (aref hash 1) b))
                     (aref hash 2) (u32 (+ (aref hash 2) c))
                     (aref hash 3) (u32 (+ (aref hash 3) d))
                     (aref hash 4) (u32 (+ (aref hash 4) e))
                     (aref hash 5) (u32 (+ (aref hash 5) f))
                     (aref hash 6) (u32 (+ (aref hash 6) g))
                     (aref hash 7) (u32 (+ (aref hash 7) h)))))
    (with-output-to-string (out)
      (loop for word across hash
            do (format out "~(~8,'0x~)" word)))))

(-> chromium-extension-id ((vector (unsigned-byte 8))) (or null string))
(defun chromium-extension-id (octets)
  "Return the Chromium extension id for OCTETS.
The id is the first 16 bytes of SHA-256, written with the letters a through p."
  (let ((hex (sha256-hex octets)))
    (when (and hex (>= (length hex) 32))
      (map 'string
           (lambda (char)
             (code-char (+ (char-code #\a) (digit-char-p char 16))))
           (subseq hex 0 32)))))

(-> base64-pad (string) string)
(defun base64-pad (string)
  (let ((remainder (mod (length string) 4)))
    (if (zerop remainder)
        string
        (uiop:strcat string (make-string (- 4 remainder) :initial-element #\=)))))

(-> extension-id-from-key (string) (or null string))
(defun extension-id-from-key (key)
  (let* ((stripped (remove-if (lambda (char)
                                (member char '(#\newline #\return #\space #\tab)
                                        :test #'char=))
                              key))
         (bytes (handler-case
                    (cl-base64:base64-string-to-usb8-array (base64-pad stripped))
                  (error ()
                    nil))))
    (when bytes
      (chromium-extension-id bytes))))

(-> extension-id-from-path (pathname) (or null string))
(defun extension-id-from-path (directory)
  (handler-case
      (let ((native (string-right-trim '(#\/ #\\)
                                       (uiop:native-namestring (truename directory)))))
        (chromium-extension-id
         (flexi-streams:string-to-octets native :external-format :utf-8)))
    (file-error ()
      nil)))

(-> extension-id-for (hash-table pathname) string)
(defun extension-id-for (manifest directory)
  (let ((key (json-text manifest "key")))
    (cond
      (key
       (or (extension-id-from-key key)
           (progn
             (log:warn "Web extension ~a has a manifest key that could not be read."
                       directory)
             "")))
      (t
       (or (extension-id-from-path directory) "")))))

(-> manifest-message (hash-table pathname string) (or null string))
(defun manifest-message (manifest directory text)
  "Return TEXT, resolving a __MSG_key__ name from the default locale."
  (unless (and (>= (length text) 8)
               (string= text "__MSG_" :end1 6)
               (string= text "__" :start1 (- (length text) 2)))
    (return-from manifest-message text))
  (let* ((key (subseq text 6 (- (length text) 2)))
         (locale (or (json-text manifest "default_locale") "en"))
         (file (uiop:merge-pathnames*
                (make-pathname :directory (list :relative "_locales" locale)
                               :name "messages"
                               :type "json")
                (uiop:ensure-directory-pathname directory))))
    (or (handler-case
            (let ((messages (j:decode (uiop:read-file-string file))))
              (when (hash-table-p messages)
                (let ((entry (j:get key messages)))
                  (when (hash-table-p entry)
                    (json-text entry "message")))))
          (error ()
            nil))
        text)))

(-> extension-from-directory (pathname) (or null web-extension))
(defun extension-from-directory (directory)
  (let ((manifest (read-manifest directory)))
    (unless manifest
      (return-from extension-from-directory nil))
    (let ((name (let ((raw-name (json-text manifest "name")))
                  (when raw-name
                    (manifest-message manifest directory raw-name))))
          (version (json-text manifest "version"))
          (manifest-version (manifest-version-number (j:get "manifest_version" manifest))))
      (cond
        ((null name)
         (log:warn "Skipping web extension ~a: the manifest has no name." directory)
         nil)
        ((null version)
         (log:warn "Skipping web extension ~a: the manifest has no version." directory)
         nil)
        ((null manifest-version)
         (log:warn "Skipping web extension ~a: manifest version ~a is not 2 or 3."
                   directory (j:get "manifest_version" manifest))
         nil)
        (t
         (make-instance 'web-extension
                        :extension-name name
                        :version version
                        :description (or (manifest-message manifest directory
                                                              (or (json-text manifest "description") ""))
                                         "")
                        :manifest-version manifest-version
                        :path (uiop:ensure-directory-pathname
                               (or (ignore-errors (truename directory))
                                   directory))
                        :extension-id (extension-id-for manifest directory)
                        :page (or (manifest-page manifest) "")))))))

(-> extension-load-path (web-extension) (or null string))
(defun extension-load-path (extension)
  "Return the native path CEF should load, or NIL when the path cannot be passed.
`--load-extension' separates directories with commas."
  (let ((native (string-right-trim '(#\/ #\\)
                                   (uiop:native-namestring (path extension)))))
    (when (find #\, native)
      (log:warn "Skipping web extension ~a because its path contains a comma." native)
      (return-from extension-load-path nil))
    native))

(-> switch-prefix (string) string)
(defun switch-prefix (name)
  (uiop:strcat "--" name "="))

(-> split-csv (string) list)
(defun split-csv (value)
  (remove-if #'uiop:emptyp (str:split "," value)))

(-> switch-present-p (list string) boolean)
(defun switch-present-p (options name)
  (let ((flag (uiop:strcat "--" name))
        (prefix (switch-prefix name)))
    (some (lambda (option)
            (or (string= option flag)
                (and (stringp option)
                     (str:starts-with-p prefix option))))
          options)))

(-> add-csv-switch (list string string) list)
(defun add-csv-switch (options name value)
  "Return OPTIONS with each comma-separated item of VALUE joined onto --NAME.
A switch already in OPTIONS is removed and written again at the end, so the
merged value is the one CEF keeps."
  (let ((prefix (switch-prefix name))
        (flag (uiop:strcat "--" name))
        (kept '())
        (items '()))
    (loop for tail = options then (rest tail)
          while tail
          do (let ((option (first tail)))
               (cond
                 ((and (stringp option)
                       (string= option flag)
                       (rest tail)
                       (stringp (second tail)))
                  (setf items (append items (split-csv (second tail)))
                        tail (rest tail)))
                 ((and (stringp option)
                       (str:starts-with-p prefix option))
                  (setf items (append items (split-csv (subseq option (length prefix))))))
                 (t
                  (push option kept)))))
    (dolist (item (split-csv value))
      (unless (member item items :test #'string=)
        (setf items (append items (list item)))))
    (append (nreverse kept)
            (when items
              (list (uiop:strcat prefix (str:join "," items)))))))

(-> configure-web-extensions (cef-browser cef:interface) list)
(defun configure-web-extensions (browser interface)
  "Pass unpacked Manifest V2 and V3 extensions to CEF before it starts.
Manifest V2 stays enabled through Chromium's AllowLegacyMV2Extensions feature,
which CEF 150 still honors for unpacked directories."
  (unless (web-extensions-p browser)
    (return-from configure-web-extensions '()))
  (when (switch-present-p (cef:launch-options interface) "disable-extensions")
    (log:warn "Web extensions were not loaded because --disable-extensions is set.")
    (return-from configure-web-extensions '()))
  (let* ((found (remove nil (mapcar #'extension-from-directory
                                    (extension-directories browser))))
         (loadable (remove-if-not #'extension-load-path found)))
    (setf (web-extensions browser) loadable)
    (when loadable
      (setf (cef:launch-options interface)
            (add-csv-switch
             (add-csv-switch (cef:launch-options interface)
                             "enable-features"
                             "AllowLegacyMV2Extensions")
             "load-extension"
             (str:join "," (mapcar #'extension-load-path loadable))))
      (dolist (extension loadable)
        (log:info "Loading web extension ~a ~a from ~a."
                  (extension-name extension)
                  (version extension)
                  (uiop:native-namestring (path extension)))))
    loadable))

(-> web-extension-url (web-extension) (or null string))
(defun web-extension-url (extension)
  (when (and (plusp (length (page extension)))
             (plusp (length (extension-id extension))))
    (uiop:strcat "chrome-extension://"
                 (extension-id extension)
                 "/"
                 (string-left-trim "/" (page extension)))))

(-> extension-loaded-p (web-extension) boolean)
(defun extension-loaded-p (extension)
  "Return T when EXTENSION was passed to CEF for this process."
  (not (null (find (path extension) (web-extensions *browser*)
                   :key #'path
                   :test #'uiop:pathname-equal))))

(-> web-extension-detail (web-extension) string)
(defun web-extension-detail (extension)
  (format nil "~aManifest V~d · ~a"
          (if (extension-loaded-p extension)
              ""
              "Starts on next launch · ")
          (manifest-version extension)
          (version extension)))

(-> disk-extension-directories () list)
(defun disk-extension-directories ()
  (let ((root (web-extensions-directory-path)))
    (if (and root (uiop:directory-exists-p root))
        (extension-roots root)
        '())))

(-> disk-web-extensions () list)
(defun disk-web-extensions ()
  "Return extensions stored in the web-extensions data directory."
  (remove nil (mapcar #'extension-from-directory (disk-extension-directories))))

(-> managed-extension-p (pathname) boolean)
(defun managed-extension-p (directory)
  "Return T when DIRECTORY is a direct child of the web-extensions directory."
  (let ((root (web-extensions-directory-path)))
    (and root
         directory
         (uiop:directory-exists-p directory)
         (handler-case
             (uiop:pathname-equal
              (truename (uiop:ensure-directory-pathname root))
              (truename (uiop:pathname-parent-directory-pathname
                         (uiop:ensure-directory-pathname directory))))
           (file-error ()
             nil)))))

(-> managed-web-extensions () list)
(defun managed-web-extensions ()
  (remove-if-not (lambda (extension)
                   (managed-extension-p (path extension)))
                 (disk-web-extensions)))

(-> listed-web-extensions () list)
(defun listed-web-extensions ()
  "Return installed extensions, plus any loaded from outside the data directory."
  (let* ((on-disk (disk-web-extensions))
         (external (remove-if (lambda (loaded)
                                (find (path loaded) on-disk
                                      :key #'path
                                      :test #'uiop:pathname-equal))
                              (web-extensions *browser*))))
    (sort (append on-disk external) #'string-lessp :key #'extension-name)))

(-> symbolic-link-p (pathname) boolean)
(defun symbolic-link-p (path)
  #+sbcl
  (handler-case
      (sb-posix:s-islnk
       (sb-posix:stat-mode
        (sb-posix:lstat (string-right-trim "/" (uiop:native-namestring path)))))
    (error ()
      nil))
  #-sbcl
  nil)

(-> directory-name (pathname) string)
(defun directory-name (directory)
  (let ((name (first (last (pathname-directory
                            (uiop:ensure-directory-pathname directory))))))
    (unless (stringp name)
      (error "The directory ~a has no name." directory))
    name))

(-> extension-directory-name (string) string)
(defun extension-directory-name (name)
  "Return a single path segment for NAME."
  (let ((slug (with-output-to-string (out)
                (let ((dash nil))
                  (loop for char across (string-downcase name)
                        do (cond
                             ((alphanumericp char)
                              (write-char char out)
                              (setf dash nil))
                             ((not dash)
                              (write-char #\- out)
                              (setf dash t))))))))
    (let ((trimmed (string-trim "-" slug)))
      (if (uiop:emptyp trimmed) "extension" trimmed))))

(-> child-directory (pathname string) pathname)
(defun child-directory (root name)
  (uiop:ensure-directory-pathname
   (uiop:merge-pathnames* (uiop:strcat name "/")
                          (uiop:ensure-directory-pathname root))))

(-> available-extension-directory (pathname string) pathname)
(defun available-extension-directory (root slug)
  (loop for index from 0
        for name = (if (zerop index) slug (format nil "~a-~d" slug index))
        for candidate = (child-directory root name)
        unless (uiop:directory-exists-p candidate)
          return candidate))

(-> copy-directory-contents (pathname pathname) pathname)
(defun copy-directory-contents (source destination)
  "Copy files under SOURCE into DESTINATION, skipping symbolic links."
  (let ((destination (uiop:ensure-directory-pathname destination)))
    (ensure-directories-exist destination)
    (dolist (file (uiop:directory-files source))
      (unless (symbolic-link-p file)
        (uiop:copy-file file (uiop:merge-pathnames* (file-namestring file)
                                                    destination))))
    (dolist (child (uiop:subdirectories source))
      (unless (symbolic-link-p child)
        (copy-directory-contents child (child-directory destination
                                                        (directory-name child)))))
    destination))

(-> path-under-p (pathname pathname) boolean)
(defun path-under-p (path root)
  (let ((root-name (uiop:native-namestring (uiop:ensure-directory-pathname root)))
        (path-name (handler-case (uiop:native-namestring path)
                     (error ()
                       nil))))
    (and path-name
         (plusp (length root-name))
         (or (string= (string-right-trim "/" path-name)
                      (string-right-trim "/" root-name))
             (str:starts-with-p root-name path-name)))))

(-> delete-tree (pathname) null)
(defun delete-tree (directory)
  (let ((root (uiop:ensure-directory-pathname directory)))
    (when (equal (pathname-directory root) '(:absolute))
      (error "Refusing to delete ~a." root))
    (when (uiop:directory-exists-p root)
      (uiop:delete-directory-tree
       root
       :validate (lambda (path) (path-under-p path root))
       :if-does-not-exist :ignore))
    nil))

(-> install-staging-directory (pathname) pathname)
(defun install-staging-directory (root)
  (ensure-directories-exist
   (uiop:ensure-directory-pathname
    (uiop:merge-pathnames*
     (format nil ".web-extension-install/~a/" (random 1000000000))
     (uiop:pathname-parent-directory-pathname
      (uiop:ensure-directory-pathname root))))))

(-> move-directory (pathname pathname) pathname)
(defun move-directory (source destination)
  (let ((from (string-right-trim "/" (uiop:native-namestring source)))
        (to (string-right-trim "/" (uiop:native-namestring destination))))
    #+sbcl
    (sb-posix:rename from to)
    #-sbcl
    (uiop:run-program (list "mv" from to) :error-output :string)
    destination))

(-> place-extension (pathname) (values web-extension keyword))
(defun place-extension (source)
  "Copy SOURCE into the web-extensions directory.
The second value is :already, :updated, or :installed."
  (let ((extension (extension-from-directory source)))
    (unless extension
      (error "The directory ~a has no usable manifest." source))
    (when (managed-extension-p source)
      (return-from place-extension (values extension :already)))
    (let ((root (ensure-web-extensions-directory)))
      (unless root
        (error "The web extensions directory could not be created."))
      (let* ((existing (find (extension-name extension) (disk-web-extensions)
                             :key #'extension-name :test #'string=))
             (destination (if existing
                              (path existing)
                              (available-extension-directory
                               root
                               (extension-directory-name (extension-name extension)))))
             (staging (install-staging-directory root)))
        (when (find #\, (uiop:native-namestring destination))
          (error "The path ~a contains a comma, so it cannot be loaded." destination))
        (unwind-protect
             (progn
               (copy-directory-contents source staging)
               (unless (uiop:file-exists-p (manifest-file staging))
                 (error "The extension ~a could not be copied."
                        (extension-name extension)))
               (when (uiop:directory-exists-p destination)
                 (unless (managed-extension-p destination)
                   (error "Refusing to replace ~a." destination))
                 (delete-tree destination))
               (handler-case
                   (move-directory staging destination)
                 (error ()
                   (copy-directory-contents staging destination)))
               (let ((installed (extension-from-directory destination)))
                 (unless installed
                   (error "The extension ~a could not be read after copying."
                          (extension-name extension)))
                 (log:info "~a web extension ~a ~a into ~a."
                           (if existing "Updated" "Installed")
                           (extension-name installed)
                           (version installed)
                           (uiop:native-namestring (path installed)))
                 (values installed (if existing :updated :installed))))
          (when (uiop:directory-exists-p staging)
            (delete-tree staging))
          (let ((parent (uiop:pathname-parent-directory-pathname staging)))
            (when (and (uiop:directory-exists-p parent)
                       (null (uiop:subdirectories parent))
                       (null (uiop:directory-files parent)))
              (handler-case
                  (delete-tree parent)
                (error ()
                  nil)))))))))

(-> install-directories (list) list)
(defun install-directories (directories)
  "Copy each directory that has a usable manifest.
Return a list of (extension status) lists."
  (let ((results '()))
    (dolist (directory directories)
      (when (extension-from-directory directory)
        (push (multiple-value-list (place-extension directory)) results)))
    (unless results
      (error "No usable manifest.json was found."))
    (nreverse results)))

(-> extension-root-containing (pathname) (or null pathname))
(defun extension-root-containing (path)
  "Return the nearest directory at or above PATH that contains manifest.json."
  (let ((dir (uiop:ensure-directory-pathname
              (if (uiop:directory-exists-p path)
                  path
                  (uiop:pathname-directory-pathname path)))))
    (loop
      (unless (and dir (uiop:directory-exists-p dir))
        (return nil))
      (when (uiop:file-exists-p (manifest-file dir))
        (return dir))
      (let ((parent (uiop:pathname-parent-directory-pathname dir)))
        (when (or (null parent) (uiop:pathname-equal parent dir))
          (return nil))
        (setf dir parent)))))

(-> read-u32 (stream) (or null integer))
(defun read-u32 (stream)
  (let ((bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (unless (= 4 (read-sequence bytes stream))
      (return-from read-u32 nil))
    (logior (aref bytes 0)
            (ash (aref bytes 1) 8)
            (ash (aref bytes 2) 16)
            (ash (aref bytes 3) 24))))

(-> crx-payload-offset (stream) (or null integer))
(defun crx-payload-offset (stream)
  "Return the zip start for a Cr24 header, or NIL when the header is unusable."
  (let ((version (read-u32 stream))
        (limit (* 1024 1024)))
    (case version
      (2 (let ((public-key (read-u32 stream))
               (signature (read-u32 stream)))
           (when (and public-key signature
                      (<= public-key limit)
                      (<= signature limit))
             (+ 16 public-key signature))))
      (3 (let ((header (read-u32 stream)))
           (when (and header (<= header limit))
             (+ 12 header)))))))

(-> crx-zip-offset (pathname) (or null integer))
(defun crx-zip-offset (path)
  "Return the byte offset of the zip payload in PATH.
0 means PATH is itself a zip. NIL means PATH is neither a zip nor a crx."
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
          (unless (= 4 (read-sequence magic in))
            (return-from crx-zip-offset nil))
          (cond
            ((equalp magic #(80 75 3 4))
             0)
            ((equalp magic #(67 114 50 52))
             (crx-payload-offset in))
            (t nil))))
    (file-error ()
      nil)))

(-> write-zip-payload (pathname integer) pathname)
(defun write-zip-payload (archive offset)
  (let ((zip (uiop:tmpize-pathname
              (uiop:merge-pathnames* #p"nyxt-webext.zip"
                                     (uiop:temporary-directory)))))
    (with-open-file (in archive :element-type '(unsigned-byte 8))
      (unless (file-position in offset)
        (error "The file ~a is not a .zip or a .crx." archive))
      (with-open-file (out zip :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede
                               :if-does-not-exist :create)
        (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
          (loop for read = (read-sequence buffer in)
                until (zerop read)
                do (write-sequence buffer out :end read)))))
    zip))

(-> temporary-directory (string) pathname)
(defun temporary-directory (name)
  (ensure-directories-exist
   (uiop:ensure-directory-pathname
    (uiop:merge-pathnames*
     (format nil "nyxt-webext-~a-~a/" name (random 1000000000))
     (uiop:temporary-directory)))))

(-> program-available-p (string) boolean)
(defun program-available-p (name)
  (handler-case
      (zerop (nth-value 2
                        (uiop:run-program (list name "--version")
                                          :ignore-error-status t
                                          :output nil
                                          :error-output nil)))
    (error ()
      nil)))

(-> unpack-zip (pathname pathname) pathname)
(defun unpack-zip (zip directory)
  "Extract ZIP into DIRECTORY, ignoring entries that leave DIRECTORY."
  (unless (program-available-p "python3")
    (error "Unpacking an extension needs python3."))
  (let ((archive (uiop:native-namestring zip))
        (destination (uiop:native-namestring directory)))
    (multiple-value-bind (output error-output code)
        (uiop:run-program
         (list "python3" "-c"
               "import sys, zipfile
z = zipfile.ZipFile(sys.argv[1])
destination = sys.argv[2]
for info in z.infolist():
    parts = info.filename.split('/')
    if info.filename.startswith('/') or '..' in parts:
        continue
    z.extract(info, destination)
"
               archive destination)
         :output :string
         :error-output :string
         :ignore-error-status t)
      (declare (ignore output))
      (unless (zerop code)
        (error "Could not unpack ~a.~@[ ~a~]"
               archive
               (unless (uiop:emptyp error-output) error-output)))))
  directory)

(-> install-archive (pathname integer) list)
(defun install-archive (archive offset)
  (let ((zip (if (zerop offset)
                 archive
                 (write-zip-payload archive offset)))
        (directory (temporary-directory "archive")))
    (unwind-protect
         (progn
           (unpack-zip zip directory)
           (install-directories
            (or (extension-roots directory)
                (error "The archive ~a contains no manifest.json." archive))))
      (when (and zip
                 (not (uiop:pathname-equal zip archive))
                 (uiop:file-exists-p zip))
        (delete-file zip))
      (when (uiop:directory-exists-p directory)
        (delete-tree directory)))))

(-> archive-type-p (t) boolean)
(defun archive-type-p (path)
  (not (null (member (string-downcase (or (pathname-type (pathname path)) ""))
                     '("zip" "crx")
                     :test #'string=))))

(-> install-path (pathname) list)
(defun install-path (path)
  (cond
    ((uiop:directory-exists-p path)
     (install-directories
      (or (extension-roots path)
          (let ((root (extension-root-containing path)))
            (when root (list root)))
          (error "No manifest.json was found at ~a." path))))
    ((uiop:file-exists-p path)
     (let ((offset (crx-zip-offset path)))
       (cond
         ((numberp offset)
          (install-archive path offset))
         ((archive-type-p path)
          (error "The file ~a is not a .zip or a .crx." path))
         (t
          (let ((root (extension-root-containing path)))
            (unless root
              (error "No manifest.json was found at ~a." path))
            (install-directories (list root)))))))
    (t
     (error "No file or directory was found at ~a." path))))

(-> url-string-p (t) boolean)
(defun url-string-p (source)
  (and (stringp source)
       (let ((uri (ignore-errors (quri:uri source))))
         (and (quri:uri-p uri)
              (not (null (member (quri:uri-scheme uri)
                                 '("http" "https")
                                 :test #'string-equal)))))))

(-> download-archive (string) pathname)
(defun download-archive (url)
  (let* ((uri (quri:uri url))
         (type (string-downcase (or (pathname-type (or (quri:uri-path uri) "")) "bin")))
         (path (uiop:tmpize-pathname
                (uiop:merge-pathnames*
                 (format nil "nyxt-webext.~a" type)
                 (uiop:temporary-directory))))
         (body (dex:get url :force-binary t)))
    (unless (typep body '(vector (unsigned-byte 8)))
      (error "The page ~a is not an extension package." url))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (write-sequence body out))
    path))

(-> install-web-extensions (t) list)
(defun install-web-extensions (source)
  "Install SOURCE, a directory, a .zip, a .crx, a path string, or an http URL."
  (cond
    ((url-string-p source)
     (let ((path (download-archive source)))
       (unwind-protect
            (install-path path)
         (when (and path (uiop:file-exists-p path))
           (delete-file path)))))
    ((pathnamep source)
     (install-path source))
    ((stringp source)
     (install-web-extensions (uiop:ensure-pathname source :want-pathname t)))
    (t
     (error "No extension was found at ~a." source))))

(-> file-url-path (quri:uri) pathname)
(defun file-url-path (uri)
  (let* ((raw (or (quri:uri-path uri) ""))
         (decoded (or (ignore-errors (quri:url-decode raw)) raw)))
    (uiop:ensure-pathname (if (or (uiop:file-exists-p raw)
                                  (uiop:directory-exists-p raw))
                              raw
                              decoded)
                          :want-pathname t)))

(-> installable-path-p (pathname) boolean)
(defun installable-path-p (path)
  (cond
    ((uiop:directory-exists-p path)
     (not (null (or (extension-roots path)
                    (extension-root-containing path)))))
    ((uiop:file-exists-p path)
     (or (numberp (crx-zip-offset path))
         (archive-type-p path)
         (not (null (extension-root-containing path)))))
    (t nil)))

(-> page-extension-source (string) t)
(defun page-extension-source (url-string)
  "Return a path or URL when URL-STRING itself is an extension package."
  (let ((uri (ignore-errors (quri:uri url-string))))
    (unless (quri:uri-p uri)
      (return-from page-extension-source nil))
    (cond
      ((quri:uri-file-p uri)
       (let ((path (handler-case (file-url-path uri)
                     (error ()
                       nil))))
         (when (and path (installable-path-p path))
           path)))
      ((and (member (quri:uri-scheme uri) '("http" "https") :test #'string-equal)
            (archive-type-p (or (quri:uri-path uri) "")))
       url-string)
      (t nil))))

(-> prompt-web-extension-source () t)
(defun prompt-web-extension-source ()
  (prompt1 :prompt "Extension directory or package"
           :extra-modes 'nyxt/mode/file-manager:file-manager-mode
           :input (uiop:native-namestring (cef-download-directory))
           :sources (make-instance 'nyxt/mode/file-manager:file-source
                                   :name "Extensions"
                                   :enable-marks-p nil
                                   :allow-directories t
                                   :path-filter
                                   (lambda (path)
                                     (or (uiop:directory-pathname-p path)
                                         (archive-type-p path))))))

(-> extension-name-list (list) string)
(defun extension-name-list (results)
  (str:join ", " (mapcar (lambda (result)
                           (extension-name (first result)))
                         results)))

(-> report-install (list) t)
(defun report-install (results)
  (let ((installed (remove-if (lambda (result) (eq (second result) :already))
                              results))
        (updated (remove-if-not (lambda (result) (eq (second result) :updated))
                                results)))
    (cond
      ((null installed)
       (echo "~a ~a already installed."
             (extension-name-list results)
             (if (cdr results) "are" "is")))
      ((and updated (= (length updated) (length installed)))
       (echo "Updated ~a. Restart Nyxt to load ~a."
             (extension-name-list installed)
             (if (cdr installed) "them" "it")))
      (t
       (echo "Installed ~a. Restart Nyxt to load ~a."
             (extension-name-list installed)
             (if (cdr installed) "them" "it"))))))

(-> reload-web-extension-pages () null)
(defun reload-web-extension-pages ()
  (let ((path (quri:uri-path (quri:uri (nyxt-url 'list-web-extensions)))))
    (when (stringp path)
      (dolist (buffer (buffer-list))
        (let ((buffer-path (and (url buffer)
                                (quri:uri-p (url buffer))
                                (quri:uri-path (url buffer)))))
          (when (and (stringp buffer-path) (string= buffer-path path))
            (ffi-buffer-reload buffer))))))
  nil)

(define-command-global install-web-extension (&optional source)
  "Install a Chromium web extension from a directory, a .zip, a .crx, or a page."
  (handler-case
      (let ((source (or source
                        (let ((buffer (current-buffer)))
                          (when (and buffer (url buffer))
                            (page-extension-source (render-url (url buffer)))))
                        (prompt-web-extension-source))))
        (when source
          (let ((results (install-web-extensions source)))
            (report-install results)
            (reload-web-extension-pages)
            (mapcar (lambda (result) (extension-name (first result))) results))))
    (prompt-buffer-canceled ()
      nil)
    (error (condition)
      (echo-warning "Could not install the web extension: ~a" condition)
      nil)))

(define-class web-extension-source (prompter:source)
  ((prompter:name "Web extensions")
   (prompter:constructor '())
   (prompter:enable-marks-p nil))
  (:export-class-name-p t)
  (:metaclass user-class)
  (:documentation "Prompt source for installed web extensions."))

(defmethod prompter:object-attributes ((extension web-extension)
                                       (source web-extension-source))
  `(("Name" ,(extension-name extension))
    ("Version" ,(version extension))
    ("Manifest" ,(format nil "V~d" (manifest-version extension)))))

(-> prompt-web-extension-removal () (or null web-extension))
(defun prompt-web-extension-removal ()
  (let ((extensions (managed-web-extensions)))
    (unless extensions
      (echo "No web extensions are installed.")
      (return-from prompt-web-extension-removal nil))
    (prompt1 :prompt "Remove web extension"
             :sources (make-instance 'web-extension-source
                                     :constructor extensions))))

(-> delete-installed-extension (web-extension) (values web-extension &optional))
(defun delete-installed-extension (extension)
  (unless (managed-extension-p (path extension))
    (error "The extension ~a is not in the web extensions directory."
           (extension-name extension)))
  (delete-tree (path extension))
  (setf (web-extensions *browser*)
        (remove-if (lambda (loaded)
                     (uiop:pathname-equal (path loaded) (path extension)))
                   (web-extensions *browser*)))
  extension)

(define-command-global remove-web-extension (&optional extension)
  "Remove an installed web extension."
  (handler-case
      (let ((extension (if (web-extension-p extension)
                           extension
                           (prompt-web-extension-removal))))
        (when extension
          (let ((loaded-p (extension-loaded-p extension)))
            (delete-installed-extension extension)
            (reload-web-extension-pages)
            (if loaded-p
                (echo "Removed ~a. It stays loaded until the next start."
                      (extension-name extension))
                (echo "Removed ~a." (extension-name extension)))
            (extension-name extension))))
    (prompt-buffer-canceled ()
      nil)
    (error (condition)
      (echo-warning "Could not remove the web extension: ~a" condition)
      nil)))

(-> open-web-extension-page (web-extension buffer) t)
(defun open-web-extension-page (extension buffer)
  "Open EXTENSION's page in BUFFER."
  (when-let ((url-string (web-extension-url extension)))
    (ffi-buffer-load buffer (url url-string))))

(-> openable-web-extensions () list)
(defun openable-web-extensions ()
  "Return loaded extensions that have a page."
  (remove-if-not (lambda (extension)
                   (and (extension-loaded-p extension)
                        (web-extension-url extension)))
                 (listed-web-extensions)))

(-> web-extension-named (string) (or null web-extension))
(defun web-extension-named (name)
  "Return the installed extension named NAME."
  (find name (listed-web-extensions) :key #'extension-name :test #'string-equal))

(-> prompt-web-extension () (or null web-extension))
(defun prompt-web-extension ()
  (let ((extensions (openable-web-extensions)))
    (unless extensions
      (echo "No web extensions can be opened.")
      (return-from prompt-web-extension nil))
    (prompt1 :prompt "Open web extension"
             :sources (make-instance 'web-extension-source
                                     :constructor extensions))))

(define-command-global open-web-extension (&optional extension)
  "Open a web extension by name."
  (handler-case
      (let ((extension (cond
                         ((web-extension-p extension) extension)
                         ((stringp extension) (web-extension-named extension))
                         (t (prompt-web-extension)))))
        (cond
          ((null extension)
           nil)
          ((not (extension-loaded-p extension))
           (echo "~a starts on the next launch." (extension-name extension))
           nil)
          ((null (web-extension-url extension))
           (echo "~a has no page." (extension-name extension))
           nil)
          (t
           (open-web-extension-page extension (current-buffer))
           (extension-name extension))))
    (prompt-buffer-canceled ()
      nil)
    (error (condition)
      (echo-warning "Could not open the web extension: ~a" condition)
      nil)))

(defun web-extension-row (extension buffer)
  (let ((url (when (extension-loaded-p extension)
               (web-extension-url extension)))
        (extension extension))
    (spinneret:with-html
      (:li
       (:a :href (or url "#")
           :title (uiop:native-namestring (path extension))
           :onclick (ps:ps
                      (ps:chain event (prevent-default))
                      (when (ps:lisp url)
                        (nyxt/ps:lisp-eval
                         (:title "Open web extension"
                          :buffer buffer)
                         (open-web-extension-page extension buffer)))
                      nil)
           (:span (extension-name extension))
           (:span (web-extension-detail extension)))
       (when (managed-extension-p (path extension))
         (:button :class "close"
                  :type "button"
                  :title "Remove"
                  :onclick (ps:ps
                             (nyxt/ps:lisp-eval
                              (:title "Remove web extension"
                               :buffer buffer)
                              (remove-web-extension extension)))
                  "×"))))
    nil))

(defun web-extensions-html (buffer)
  (let ((extensions (listed-web-extensions)))
    (spinneret:with-html-string
      (:nstyle
        '("h1"
          :margin-bottom "17px")
        `("p.lead"
          :margin "0 0 17px"
          :font-size "17px"
          :line-height "24px"
          :color ,theme:primary-color)
        '("div.actions"
          :margin "0 0 37px")
        `("button.install"
          :appearance "none"
          :display "flex"
          :align-items "center"
          :box-sizing "border-box"
          :width "fit-content"
          :height "37px"
          :margin "0"
          :padding "0 17px"
          :border "none"
          :border-radius "7px"
          :background-color ,theme:background-color+
          :background-image ,(format nil "linear-gradient(~a, ~a)"
                                     theme:action-color theme:action-color)
          :background-size "calc(100% - 14px) 3px"
          :background-repeat "no-repeat"
          :background-position "center bottom"
          :color ,theme:on-background-color
          :font-family ,theme:font-family
          :font-size "17px"
          :font-weight "400"
          :line-height "24px"
          :cursor "pointer")
        `("button.install:hover"
          :background-color ,theme:background-color-)
        `("button.install:focus-visible"
          :outline "2px solid"
          :outline-color ,theme:action-color
          :outline-offset "2px")
        '("ul.list > li > a"
          :padding-right "62px")
        `("ul.list > li:hover > a"
          :background-color ,theme:background-color-
          :opacity "1")
        '("ul.list > li:hover::after"
          :background-color "transparent")
        '("ul.list > li:has(+ li:hover)::after"
          :background-color "transparent")
        `("ul.list button.close"
          :position "absolute"
          :z-index "1"
          :top "13px"
          :right "17px"
          :display "flex"
          :align-items "center"
          :justify-content "center"
          :box-sizing "border-box"
          :width "28px"
          :height "28px"
          :margin "0"
          :padding "0"
          :border "none"
          :border-radius "14px"
          :appearance "none"
          :background-color "transparent"
          :color ,theme:primary-color
          :font-size "17px"
          :line-height "17px"
          :cursor "pointer"
          :opacity "0"
          :transition "opacity 120ms ease, background-color 120ms ease")
        '("ul.list > li:hover > button.close, ul.list button.close:focus-visible"
          :opacity "1")
        `("ul.list button.close:hover"
          :background-color ,theme:background-color
          :color ,theme:on-background-color)
        `("ul.list button.close:focus-visible"
          :outline "2px solid"
          :outline-color ,theme:action-color
          :outline-offset "2px"))
      (:h1 "Web extensions")
      (:p :class "lead"
          "Point at a directory, a .zip, or a .crx, or install the page you are viewing when that page is the package. "
          "New extensions load on the next start.")
      (:div :class "actions"
            (:button :class "install"
                     :type "button"
                     :title "Choose a directory, a .zip, or a .crx."
                     :onclick (ps:ps
                                (nyxt/ps:lisp-eval
                                 (:title "Install web extension"
                                  :buffer buffer)
                                 (install-web-extension)))
                     "Install"))
      (if extensions
          (:ul :class "list"
               (dolist (extension extensions)
                 (web-extension-row extension buffer)))
          (:p :class "lead" "No web extensions are installed.")))))

(define-internal-page-command-global list-web-extensions ()
    (buffer "Web extensions")
  "Install, open, or remove Chromium web extensions."
  (web-extensions-html buffer))
