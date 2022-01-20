(in-package :nyxt/web-extensions)

(-> extension->extension-info ((or null nyxt/web-extensions:extension)) (values list &optional))
(defun extension->extension-info (extension)
  (when extension
    `(("description" . ,(or (nyxt/web-extensions:description extension) ""))
      ("homepageUrl" . ,(or (nyxt/web-extensions:homepage-url extension) ""))
      ("id" . ,(id extension))
      ("installType" . "development")
      ("mayDisable" . t)
      ("name" . ,(or (name extension) ""))
      ("permissions" . ,(nyxt/web-extensions:permissions extension))
      ("version" ,(or (nyxt/web-extensions:version extension) ""))
      ;; TODO: Make those meaningful
      ("disabledReason" . "unknown")
      ("enabled" . t)
      ("hostPermissions" . ,(vector))
      ("icons" . ,(vector))
      ("offlineEnabled" . nil)
      ("optionsUrl" . "")
      ("shortName" . ,(or (name extension) ""))
      ("type" . "extension")
      ("updateUrl" . "")
      ("versionName" . ""))))

(-> buffer->tab-description ((or null buffer)) (values list &optional))
(defun buffer->tab-description (buffer)
  (when buffer
    `(("active" . ,(if (member buffer (mapcar #'nyxt::active-buffer
                                              (alex:hash-table-values (nyxt::windows *browser*))))
                       t nil))
      #+webkit2-mute
      ("audible" . ,(not (webkit:webkit-web-view-is-muted (nyxt::gtk-object buffer))))
      ("height" . ,(gdk:gdk-rectangle-height
                    (gtk:gtk-widget-get-allocation (nyxt::gtk-object buffer))))
      ("width" . ,(gdk:gdk-rectangle-width
                   (gtk:gtk-widget-get-allocation (nyxt::gtk-object buffer))))
      ("highlighted" . ,(eq buffer (nyxt::active-buffer (current-window))))
      ("id" . ,(or (parse-integer (id buffer) :junk-allowed t) 0))
      ("incognito" . ,(nosave-buffer-p buffer))
      ("lastAccessed" . ,(* 1000 (local-time:timestamp-to-unix (nyxt::last-access buffer))))
      ("selected" . ,(eq buffer (nyxt::active-buffer (current-window))))
      ("status" . ,(if (web-buffer-p buffer)
                       (case (slot-value buffer 'nyxt::status)
                         ((:finished :failed) "complete")
                         ((:unloaded :loading) "loading"))
                       "complete"))
      ;; TODO: Check "tabs" permission for those two
      ("title" . ,(title buffer))
      ("url" . ,(render-url (url buffer)))

      ;; TODO: Make those meaningful:
      ("attention" . nil)
      ("autoDiscardable" . nil)
      ("cookieStoreId" . 0)
      ("currentWindow" . t)
      ("discarded" . nil)
      ("hidden" . nil)
      ("favIconUrl" . "")
      ("index" . 0)
      ("isArticle" . nil)
      ("isInReaderMode" . nil)
      ("mutedInfo" . nil)
      ("openerTabId" . 0)
      ("pinned" . nil)
      ("sessionId" . 0)
      ("successorTabId" . 0)
      ("windowId" . 0))))

(defun all-extensions (&key (buffers (buffer-list)))
  (loop for buffer in buffers
        append (sera:filter #'nyxt/web-extensions::extension-p (modes buffer))))

(defmacro fire-extension-event (extension object event &rest args)
  (alex:once-only (extension)
    `(if (ffi-buffer-evaluate-javascript
          (buffer ,extension)
          (ps:ps (ps:instanceof (ps:chain browser ,object ,event) *Object))
          (name ,extension))
         (ffi-buffer-evaluate-javascript
          (buffer ,extension)
          (ps:ps (ps:chain browser ,object ,event
                           (run ,@args)))
          (name ,extension))
         (log:debug "Event not injected: ~a" (ps:ps (ps:@ ,object ,event))))))

(defmethod tabs-on-activated ((old-buffer buffer) (new-buffer buffer))
  (flet ((integer-id (object)
           (or (ignore-errors (parse-integer (id object)))
               0)))
    (dolist (extension (all-extensions))
      (fire-extension-event
       extension tabs on-activated
       (ps:create previous-tab-id (ps:lisp (integer-id old-buffer))
                  tab-id (ps:lisp (integer-id new-buffer))
                  window-id (ps:lisp (integer-id (current-window)))))
      ;; tabs.onActiveChanged is deprecated. No harm in having it, though.
      (fire-extension-event
       extension tabs on-active-changed
       (ps:lisp (integer-id new-buffer))
       ;; FIXME: Any way to get the window buffer belongs to?
       (ps:create window-id (ps:lisp (integer-id (current-window))))))))

(defmethod tabs-on-created ((buffer buffer))
  (dolist (extension (all-extensions))
    (fire-extension-event
     extension tabs on-created
     ;; buffer->tab-description returns the representation that Parenscript has
     ;; trouble encoding, thus this JSON parsing hack.
     (ps:chain *j-s-o-n (parse (ps:lisp (encode-json (buffer->tab-description (buffer extension)))))))))

(defmethod tabs-on-updated ((buffer buffer) properties)
  "Invoke the browser.tabs.onUpdated event with PROPERTIES being an alist of BUFFER changes."
  (dolist (extension (all-extensions))
    (fire-extension-event
     extension tabs on-updated
     (ps:lisp (parse-integer (id buffer)))
     (ps:chain *j-s-o-n (parse (ps:lisp (encode-json properties))))
     ;; buffer->tab-description returns the representation that Parenscript has
     ;; trouble encoding, thus this JSON parsing hack.
     (ps:chain *j-s-o-n (parse (ps:lisp (encode-json (buffer->tab-description (buffer extension)))))))))

(defmethod tabs-on-removed ((buffer buffer))
  (flet ((integer-id (object)
           (or (ignore-errors (parse-integer (id object)))
               0)))
    (dolist (extension (all-extensions))
      (fire-extension-event
       extension tabs on-removed
       (ps:lisp (integer-id buffer))
       (ps:create window-id (integer-id (current-window))
                  ;; FIXME: How do we know if it's closing?
                  is-window-closing false)))))

(-> tabs-query ((or null string)) (values string &optional))
(defun tabs-query (query-object)
  (flet ((%tabs-query (query-object)
           (let ((buffer-descriptions (mapcar #'buffer->tab-description (buffer-list))))
             (if query-object
                 (or (sera:filter (lambda (bd)
                                    (every #'identity
                                           (sera:maphash-return
                                            (lambda (key value)
                                              (equal value (str:s-assoc-value bd key)))
                                            query-object)))
                                  buffer-descriptions)
                     ;; nil translates to null, we need to pass empty vector instead.
                     (vector))
                 buffer-descriptions))))
    (encode-json
     (%tabs-query (decode-json (or query-object "{}"))))))

(-> tabs-create ((or null string)) (values string &optional))
(defun tabs-create (create-properties)
  (let* ((properties (decode-json (or create-properties "{}")))
         (parent-buffer (when (gethash "openerTabId" properties)
                          (nyxt::buffers-get
                           (format nil "~d" (gethash "openerTabId" properties)))))
         (url (quri:uri (or (gethash "url" properties)
                            "about:blank")))
         ;; See https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/API/tabs/create
         (url (if (str:s-member '("chrome" "javascript" "data" "file") (quri:uri-scheme url))
                  (quri:uri "about:blank")
                  url))
         (buffer (make-buffer :url url
                              :title (or (gethash "title" properties) "")
                              :load-url-p (gethash "discarded" properties)
                              :parent-buffer parent-buffer)))
    (when (or (gethash "active" properties)
              (gethash "selected" properties))
      (set-current-buffer buffer))
    (encode-json (buffer->tab-description buffer))))

(defvar %message-channels% (make-hash-table)
  "A hash-table mapping message pointer addresses to the channels they return values from.

Introduced to communicate `process-user-message' and `reply-user-message'
running on separate threads. These run on separate threads because we need to
free GTK main thread to allow JS callbacks to run freely.")

(-> trigger-message (t buffer nyxt/web-extensions:extension webkit:webkit-user-message) string)
(defun trigger-message (message buffer extension original-message)
  "Send a MESSAGE to the WebKitWebPage associated with BUFFER and wait for the result.

Respond to ORIGINAL-MESSAGE once there's a result.

See `%message-channels%',`process-user-message', and `reply-user-message' for
the description of the mechanism that sends the results back."
  (let ((result-channel (nyxt::make-channel 1)))
    (run-thread
      "Send the message"
      (flet ((send-message (channel)
               (webkit:webkit-web-view-send-message-to-page
                (nyxt::gtk-object buffer)
                (webkit:webkit-user-message-new
                 "message"
                 (glib:g-variant-new-string
                  (encode-json `(("message" . ,message)
                                 ("sender" . (("tab" . ,(buffer->tab-description buffer))
                                              ("url" . ,(render-url (url buffer)))
                                              ("tlsChannelId" . "")
                                              ("frameId" . 0)
                                              ("id" . "")))
                                 ("extensionName" . ,(name extension))))))
                (lambda (reply)
                  (calispel:! channel (webkit:g-variant-get-maybe-string
                                       (webkit:webkit-user-message-get-parameters reply))))
                (lambda (condition)
                  (echo-warning "Message error: ~a" condition)
                  ;; Notify the listener that we are done.
                  (calispel:! channel nil)))))
        (if (not (member (slot-value buffer 'nyxt::status) '(:finished :failed)))
            (let ((channel (nyxt::make-channel 1)))
              (hooks:add-hook (buffer-loaded-hook buffer)
                              (make-instance
                               'nyxt::handler-buffer
                               :fn (lambda (buffer)
                                     (calispel:! channel (send-message result-channel))
                                     (hooks:remove-hook (buffer-loaded-hook buffer) 'send-message-when-loaded))
                               :name 'send-message-when-loaded))
              (calispel:? channel))
            (send-message result-channel))))
    (setf (gethash (cffi:pointer-address (g:pointer original-message)) %message-channels%)
          result-channel))
  "")

(defvar %style-sheets% (make-hash-table :test #'equal)
  "WebKitUserStyleSheet-s indexed by the JSON describing them.")

(-> tabs-insert-css (buffer string) string)
(defun tabs-insert-css (buffer message-params)
  (let* ((json (decode-json message-params))
         (css-data (gethash "css" json))
         (code (gethash "code" css-data))
         (file (gethash "file" css-data))
         (level (gethash "cssOrigin" css-data))
         (tab-id (gethash "tabId" json))
         (extension (find (gethash "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (buffer-to-insert (if (zerop tab-id)
                               (current-buffer)
                               (or (find (format nil "~d" tab-id) (buffer-list) :key #'id)
                                   (current-buffer))))
         (style-sheet (when (nyxt/web-extensions:tab-apis-enabled-p extension buffer-to-insert)
                        (ffi-buffer-add-user-style
                         buffer-to-insert
                         (if file
                             (uiop:read-file-string (uiop:merge-pathnames*
                                                     file (nyxt/web-extensions:extension-directory
                                                           extension)))
                             code)
                         :inject-as-author-p (not (and level (stringp level) (string= level "user")))
                         :all-frames-p (gethash "allFrames" css-data)
                         :world-name (name extension)))))
    (when style-sheet
      (setf (gethash message-params %style-sheets%)
            style-sheet))
    "null"))

(-> tabs-remove-css (string) string)
(defun tabs-remove-css (message-params)
  (let* ((json (decode-json message-params))
         (tab-id (gethash "tabId" json))
         (buffer-to-remove (if (zerop tab-id)
                               (current-buffer)
                               (or (find (format nil "~d" tab-id) (buffer-list) :key #'id)
                                   (current-buffer))))
         (style-sheet (gethash message-params %style-sheets%)))
    (ffi-buffer-remove-user-style buffer-to-remove style-sheet)
    (remhash message-params %style-sheets%)
    "null"))

(-> tabs-execute-script (buffer string) string)
(defun tabs-execute-script (buffer message-params)
  (let* ((json (decode-json message-params))
         (script-data (gethash "script" json))
         (code (gethash "code" script-data))
         (file (gethash "file" script-data))
         (tab-id (gethash "tabId" json))
         (buffer-to-insert (if (zerop tab-id)
                               (current-buffer)
                               (or (find (format nil "~d" tab-id) (buffer-list) :key #'id)
                                   (current-buffer))))
         (extension (find (gethash "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal)))
    (when (nyxt/web-extensions:tab-apis-enabled-p extension buffer-to-insert)
      (ffi-buffer-add-user-script
       buffer-to-insert (if file
                            (uiop:read-file-string
                             (nyxt/web-extensions:merge-extension-path extension file))
                            code)
       :run-now-p t
       :at-document-start-p (and (gethash "runAt" script-data)
                                 (string= (gethash "runAt" script-data) "document_start"))
       :all-frames-p (gethash "allFrames" script-data)
       :world-name (name extension)))
    "[]"))

(-> storage-local-get (buffer string) (values string &optional))
(defun storage-local-get (buffer message-params)
  (let* ((json (decode-json message-params))
         (extension (find (gethash "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (keys (gethash "keys" json)))
    (with-data-unsafe (data (nyxt/web-extensions:storage-path extension)
                       :default (make-hash-table))
      (if (uiop:emptyp keys)
          "{}"
          (encode-json
           (typecase keys
             (null data)
             (list (mapcar (lambda (key-value)
                             (let ((key-value (uiop:ensure-list key-value))
                                   (value (or (gethash (first key-value) data)
                                              (rest key-value))))
                               (when value
                                 (cons (first key-value) value))))
                           keys))
             (string (or (gethash keys data)
                         (vector)))))))))

(-> storage-local-set (buffer string) string)
(defun storage-local-set (buffer message-params)
  (let* ((json (decode-json message-params))
         (extension (find (gethash "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (keys (gethash "keys" json)))
    (with-data-access (data (nyxt/web-extensions:storage-path extension)
                       :default (make-hash-table))
      (unless (uiop:emptyp keys)
        (dolist (key-value keys)
          (setf (gethash (first key-value) data)
                (rest key-value))))))
  "")

(-> storage-local-remove (buffer string) string)
(defun storage-local-remove (buffer message-params)
  (let* ((json (decode-json message-params))
         (extension (find (gethash "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (keys (uiop:ensure-list (gethash "keys" json))))
    (with-data-access (data (nyxt/web-extensions:storage-path extension)
                       :default (make-hash-table))
      (unless (uiop:emptyp keys)
        (dolist (key keys)
          (remhash key data)))))
  "")

(-> storage-local-clear (buffer string) string)
(defun storage-local-clear (buffer message-params)
  (let* ((extension (find message-params
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id)))
    (with-data-access (data (nyxt/web-extensions:storage-path extension)
                       :default (make-hash-table))
      (clrhash data)))
  "")

(export-always 'process-user-message)
(-> process-user-message (buffer webkit:webkit-user-message) t)
(defun process-user-message (buffer message)
  "A dispatcher for all the possible WebKitUserMessage types there can be.
Uses name of the MESSAGE as the type to dispatch on.

Creates a result channel for almost every message type (with the exception of
those using `trigger-message') and sends the response of the helper function
there. `reply-user-mesage' takes care of sending the response back."
  (let* ((message-name (webkit:webkit-user-message-get-name message))
         (message-params (webkit:g-variant-get-maybe-string
                          (webkit:webkit-user-message-get-parameters message)))
         (extensions (when buffer
                       (sera:filter #'nyxt/web-extensions::extension-p (modes buffer)))))
    (log:debug "Message ~a with ~s parameters received."
               message-name message-params)
    (flet ((wrap-in-channel (value)
             (let ((channel (nyxt::make-channel 1)))
               (setf (gethash (cffi:pointer-address (g:pointer message))
                              %message-channels%)
                     channel)
               (calispel:! channel value)))
           (cons* (se1 se2 &rest ignore)
             ;; This is useful when conditional reader macro reads in some
             ;; superfluous items.
             (declare (ignore ignore))
             (cons se1 se2)))
      (str:string-case message-name
        ("management.getSelf"
         (wrap-in-channel
          (encode-json (extension->extension-info (find message-params extensions
                                                        :key #'name :test #'string=)))))
        ("runtime.sendMessage"
         (sera:and-let* ((json (decode-json message-params))
                         (extension-instances
                          (sera:filter (alex:curry #'string=
                                                   (gethash "extensionId" json))
                                       extensions
                                       :key #'id))
                         (context (webkit:jsc-context-new)))
           ;; Store a pointer to the message and reply to it later!
           (if (or (background-buffer-p buffer)
                   (nyxt::panel-buffer-p buffer))
               (dolist (instance extension-instances)
                 (trigger-message (gethash "message" json)
                                  (buffer instance) instance message))
               (trigger-message (gethash "message" json)
                                (background-buffer (first extensions))
                                (first extensions)
                                message))))
        ("runtime.getPlatformInfo"
         (wrap-in-channel
          (encode-json
           (list
            ;; TODO: This begs for trivial-features.
            (cons* "os"
                   #+(or darwin macos macosx)
                   "mac"
                   #+bsd
                   "openbsd"
                   #+linux
                   "linux"
                   "")
            (cons* "arch"
                   #+X86-64
                   "x86-64"
                   #+(or X86 X86-32)
                   "x86-32"
                   "arm")))))
        ("runtime.getBrowserInfo"
         (wrap-in-channel
          (encode-json
           (let ((nyxt-version (str:split "-" nyxt:+version+)))
             `(("name" . "Nyxt")
               ("vendor" . "Atlas Engineer LLC")
               ("version" ,(first nyxt-version))
               ("build" ,(if (rest nyxt-version)
                             (third nyxt-version)
                             "")))))))
        ("storage.local.get"
         (wrap-in-channel (storage-local-get buffer message-params)))
        ("storage.local.set"
         (wrap-in-channel (storage-local-set buffer message-params)))
        ("storage.local.remove"
         (wrap-in-channel (storage-local-remove buffer message-params)))
        ("storage.local.clear"
         (wrap-in-channel (storage-local-clear buffer message-params)))
        ("tabs.query"
         (wrap-in-channel
          (tabs-query message-params)))
        ("tabs.create"
         (wrap-in-channel
          (tabs-create message-params)))
        ("tabs.getCurrent"
         (wrap-in-channel
          (encode-json (buffer->tab-description buffer))))
        ("tabs.print"
         (wrap-in-channel (print-buffer)))
        ("tabs.get"
         (wrap-in-channel
          (encode-json (buffer->tab-description (nyxt::buffers-get message-params)))))
        ("tabs.sendMessage"
         (let* ((json (decode-json message-params))
                (id (gethash "tabId" json))
                (buffer (if (zerop id)
                            (current-buffer)
                            (nyxt::buffers-get (format nil "~d" id))))
                (extension (find (gethash "extensionId" json)
                                 (sera:filter #'nyxt/web-extensions::extension-p
                                              (modes buffer))
                                 :key #'id
                                 :test #'string-equal)))
           (trigger-message (gethash "message" json) buffer extension message)))
        ("tabs.insertCSS"
         (wrap-in-channel
          (tabs-insert-css buffer message-params)))
        ("tabs.removeCSS"
         (wrap-in-channel
          (tabs-remove-css message-params)))
        ("tabs.executeScript"
         (wrap-in-channel
          (tabs-execute-script buffer message-params)))))))

(export-always 'reply-user-message)
(-> reply-user-message (buffer webkit:webkit-user-message) t)
(defun reply-user-message (buffer message)
  "Send the response to the MESSAGE received from the BUFFER-associated WebPage.
Wait on the channel associated to the MESSAGE until there's a result.
Time out and send an empty reply after 5 seconds of waiting."
  (declare (ignore buffer))
  (loop until (gethash (cffi:pointer-address (g:pointer message))
                       %message-channels%)
        finally (let* ((reply (calispel:? (gethash (cffi:pointer-address (g:pointer message))
                                                   %message-channels%)
                                          5))
                       (reply-message (webkit:webkit-user-message-new
                                       (webkit:webkit-user-message-get-name message)
                                       (if reply
                                           (glib:g-variant-new-string reply)
                                           (cffi:null-pointer)))))
                  (webkit:webkit-user-message-send-reply message reply-message))))
