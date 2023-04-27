;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

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
      ("audible" . ,(not (ffi-muted-p buffer)))
      ("height" . ,(ffi-height buffer))
      ("width" . ,(ffi-width buffer))
      ("highlighted" . ,(eq buffer (nyxt::active-buffer (current-window))))
      ("id" . ,(or (parse-integer (id buffer) :junk-allowed t) 0))
      ("incognito" . ,(nosave-buffer-p buffer))
      ("lastAccessed" . ,(* 1000 (time:timestamp-to-unix (nyxt::last-access buffer))))
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
        when (modable-buffer-p buffer)
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
     (ps:chain *j-s-o-n (parse (ps:lisp (j:encode (buffer->tab-description (buffer extension)))))))))

(defmethod tabs-on-updated ((buffer buffer) properties)
  "Invoke the browser.tabs.onUpdated event with PROPERTIES being an alist of BUFFER changes."
  (dolist (extension (all-extensions))
    (fire-extension-event
     extension tabs on-updated
     (ps:lisp (parse-integer (id buffer)))
     (ps:chain *j-s-o-n (parse (ps:lisp (j:encode properties))))
     ;; buffer->tab-description returns the representation that Parenscript has
     ;; trouble encoding, thus this JSON parsing hack.
     (ps:chain *j-s-o-n (parse (ps:lisp (j:encode (buffer->tab-description (buffer extension)))))))))

(defmethod tabs-on-removed ((buffer buffer))
  (flet ((integer-id (object)
           (or (ignore-errors (parse-integer (symbol-name (id object))))
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
    (j:encode (%tabs-query (j:decode (or query-object "{}"))))))

(-> tabs-create ((or null string)) (values string &optional))
(defun tabs-create (create-properties)
  (let* ((properties (j:decode (or create-properties "{}")))
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
    (j:encode (buffer->tab-description buffer))))

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
               (ffi-web-extension-send-message
                buffer
                (webkit:webkit-user-message-new
                 "message"
                 (glib:g-variant-new-string
                  (j:encode `(("message" . ,message)
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
              (hooks:once-on (buffer-loaded-hook buffer) _
                (calispel:! channel (send-message result-channel)))
              (calispel:? channel))
            (send-message result-channel))))
    (setf (gethash (cffi:pointer-address (g:pointer original-message)) %message-channels%)
          result-channel))
  "")

(defvar %style-sheets% (make-hash-table :test #'equal)
  "WebKitUserStyleSheet-s indexed by the JSON describing them.")

(-> tabs-insert-css (buffer string) string)
(defun tabs-insert-css (buffer message-params)
  (let* ((json (j:decode message-params))
         (css-data (j:get "css" json))
         (code (j:get "code" css-data))
         (file (j:get "file" css-data))
         (level (j:get "cssOrigin" css-data))
         (tab-id (j:get "tabId" json))
         (extension (find (j:get "extensionId" json)
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
                         (apply #'make-instance
                                'nyxt/mode/user-script:user-style
                                :level (if (not (and level (stringp level) (string= level "user")))
                                           :author
                                           :user)
                                :all-frames-p (gethash "allFrames" css-data)
                                :world-name (name extension)
                                (if file
                                    (list :base-path
                                          (uiop:merge-pathnames*
                                           file (nyxt/web-extensions:extension-directory
                                                 extension)))
                                    (list :code code)))))))
    (when style-sheet
      (setf (gethash message-params %style-sheets%)
            style-sheet))
    "null"))

(-> tabs-remove-css (string) string)
(defun tabs-remove-css (message-params)
  (let* ((json (j:decode message-params))
         (tab-id (j:get "tabId" json))
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
  (let* ((json (j:decode message-params))
         (script-data (j:get "script" json))
         (code (j:get "code" script-data))
         (file (j:get "file" script-data))
         (tab-id (j:get "tabId" json))
         (buffer-to-insert (if (zerop tab-id)
                               (current-buffer)
                               (or (find (format nil "~d" tab-id) (buffer-list) :key #'id)
                                   (current-buffer))))
         (extension (find (j:get "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal)))
    (when (nyxt/web-extensions:tab-apis-enabled-p extension buffer-to-insert)
      (ffi-buffer-add-user-script
       buffer-to-insert
       (make-instance
        'nyxt/mode/user-script:user-script
        :code (if file
                  (uiop:read-file-string
                   (nyxt/web-extensions:merge-extension-path extension file))
                  code)
        :run-at (if (and (gethash "runAt" script-data)
                         (string= (gethash "runAt" script-data) "document_start"))
                    :document-start
                    :document-end)
        :all-frames-p (gethash "allFrames" script-data)
        :world-name (name extension))))
    "[]"))

(-> storage-local-get (buffer string) (values string &optional))
(defun storage-local-get (buffer message-params)
  (let* ((json (j:decode message-params))
         (extension (find (j:get "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (keys (j:get "keys" json)))
    (let ((data (or (files:content (nyxt/web-extensions:storage-path extension))
                    (make-hash-table))))
      (if (uiop:emptyp keys)
          "{}"
          (j:encode
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
  (let* ((json (j:decode message-params))
         (extension (find (j:get "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (keys (j:get "keys" json)))
    (let ((data (or (files:content (nyxt/web-extensions:storage-path extension))
                    (make-hash-table))))
      (unless (uiop:emptyp keys)
        (dolist (key-value keys)
          (setf (gethash (first key-value) data)
                (rest key-value))))))
  "")

(-> storage-local-remove (buffer string) string)
(defun storage-local-remove (buffer message-params)
  (let* ((json (j:decode message-params))
         (extension (find (j:get "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (keys (uiop:ensure-list (j:get "keys" json))))
    (let ((data (or (files:content (nyxt/web-extensions:storage-path extension))
                    (make-hash-table))))
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
    (let ((data (or (files:content (nyxt/web-extensions:storage-path extension))
                    (make-hash-table))))
      (clrhash data)))
  "")

(export-always 'process-user-message)
(-> process-user-message (buffer webkit:webkit-user-message) t)
(defun process-user-message (buffer message)
  "A dispatcher for all the possible WebKitUserMessage types there can be.
Uses name of the MESSAGE as the type to dispatch on.

Creates a result channel for almost every message type (with the exception of
those using `trigger-message') and sends the response of the helper function
there. `reply-user-message' takes care of sending the response back."
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
             (cons se1 se2))
           ;;; Only used in "ready" message.
           ;; (extension->cons (extension)
           ;;   (cons (nyxt/web-extensions::name extension)
           ;;         (vector (id extension)
           ;;                 (nyxt/web-extensions::manifest extension)
           ;;                 (or (background-buffer-p (buffer extension))
           ;;                     (nyxt::panel-buffer-p (buffer extension)))
           ;;                 (nyxt/web-extensions::extension-files extension)
           ;;                 (id (buffer extension)))))
           )
      (str:string-case message-name
        ;;; Commented out due to CPU hogging when enabled.
        ;; ("ready"
        ;;  (ffi-web-extension-send-message
        ;;   buffer
        ;;   (webkit:webkit-user-message-new
        ;;    "injectAPIs" (glib:g-variant-new-string
        ;;                  (j:encode (mapcar #'extension->cons extensions))))))
        ("management.getSelf"
         (wrap-in-channel
          (j:encode (extension->extension-info (find message-params extensions
                                                     :key #'name :test #'string=)))))
        ("runtime.sendMessage"
         (sera:and-let* ((json (j:decode message-params))
                         (extension-instances
                          (sera:filter (curry #'string=
                                              (j:get "extensionId" json))
                                       extensions
                                       :key #'id))
                         (context (webkit:jsc-context-new)))
           ;; Store a pointer to the message and reply to it later!
           (if (or (background-buffer-p buffer)
                   (nyxt::panel-buffer-p buffer))
               (dolist (instance extension-instances)
                 (trigger-message (j:get "message" json)
                                  (buffer instance) instance message))
               (trigger-message (j:get "message" json)
                                (background-buffer (first extensions))
                                (first extensions)
                                message))))
        ("runtime.getPlatformInfo"
         (wrap-in-channel
          (j:encode
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
          (j:encode
           (multiple-value-bind (major _ patch)
               (nyxt::version)
             (declare (ignore _))
             `(("name" . "Nyxt")
               ("vendor" . "Atlas Engineer LLC")
               ("version" ,(or major ""))
               ("build" ,(or patch "")))))))
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
          (j:encode (buffer->tab-description buffer))))
        ("tabs.print"
         (wrap-in-channel (nyxt/mode/document:print-buffer)))
        ("tabs.get"
         (wrap-in-channel
          (j:encode (buffer->tab-description (nyxt::buffers-get message-params)))))
        ("tabs.sendMessage"
         (let* ((json (j:decode message-params))
                (id (j:get "tabId" json))
                (buffer (if (zerop id)
                            (current-buffer)
                            (nyxt::buffers-get (format nil "~d" id))))
                (extension (find (j:get "extensionId" json)
                                 (sera:filter #'nyxt/web-extensions::extension-p
                                              (modes buffer))
                                 :key #'id
                                 :test #'string-equal)))
           (trigger-message (j:get "message" json) buffer extension message)))
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
