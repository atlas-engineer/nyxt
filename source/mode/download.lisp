;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/download
  (:documentation "Package for `download-mode', mode to manage downloads and the downloads page."))
(in-package :nyxt/mode/download)

(export-always 'renderer-download)
(defclass renderer-download ()
  ()
  (:metaclass interface-class)
  (:documentation "A basis for renderer-specific download objects.
Should be redefined by the renderer."))

(define-class download (renderer-download)
  ((url
    (error "URL required.")
    :documentation "A string representation of the URL.")
   (status
    :unloaded
    :export t
    :reader status
    :type (member :unloaded :loading :finished :failed :canceled)
    :documentation "Status of the download.")
   (status-text
    (make-instance 'user-interface:paragraph)
    :export nil)
   (completion-percentage
    0.0
    :reader t
    :export t
    :type float
    :documentation "The download complete percentage.")
   (bytes-downloaded
    "-"
    :reader t
    :export t
    :documentation "The number of bytes downloaded.")
   (bytes-text
    (make-instance 'user-interface:paragraph)
    :export nil
    :documentation "The interface element that shows `bytes-downloaded'.")
   (destination-path
    #p""
    :reader t
    :export t
    :type pathname
    :documentation "The path where the download is written to.")
   (before-download-hook
    (make-instance 'hook-download)
    :type hook-download
    :documentation "Hook run before downloading a file.
The handlers take the `download' instance as argument.

Example: echo the file name to be downloaded

\(define-configuration nyxt/mode/download:download
  ((nyxt/mode/download:before-download-hook
    (hooks:add-hook %slot-value%
                    (make-instance 'hooks:handler
                                   :fn (lambda (download)
                                         (echo \"The URL for the download is ~a\" (render-url (url download))))
                                   :name 'echo-name)))))")
   (after-download-hook
    (make-instance 'hook-download)
    :type hook-download
    :documentation "Hook run after a download has completed.
The handlers take the `download' instance as argument.

Example: open the loaded files with XDG-open
\(define-configuration nyxt/mode/download:download
  ((nyxt/mode/download:after-download-hook
    (hooks:add-hook
     %slot-value%
     (make-instance 'hooks:handler
                    :fn (lambda (download)
                          (uiop:launch-program
                           (list \"xdg-open\" (uiop:native-namestring
                                             (nyxt/mode/download:destination-path download)))))
                    :name 'xdg-open-download)))))")
   (cancel-function
    nil
    :reader t
    :export t
    :type (or null function)
    :documentation "The function that cancels the download.
It can be set by the download engine.")
   (cancel-button
    (make-instance 'user-interface:button
                   :text "✕ Cancel"
                   :action (ps:ps (nyxt/ps:lisp-eval
                                   () (echo "Can't cancel download."))))
    :export nil
    :documentation "The interface element to cancel the download.
The download is referenced by its URL. The URL for this button is therefore
encoded as a funcall to cancel-download with an argument of the URL to cancel.")
   (open-button
    (make-instance 'user-interface:button
                   :text "🗁 Open"
                   :action (ps:ps (nyxt/ps:lisp-eval
                                   () (echo "Can't open file, file path unknown."))))
    :export nil
    :documentation "The interface element to open the download.
The file name to open is encoded within the button's URL when the destination
path is set.")
   (progress-text
    (make-instance 'user-interface:paragraph)
    :export nil)
   (progress
    (make-instance 'user-interface:progress-bar)
    :export nil))
  (:metaclass user-class)
  (:export-accessor-names-p t)
  (:export-class-name-p t)
  (:documentation "This class is used to represent a download within the *Downloads* buffer.
The `downloads' slot is populated by a list of these objects."))

(hooks:define-hook-type download (function (download))
  "Hook acting on `download' objects.")

(-> cancel-download (nyxt::url-designator) t)
(defun cancel-download (url)
  "This function is called by the cancel-button with an argument of the URL.

It will search the URLs of all the existing downloads, if it finds it, it will
invoke its cancel-function."
  (alex:when-let ((download (find url (downloads *browser*) :key  #'url :test #'string=)))
    (funcall (cancel-function download))
    (echo "Download canceled: ~a." url)))

(defmethod (setf cancel-function) (cancel-function (download download))
  (setf (slot-value download 'cancel-function) cancel-function)
  (setf (user-interface:action (cancel-button download))
        (ps:ps (nyxt/ps:lisp-eval (:title "cancel-download") (cancel-download (url download))))))

(defmethod (setf status) (value (download download))
  (setf (slot-value download 'status) value)
  (setf (user-interface:text (status-text download))
        (format nil "Status: ~(~a~)." value)))

(defmethod (setf completion-percentage) (percentage (download download))
  (setf (slot-value download 'completion-percentage) percentage)
  (setf (user-interface:percentage (progress download))
        (completion-percentage download))
  (setf (user-interface:text (progress-text download))
        (format nil "Completion: ~,2f%" (completion-percentage download))))

(defmethod (setf bytes-downloaded) (bytes (download download))
  (setf (slot-value download 'bytes-downloaded) bytes)
  (setf (user-interface:text (bytes-text download))
        (format nil "Bytes downloaded: ~a" (bytes-downloaded download))))

(defmethod (setf destination-path) (path (download download))
  (setf (slot-value download 'destination-path) path)
  (setf (user-interface:action (open-button download))
        (ps:ps (nyxt/ps:lisp-eval (:title "open-file") (nyxt/mode/file-manager:default-open-file-function path)))))

(defmethod connect ((download download) buffer)
  "Connect the user-interface objects within the download to the
buffer. This allows the user-interface objects to update their
appearance in the buffer when they are setf'd."
  (user-interface:connect (status-text download) buffer)
  (user-interface:connect (progress-text download) buffer)
  (user-interface:connect (bytes-text download) buffer)
  (user-interface:connect (open-button download) buffer)
  (user-interface:connect (cancel-button download) buffer)
  (user-interface:connect (progress download) buffer))

(define-mode download-mode ()
  "Display list of downloads."
  ((style
    (theme:themed-css (theme *browser*)
      `(".download"
        :background-color ,theme:background
        :color ,theme:on-background
        :margin-top "10px"
        :padding-left "5px"
        :brightness "80%"
        :border-radius "3px")
      `(".download-url"
        :overflow "auto"
        :white-space "nowrap")
      `(".download-url a"
        :color ,theme:on-background
        :font-size "small")
      `(".status p"
        :display "inline-block"
        :margin-right "10px")
      `(".progress-bar-container"
        :height "20px"
        :width "100%")
      `(".progress-bar-base"
        :background-color ,theme:secondary
        :height "100%")
      `(".progress-bar-fill"
        :background-color ,theme:success
        :height "100%"))))
  (:toggler-command-p nil))

(define-internal-page-command-global list-downloads ()
    (buffer "*Downloads*" 'download-mode)
  "Display a buffer listing all downloads.
We iterate through the browser's downloads to draw every single
download."
  (spinneret:with-html-string
    (:nstyle (style (find-submode 'download-mode)))
    (render-menu 'download-mode buffer)
    (:h1 "Downloads")
    (:hr)
    (:div
     (loop for download in (downloads *browser*)
           for url = (url download)
           for status-text = (status-text download)
           for progress-text = (progress-text download)
           for bytes-text = (bytes-text download)
           for progress = (progress download)
           for open-button = (open-button download)
           for cancel-button = (cancel-button download)
           do (connect download buffer)
           collect
           (:div :class "download"
                 (when (member (status download) '(:unloaded :loading))
                   (:raw (user-interface:to-html-string cancel-button)))
                 (when (eq (status download) :finished)
                   (:raw (user-interface:to-html-string open-button)))
                 (:p :class "download-url" (:a :href url url))
                 (:div :class "progress-bar-container"
                       (:raw (user-interface:to-html-string progress)))
                 (:div :class "status"
                       (:raw (user-interface:to-html-string progress-text))
                       (:raw (user-interface:to-html-string bytes-text))
                       (:raw (user-interface:to-html-string status-text))))))))

(defun download-watch (download-render download-object)
  "Update the *Downloads* buffer.
This function is meant to be run in the background. There is a potential thread
starvation issue if one thread consumes all messages. If in practice this
becomes a problem, we should poll on each thread until the completion percentage
is 100 OR a timeout is reached (during which no new progress has been made)."
  (when download-manager:*notifications*
    (loop for d = (calispel:? download-manager:*notifications*)
          while d
          when (download-manager:finished-p d)
            do (hooks:run-hook (after-download-hook *browser*) download-render)
          do (sleep 0.1) ; avoid excessive polling
             (setf (bytes-downloaded download-render)
                   (download-manager:bytes-fetched download-object))
             (setf (completion-percentage download-render)
                   (* 100 (/ (download-manager:bytes-fetched download-object)
                             (max 1 (download-manager:bytes-total download-object))))))))

;; TODO: To download any URL at any moment and not just in resource-query, we
;; need to query the cookies for URL.  Thus we need to add an IPC endpoint to
;; query cookies.
(export-always 'download)
(defmethod download ((buffer buffer) url &key cookies (proxy-url :auto))
  "Download URL in BUFFER.
When PROXY-URL is :AUTO (the default), the proxy address is guessed from the
current buffer.
Rely on `download-engine' of the BUFFER.
Return the `download' object matching the download."
  (prog1
      (match (download-engine buffer)
        (:lisp
         (alex:when-let* ((path (download-directory buffer))
                          (download-dir (files:expand path)))
           (when (eq proxy-url :auto)
             (setf proxy-url (nyxt::proxy-url buffer :downloads-only t)))
           (let* ((download nil))
             (with-protect ("Download error: ~a" :condition)
               (files:with-file-content (downloads path)
                 (setf download
                       (download-manager:resolve url
                                                 :directory download-dir
                                                 :cookies cookies
                                                 :proxy proxy-url))
                 (push download downloads)
                 ;; Add a watcher / renderer for monitoring download
                 (let ((download-render (make-instance 'download :url (render-url url))))
                   (hooks:run-hook (before-download-hook download) download)
                   (setf (destination-path download-render)
                         (uiop:ensure-pathname
                          (download-manager:filename download)))
                   (push download-render (downloads *browser*))
                   (run-thread
                     "download watcher"
                     (download-watch download-render download)))
                 download)))))
        (:renderer
         (ffi-buffer-download buffer (render-url url))))
    (list-downloads)))

(define-command-global download-url ()
  "Download the page or file of the current buffer."
  (download (current-buffer) (url (current-buffer))))

(define-command download-hint-url ()
  "Prompt for element hints and download them."
  (let ((buffer (current-buffer)))
    (nyxt/mode/hint:query-hints
     "Download link URL"
     (lambda (selected-links)
       (loop for link in selected-links
             ;; TODO: sleep should NOT be necessary to avoid breaking download
             do (nyxt/mode/download:download buffer (url link))
                (sleep 0.25)))
     :selector "a")))
