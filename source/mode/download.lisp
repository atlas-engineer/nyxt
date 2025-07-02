;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/download
  (:documentation "Package for `download-mode', mode to manage downloads and the
downloads page."))
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
                           (echo \"The URL for the download is ~a\"
                             (render-url (url download))))
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
     (make-instance
       'hooks:handler
       :fn (lambda (download)
             (uiop:launch-program
              (list \"xdg-open\"
                 (uiop:native-namestring
                   (nyxt/mode/download:destination-path download)))))
       :name 'xdg-open-download)))))")
   (cancel-function
    nil
    :export t
    :type (or null function)
    :documentation "The function that cancels the download.
It can be set by the download engine.")
   (progress-text
    (make-instance 'user-interface:paragraph)
    :export nil)
   (progress
    (make-instance 'user-interface:progress-bar)
    :export nil))
  (:metaclass user-class)
  (:export-accessor-names-p t)
  (:export-class-name-p t)
  (:documentation "This class is used to represent a download.
The `downloads' slot is populated by a list of these objects."))

(hooks:define-hook-type download (function (download))
  "Hook acting on `download' objects.")

(defmethod initialize-instance :after ((download download)
                                       &key &allow-other-keys)
  (hooks:run-hook (before-download-hook download) download)
  (list-downloads))

(defmethod cancel-download ((download download))
  "Call `cancel-function' with URL as argument."
  (funcall (cancel-function download))
  (echo "Download canceled: ~a." (url download))
  (buffer-load-internal-page-focus 'list-downloads))

(defmethod (setf status) (value (download download))
  (setf (slot-value download 'status) value)
  (setf (user-interface:text (status-text download))
        (format nil "Status: ~(~a~)." value))
  ;; Refresh downloads page and show it upon download completion.
  (when (eq value :finished)
    (buffer-load-internal-page-focus 'list-downloads)
    (hooks:run-hook (after-download-hook download) download)))

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

(defmethod connect ((download download) buffer)
  "Connect the user-interface objects within the download to the
buffer. This allows the user-interface objects to update their
appearance in the buffer when they are setf'd."
  (user-interface:connect (status-text download) buffer)
  (user-interface:connect (progress-text download) buffer)
  (user-interface:connect (bytes-text download) buffer)
  (user-interface:connect (progress download) buffer))

(define-mode download-mode ()
  "Display list of downloads."
  ((style
    (theme:themed-css (theme *browser*)
      `(".download"
        :background-color ,theme:background-color
        :color ,theme:on-background-color
        :margin-top "10px"
        :padding-left "5px"
        :brightness "80%"
        :border-radius "2px")
      '(".download-url"
        :overflow "auto"
        :white-space "nowrap")
      `(".download-url a"
        :color ,theme:on-background-color
        :font-size "small")
      '(".status p"
        :display "inline-block"
        :margin-right "10px")
      '(".progress-bar-container"
        :height "20px"
        :width "100%")
      `(".progress-bar-base"
        :background-color ,theme:secondary-color
        :height "100%")
      `(".progress-bar-fill"
        :background-color ,theme:success-color
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
     (or
      (loop for download in (downloads *browser*)
            for url = (url download)
            for status-text = (status-text download)
            for progress-text = (progress-text download)
            for bytes-text = (bytes-text download)
            for progress = (progress download)
            for destination-path = (destination-path download)
            do (connect download buffer)
            collect
            (:div :class "download"
                  (when (member (status download) '(:unloaded :loading))
                    (:nbutton
                      :text "✕ Cancel Download"
                      :title "Cancel Download"
                      `(cancel-download ,download)))
                  (when (eq (status download) :finished)
                    (:nbutton
                      :text "Open File"
                      :title "Open File"
                      `(nyxt/mode/file-manager:default-open-file-function
                        ,destination-path)))
                  (:p :class "download-url" (:a :href url url))
                  (:div :class "progress-bar-container"
                        (user-interface:to-html progress))
                  (:div :class "status"
                        (user-interface:to-html progress-text)
                        (user-interface:to-html bytes-text)
                        (user-interface:to-html status-text))))
      (:p "No downloads available.")))))

(define-command-global download-url ()
  "Download the page or file of the current buffer."
  (ffi-buffer-download (current-buffer) (render-url (url (current-buffer)))))

(define-command-global download-hint-url ()
  "Prompt for element hints and download them."
  (let ((buffer (current-buffer)))
    (nyxt/mode/hint:query-hints
     "Download link URL"
     (lambda (selected-links)
       (loop for link in selected-links
             do (ffi-buffer-download buffer (render-url (url link)))))
     :selector "a")))
