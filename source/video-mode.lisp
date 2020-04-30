(uiop:define-package :next/video        ; TODO: Rename file to video.lisp since there is no mode?
    (:use :common-lisp :next)
  (:export :download
           :download-arguments
           :*download-program*
           :*download-args*
           :*download-function*
           :*preferred-download-directories*)
  (:documentation "Download videos on the Web.

Command defined in this package: `download-video'.

It tries by default to use the program `youtube-dl', that you must
have in your path.

You can also write a new function that takes a URL as parameter, and
bind it to `next/video:*download-function*'.  In doing so, you can
rely on the `next/video:download' function, that does error handling
and process management.

***********************************************************************
*Disclaimer*: this feature is meant to grow with Next 1.4 and onwards!
***********************************************************************

;XXX: show progress
;XXX: make it appear in the recent downloaded files
;XXX: ask any destination

What can be done: automatically install the required program, better
notifications, choose videos, etc.
"))

(in-package :next/video)

(declaim (type (or null string) *download-program*))
(defparameter *download-program* "youtube-dl"
  "The external program to download videos with. Defaults to youtube-dl.")

(declaim (type (or null list-of-strings) *download-args*))
(defparameter *download-args* nil
  "Default arguments for the download command as a list of strings. See also `download-arguments' which adds more.")

;; TODO: Make browser's download-directory a list.
(declaim (type (or null list) *preferred-download-directories*))
(defparameter *preferred-download-directories* (list (xdg-download-dir))
  "List of favorite directories to save videos to. If it contains more than one entry, we are asked for the destination.")


(defun download-arguments (url target-dir)
  "Return a list of arguments for the download command.

Appends `*download-args' and -o /target/directory/%(title)s.%(ext)s (for youtube-dl)."
  (declare (ignorable url))
  (append *download-args* (list "-o" (format nil "~a/%(title)s.%(ext)s" target-dir))))

(defun resolve-download-directory (target-dir)
  (or target-dir
      (first *preferred-download-directories*)
      download-manager::*default-download-directory*))

(defun download-command (url &optional target-dir)
  "Return a list of strings composing the full download command, ready to feed to uiop:launch-program."
  (let ((target-dir (resolve-download-directory target-dir)))
    (setf target-dir (string-right-trim (list #\/) (namestring target-dir)))
    (append (list *download-program*
                  url)
            (download-arguments url target-dir))))

(defun download (url &optional target-dir)
  "Download asynchronously and notify on success or failure."
  (handler-case
      (progn
        (unless target-dir
          (setf target-dir (resolve-download-directory target-dir)))
        (echo "Starting download of ~a to ~a." url target-dir)
        (log:info "Starting download of ~a to ~a." url target-dir)
        ;XXX notify progress.
        (launch-and-notify (download-command url target-dir)
                                :success-msg (format nil "Video downloaded to ~a." target-dir)
                                :error-msg (format nil "Failed to download video.~&")))
    (error (c)
      (log:warn "Error downloading ~a to ~a: ~a" url target-dir c))))

(defparameter *download-function* #'download
  "Default download function.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :next)

(define-command download-video ()
  "Download the video of the current URL with an external program."
  (let* ((url (url (current-buffer))))
    (cond
      ((null next/video::*preferred-download-directories*)
       ;XXX: ask destination.
       (funcall next/video:*download-function* url))
      ((= 1 (length next/video::*preferred-download-directories*))
       (funcall next/video:*download-function* url (first next/video::*preferred-download-directories*)))
      (t
       (with-result (target-dir (read-from-minibuffer
                                 (make-minibuffer
                                  :input-prompt "Target directory"
                                  :completion-function
                                  (lambda (input)
                                    (file-completion-function input next/video::*preferred-download-directories*)))))
         (funcall next/video:*download-function* url target-dir))))))
