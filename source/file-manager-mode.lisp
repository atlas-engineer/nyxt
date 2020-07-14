(uiop:define-package :nyxt/file-manager-mode
  (:use :common-lisp :trivia :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Manage files.

Open any file from within Nyxt, with the usual fuzzy suggestion.

`M-x open-file (C-x C-f)'

\"file manager\" is a bit excessive for now. Currently, we can:
- browse files, with fuzzy-suggestion
- go one directory up (C-l)
- enter a directory (C-j)
- open files. By default, with xdg-open. See `open-file-function'.
"))
(in-package :nyxt/file-manager-mode)
(trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)

;;; ***********************************************************************
;;; *Disclaimer*: this feature is meant to grow with Next 1.4 and onwards!
;;; ***********************************************************************
;;;
;;; Much can be done:
;;; - configuration options to choose what to open with
;;; - more configuration in general
;;; - sort by last access, etc
;;; - multi-selection
;;; - bookmarks
;;; - open files in Nyxt
;;; - a UI to list files
;;; - lazy loading for large directories
;;; - many things...

(defparameter *open-file-in-new-buffer* t
  "If nil, don't open files and directories in a new buffer.")

(defparameter *supported-media-types* '("mp3" "ogg" "mp4" "flv" "wmv" "webm" "mkv")
  ;XXX: either trully read supported MIME types, either try to open
  ;all, either make this list a different purpose (for a user selection).
  "Supported media types.")

(defun supported-media (filename)
  "Return T if this filename's extension is a media that Nyxt can open.
See `*supported-media-types*'."
  (when (uiop:file-pathname-p filename)
    (let ((extension (str:downcase (pathname-type filename))))
      (find extension *supported-media-types* :test #'string-equal))))

;; TODO: By default, we should open directories in a new buffer (see NEW-BUFFER-P), and open supported media in a new buffer (see `supported-media').
(serapeum:export-always 'open-file-function)
(defun open-file-function (filename &key ;; (new-buffer-p *open-file-in-new-buffer*)
                                      )
  "Open FILENAME in Nyxt if supported, or externally otherwise.
FILENAME is the full path of the file (or directory).

See `*supported-media-types*' to customize the file types that are opened in
Nyxt and those that are opened externally.

Can be used as a `*open-file-function*'."
  (handler-case
      (cond
        ((and (uiop:directory-pathname-p filename)
              (uiop:directory-exists-p filename))
         (buffer-load (format nil "file://~a" (uiop:ensure-directory-pathname filename))
                      :buffer (make-buffer-focus :url nil)))
        ((supported-media filename)
         (buffer-load (format nil "file://~a" filename)
                      :buffer (make-buffer-focus :url nil)))
        (t
         (uiop:launch-program
          #+linux
          (list "xdg-open" (namestring filename))
          #+darwin
          (list "open" (namestring filename)))))
    ;; We can probably signal something and display a notification.
    (error (c) (log:error "Opening ~a: ~a~&" filename c))))

;; note: put under the function definition.
;; the user is encouraged to override this in her init file.
(serapeum:export-always '*open-file-function*)
(defparameter *open-file-function* #'open-file-function
  "Function triggered to open files.

Example to open directories with =emacsclient= and some music ad
videos with =mpv=:

\(defun my-open-files (filename)
  \"Open music and videos with mpv, open directories with emacsclient.\"
  (let ((args)
        (extension (pathname-type filename)))
    (cond
      ((uiop:directory-pathname-p filename)
       (log:info \"Opening ~a with emacsclient.\" filename)
       (setf args (list \"emacsclient\" filename)))

      ((member extension '(\"flv\" \"mkv\" \"mp4\") :test #'string-equal)
       (setf args (list \"mpv\" filename))))

    (handler-case (if args
                      (uiop:launch-program args)
                      ;; fallback to Nyxt's default.
                      (nyxt/file-manager-mode:open-file-function filename))
      (error (c) (log:error \"Error opening ~a: ~a\" filename c)))))

\(setf nyxt/file-manager-mode:*open-file-function* #'my-open-files)")

(define-mode file-manager-mode (nyxt/minibuffer-mode:minibuffer-mode)
  "Mode to open any file from the filesystem with fuzzy suggestion
on the minibuffer. Specialize keybindings on this mode. See the
command `open-file'."
  ((keymap-scheme
    :initform
    (define-scheme "file-manager"
      scheme:emacs
      (list
       "M-left" 'display-parent-directory
       "C-l" 'display-parent-directory
       "C-j" 'enter-directory
       "M-right" 'enter-directory)

      scheme:vi-normal
      (list
       "M-right" 'enter-directory
       "M-left" 'display-parent-directory)))))

(serapeum:export-always 'open-file-from-directory-suggestion-filter)
(defun open-file-from-directory-suggestion-filter (minibuffer &optional (directory (uiop:getcwd)))
  "Fuzzy-match files and directories from DIRECTORY."
  (let ((filenames (uiop:directory-files directory))
        (dirnames (uiop:subdirectories directory)))
    (fuzzy-match (input-buffer minibuffer) (append filenames dirnames))))

(define-command display-parent-directory (&optional (minibuffer (current-minibuffer)))
  "Get the parent directory and update the minibuffer.

Default keybindings: `M-Left' and `C-l'."
  (uiop:chdir "..")
  (erase-input minibuffer)
  (update-display minibuffer))

(define-command enter-directory (&optional (minibuffer (current-minibuffer)))
  "If the suggestion at point is a directory, refresh the minibuffer suggestions with its list of files.

Default keybindings: `M-Right' and `C-j'. "
  (let ((filename (get-suggestion minibuffer)))
    (when (and (uiop:directory-pathname-p filename)
               (uiop:directory-exists-p filename))
      (uiop:chdir filename)
      (erase-input minibuffer)
      (update-display minibuffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :nyxt)
(define-command open-file ()
  "Open a file from the filesystem.

The user is prompted with the minibuffer, files are browsable with
fuzzy suggestion.

The default directory is the one computed by
`download-manager:default-download-directory' (usually `~/Downloads').

Press `Enter' to visit a file, `M-Left' or `C-l' to go one directory
up, `M-Right' or `C-j' to browse the directory at point.

By default, it uses the `xdg-open' command. The user can override the
`nyxt:open-file-function' function, which takes the filename (or directory
name) as parameter.

The default keybinding is `C-x C-f'.

Note: this feature is alpha, get in touch for more!"
  ;; TODO: How do we set the current directory permanently?
  (uiop:with-current-directory ((uiop:getcwd))
    ;; Allow the current minibuffer to recognize our keybindings.
    (with-result (filename (read-from-minibuffer
                            (make-minibuffer
                             :default-modes '(nyxt/file-manager-mode::file-manager-mode minibuffer-mode)
                             :input-prompt (file-namestring (uiop:getcwd))
                             :suggestion-function #'nyxt/file-manager-mode::open-file-from-directory-suggestion-filter)))

      (funcall nyxt/file-manager-mode::*open-file-function* (namestring filename)))))
