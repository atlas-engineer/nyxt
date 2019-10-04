(uiop:define-package :next/file-manager-mode
    (:use :common-lisp :trivia :next)
  (:export
   :open-file-function
   :*open-file-function*
   :open-file-from-directory-completion-filter)
  (:documentation "Manage files.

Open any file from within Next, with the usual fuzzy completion.

`M-x open-file (C-x C-f)'

\"file manager\" is a bit excessive for now. Currently, we can:
- browse files, with fuzzy-completion
- go one directory up (C-l)
- enter a directory (C-j)
- open files. By default, with xdg-open. See `open-file-function'.
"))
(in-package :next/file-manager-mode)
(annot:enable-annot-syntax)

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
;;; - open files in Next
;;; - a UI to list files
;;; - lazy loading for large directories
;;; - many things...

;; TODO: a global isn't satisfactory. Two separate instances of open-file will interfere with this global variable.
;; - store it as a class slot ?
;; see also: uiop:with-current-directory, uiop:getcwd
(defvar *current-directory* download-manager::*default-download-directory*
  "Default directory to open files from. Defaults to the downloads directory.")

@export
(defun open-file-function (filename)
  "Open FILENAME.
FILENAME is the full path of the file (or directory), as a string.
By default, try to open it with the system's default external program, using `xdg-open'.
The user can override this function to decide what to do with the file."
  (handler-case (uiop:launch-program (list "xdg-open" (namestring filename)))
    ;; We can probably signal something and display a notification.
    (error (c) (log:error "Error opening ~a: ~a~&" filename c))))

;; note: put under the function definition.
;; the user is encouraged to override this in her init file.
@export
(defparameter *open-file-function* #'open-file-function
  "Function triggered to open files.")

(define-mode file-manager-mode (minibuffer-mode)
    "Mode to open any file from the filesystem with fuzzy completion
on the minibuffer. Specialize keybindings on this mode. See the
command `open-file'."
    ((keymap-schemes
      :initform
      (let ((emacs-map (make-keymap))
            (vi-map (make-keymap)))

        (define-key :keymap emacs-map
          "M-Left" #'display-parent-directory
          "C-l" #'display-parent-directory
          "C-j" #'enter-directory
          "M-Right" #'enter-directory)

        (define-key :keymap vi-map
          "M-Right" #'enter-directory
          "M-Left" #'display-parent-directory)

        (list :emacs emacs-map
              :vi-normal vi-map)))))

@export
(defun open-file-from-directory-completion-filter (input &optional (directory *current-directory*))
  "Fuzzy-match files and directories from `*current-directory*'."
  (let ((filenames (uiop:directory-files directory))
        (dirnames (uiop:subdirectories directory)))
    (fuzzy-match input (append filenames dirnames))))

(define-command display-parent-directory (&optional (minibuffer (current-minibuffer)))
  "Get the parent directory and update the minibuffer.

Default keybindings: `M-Left' and `C-l'."
  (setf *current-directory* (uiop:pathname-parent-directory-pathname *current-directory*))
  (erase-input minibuffer)
  (update-display minibuffer))

(define-command enter-directory (&optional (minibuffer (current-minibuffer)))
  "If the candidate at point is a directory, refresh the minibuffer candidates with its list of files.

Default keybindings: `M-Right' and `C-j'. "
  (let ((filename (get-candidate minibuffer)))
    (when (and (uiop:directory-pathname-p filename)
               (uiop:directory-exists-p filename))
      (setf *current-directory* filename)
      (erase-input minibuffer)
      (update-display minibuffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :next)
(define-command open-file ()
  "Open a file from the filesystem.

The user is prompted with the minibuffer, files are browsable with
fuzzy completion.

The default directory is the one computed by
`download-manager:default-download-directory' (usually `~/Downloads').

Press `Enter' to visit a file, `M-Left' or `C-l' to go one directory
up, `M-Right' or `C-j' to browse the directory at point.

By default, it uses the `xdg-open' command. The user can override the
`next:open-file-function' function, which takes the filename (or directory
name) as parameter.

The default keybinding is `C-x C-f'.

Note: this feature is alpha, get in touch for more!"
  (let ((directory next/file-manager-mode::*current-directory*))
    ;; Allow the current minibuffer to recognize our keybindings.
    (with-result (filename (read-from-minibuffer
                            (make-instance 'minibuffer
                                           :default-modes '(next/file-manager-mode::file-manager-mode minibuffer-mode)
                                           :input-prompt (file-namestring directory)
                                           :completion-function #'next/file-manager-mode::open-file-from-directory-completion-filter)))

      (funcall next/file-manager-mode::*open-file-function* (namestring filename)))))
