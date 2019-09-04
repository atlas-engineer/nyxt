;;; file-manager-mode.lisp -- Manage files.
;;;
;;; Open any file from within Next, with the usual fuzzy completion.
;;;
;;; `M-x open-file (C-x C-f)'
;;;
;;; "file manager" is a big excessive for now. Currently, we can:
;;; - browse files, with fuzzy-completion
;;; - go one directory up (C-l)
;;; - enter a directory (C-j)
;;; - open files. By default, with xdg-open. See `open-file-fn'.
;;;
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
;;;

(in-package :next)

(defvar *current-directory* download-manager::*default-download-directory*
  "Default directory to open files from. Defaults to the downloads directory.")

(defun open-file-fn-default (filename)
  "Open this file with `xdg-open'."
  (handler-case (uiop:run-program (list "xdg-open" (namestring filename)))
    ;; We can probably signal something and display a notification.
    (error (c) (log:error "Error opening ~a: ~a~&" filename c))))

;; TODO: Remove `open-file-fn` (it's just a one-liner) and instead store the
;; "open-file-function" into a download-mode slot, which is then called from
;; `download-open-file' with `(funcall (open-file-function download-mode)
;; filename).
;; (defun open-file-fn (filename)
;;   "Open `filename'.
;; `filename' is the full path of the file (or directory), as a string.
;; By default, try to open it with the system's default external program, using `xdg-open'.
;; The user can override this function to decide what to do with the file."
;;   (open-file-fn-default filename))

;; note: put under the function definition.
;; (export '*open-file-fn*)
;; the user is encouraged to override this in her init file.
;; (defparameter *open-file-fn* #'open-file-fn
;;   "Function triggered to open files.")

(defun open-file-from-directory-completion-fn (input &optional (directory *current-directory*))
  "Fuzzy-match files and directories from `*current-directory*'."
  (let ((filenames (uiop:directory-files directory))
        (dirnames (uiop:subdirectories directory)))
    (fuzzy-match input (append filenames dirnames))))

(define-mode open-file-mode (minibuffer-mode)
    "Mode to open any file from the filesystem with fuzzy completion
on the minibuffer. Specialize keybindings on this mode. See the
command `open-file'."
    ((keymap-schemes
      :initform
      (let ((emacs-map (make-keymap))
            (vi-map (make-keymap)))

        (define-key :keymap emacs-map
          "M-Left" 'display-parent-directory
          "C-l" 'display-parent-directory
          "C-j" 'enter-directory
          "M-Right" 'enter-directory)

        (define-key :keymap vi-map
          "M-Left" 'display-parent-directory)

        (list :emacs emacs-map
              :vi-normal vi-map)))))

(defclass open-file-instance ()
  ((default-modes :initform '(open-file-mode))
   (open-file-function :accessor open-file-function
                       :initform #'open-file-fn-default))
  (:documentation "Open a file interactively."))

(define-command display-parent-directory (open-file-mode &optional (minibuffer (minibuffer *interface*)))
  "Get the parent directory and update the minibuffer.

Default keybindings: `M-Left' and `C-l'."
  (setf *current-directory* (uiop:pathname-parent-directory-pathname *current-directory*))
  (erase-input minibuffer)
  (update-display minibuffer))

(define-command enter-directory (open-file-mode &optional (minibuffer (minibuffer *interface*)))
  "If the candidate at point is a directory, refresh the minibuffer candidates with its list of files.

Default keybindings: `M-Right' and `C-j'. "
  (let ((filename (get-candidate minibuffer)))
    (when (and (uiop:directory-pathname-p filename)
               (uiop:directory-exists-p filename))
      (setf *current-directory* filename)
      (erase-input minibuffer)
      (update-display minibuffer))))

(define-command open-file ()
  "Open a file from the filesystem.

The user is prompted with the minibuffer, files are browsable with the
fuzzy completion.

The default directory is the one computed by
`download-manager:default-download-directory' (certainly `~/Downloads').

Press `Enter' to visit a file, `M-Left' or `C-l' to go one directory
up, `M-Right' or `C-j' to browse the directory at point.

By default, it uses the `xdg-open' command. The user can override the
`next:open-file-fn' function, which takes the filename (or directory
name) as parameter.

The default keybinding is `C-x C-f'.

Note: this feature is alpha, get in touch for more !"
  (let ((directory *current-directory*)
        mode)
    ;; "Activate" open-file-mode
    (setf mode (make-instance  'open-file-mode))
    ;; populate needed slots...
    (setf (buffer mode) (minibuffer *interface*))
    ;; Allow the current minibuffer to recognize our keybindings.
    (push mode (modes (minibuffer *interface*)))
    (with-result (filename (read-from-minibuffer
                            (minibuffer *interface*)
                            :input-prompt (file-namestring directory)
                            :completion-function #'open-file-from-directory-completion-fn))

      ;; (funcall (open-file-function open-file-mode) (namestring filename))
      (funcall #'open-file-fn-default (namestring filename))
      ;TODO: remove open-file-mode
      )))


(define-key  "C-x C-f" #'open-file)

;; This currently dosen't work, we must use the keymap at the mode definition:
;; (define-key :mode 'open-file-mode  "M-Left" #'display-parent-directory)
