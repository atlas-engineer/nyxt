;;; utility.lisp --- utility classes and functions

(in-package :next)

(defmethod object-string ((object t))
  (princ-to-string object))

;; data node used to represent tree history
(defclass node ()
    ((parent :accessor node-parent :initarg :parent :initform nil)
     (children :accessor node-children :initform nil)
     (data :accessor node-data :initarg :data :initform nil)))

(defmethod object-string ((node node))
  (node-data node))

(define-command start-swank (root-mode &optional (swank-port *swank-port*))
  "Start a Swank server that can be connected to, for instance, in Emacs via
SLIME."
  (swank:create-server :port swank-port :dont-close t))

(defun parse-url (input-url)
  "From user input, return the full url to visit.

If the first word references a search engine, generate a search query.
If the input starts with an uri scheme, open it as is.
If the input is actually a file path, open it.
Suppose the user omitted the scheme: if the input prefixed by 'https://' gives a valid uri, go to it.
Otherwise, build a search query with the default search engine."
  (let* ((window (%%window-active *interface*))
         (engine (assoc (first (cl-strings:split input-url))
                        (search-engines window) :test #'string=))
         (default (assoc "default"
                         (search-engines window) :test #'string=)))
    (if engine
        (generate-search-query
         (subseq input-url
                 (length (first (cl-strings:split input-url))))
         (rest engine))
        (let ((recognized-scheme (ignore-errors (quri:uri-scheme (quri:uri input-url)))))
          (cond
            ((and recognized-scheme
                  (not (string= "file" recognized-scheme)))
             input-url)
            ((or (string= "file" recognized-scheme)
                 (probe-file input-url))
             (if (string= "file" recognized-scheme)
                 input-url
                 (format nil "file://~a"
                         (uiop:ensure-absolute-pathname input-url *default-pathname-defaults*))))
            ((quri:uri-p (ignore-errors
                          (quri:uri (str:concat "https://" input-url))))
             (str:concat "https://" input-url))
            (t (generate-search-query input-url (rest default))))))))

(defun generate-search-query (search-string search-url)
  (let* ((encoded-search-string
           (cl-string-match:replace-re "  *" "+" search-string :all t))
         (url (format nil search-url encoded-search-string)))
    url))

(defun fuzzy-match (input candidates &key (accessor-function nil)
                                          (case-sensitive nil))
  "fuzzy-match works by taking a string input from the user. The
string is then populated with '*' between each character to create a
regex. As an example, 'nt' will become 'n.*t.*'. This will enable
matching of 'next' or 'note', etc. This function currently limits the
type of input that it accepts, only matching against alphabetical input. An
advanced version of this command may allow for complete regular expressions, but
will have to consider malformed ones."
  (let* ((cleaned-input (cl-string-match:replace-re
                         "[^a-zA-Z]" "" input :all t))
         (cleaned-input (if case-sensitive
                            cleaned-input
                            (string-downcase cleaned-input)))
         (regex
           (with-output-to-string (stream)
             (loop for char across cleaned-input do
               (princ #\. stream)
               (princ #\* stream)
               (princ char stream))
             ;; match any chars after final char in cleaned-input
             (princ #\. stream)
             (princ #\* stream)))
         (completions nil))
    ;; use constructed regex to see which options match
    (flet ((candidate-representation (candidate)
             (if accessor-function
                 (funcall accessor-function candidate) candidate)))
      (loop for candidate in candidates do
        (when (cl-string-match:match-re
               regex
               (if case-sensitive
                   (candidate-representation candidate)
                   (string-downcase (candidate-representation candidate))))
          (push candidate completions))))
    completions))

(defun xdg-data-home (&optional (file-name ""))
  "Return XDG_DATA_HOME as per XDG directory specification.
FILE-NAME is appended to the result."
  (merge-pathnames
   file-name
   (merge-pathnames
    (make-pathname :directory '(:relative "next"))
    (uiop:xdg-data-home))))

(defun xdg-config-home (&optional (file-name ""))
  "Return XDG_CONFIG_HOME as per XDG directory specification.
FILE-NAME is appended to the result."
  (merge-pathnames
   file-name
   (merge-pathnames
    (make-pathname :directory '(:relative "next"))
    (uiop:xdg-config-home))))

(defun ensure-parent-exists (path)
  "Create parent directories of PATH if they don't exist and return PATH."
  (ensure-directories-exist (directory-namestring path))
  path)

(defun ensure-file-exists (path &optional (init-function))
  "Create file pointed by PATH if it does not exists.  Return PATH's truename.
When non-nil, INIT-FUNCTION is used to create the file, else the file will be empty."
  (unless (probe-file path)
    (if init-function
        (funcall init-function path)
        (close (open (ensure-parent-exists path) :direction :probe :if-does-not-exist :create))))
  (truename path))

(defun find-slot (class slot-name)
  "CLASS can be a symbol or a class."
  (when (symbolp class)
    (setf class (closer-mop:ensure-finalized (find-class class))))
  (find-if (lambda (slot)
             (eq (closer-mop:slot-definition-name slot)
                 slot-name))
           (closer-mop:class-slots class)))

(defun get-default (class-name slot-name)
  "Get default value of slot SLOT-NAME from class CLASS-NAME.
The second value is the initfunction."
  (let* ((class (closer-mop:ensure-finalized (find-class class-name)))
         (slot (find-slot class slot-name))
         (value (closer-mop:slot-definition-initform slot)))
    ;; When querying quoted lists, the return value of slot-definition-initform
    ;; is quoted.  For lists declared with LIST, the return value is a list starting with symbol LIST.
    ;; In those cases, we eval it here so that the caller does not have to do it.
    ;; Besides, when the slot value is updated with SETF, the list is stored
    ;; unquoted / without LIST.  By evaluating here, we make sure that all calls to GET-DEFAULT
    ;; have consistent return types.  WARNING: This could be limitating if slot
    ;; was meant to actually store a quoted list.  Should this happen, we would
    ;; have to take some provision.
    (if (and (listp value)
             (or
              (eq 'quote (first value))
              (eq 'list (first value))))
        (eval value)
        value)))

(defun (setf get-default) (value class-name slot-name)
  "Set default value of SLOT-NAME from CLASS-NAME.
Return VALUE."
  ;; Warning: This is quite subtle: the :initform and :initfunction are tightly
  ;; coupled, it seems that both must be changed together.  We need to change
  ;; the class-slots and not the class-direct-slots.  TODO: Explain why.
  (let* ((class (closer-mop:ensure-finalized (find-class class-name)))
         (slot (find-slot class slot-name)))
    (setf (closer-mop:slot-definition-initfunction slot) (lambda () value))
    (setf (closer-mop:slot-definition-initform slot) value)))

(defun add-to-default-list (value class-name slot-name)
  "Add VALUE to the list SLOT-NAME from CLASS-NAME.
If VALUE is already present, move it to the head of the list."
  (setf (get-default class-name slot-name)
        (remove-duplicates (cons value
                                 (get-default class-name slot-name))
                           :from-end t)))

(defun member-string (string list)
  "Return the tail of LIST beginning whose first element is STRING."
  (check-type string string)
  (member string list :test #'string=))

;; This is mostly inspired by Emacs 26.2.
(defun file-size-human-readable (file-size &optional flavor)
  "Produce a string showing FILE-SIZE in human-readable form.

Optional second argument FLAVOR controls the units and the display format:

 If FLAVOR is nil or omitted, each kilobyte is 1024 bytes and the produced
    suffixes are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `si', each kilobyte is 1000 bytes and the produced suffixes
    are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `iec', each kilobyte is 1024 bytes and the produced suffixes
    are \"KiB\", \"MiB\", \"GiB\", \"TiB\", etc."
  (let ((power (if (or (null flavor) (eq flavor 'iec))
                   1024.0
                   1000.0))
        (post-fixes
          ;; none, kilo, mega, giga, tera, peta, exa, zetta, yotta
          (list "" "k" "M" "G" "T" "P" "E" "Z" "Y"))
        (format-string "~d~a~a"))
    (loop while (and (>= file-size power) (rest post-fixes))
          do (setf file-size (/ file-size power)
                   post-fixes (rest post-fixes)))
    (if (> (abs (- file-size (round file-size))) 0.05)
        (setf format-string "~,1f~a~a")
        (setf file-size (round file-size)))
    (format nil format-string
            file-size
            (if (and (eq flavor 'iec) (string= (first post-fixes) "k"))
                "K"
                (first post-fixes))
            (cond
              ((and (eq flavor 'iec)
                    (string= (first post-fixes) ""))
               "B")
              ((eq flavor 'iec) "iB")
              (t "")))))

(defmethod %%minibuffer-evaluate-javascript ((interface remote-interface)
                                             (window-id string) javascript
                                             &optional callback)
  "window-id: string. Dev purposes."
  (%%minibuffer-evaluate-javascript interface
                                    (gethash window-id (windows interface))
                                    javascript callback))

(defmethod %%window-delete ((interface remote-interface) (window-id string))
  "window-id: string. Dev purposes."
  (%rpc-send interface "window_delete" window-id)
  (with-slots (windows) interface
    (remhash window-id windows)))

(defmethod %%window-exists ((interface remote-interface) (window-id string))
  "window-id: string. Dev purposes."
  ;; we don't use (gethash window-id (windows interface)) because,
  ;; with manual tests, it can get out of sync.
  (%rpc-send interface "window_exists" window-id))

(defmethod %%window-set-minibuffer-height ((interface remote-interface)
                                           (window-id string)
                                           height)
  "window-id: string. Dev purposes."
  (%rpc-send interface "window_set_minibuffer_height" window-id height))

(defun make-window-and-active-buffer (interface &key (url "https://ddg.gg"))
  "For development purposes.
Make a window and an active buffer."
  (let* ((window (%%window-make interface))
         (buffer (%%buffer-make interface)))
    (set-active-buffer interface buffer)
    (%%buffer-load interface buffer url)
    (values window buffer)))

(defun list-windows (interface)
  "List the windows of the given interface. Dev purposes."
  (maphash (lambda (key val)
             (format t "~a ~a~&" key val))
          (windows interface)))

(defun reset (interface)
  "Clean up our list of windows and buffers.
Use this when you restart a platform port, that consequently our lisp
data structures get out of sync and when you don't want to restart the
lisp core."
  (setf (windows interface) (make-hash-table :test #'equal))
  (setf (buffers interface) (make-hash-table :test #'equal))
  (setf (last-active-window interface) nil)
  (setf (total-window-count interface) 0)
  (setf (total-buffer-count interface) 0)
  (setf (key-chord-stack interface) nil))
