;;; search-buffer.lisp --- functions to enable searching within a buffer

(in-package :next)

(define-parenscript query-buffer (query (case-insensitive-p nil))
  (defvar *identifier* 0)
  (defvar *matches* (array))
  (defvar *nodes* (ps:new (-Object)))
  (defvar *node-replacements* (array))

  (defun qs (context selector)
    "Alias of document.querySelector"
    (ps:chain context (query-selector selector)))

  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))

  (defun add-stylesheet ()
    (unless (qs document "#next-stylesheet") 
      (ps:let* ((style-element (ps:chain document (create-element "style")))
                (box-style (ps:lisp (box-style (current-buffer))))
                (highlighted-style (ps:lisp (highlighted-box-style (current-buffer)))))
        (setf (ps:@ style-element id) "next-stylesheet")
        (ps:chain document head (append-child style-element))
        (ps:chain style-element sheet (insert-rule box-style 0))
        (ps:chain style-element sheet (insert-rule highlighted-style 1)))))

  (defun create-match-object (body identifier)
    (ps:create "type" "match" "identifier" identifier "body" body))

  (defun create-match-span (body identifier)
    (ps:let* ((el (ps:chain document (create-element "span"))))
      (setf (ps:@ el class-name) "next-hint")
      (setf (ps:@ el text-content) body)
      (setf (ps:@ el id) (+ "next-hint-" identifier))
      el))

  (defun get-substring (string query index)
    "Return the substring and preceding/trailing text for a given
     index."
    (let* ((character-preview-count 40))
           (ps:chain string
                     (substring (- index character-preview-count)
                                (+ index (length query) character-preview-count)))))

  (defun get-substring-indices (query string)
    "Get the indices of all matching substrings."
    (let ((rgx (ps:new (|RegExp| query (if (ps:lisp case-insensitive-p) "i" "")))))
      (loop with index = 0
            until (= index -1)
            do (setf index (ps:chain string (search rgx)))
               (setf string (ps:chain string (substring (+ index (length query)))))
            when (not (= index -1))
              collect index)))

  (defun matches-from-node (node query)
    "Return all of substrings that match the search-string."
    (when (= (ps:chain (typeof (ps:@ node node-value))) "string")
      (let* ((node-text (ps:@ node text-content))
             (substring-indices (get-substring-indices query node-text))
             (node-identifier (incf (ps:chain *nodes* identifier)))
             (new-node (ps:chain document (create-element "span"))))
        (setf (ps:@ new-node class-name) "next-search-node")
        (setf (ps:@ new-node id) node-identifier)
        (setf (aref *nodes* node-identifier) node)
        (when (> (length substring-indices) 0)
          (loop for index in substring-indices
                with last-index = 0
                do (incf *identifier*)
                   (ps:chain new-node (append-child (ps:chain document (create-text-node (ps:chain node-text (substring last-index index))))))
                   (ps:chain new-node (append-child (create-match-span query *identifier*)))
                   (setf last-index (+ (length query) index))
                   (ps:chain *matches* (push (create-match-object (get-substring node-text query index) *identifier*)))
                finally (progn
                          (ps:chain new-node (append-child (ps:chain document (create-text-node (ps:chain node-text (substring (+ (length query) index)))))))
                          (ps:chain *node-replacements*
                                    (push (list node new-node)))))))))

  (defun replace-original-nodes ()
    "Replace original nodes with recreated search nodes"
    (loop for node-pair in *node-replacements*
          do (ps:chain (elt node-pair 0) (replace-with (elt node-pair 1)))))

  (defun walk-document (node process-node)
    (when (and node (not (ps:chain node first-child)))
      (funcall process-node node (ps:lisp query)))
    (setf node (ps:chain node first-child))
    (loop while node
          do (walk-document node process-node)
          do (setf node (ps:chain node next-sibling))))

  (defun remove-search-nodes ()
    "Removes all the search elements"
    (ps:dolist (node (qsa document ".next-search-node"))
      (ps:chain node (replace-with (aref *nodes* (ps:@ node id))))))

  (let ((*matches* (array))
        (*node-replacements* (array))
        (*identifier* 0))
    (add-stylesheet)
    (remove-search-nodes)
    (setf (ps:chain *nodes* identifier) 0)
    (walk-document (ps:chain document body) matches-from-node)
    (replace-original-nodes)
    (ps:chain |json| (stringify *matches*))))

(defclass match ()
  ((identifier :accessor identifier :initarg :identifier)
   (body :accessor body :initarg :body)
   (buffer :accessor buffer :initarg :buffer)
   (multi-buffer :accessor multi-buffer :initarg :multi-buffer)))

(defmethod object-string ((match match))
  (body match))

(defmethod object-display ((match match))
  (let* ((id (identifier match))
         (buffer-id (id (buffer match))))
    (if (multi-buffer match)
        (format nil "~a:~a …~a…  ~a" buffer-id id (body match) (title (buffer match)))
        (format nil "~a …~a…" id (body match)))))

(defun matches-from-json (matches-json &optional (buffer (current-buffer)) (multi-buffer nil))
  (loop for element in (handler-case (cl-json:decode-json-from-string matches-json)
                         (error () nil))
        collect (make-instance 'match
                               :identifier (cdr (assoc :identifier element))
                               :multi-buffer multi-buffer
                               :body (cdr (assoc :body element))
                               :buffer buffer)))

(defun match-completion-function (input &optional (buffers (list (current-buffer))) (case-insensitive-p nil))
  "Update the completions asynchronously via query-buffer."
  (when (> (length input) 2)
    (let ((input (str:replace-all " " " " input))
          (all-matches nil)
          (multi-buffer (if (> (list-length buffers) 1) t nil)))
      (map nil
           (lambda (buffer)
             (query-buffer
              :query input
              :buffer buffer
              :case-insensitive-p case-insensitive-p
              :callback (lambda (result)
                          (let* ((matches (matches-from-json
                                           result buffer multi-buffer)))
                            (setf all-matches (append all-matches matches))
                            (set-completions (current-minibuffer) all-matches)))))
           buffers)))
  ;; return NIL, the completions will be updated asynchronously by the
  ;; callback from query-buffer
  ())

(define-parenscript %remove-search-hints ()
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (defun remove-search-nodes ()
    "Removes all the search elements"
    (ps:dolist (node (qsa document ".next-search-node"))
      (ps:chain node (replace-with (aref *nodes* (ps:@ node id))))))
  (remove-search-nodes))

(define-command search-buffer (&key (case-insensitive-p t explicit-case-p))
  "Start a search on the current buffer."
  (apply #'search-over-buffers (list (current-buffer))
         (if explicit-case-p
             `(:case-insensitive-p ,case-insensitive-p)
             '())))

(define-command search-buffers (&key (case-insensitive-p t explicit-case-p))
  "Show a prompt in the minibuffer that allows to choose
one or more buffers, and then start a search prompt that
searches over the selected buffer(s)."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Search buffer(s)"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
    (apply #'search-over-buffers buffers
           (if explicit-case-p
               `(:case-insensitive-p ,case-insensitive-p)
               '()))))

(defun search-over-buffers (buffers &key (case-insensitive-p t explicit-case-p))
  "Add search boxes for a given search string over the
provided buffers."
  (let* ((num-buffers (list-length buffers))
         (prompt-text
           (if (> num-buffers 1)
               (format nil "Search over ~d buffers for (3+ characters)" num-buffers)
               "Search for (3+ characters)"))
         (minibuffer (make-minibuffer
                      :input-prompt prompt-text
                      :completion-function
                      #'(lambda (input)
                          (unless explicit-case-p
                            (setf case-insensitive-p (str:downcasep input)))
                          (match-completion-function input buffers case-insensitive-p))
                      :changed-callback
                      (let ((subsequent-call nil))
                        (lambda ()
                          ;; when the minibuffer initially appears, we don't
                          ;; want update-selection-highlight-hint to scroll
                          ;; but on subsequent calls, it should scroll
                          (update-selection-highlight-hint
                           :scroll subsequent-call)
                          (setf subsequent-call t)))
                      :cleanup-function (lambda () (remove-focus))
                      :history (minibuffer-search-history *browser*)))
         (keymap-scheme (keymap-scheme-name minibuffer))
         (keymap (keymap:get-keymap keymap-scheme
                                    (keymap-scheme (first (modes minibuffer))))))
    (define-key keymap
      "C-s" #'(lambda ()
                (update-selection-highlight-hint :follow t :scroll t)))
    (with-result (match (read-from-minibuffer minibuffer))
      (declare (ignore match))
      (update-selection-highlight-hint :follow t :scroll t))))

(define-command remove-search-hints ()
  "Remove all search hints."
  (%remove-search-hints))
