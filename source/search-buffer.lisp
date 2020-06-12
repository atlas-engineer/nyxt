(in-package :nyxt/web-mode)

(define-parenscript query-buffer (&key query (case-sensitive-p nil))
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
    (unless (qs document "#nyxt-stylesheet")
      (ps:try
       (ps:let* ((style-element (ps:chain document (create-element "style")))
                 (box-style (ps:lisp (box-style (current-buffer))))
                 (highlighted-style (ps:lisp (highlighted-box-style (current-buffer)))))
         (setf (ps:@ style-element id) "nyxt-stylesheet")
         (ps:chain document head (append-child style-element))
         (ps:chain style-element sheet (insert-rule box-style 0))
         (ps:chain style-element sheet (insert-rule highlighted-style 1)))
       (:catch (error)))))

  (defun create-match-object (body identifier)
    (ps:create "type" "match" "identifier" identifier "body" body))

  (defun create-match-span (body identifier)
    (ps:let* ((el (ps:chain document (create-element "span"))))
      (setf (ps:@ el class-name) "nyxt-hint")
      (setf (ps:@ el text-content) body)
      (setf (ps:@ el id) (+ "nyxt-hint-" identifier))
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
    (let ((rgx (ps:new (|RegExp| query (if (ps:lisp case-sensitive-p) "" "i")))))
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
        (setf (ps:@ new-node class-name) "nyxt-search-node")
        (setf (ps:@ new-node id) node-identifier)
        (setf (aref *nodes* node-identifier) node)
        (when (> (length substring-indices) 0)
          (loop for index in substring-indices
                with last-index = 0
                do (incf *identifier*)
                   (ps:chain new-node (append-child (ps:chain document (create-text-node (ps:chain node-text (substring last-index index))))))
                   (ps:chain new-node (append-child (create-match-span (ps:chain node-text (substring index (+ index (length query)))) *identifier*)))
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
          do (setf node (ps:chain node nyxt-sibling))))

  (defun remove-search-nodes ()
    "Removes all the search elements"
    (ps:dolist (node (qsa document ".nyxt-search-node"))
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
   (buffer :accessor buffer :initarg :buffer)))

(defclass multi-buffer-match (match) ())

(defmethod object-string ((match match))
  (body match))

(defmethod object-display ((match match))
  (let* ((id (identifier match)))
    (format nil "~a …~a…" id (body match))))

(defmethod object-display ((match multi-buffer-match))
  (let* ((id (identifier match))
         (buffer-id (id (buffer match))))
    (format nil "~a:~a …~a…  ~a" buffer-id id (body match) (title (buffer match)))))

(defun matches-from-json (matches-json &optional (buffer (current-buffer)) (multi-buffer nil))
  (loop for element in (handler-case (cl-json:decode-json-from-string matches-json)
                         (error () nil))
        collect (make-instance (if multi-buffer 'multi-buffer-match 'match)
                               :identifier (cdr (assoc :identifier element))
                               :body (cdr (assoc :body element))
                               :buffer buffer)))

(defun match-completion-function (input &optional (buffers (list (current-buffer))) (case-sensitive-p nil))
  "Update the completions asynchronously via query-buffer."
  (when (> (length input) 2)
    (let ((input (str:replace-all " " " " input))
          (all-matches nil)
          (multi-buffer (if (> (list-length buffers) 1) t nil)))
      (map nil
           (lambda (buffer)
             (with-current-buffer buffer
               (with-result (result (query-buffer
                                     :query input
                                     :case-sensitive-p case-sensitive-p))
                 (let* ((matches (matches-from-json
                                  result buffer multi-buffer)))
                   (setf all-matches (append all-matches matches))
                   (nyxt::set-completions (current-minibuffer) all-matches)))))
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
    (ps:dolist (node (qsa document ".nyxt-search-node"))
      (ps:chain node (replace-with (aref *nodes* (ps:@ node id))))))
  (remove-search-nodes))

(define-command search-buffer (&key (case-sensitive-p nil explicit-case-p))
  "Start a search on the current buffer."
  (apply #'search-over-buffers (list (current-buffer))
         (if explicit-case-p
             `(:case-sensitive-p ,case-sensitive-p)
             '())))

(define-command search-buffers (&key (case-sensitive-p nil explicit-case-p))
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
               `(:case-sensitive-p ,case-sensitive-p)
               '()))))

(defun search-over-buffers (buffers &key (case-sensitive-p nil explicit-case-p))
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
                      #'(lambda (minibuffer)
                          (unless explicit-case-p
                            (setf case-sensitive-p (not (str:downcasep (input-buffer minibuffer)))))
                          (match-completion-function (input-buffer minibuffer) buffers case-sensitive-p))
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
                      :history (nyxt::minibuffer-search-history *browser*))))
    (with-result (match (read-from-minibuffer minibuffer))
      (declare (ignore match))
      (update-selection-highlight-hint :follow t :scroll t))))

(define-command remove-search-hints ()
  "Remove all search hints."
  (%remove-search-hints))
