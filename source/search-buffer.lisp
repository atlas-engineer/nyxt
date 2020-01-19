;;; search-buffer.lisp --- functions to enable searching within a buffer

(in-package :next)

(define-parenscript query-buffer (query)
  (defvar *identifier* 0)
  (defvar *matches* (array))
  (defvar *nodes* (ps:new (-Object)))

  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))

  (defun create-match-object (body identifier)
    (ps:create "type" "match" "identifier" identifier "body" body))

  (defun create-match-span (body identifier)
    (ps:let* ((el (ps:chain document (create-element "span"))))
      (setf (ps:@ el style) (ps:lisp (box-style (current-buffer))))
      (setf (ps:@ el text-content) body)
      (setf (ps:@ el id) identifier)
      el))

  (defun get-substring (string-before string-after query)
    "Return the substring and preceding/trailing text for a given
index. TODO: figure out why string-after trimming causes break of
search (string-after (ps:chain string-after (substring 0
character-preview-count)))."
    (let* ((character-preview-count 40)
           (string-before (ps:chain string-before (substring (- (length string-before) character-preview-count)))))
      (+ string-before query string-after)))

  (defun create-substring-matches (query node)
    "Return all of substrings that match the search-string."
    (let ((substrings (ps:chain (ps:@ node text-content) (split query)))
          (node-identifier (incf (ps:chain *nodes* identifier)))
          (new-node (ps:chain document (create-element "span"))))
      (setf (ps:@ new-node class-name) "next-search-node")
      (setf (ps:@ new-node id) node-identifier)
      (setf (aref *nodes* node-identifier) node)
      (when (> (length substrings) 1)
        (loop for i from 0 to (- (length substrings) 1) by 2
              do (incf *identifier*)
                 (ps:chain new-node (append-child
                                     (ps:chain document (create-text-node (elt substrings i)))))
                 (ps:chain new-node (append-child
                                     (create-match-span query *identifier*)))
                 (unless (= i (- (length substrings) 1))
                   (ps:chain new-node (append-child
                                       (ps:chain document (create-text-node (elt substrings (+ i 1)))))))
              collect (create-match-object 
                       (get-substring (elt substrings i)
                                      (elt substrings (+ i 1))
                                      query)
                       *identifier*)
              finally (ps:chain node (replace-with new-node))))))

  (defun matches-from-node (node query)
    (when (= (ps:chain (typeof (ps:@ node node-value))) "string")
      (let ((matches (create-substring-matches query node)))
        (ps:chain *matches* push (apply *matches* matches)))))

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
        (*identifier* 0))
    (remove-search-nodes)
    (setf (ps:chain *nodes* identifier) 0)
    (walk-document (ps:chain document body) matches-from-node)
    (ps:chain -j-s-o-n (stringify *matches*))))

(define-parenscript focus-match (match)
  (ps:lisp (when (not (equal (buffer match) (current-buffer)))
             (set-current-buffer (buffer match))))
  ;; TODO: scroll doesn't work properly because it seems it tries to scroll
  ;; before switching buffer in cases where the buffer should be switched
  ;; so have to find a way to sync that up
  (let* ((rel-identifier
           (ps:lisp
            (let ((id (identifier match)))
              (if (stringp id)
                  (cadr (str:split ":" id))
                  id))))
         (element (ps:chain document (get-element-by-id rel-identifier))))
    (ps:chain element (scroll-into-view t))))

(defclass match ()
  ((identifier :accessor identifier :initarg :identifier)
   (body :accessor body :initarg :body)
   (buffer :accessor buffer :initarg :buffer)))

(defmethod object-string ((match match))
  (format nil "~a ...~a..." (identifier match) (body match)))

(defun matches-from-json (matches-json &optional (buffer (current-buffer)) (multi-buffer nil))
  (loop for element in (cl-json:decode-json-from-string matches-json)
        collect (make-instance 'match
                               :identifier (if multi-buffer
                                               (format nil "~d:~d" (id buffer) (cdr (assoc :identifier element)))
                                               (cdr (assoc :identifier element)))
                               :body (cdr (assoc :body element))
                               :buffer buffer)))

(defun match-completion-function (input &optional (buffers (list (current-buffer))))
  "Update the completions asynchronously via query-buffer. TODO:
capture the current-buffer and current-minibuffer in a closure."
  (when (> (length input) 2)
    (let ((input (str:replace-all " " " " input))
          (all-matches nil)
          (multi-buffer (if (> (list-length buffers) 1) t nil)))
      (map nil
           (lambda (buffer)
             (query-buffer :query input :buffer buffer
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

(define-command search-buffer ()
  "Start a search on the current buffer."
  (search-over-buffers (list (current-buffer))))

(define-command search-buffers ()
  "Show a prompt in the minibuffer that allows to choose
one or more buffers, and then start a search prompt that
searches over the selected buffer(s)."
  (with-result (buffers (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Search buffer(s)"
                          :multi-selection-p t
                          :completion-function (buffer-completion-filter))))
   (search-over-buffers buffers)))

(defun search-over-buffers (buffers)
  "Add search boxes for a given search string over the
provided buffers."
  (let* ((num-buffers (list-length buffers))
         (prompt-text
          (if (> num-buffers 1)
              (format nil "Search over ~d buffers for (3+ characters)" num-buffers)
              "Search for (3+ characters)"))
         (minibuffer (make-minibuffer
                      :input-prompt prompt-text
                      :completion-function #'(lambda (input)
                                               (match-completion-function
                                                input buffers))
                      :history (minibuffer-search-history *interface*)))
         (keymap-scheme (current-keymap-scheme minibuffer))
         (keymap (getf (keymap-schemes (first (modes minibuffer))) keymap-scheme)))
    (define-key :keymap keymap "C-s"
                #'(lambda ()
                   (when (completions minibuffer)
                    (focus-match :match (nth (completion-cursor minibuffer)
                                             (completions minibuffer))))))
    (with-result (input (read-from-minibuffer minibuffer))
     (focus-match :match input))))


(define-command remove-search-hints ()
  "Remove all search hints."
  (%remove-search-hints))

(define-deprecated-command next-search-hint ()
  "Go to next search hint.")

(define-deprecated-command previous-search-hint ()
  "Go to previous search hint.")

(define-deprecated-command add-search-hints ()
  "Deprecated by `search-buffer'."
  (search-buffer))
