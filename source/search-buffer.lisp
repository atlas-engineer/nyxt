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
  (let ((element (ps:chain document (get-element-by-id (ps:lisp (identifier match))))))
    (ps:chain element (scroll-into-view t))))

(defclass match ()
  ((identifier :accessor identifier :initarg :identifier)
   (body :accessor body :initarg :body)))

(defmethod object-string ((match match))
  (format nil "~a ...~a..." (identifier match) (body match)))

(defun matches-from-json (matches-json)
  (loop for element in (cl-json:decode-json-from-string matches-json)
        collect (make-instance 'match
                               :identifier (cdr (assoc :identifier element))
                               :body (cdr (assoc :body element)))))

(defun match-completion-function (input)
  "Update the completions asynchronously via query-buffer. TODO:
capture the current-buffer and current-minibuffer in a closure."
  (when (> (length input) 2)
    (let ((input (str:replace-all " " " " input)))
      (query-buffer
       :query input
       :callback (lambda (result)
                   (set-completions (current-minibuffer)
                                    (matches-from-json result))))))
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
  "Add search boxes for a given search string."
  (let* ((minibuffer (make-minibuffer
                      :input-prompt "Search for (3+ characters)"
                      :completion-function 'match-completion-function
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
