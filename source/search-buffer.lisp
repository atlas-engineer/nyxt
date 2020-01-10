;;; search-buffer.lisp --- functions to enable searching within a buffer

(in-package :next)

(define-parenscript query-buffer (query)
  (defvar *identifier* 0)
  (defvar *matches* (array))
  
  (defun create-match-object (body identifier)
    (ps:create "type" "match" "identifier" identifier "body" body))
  
  (defun create-match-span (body identifier)
    (ps:let* ((el (ps:chain document (create-element "span"))))
      (setf (ps:@ el class-name) "next-search-hint")
      (setf (ps:@ el style) (ps:lisp (box-style (current-buffer))))
      (setf (ps:@ el text-content) body)
      (setf (ps:@ el id) identifier)
      el))
  
  (defun get-substring (string-before string-after query)
    "Return the substring and preceding/trailing text for a given index."
    (let ((character-preview-count 40))
      (+ (ps:chain string-before (substring (- (length string-before) character-preview-count)))
         query
         (ps:chain string-after (substring 0 character-preview-count)))))
  
  (defun get-substring-matches (query node)
    "Return all of substrings that match the search-string."
    (let ((substrings (ps:chain (ps:@ node text-content) (split query))))
      (when (> (length substrings) 1)
        (loop for i from 0 to (- (length substrings) 1) by 2
              collect (create-match-object 
                       (get-substring (elt substrings i)
                                      (elt substrings (+ i 1))
                                      query)
                       (incf *identifier*))))))
  
  (defun create-substring-highlights (node matches query)
    "Modify the nodes in the document to highlight the matches"
    (let ((new-node (ps:chain document (create-element "span")))
          (substrings (ps:chain (ps:@ node text-content) (split query))))
      (when (> (length substrings) 1)
        (loop for i from 0 to (- (length substrings) 1) do
          (ps:chain new-node (append-child
                              (ps:chain document (create-text-node (elt substrings i)))))
          (unless (= i (- (length substrings) 1))
            (ps:chain new-node (append-child
                                (create-match-span query (ps:chain (elt matches i) identifier))))))
        (ps:chain node (replace-with new-node)))))
  
  (defun matches-from-node (node query)
    (when (= (ps:chain (typeof (ps:@ node node-value))) "string")
      (let ((matches (get-substring-matches query node t)))
        (create-substring-highlights node matches query)
        (ps:chain *matches* push (apply *matches* matches)))))
  
  (defun walk-document (node process-node)
    (when (and node (not (ps:chain node first-child)))
      (funcall process-node node (ps:lisp query)))
    (setf node (ps:chain node first-child))
    (loop while node
          do (walk-document node process-node)
          do (setf node (ps:chain node next-sibling))))
  
  (let ((*matches* (array))
        (*identifier* 0))
    (walk-document (ps:chain document body) matches-from-node)
    (ps:chain -j-s-o-n (stringify *matches*))))

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
  "This function will update the completions asynchronously via
query-buffer. TODO: capture the current-buffer and current-minibuffer
in a closure."
  (when (> (length input) 2)
    (query-buffer
     :query input
     :callback (lambda (result)
                 (set-completions (current-minibuffer)
                                  (matches-from-json result)))))
  ;; return an empty list, the completions will be updated
  ;; asynchronously by the callback from query-buffer
  ())

(define-command search-buffer ()
  "Add search boxes for a given search string."
  (with-result (input (read-from-minibuffer
                       (make-minibuffer
                        :input-prompt "Search for (3+ characters)"
                        :completion-function 'match-completion-function
                        :history (minibuffer-search-history *interface*))))
    (print input)))

(define-command remove-search-hints ()
  "Remove all search hints.")

(define-command next-search-hint ()
  "Go to next search hint.")

(define-command previous-search-hint ()
  "Go to previous search hint.")

(define-deprecated-command add-search-hints ()
  "Deprecated by `search-buffer'."
  (search-buffer))
