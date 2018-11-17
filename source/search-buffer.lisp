;;; search-buffer.lisp --- functions to enable searching within a webview

(in-package :next)

(define-parenstatic initialize-search-buffer
  (ps:defvar current-search 0)
  (ps:defvar match-count)
  (defun insert (str index value)
    (+ (ps:chain str (substr 0 index)) value (ps:chain str (substr index))))
  (defun create-search-span (index)
    (ps:let* ((el (ps:chain document (create-element "span"))))
      (setf (ps:@ el class-name) "next-search-hint")
      (setf (ps:@ el style background) "rgba(255, 255, 255, 0.75)")
      (setf (ps:@ el style border) "1px solid red")
      (setf (ps:@ el style font-weight) "bold")
      (setf (ps:@ el style text-align) "center")
      (setf (ps:@ el text-content) index)
      (setf (ps:@ el id) index)
      el)))

(define-parenscript paren-add-search-boxes (search-string)
  (let* ((regex-string (ps:lisp (concatenate 'string search-string "[A-Za-z]*")))
         (regex-flags "gi")
         (matcher (ps:new (-reg-exp regex-string regex-flags)))
         (body (ps:chain document body inner-h-t-m-l))
         (last-match t)
         (matches (loop while (setf last-match (ps:chain matcher (exec body)))
                     collect (ps:chain last-match index))))
    (setf match-count (length matches))
    (setf matches (ps:chain matches (reverse)))
    (loop for i from 0 to (- match-count 1)
       do (setf body (insert body (ps:elt matches i)
                             (ps:chain (create-search-span (- match-count i)) outer-h-t-m-l))))
    (setf (ps:chain document body inner-h-t-m-l) body))
  nil)

(define-command add-search-boxes ()
  "Add search boxes for a given search string"
  (initialize-search-buffer)
  (with-result (input (read-from-minibuffer
                       *minibuffer*
                       :input-prompt "Search for:"))
    (buffer-evaluate-javascript *interface*
                                (active-buffer *interface*)
                                (paren-add-search-boxes input))))

(define-parenstatic remove-search-hints
  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))
  (defun search-hints-remove-all ()
    "Removes all the links"
    (ps:dolist (el (qsa document ".next-search-hint"))
      (ps:chain el (remove))))
  (search-hints-remove-all))

(define-parenstatic next-search-hint
  (when (> match-count current-search)
    (setf current-search (+ current-search 1)))
  (let ((element (ps:chain document (get-element-by-id current-search))))
    (ps:chain element (scroll-into-view t))))

(define-parenstatic previous-search-hint
  (when (> current-search 0)
    (setf current-search (- current-search 1)))
  (let ((element (ps:chain document (get-element-by-id current-search))))
    (ps:chain element (scroll-into-view t))))
