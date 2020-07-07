(uiop:define-package :nyxt/reading-line-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for reading lines."))

(in-package :nyxt/reading-line-mode)

(define-mode reading-line-mode ()
  "Mode for drawing a line on screen that the user can use to keep
track of their reading position."
  ((keymap-scheme
    :initform
    (define-scheme "reading-line-mode"
      scheme:cua
      (list
       "M-up" 'reading-line-cursor-up
       "M-down" 'reading-line-cursor-down)
      scheme:emacs
      (list
       "M-p" 'reading-line-cursor-up
       "M-n" 'reading-line-cursor-down)))
   (style :accessor style
          :initform (cl-css:css
                     '(("#reading-line-cursor"
                        :position "absolute"
                        :top "10px"
                        :left "0"
                        :width "100%"
                        :background-color "gray"
                        :z-index "2147483647" ; 32 bit unsigned integer max
                        :opacity "15%"
                        :height "20px")))
          :documentation "The CSS applied to the reading line.")
   (constructor
    :initform
    (lambda (mode)
      (initialize-display mode)))
   (destructor
    :initform
    (lambda (mode)
      (destroy-display mode)))))

(define-command jump-to-reading-line-cursor (&key (buffer (current-buffer)))
  "Move the view port to show the reading line cursor."
  (pflet ((jump-to-cursor ()
    (ps:chain (ps:chain document (query-selector "#reading-line-cursor")) (scroll-into-view-if-needed))))
    (with-current-buffer buffer
      (jump-to-cursor))))

(define-command reading-line-cursor-up (&key (step-size 10) (buffer (current-buffer)))
  "Move the reading line cursor up. If scrolling off screen, move the
screen as well."
  (pflet ((cursor-up ()
    (let ((original-position 
            (ps:chain (parse-int
                       (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top) 10))))
      (setf (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top)
            (+ (- original-position (ps:lisp step-size)) "px")))))
    (with-current-buffer buffer
      (cursor-up)))
  (jump-to-reading-line-cursor :buffer buffer))

(define-command reading-line-cursor-down (&key (step-size 10) (buffer (current-buffer)))
  "Move the reading line cursor down. If scrolling off screen, move
the screen as well."
  (pflet ((cursor-down ()
    (let ((original-position 
            (ps:chain (parse-int
                       (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top) 10))))
      (setf (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top)
            (+ (+ original-position (ps:lisp step-size)) "px")))))
    (with-current-buffer buffer
      (cursor-down)))
  (jump-to-reading-line-cursor :buffer buffer))

(defmethod initialize-display ((mode reading-line-mode))
  (let* ((content (markup:markup
                   (:style (style mode))
                   (:span :id "reading-line-cursor" "")))
         (insert-content (ps:ps 
                           (ps:chain document body (|insertAdjacentHTML| "afterbegin" (ps:lisp content)))
                           (setf (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top) "10px"))))
    (ffi-buffer-evaluate-javascript (buffer mode) insert-content)))

(defmethod destroy-display ((mode reading-line-mode))
  (let ((destroy-content (ps:ps (setf (ps:chain document (query-selector "#reading-line-cursor") |outerHTML|) ""))))
    (ffi-buffer-evaluate-javascript (buffer mode) destroy-content)))
