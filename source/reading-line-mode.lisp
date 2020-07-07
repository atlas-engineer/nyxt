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
       "M-up" 'line-cursor-up
       "M-down" 'line-cursor-down)
      scheme:emacs
      (list
       "M-p" 'line-cursor-up
       "M-n" 'line-cursor-down)))
   (style :accessor style
          :initform (cl-css:css
                     '(("#reading-line-cursor"
                        :position "absolute"
                        :top "10px"
                        :left "0"
                        :width "100%"
                        :background-color "gray"
                        :z-index "2147483647"
                        :opacity "10%"
                        :height "20px")))
          :documentation "The CSS applied to the reading line.")
   (constructor
    :initform
    (lambda (mode)
      (initialize-display mode)))))

(define-command line-cursor-up (&key (step-size 10) (buffer (current-buffer)))
  "Move the reading line cursor up. If scrolling off screen, move the
screen as well."
  (pflet ((cursor-up ()
    (let ((original-position 
            (ps:chain (parse-int
                       (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top) 10))))
      (setf (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top)
            (+ (- original-position (ps:lisp step-size)) "px")))))
    (with-current-buffer buffer
      (cursor-up))))

(define-command line-cursor-down (&key (step-size 10) (buffer (current-buffer)))
  "Move the reading line cursor down. If scrolling off screen, move
the screen as well."
  (pflet ((cursor-down ()
    (let ((original-position 
            (ps:chain (parse-int
                       (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top) 10))))
      (setf (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top)
            (+ (+ original-position (ps:lisp step-size)) "px")))))
    (with-current-buffer buffer
      (cursor-down))))

(defmethod initialize-display ((mode reading-line-mode))
  (let* ((content (markup:markup
                   (:style (style mode))
                   (:span :id "reading-line-cursor" "")))
         (insert-content (ps:ps 
                           (ps:chain document body (|insertAdjacentHTML| "afterbegin" (ps:lisp content)))
                           (setf (ps:@ (ps:chain document (query-selector "#reading-line-cursor")) style top) "10px"))))
    (ffi-buffer-evaluate-javascript (buffer mode) insert-content)))
