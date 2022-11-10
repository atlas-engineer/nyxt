;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/reading-line-mode
    (:documentation "Mode for drawing a line to keep track of the reading position."))
(in-package :nyxt/reading-line-mode)

(define-mode reading-line-mode ()
  "Mode for drawing a line on screen that you can use to keep track of
your reading position. To use this mode, first enable this mode and
then use the bindings for `reading-line-cursor-up' and
`reading-line-cursor-down' to move the reading line cursor. If you
navigate away from the reading line, you can always invoke the command
`jump-to-reading-line-cursor' to jump back to your reading
position. To remove the reading line from the screen, disable this
mode."
  ((rememberable-p nil)
   (visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "reading-line-mode" ()
      keyscheme:cua
      (list
       "M-up" 'reading-line-cursor-up
       "M-down" 'reading-line-cursor-down)
      keyscheme:emacs
      (list
       "M-p" 'reading-line-cursor-up
       "M-n" 'reading-line-cursor-down)
      keyscheme:vi-normal
      (list
       "K" 'reading-line-cursor-up
       "J" 'reading-line-cursor-down)))
   (style (theme:themed-css (theme *browser*)
            ("#reading-line-cursor"
             :position "absolute"
             :top "10px"
             :left "0"
             :width "100%"
             :background-color theme:primary
             :z-index #.(1- (expt 2 31)) ; 32 bit signed integer max
             :opacity "15%"
             :height "20px"))
          :documentation "The CSS applied to the reading line.")))

(define-command jump-to-reading-line-cursor (&key (buffer (current-buffer)))
  "Move the view port to show the reading line cursor."
  (ps-eval :buffer buffer
    (ps:chain (nyxt/ps:qs document "#reading-line-cursor") (scroll-into-view-if-needed))))

(define-command reading-line-cursor-up (&key (step-size 20) (buffer (current-buffer)))
  "Move the reading line cursor up. If scrolling off screen, move the
screen as well."
  (ps-eval :buffer buffer
    (let ((original-position
            (ps:chain (parse-int
                       (ps:@ (nyxt/ps:qs document "#reading-line-cursor") style top) 10))))
      (setf (ps:@ (nyxt/ps:qs document "#reading-line-cursor") style top)
            (+ (- original-position (ps:lisp step-size)) "px"))))
  (jump-to-reading-line-cursor :buffer buffer))

(define-command reading-line-cursor-down (&key (step-size 20) (buffer (current-buffer)))
  "Move the reading line cursor down. If scrolling off screen, move the screen as well."
  (ps-eval :buffer buffer
    (let ((original-position
            (ps:chain (parse-int
                       (ps:@ (nyxt/ps:qs document "#reading-line-cursor") style top) 10))))
      (setf (ps:@ (nyxt/ps:qs document "#reading-line-cursor") style top)
            (+ (+ original-position (ps:lisp step-size)) "px"))))
  (jump-to-reading-line-cursor :buffer buffer))

(defmethod on-signal-load-finished ((mode reading-line-mode) url)
  (declare (ignore url))
  (enable mode))

(defmethod enable ((mode reading-line-mode) &key)
  (let ((content (spinneret:with-html-string
                   (:style (style mode))
                   (:span :id "reading-line-cursor" ""))))
    (ps-eval :async t :buffer (buffer mode)
      (ps:chain document body (|insertAdjacentHTML| "afterbegin" (ps:lisp content)))
      (setf (ps:@ (nyxt/ps:qs document "#reading-line-cursor") style top) "10px"))))

(defmethod disable ((mode reading-line-mode) &key)
  (ps-eval :async t :buffer (buffer mode)
    (setf (ps:@ (nyxt/ps:qs document "#reading-line-cursor") |outerHTML|)
          "")))
