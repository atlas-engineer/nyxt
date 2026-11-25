;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test buffer-table ()
  (let* ((*browser* (make-instance 'browser))
         (first-buffer (make-instance 'buffer :id 1))
         (second-buffer (make-instance 'buffer :id 2)))
    (nyxt::buffer-set 1 first-buffer)
    (nyxt::buffer-set 2 second-buffer)
    (assert-eq first-buffer (nyxt::buffer-get 1))
    (assert-eq second-buffer (nyxt::buffer-get 2))
    ;; Higher ids come first.
    (assert-equal (list second-buffer first-buffer) (buffer-list))
    (nyxt::buffer-delete 1)
    (assert-false (nyxt::buffer-get 1))
    (assert-equal (list second-buffer) (buffer-list))
    (assert-true (nyxt::dead-buffer-p first-buffer))
    (assert-false (nyxt::dead-buffer-p second-buffer))))

(define-test panel-buffer-base-mode ()
  (let* ((*browser* (make-instance 'browser))
         (panel (make-instance 'panel-buffer)))
    (unwind-protect
         (progn
           (assert-equal '(panel-mode) (default-modes panel))
           (assert-true (find-mode panel 'panel-mode))
           (assert-false (find-mode panel 'base-mode))
           (assert-equal "unbound" (nyxt::binding-keys 'set-url :buffer panel))
           (assert-equal "Shift+=" (nyxt::binding-keys 'widen-panel :buffer panel))
           (assert-equal "hyphen" (nyxt::binding-keys 'narrow-panel :buffer panel))
           (assert-equal "Ctrl+w" (nyxt::binding-keys 'delete-panel-buffer :buffer panel))
           (assert-equal "Ctrl+space" (nyxt::binding-keys 'execute-command :buffer panel))
           (assert-eq 'focus-buffer-left
                      (keymaps:lookup-key
                       "shift-left"
                       (keymaps:get-keymap keyscheme:cua
                                           (keyscheme-map (find-mode panel 'panel-mode))))))
      (nyxt::buffer-delete (id panel)))))

(define-test auxiliary-buffers-have-focus-move-mode ()
  (let ((*browser* (make-instance 'browser)))
    (dolist (class '(status-buffer message-buffer))
      (let ((buffer (make-instance class)))
        (assert-true (modable-buffer-p buffer))
        (assert-true (auxiliary-buffer-p buffer))
        (assert-true (find-mode buffer 'focus-move-mode))
        (assert-false (find-mode buffer 'base-mode))
        (assert-false (find-mode buffer 'nyxt/mode/document:document-mode))))))

(define-test focus-buffer-direction ()
  (let* ((left (nyxt::make-screen-rect :buffer :left :x 0 :y 0 :width 100 :height 800))
         (right (nyxt::make-screen-rect :buffer :right :x 700 :y 0 :width 100 :height 800))
         (status (nyxt::make-screen-rect :buffer :status :x 0 :y 400 :width 800 :height 57))
         (message (nyxt::make-screen-rect :buffer :message :x 0 :y 457 :width 800 :height 16))
         (rects (nyxt::layout-screen-rects :page 800 473
                                           :left-panels (list left)
                                           :right-panels (list right)
                                           :bars (list status message))))
    (labels ((named (name)
               (find name rects :key #'nyxt::screen-rect-buffer))
             (toward (origin direction)
               (nyxt::screen-rect-buffer
                (nyxt::screen-rect-in-direction (named origin) rects direction))))
      (assert-eql 100 (nyxt::screen-rect-x (named :page)))
      (assert-eql 600 (nyxt::screen-rect-width (named :page)))
      (assert-eql 400 (nyxt::screen-rect-height (named :page)))
      (assert-eql 400 (nyxt::screen-rect-height (named :left)))
      (assert-eq :left (toward :page :left))
      (assert-eq :right (toward :page :right))
      (assert-eq :status (toward :page :down))
      (assert-false (nyxt::screen-rect-in-direction (named :page) rects :up))
      (assert-eq :page (toward :left :right))
      (assert-eq :page (toward :status :up))
      (assert-eq :message (toward :status :down))
      (assert-eq :status (toward :left :down))))
  (let* ((page (nyxt::make-screen-rect :buffer :page :x 0 :y 0 :width 800 :height 400))
         (prompt (nyxt::make-screen-rect :buffer :prompt :x 0 :y 420 :width 800 :height 40)))
    (assert-eq :prompt
               (nyxt::screen-rect-buffer
                (nyxt::screen-rect-in-direction page (list page prompt) :down))))
  (let* ((origin (nyxt::make-screen-rect :buffer :origin :x 0 :y 0 :width 100 :height 100))
         (near (nyxt::make-screen-rect :buffer :near :x 200 :y 0 :width 50 :height 100))
         (far (nyxt::make-screen-rect :buffer :far :x 400 :y 0 :width 50 :height 100)))
    (assert-eq :near
               (nyxt::screen-rect-buffer
                (nyxt::screen-rect-in-direction origin (list origin near far) :right)))))
