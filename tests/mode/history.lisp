;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-history-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (assert-true (enable-modes* 'nyxt/mode/history:history-mode buffer))
    (assert-true (disable-modes* 'nyxt/mode/history:history-mode buffer))))

(define-test history-blocklist-and-record ()
  (let* ((*browser* (make-instance 'browser
                                   :history-file
                                   (make-instance 'history-file
                                                  :profile (make-instance 'files:virtual-profile))))
         (buffer (make-instance 'modable-buffer)))
    (enable-modes* 'nyxt/mode/history:history-mode buffer)
    (let ((mode (find-mode buffer 'nyxt/mode/history:history-mode)))
      (setf (nyxt/mode/history:history-blocklist mode)
            '("https://blocked.example"))
      (assert-true (nyxt/mode/history:blocked-p
                    (quri:uri "https://blocked.example/x") mode))
      (assert-false (nyxt/mode/history:blocked-p
                     (quri:uri "https://ok.example") mode))
      (assert-equality #'quri:uri=
                       (quri:uri "https://ok.example")
                       (nyxt/mode/history::add-url-to-history
                        (quri:uri "https://ok.example") mode :title "OK"))
      (assert-false (nyxt/mode/history::add-url-to-history
                     (quri:uri "https://blocked.example/x") mode :title "No"))
      (assert-eq 1 (length (history-vector *browser*)))
      (assert-string= "OK" (title (aref (history-vector *browser*) 0)))
      (assert-eq 1 (length (nyxt/mode/history:visits mode)))
      (assert-string= "OK" (title (aref (nyxt/mode/history:visits mode) 0))))))

(defun history-binding (keyscheme keyspec)
  (let ((mode (make-instance 'nyxt/mode/history:history-mode
                             :buffer (make-instance 'modable-buffer))))
    (keymaps:lookup-key keyspec
                        (keymaps:get-keymap keyscheme
                                            (keyscheme-map mode)))))

(define-test list-history-follows-the-browser ()
  (let* ((*browser* (make-instance 'browser
                                   :history-file
                                   (make-instance 'history-file
                                                  :profile (make-instance 'files:virtual-profile))))
         (buffer (make-instance 'web-buffer)))
    (enable-modes* 'nyxt/mode/history:history-mode buffer)
    (enable-modes* 'nyxt/mode/history:history-listing-mode buffer)
    (let ((history (find-mode buffer 'nyxt/mode/history:history-mode))
          (mode (find-mode buffer 'nyxt/mode/history:history-listing-mode)))
      (flet ((page ()
               (spinneret:with-html-string
                 (user-interface:to-html
                  (nyxt/mode/history::listing mode)))))
        (assert-eq :p (user-interface:tag (nyxt/mode/history::listing mode)))
        (assert-true (search "No history" (page)))
        (nyxt/mode/history::add-url-to-history
         (quri:uri "https://example.com/a") history :title "Alpha")
        (assert-eq :ul (user-interface:tag (nyxt/mode/history::listing mode)))
        (assert-true (search "Alpha" (page)))
        (assert-true (search "https://example.com/a" (page)))
        (nyxt::buffer-delete (id buffer))
        (nyxt/mode/history::add-url-to-history
         (quri:uri "https://example.com/b") history :title "Gamma")
        (assert-false (search "Gamma" (page)))))))

(define-test history-row-in-a-panel-opens-the-page ()
  (let* ((*browser* (make-instance 'browser))
         (entry (make-instance 'history-entry
                               :url "https://example.com/a"
                               :title "Alpha"))
         (panel (make-instance 'panel-buffer))
         (page (make-instance 'web-buffer))
         (panel-html (spinneret:with-html-string
                       (user-interface:to-html
                        (nyxt/mode/history::history-row entry panel))))
         (page-html (spinneret:with-html-string
                      (user-interface:to-html
                       (nyxt/mode/history::history-row entry page)))))
    (assert-true (search "Alpha" panel-html))
    (assert-true (search "href=#" panel-html))
    (assert-true (search "open-history" panel-html))
    (assert-true (search "href=\"https://example.com/a\"" page-html))
    (assert-false (search "open-history" page-html))
    (assert-true (fboundp 'nyxt/mode/history:add-history-panel))))

(define-test buffer-history-panel-follows-the-current-buffer ()
  (let* ((*browser* (make-instance 'browser
                                   :history-file
                                   (make-instance 'history-file
                                                  :profile (make-instance 'files:virtual-profile))))
         (alpha (make-instance 'web-buffer))
         (beta (make-instance 'web-buffer))
         (panel (make-instance 'panel-buffer :title "Buffer history"))
         (window (make-instance 'window :buffer alpha)))
    (setf (window panel) window)
    (enable-modes* 'nyxt/mode/history:history-mode alpha)
    (enable-modes* 'nyxt/mode/history:history-mode beta)
    (enable-modes* 'nyxt/mode/history:buffer-history-listing-mode panel)
    (let ((alpha-history (find-mode alpha 'nyxt/mode/history:history-mode))
          (beta-history (find-mode beta 'nyxt/mode/history:history-mode))
          (mode (find-mode panel 'nyxt/mode/history:buffer-history-listing-mode)))
      (flet ((page ()
               (spinneret:with-html-string
                 (user-interface:to-html
                  (nyxt/mode/history::listing mode)))))
        (assert-eq :p (user-interface:tag (nyxt/mode/history::listing mode)))
        (assert-true (search "No history" (page)))
        (nyxt/mode/history::add-url-to-history
         (quri:uri "https://alpha.example/a") alpha-history :title "Alpha")
        (nyxt/mode/history::add-url-to-history
         (quri:uri "https://beta.example/b") beta-history :title "Beta")
        (assert-true (search "Alpha" (page)))
        (assert-false (search "Beta" (page)))
        (assert-eq 2 (length (history-vector *browser*)))
        (setf (slot-value window 'buffer) beta)
        (nyxt/mode/history::render-buffer-history mode)
        (assert-true (search "Beta" (page)))
        (assert-false (search "Alpha" (page)))
        (nyxt/mode/history::add-url-to-history
         (quri:uri "https://beta.example/c") beta-history :title "Beta two")
        (assert-true (search "Beta two" (page)))
        (nyxt/mode/history::add-url-to-history
         (quri:uri "https://alpha.example/d") alpha-history :title "Alpha two")
        (assert-false (search "Alpha two" (page)))))
    (assert-true (fboundp 'nyxt/mode/history:add-buffer-history-panel))))

(define-test history-mode-keyscheme-bindings ()
  (assert-eq 'nyxt/mode/history:history-backwards
             (history-binding keyscheme:default "M-left"))
  (assert-eq 'nyxt/mode/history:history-forwards
             (history-binding keyscheme:default "M-right"))
  (assert-eq 'nyxt/mode/history:history-backwards
             (history-binding keyscheme:default "M-["))
  (assert-eq 'nyxt/mode/history:history-forwards
             (history-binding keyscheme:default "M-]"))
  (assert-eq 'nyxt/mode/history:history-backwards
             (history-binding keyscheme:emacs "C-b"))
  (assert-eq 'nyxt/mode/history:history-forwards
             (history-binding keyscheme:emacs "C-f"))
  (assert-eq 'nyxt/mode/history:history-backwards
             (history-binding keyscheme:vi-normal "H"))
  (assert-eq 'nyxt/mode/history:history-forwards
             (history-binding keyscheme:vi-normal "L")))
