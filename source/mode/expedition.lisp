;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/expedition-mode
    (:documentation "Traverse a list of links."))
(in-package :nyxt/expedition-mode)

(define-mode expedition-mode ()
  "Mode for traversing a set of URLs."
  ((urls (list))
   (index 0 :documentation "The index of the current element in URLs.")
   (keyscheme-map
    (define-keyscheme-map "expedition" ()
      keyscheme:cua
      (list
       "C-]" 'expedition-next
       "C-[" 'expedition-previous)
      keyscheme:emacs
      (list
       "M-n" 'expedition-next
       "M-p" 'expedition-previous)))
   (rememberable-p nil)))

(define-command expedition-next (&key (expedition (find-submode 'expedition-mode)))
  "Go to the next URL in the expedition."
  (if (> (length (urls expedition)) (+ 1 (index expedition)))
      (progn
        (incf (index expedition))
        (buffer-load (nth (index expedition) (urls expedition))))
      (echo "End of expedition.")))

(define-command expedition-previous (&key (expedition (find-submode 'expedition-mode)))
  "Go to the previous URL in the expedition."
  (if (> (index expedition) 0)
      (progn
        (decf (index expedition))
        (buffer-load (nth (index expedition) (urls expedition))))
      (echo "Start of expedition.")))

(define-command-global select-frame-expedition (&key (buffer (current-buffer)))
  "Run an expedition through a set of URLs selected with a rectangle."
  (let* ((urls (reverse
                (prompt
                 :prompt "Start expedition with the following links"
                 :sources (list (make-instance 'nyxt/document-mode::frame-source
                                               :buffer buffer
                                               :multi-selection-p t))
                 :after-destructor
                 (lambda ()
                   (with-current-buffer buffer
                     (nyxt/document-mode::frame-element-clear))))))
         (urls (mapcar #'quri:uri urls))
         (buffer (make-buffer :title "" :url (first urls))))
    (enable (make-instance 'expedition-mode :urls urls :buffer buffer))
    (set-current-buffer buffer)))
