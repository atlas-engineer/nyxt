;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/expedition
  (:documentation "Package for `expedition-mode', mode to traverse chosen URLs."))
(in-package :nyxt/mode/expedition)

(define-mode expedition-mode ()
  "Mode to traverse URLs delimited by a user specified buffer rectangle."
  ((urls (list))
   (index
    0
    :documentation "The index of the current element in URLs.")
   (keyscheme-map
    (define-keyscheme-map "expedition-mode" ()
      keyscheme:cua
      (list
       "C-[" 'expedition-previous
       "C-]" 'expedition-next)
      keyscheme:emacs
      (list
       "M-p" 'expedition-previous
       "M-n" 'expedition-next)))
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
  (let* ((urls (reverse (prompt :prompt "Start expedition with the following links"
                                :sources (make-instance 'nyxt/mode/document::frame-source
                                                        :buffer buffer
                                                        :enable-marks-p t)
                                :after-destructor
                                (lambda ()
                                  (with-current-buffer buffer
                                    (nyxt/mode/document::frame-element-clear))))))
         (urls (mapcar #'quri:uri urls))
         (buffer (make-buffer :title "" :url (first urls))))
    (enable (make-instance 'expedition-mode :urls urls :buffer buffer))
    (nyxt::remember-on-mode-toggle (list 'expedition-mode) buffer :enabled-p t)
    (set-current-buffer buffer)))
