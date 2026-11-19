;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/expedition
  (:documentation "Package for `expedition-mode',
mode to traverse chosen URLs."))
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
       "M-n" 'expedition-next)))))

(define-command expedition-next (buffer)
  "Go to the next URL in the expedition."
  (when-let ((expedition (find-mode buffer 'expedition-mode)))
    (if (> (length (urls expedition)) (+ 1 (index expedition)))
        (progn
          (incf (index expedition))
          (ffi-buffer-load (buffer expedition)
                           (nth (index expedition) (urls expedition))))
        (echo "End of expedition."))))

(define-command expedition-previous (buffer)
  "Go to the previous URL in the expedition."
  (when-let ((expedition (find-mode buffer 'expedition-mode)))
    (if (> (index expedition) 0)
        (progn
          (decf (index expedition))
          (ffi-buffer-load (buffer expedition)
                           (nth (index expedition) (urls expedition))))
        (echo "Start of expedition."))))

(define-command-global select-frame-expedition (buffer)
  "Run an expedition through a set of URLs selected with a rectangle."
  (let* ((urls
           (reverse
            (prompt :prompt "Start expedition with the following links"
                    :sources (make-instance 'nyxt/mode/document::frame-source
                                            :buffer buffer
                                            :enable-marks-p t)
                    :after-destructor
                    (lambda ()
                      (nyxt/mode/document::frame-element-clear buffer)))))
         (urls (mapcar #'quri:uri urls))
         (buffer (make-buffer :title "" :url (first urls))))
    (enable (make-instance 'expedition-mode :urls urls :buffer buffer))
    (set-current-buffer buffer)))
