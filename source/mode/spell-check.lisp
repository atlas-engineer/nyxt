;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/spell-check-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:import-from #:class-star #:define-class)
  (:documentation "Mode to spell-check text in buffers."))
(in-package :nyxt/spell-check-mode)

(define-mode spell-check-mode ()
  ""
  ((rememberable-p t)
   (spell-check-language
    "en_US"
    :documentation "Spell check language used by Nyxt. For
a list of more languages available, see `enchant:broker-list-dicts'.")
   (keymap-scheme
    (define-scheme "visual"
      scheme:cua
      (list)                            ; TODO: Add CUA bindings!
      scheme:emacs
      (list
       "M-$" 'spell-check-word)
      scheme:vi-normal
      (list
       "z =" 'spell-check-word)))))

(define-command spell-check-word (&key (word nil word-supplied-p))
  "Spell check a word."
  (if word-supplied-p
      (enchant:with-dict (lang (spell-check-language *browser*))
        (enchant:dict-check lang word))
      (let ((word (prompt1
                    :prompt "Spell check word"
                    :sources (make-instance 'prompter:raw-source))))
        (if (enchant:with-dict (lang (spell-check-language *browser*))
              (enchant:dict-check lang word))
            (echo "~s spelled correctly." word)
            (echo "~s is incorrect." word)))))

(define-command spell-check-highlighted-word ()
  "Spell check a highlighted word. If a word is incorrectly spelled,
pull up a prompt of suggestions."
  (let ((word (%copy)))
    (spell-check-prompt word)))

(defun spell-check-prompt (word)
  "Spell check `word', if incorrectly spelled, prompt the user with
suggestions."
  (if (spell-check-word :word word)
      (echo "Highlighted word ~s spelled correctly." word)
      (progn (echo "Highlighted word ~s spelled incorrectly." word)
             (spell-check-suggest-word :word word))))

(define-command spell-check-word-at-cursor ()
  "Spell check the word at the cursor."
  (let* ((contents (nyxt/input-edit-mode::active-input-area-content))
         (cursor-position (truncate (nyxt/input-edit-mode::active-input-area-cursor))))
    (let ((text-buffer (make-instance 'text-buffer:text-buffer))
          (cursor (make-instance 'text-buffer:cursor)))
      (cluffer:attach-cursor cursor text-buffer)
      (text-buffer::insert-string cursor contents)
      (setf (cluffer:cursor-position cursor)
            cursor-position)
      (spell-check-prompt (text-buffer::word-at-cursor cursor)))))

(define-command spell-check-suggest-word (&key word)
  "Suggest a spelling for a given word."
  (let ((selected-word (prompt1
                         :input word
                         :prompt "Suggest spelling (3+ characters)"
                         :sources (make-instance 'enchant-source))))
    (trivial-clipboard:text selected-word)
    (echo "Word copied to clipboard.")))

(define-class enchant-source (prompter:source)
  ((case-sensitive-p nil)
   (prompter:name "Enchant")
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions source))
      (when (> (length input) 2)
        (enchant:with-dict (lang (spell-check-language *browser*))
          (enchant:dict-suggest lang input)))))))

(define-command spell-check-list-languages ()
  "List all languages supported on your machine."
  (echo "Supported languages: ~s"
        (mapcar #'first (enchant:with-broker bkr
                          (enchant:broker-list-dicts bkr)))))

(defun spell-check-and-suggest (word)
  "Only suggest if `word' is incorrect."
  (enchant:with-dict (lang (spell-check-language *browser*))
    (let ((result (enchant:dict-check lang word)))
      (or result
          (enchant:dict-suggest lang word)))))

(define-command spell-check-text-input (&key text)
  "Spell check full text input provided by the user."
  (let ((selected-text (prompt :input text
                               :prompt "Suggest spelling"
                               :sources (make-instance 'enchant-text-input))))
    (trivial-clipboard:text selected-text)
    (echo "Text copied to clipboard.")))

(define-class enchant-text-input (prompter:source)
  ((case-sensitive-p nil)
   (minimum-search-length 3)
   (prompter:name "Enchant for text")
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions text input)
      (declare (ignore preprocessed-suggestions))
      (when (>= (length input) (slot-value text 'minimum-search-length))
        (enchant:with-dict (lang (spell-check-language *browser*))
          (mapcar #'spell-check-and-suggest (str:words input))))))))
