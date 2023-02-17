;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/spell-check-mode
    (:documentation "Mode to spell-check text in buffers."))
(in-package :nyxt/spell-check-mode)

(define-mode spell-check-mode ()
  ""
  ((visible-in-status-p nil)
   (rememberable-p t)
   (spell-check-language
    "en_US"
    :documentation "Spell check language used by Nyxt.
For a list of more languages available, see `spell-check-list-languages'.")
   (keyscheme-map
    (define-keyscheme-map "spell-check-mode" ()
      keyscheme:cua
      (list)                            ; TODO: Add CUA bindings!
      keyscheme:emacs
      (list
       "M-$" 'spell-check-word)
      keyscheme:vi-normal
      (list
       "z =" 'spell-check-word)))))

(defmacro with-spell-check ((variable) &body body)
  `(enchant:with-dict (,variable (spell-check-language
                                  (find-submode 'spell-check-mode)))
     ,@body))

(defun spell-dict-check-p (word)
  "Spell check `word' and return if correct or not."
  (with-spell-check (lang)
    (enchant:dict-check lang word)))

(define-command spell-check-word (&key (word nil word-supplied-p))
  "Spell check a word."
  (let ((word (or (and word-supplied-p word)
                  (prompt1
                   :prompt "Spell check word"
                   :sources 'prompter:raw-source))))
    (if (spell-dict-check-p word)
        (echo "~s is spelled correctly." word)
        (echo "~s is NOT correctly spelled." word))))

(define-command spell-check-highlighted-word ()
  "Spell check a highlighted word. If a word is incorrectly spelled,
pull up a prompt of suggestions."
  (let ((word (ffi-buffer-copy (current-buffer))))
    (if (str:blankp word)
        (echo "No word highlighted to spell check!")
        (spell-check-prompt word))))

(defun spell-check-prompt (word)
  "Spell check `word', if incorrectly spelled, prompt the user with
suggestions."
  (if (spell-dict-check-p word)
      (echo "Word ~s spelled correctly." word)
      (progn
        (echo "Word ~s spelled incorrectly." word)
        (spell-check-suggest-word :word word))))

(define-command spell-check-word-at-cursor ()
  "Spell check the word at the cursor."
  (nyxt/input-edit-mode:with-input-area (contents cursor-pos)
    (nyxt/input-edit-mode:with-text-buffer (text-buffer cursor contents cursor-pos)
      (spell-check-prompt (text-buffer::word-at-cursor cursor)))))

(define-command spell-check-suggest-word (&key word)
  "Suggest a spelling for a given word."
  (let ((selected-word (prompt1
                        :prompt "Suggest spelling (3+ characters)"
                        :input word
                        :sources 'enchant-source)))
    (trivial-clipboard:text selected-word)
    (echo "Word saved to clipboard.")))

(define-class enchant-source (prompt-source)
  ((case-sensitive-p nil)
   (prompter:name "Enchant")
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions source))
      (when (> (length input) 2)
        (with-spell-check (lang)
          (enchant:dict-suggest lang input)))))))

(define-command spell-check-list-languages ()
  "List all languages supported on your machine."
  (echo "Supported languages: ~s"
        (mapcar #'first (enchant:with-broker bkr
                          (enchant:broker-list-dicts bkr)))))

(defun spell-check-and-suggest (word)
  "Only suggest if `word' is incorrect."
  (with-spell-check (lang)
    (let ((result (enchant:dict-check lang word)))
      (or result
          (enchant:dict-suggest lang word)))))

(define-command spell-check-text-input (&key text)
  "Spell check full text input provided by the user."
  (let ((selected-text (prompt :prompt "Suggest spelling"
                               :input text
                               :sources 'enchant-text-input)))
    (trivial-clipboard:text selected-text)
    (echo "Text saved to clipboard.")))

(define-class enchant-text-input (prompt-source)
  ((case-sensitive-p nil)
   (minimum-search-length 3)
   (prompter:name "Enchant for text")
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions text input)
      (declare (ignore preprocessed-suggestions))
      (when (>= (length input) (slot-value text 'minimum-search-length))
        (mapcar #'spell-check-and-suggest (str:words input)))))))
