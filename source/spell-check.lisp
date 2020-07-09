(in-package :nyxt/web-mode)

(define-command spell-check-word (&key (word nil word-supplied-p))
  "Spell check a word."
    (if word-supplied-p
        (enchant:with-dict (lang (spell-check-language *browser*))
          (enchant:dict-check lang word))
        (with-result (word (read-from-minibuffer
                            (make-minibuffer
                             :input-prompt "Spell check word")))
          (if (enchant:with-dict (lang (spell-check-language *browser*))
                (enchant:dict-check lang word))
              (echo "~a spelled correctly." word)
              (echo "~a is incorrect." word)))))

(define-command spell-check-highlighted-word ()
  "Spell check a highlighted word. If a word is incorrectly spelled,
pull up a prompt of suggestions."
  (with-result (word (%copy))
    (spell-check-prompt word)))

(defun spell-check-prompt (word)
  "Spell check `word', if incorrectly spelled, prompt the user with
suggestions."
  (if (spell-check-word :word word)
        (echo "Highlighted word: ~a, spelled correctly." word)
        (progn (echo "Highlighted word: ~a, spelled incorrectly." word)
               (spell-check-suggest-word :word word))))

(define-parenscript active-input-area-content ()
  (ps:chain document active-element value))

(define-parenscript active-input-area-cursor ()
  (ps:chain document active-element selection-start))

(define-command spell-check-suggest-word (&key word)
  "Suggest a spelling for a given word."
  (with-result (selected-word (read-from-minibuffer
                               (make-minibuffer
                                :input-buffer word
                                :input-prompt "Suggest spelling (3+ characters)"
                                :suggestion-function 'enchant-suggestion)))
    (trivial-clipboard:text selected-word)
    (echo "Word copied to clipboard.")))

(defun enchant-suggestion (minibuffer)
  (let ((input (input-buffer minibuffer)))
    (when (> (length input) 2)
      (enchant:with-dict (lang (spell-check-language *browser*))
        (enchant:dict-suggest lang input)))))
