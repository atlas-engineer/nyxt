;;; spell-check.lisp --- functions to enable spell checking

(in-package :next)

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
  "Spell check a highlighted word."
  (with-result (word (%copy))
    (if (spell-check-word :word word)
        (echo "Highlighted word: ~a, spelled correctly." word)
        (echo "Highlighted word: ~a, spelled incorrectly." word))))

(define-command spell-check-suggest-word ()
  "Suggest a spelling for a given word."
  (with-result (selected-word (read-from-minibuffer
                               (make-minibuffer
                                :input-prompt "Suggest spelling (3+ characters)"
                                :completion-function 'enchant-completion)))
    selected-word))

(defun enchant-completion (input)
  (when (> (length input) 2)
    (enchant:with-dict (lang (spell-check-language *browser*))
      (enchant:dict-suggest lang input))))
