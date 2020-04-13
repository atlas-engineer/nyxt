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

