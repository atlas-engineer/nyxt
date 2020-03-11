(in-package :cl-user)

(prove:plan nil)

(prove:subtest "Make key"
  (let* ((key (keymap:make-key :code 38 :value "a" :modifiers '("C")))
         (mod (first (fset:convert 'list (keymap:key-modifiers key)))))
    (prove:is (keymap:key-code key)
              38)
    (prove:is (keymap:key-value key)
              "a")
    (prove:is mod "C" :test #'keymap:modifier=)
    (prove:is mod "control" :test #'keymap:modifier=)
    (prove:is mod keymap:+control+ :test #'keymap:modifier=)
    (prove:isnt mod "" :test #'keymap:modifier=)
    (prove:isnt mod "M" :test #'keymap:modifier=)
    (prove:isnt mod "meta" :test #'keymap:modifier=)))

(prove:subtest "Make bad key"
  (prove:is-error (keymap:make-key :value "a" :status :dummy)
                  'type-error)
  (prove:is-error (keymap:make-key ::status :pressed)
                  'simple-error))

(prove:finalize)
