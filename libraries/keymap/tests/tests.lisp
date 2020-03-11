(in-package :cl-user)

(prove:plan nil)

(prove:subtest "Make key"
  (let* ((key (keymap:make-key :code 38 :value "a" :modifiers '("C")))
         (mod (first (fset:convert 'list (keymap:key-modifiers key)))))
    (prove:is (keymap:key-code key)
              38)
    (prove:is (keymap:key-value key)
              "a")
    (prove:is (keymap:modifier= mod "C")
              t)
    (prove:is (keymap:modifier= mod "control")
              t)
    (prove:is (keymap:modifier= mod keymap:+control+)
              t)))

(prove:subtest "Make bad key"
  (prove:is-error (keymap:make-key :value "a" :status :dummy)
                  'type-error)
  (prove:is-error (keymap:make-key ::status :pressed)
                  'simple-error))

(prove:finalize)
