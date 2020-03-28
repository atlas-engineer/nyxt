(in-package :cl-user)

(prove:plan nil)

(defun void-function ()
  (format t "Void handler"))

(defvar test-hook (hooks:make-hook-void))
(hooks:add-hook test-hook
                     (hooks:make-handler-void #'void-function))

(prove:subtest "Run default void hook"
  (prove:is (length (hooks:handlers test-hook))
            1)
  (prove:is (hooks:run-hook test-hook)
            '(nil)))

(defun add1 (n)
  (1+ n))

(declaim (ftype (function (number) number) mul2))
(defun mul2 (n)
  (* 2 n))

(prove:subtest "Run default numeric hook"
  (prove:is (hooks:run-hook
             (hooks:make-hook-number->number :handlers (list #'add1))
             17)
            '(18)))

(prove:subtest "Run default numeric hook with multiple handlers"
  (prove:is (hooks:run-hook
             (hooks:make-hook-number->number
                            :handlers (list #'add1
                                            (hooks:make-handler-number->number
                                             (lambda (n) (* 2 n))
                                             :name 'mul2)))
             17)
            '(18 34)))

(prove:subtest "Don't add duplicate handlers."
  (prove:is (let ((hook (hooks:make-hook-number->number :handlers (list #'add1))))
              (hooks:add-hook hook (hooks:make-handler-number->number #'add1))
              (hooks:run-hook hook 17))
            '(18))
  (prove:is (let ((hook (hooks:make-hook-number->number :handlers (list #'add1))))
              (hooks:add-hook hook (hooks:make-handler-number->number (lambda (n) (+ 1 n)) :name 'add1))
              (hooks:run-hook hook 17))
            '(18)))

(prove:subtest "Combine handlers"
  (prove:is (let ((hook (hooks:make-hook-number->number
                         :handlers (list #'add1 #'mul2)
                         :combination #'hooks:combine-composed-hook)))
              (hooks:run-hook hook 17))
            35)
  (prove:is (let ((hook (hooks:make-hook-number->number
                         :combination #'hooks:combine-composed-hook)))
              (hooks:run-hook hook 17))
            17))

(prove:subtest "Remove handler from hook"
  (prove:is (let* ((handler1 (hooks:make-handler-number->number #'add1))
                   (hook (hooks:make-hook-number->number
                          :handlers (list handler1
                                          (hooks:make-handler-number->number (lambda (n) (* 3 n)) :name 'mul3)))))
              (hooks:remove-hook hook 'mul3)
              (hooks:remove-hook hook handler1)
              (hooks:run-hook hook 17))
            nil))

(prove:subtest "Disable hook"
  (let* ((handler1 (hooks:make-handler-number->number #'add1))
         (handler2 (hooks:make-handler-number->number #'mul2)))
    (prove:is (let* ((hook (hooks:make-hook-number->number
                            :handlers (list handler1
                                            (hooks:make-handler-number->number (lambda (n) (* 3 n)) :name 'mul3)))))
                (hooks:disable-hook hook)
                (length (hooks:disabled-handlers hook)))
              2)
    (prove:is (let* ((hook
                      (hooks:make-hook-number->number
                       :handlers (list (hooks:make-handler-number->number (lambda (n) (* 3 n)) :name 'mul3)))))
                (hooks:disable-hook hook)
                (hooks:add-hook hook handler1)
                (hooks:disable-hook hook)
                (eq (first (hooks:disabled-handlers hook))
                    handler1))
              t)
    (prove:is (let* ((hook
                       (hooks:make-hook-number->number
                        :handlers (list handler1
                                        (hooks:make-handler-number->number (lambda (n) (* 3 n)) :name 'mul3)))))
                (hooks:disable-hook hook)
                (hooks:enable-hook hook)
                (length (hooks:disabled-handlers hook)))
              0)
    (prove:is (let* ((hook
                       (hooks:make-hook-number->number
                        :handlers (list handler1 handler2))))
                (hooks:disable-hook hook handler1)
                (list (first (hooks:handlers hook))
                      (first (hooks:disabled-handlers hook))))
              (list handler2 handler1))))

(prove:subtest "Don't accept lambdas without names."
  (prove:is-error (hooks:make-handler-number->number (lambda (n) (+ 1 n)))
                  'simple-error))

(prove:subtest "Global hooks"
  (prove:is (let ((hook (hooks:define-hook 'hooks:hook-number->number 'foo)))
              (eq hook (hooks:find-hook 'foo)))
            t)
  (let ((hook (hooks:define-hook 'hooks:hook-number->number 'foo)))
    (prove:is (hooks:find-hook 'foo)
              hook))
  (let ((hook (hooks:define-hook 'hooks:hook-number->number 'foo
                :object #'mul2)))
    (prove:isnt (hooks:find-hook 'foo)
                hook))
  (let ((hook (hooks:define-hook 'hooks:hook-number->number 'foo
                :object #'mul2)))
    (prove:is (hooks:find-hook 'foo #'mul2)
              hook)))

(prove:subtest "Find handler"
  (let* ((add-handler (hooks:make-handler-number->number #'add1))
         (mul-handler (hooks:make-handler-number->number #'mul2))
         (other-handler (hooks:make-handler-void #'void-function))
         (handlers (list add-handler mul-handler)))
    (prove:is (hooks:find-handler 'add1 handlers)
              add-handler)
    (prove:is (hooks:find-handler mul-handler handlers)
              mul-handler)
    (prove:is (hooks:find-handler other-handler handlers)
              nil)))

;; TODO: Test that make-handler-* raise a warning when passed a function with the wrong type.
;; Example: (hooks:make-handler-string->string #'mul2)

;; TODO: Test that functions can be redefined.

(prove:finalize)
