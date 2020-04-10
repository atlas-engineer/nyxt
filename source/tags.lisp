(in-package :next)

(defun tag-specification-validator (s-exp)
  "Return a predicate validating the S-EXP tag specification for its argument.
S-EXP is an expression produced by `parse-tag-specification'.
Tags can be keywords, symbols or strings.

Supported operators:

- 'and: all tag selector arguments must be true.
- 'or: one of the tag selector arguments must be true.
- 'not: the single tag selector argument must be false.

Example:
  (funcall (tag-specification-validator (parse-tag-specification \"...\"))
           '(tag1 tag2...))

Throw a simpler error if the s-exp is not valid.
"
  (match s-exp
    (nil nil)
    ((guard s (symbolp s))
     (let ((s-name (symbol-name s)))
       (funcall
        (if (str:starts-with? "-" s-name)
            #'complement
            #'identity)
        (lambda (tags)
          (member (if (or (str:starts-with? "-" s-name)
                          (str:starts-with? "+" s-name))
                      (subseq s-name 1)
                      s-name)
                  tags
                  :test #'string-equal )))))
    ((cons op rest)
     (match op
       ('and
        (let ((sub-validators (mapcar #'tag-specification-validator rest)))
          (lambda (tags)
            (not (find-if (complement
                           (lambda (f)
                             (funcall f tags)))
                          sub-validators)))))
       ('or
        (let ((sub-validators (mapcar #'tag-specification-validator rest)))
          (lambda (tags)
            (find-if (lambda (f) (funcall f tags))
                     sub-validators))))
       ('not
        (if (= 1 (length rest))
            (let ((sub-validator (tag-specification-validator (first rest))))
              (lambda (tags)
                (not (funcall sub-validator tags))))
            (error "NOT takes 1 argument, got ~a" (length rest))))
       (_ (error "Operator ~a not allowed" op))))
    (_ (error "Do not know how to parse ~a" s-exp))))

(declaim (ftype (function (string)) parse-tag-specification))
(defun parse-tag-specification (input)
  "Return a s-exp from a tag specification string.
The substrings not part of the specification are returns as a list in the second
return value.

The s-exp can then be consumed by `tag-specification-validator'.
The input string is scanned for symbols starting with
- \"+\": include tag;
- \"-\": exclude tag;
- \"(\": start of a compound tag selector.

Compound tag selectors are in the
form \"(OPERATOR TAG-SPECIFICATION)\", where
- TAG-SPECIFICATION is one of the above options.
- OPERATOR is one of those supported by `tag-specification-validator'.

Example input:

  \"+foo -bar (or (and john doe) (not (and tic tac)))\""
  (let* ((non-specs nil)
         (specs (with-input-from-string (in input)
                  (loop for object = (read in nil :eof)
                        until (eq object :eof)
                        if (or (not (atom object))
                               (and (<= 2 (length (symbol-name object)))
                                    (or
                                     (str:starts-with? "-" (symbol-name object))
                                     (str:starts-with? "+" (symbol-name object)))))
                          collect object
                        else
                          do (push object non-specs)))))

    (values
     (when specs
       (cons 'and specs))
     (nreverse non-specs))))
