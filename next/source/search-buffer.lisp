;;;; search-buffer.lisp --- functions to enable searching within a webview

(in-package :next)

(defparen add-search-boxes (search-string)
  (let* ((regex-string (ps:lisp (concatenate 'string search-string "[A-Za-z]*")))
         (regex-flags "gi")
         (matcher (ps:new (-reg-exp regex-string regex-flags)))
         (stringy "some magical string breadfish stop")
         (matches ())
         (last-match t))
    (ps:chain -j-s-o-n
              (stringify
               (loop do last-match do
                    (setf last-match (ps:chain matcher (exec stringy)))
                  collect last-match)))))
