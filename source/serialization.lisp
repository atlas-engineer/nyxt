(in-package :s-serialization)

;; TODO: Report upstream.
;; https://github.com/40ants/cl-prevalence/issues/2
(defmethod serialize-sexp-internal ((object pathname) stream serialization-state)
  "Serialize pathname OBJECT to it's printed representation starting with #P.
Note: Function serialization is not part of the original cl-prevalence."
  (declare (ignore serialization-state))
  (prin1 object stream))
