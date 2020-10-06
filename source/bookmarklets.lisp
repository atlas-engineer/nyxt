;;;; This package and file serves as a source for bookmarklets that
;;;; originate outside of the Nyxt codebase. Eventually, the goal is
;;;; to translate these bookmarklets into their equivalent parenscript
;;;; forms for easier interaction and editing.

;;;; The Bookmarklets in this file are copyright Jesse Ruderman and
;;;; are released into the public domain, per the license available
;;;; here: https://www.squarefree.com/bookmarklets/copyright.html

(uiop:define-package nyxt/bookmarklets
    (:use :common-lisp)
  (:documentation "Bookmarkets with an origin outside of Nyxt are
  sourced from here."))

(in-package :nyxt/bookmarklets)

(defmacro define-bookmarklet (name text)
  `(defparameter ,name ,text "Bookmarklet."))

(define-bookmarklet color-internal-external-links
  "(function(){var i,x; for (i=0;x=document.links[i];++i)x.style.color=['blue','red','orange'][sim(x,location)]; function sim(a,b) { if (a.hostname!=b.hostname) return 0; if (fixPath(a.pathname)!=fixPath(b.pathname) || a.search!=b.search) return 1; return 2; } function fixPath(p){ p = (p.charAt(0)=='/' ? '' : '/') + p;/*many browsers*/ p=p.split('?')[0];/*opera*/ return p; } })()")

(in-package :nyxt)

(defmacro define-bookmarklet-command (name documentation source)
  `(define-command ,name (&optional (buffer (current-buffer)))
     ,documentation
  (ffi-buffer-evaluate-javascript-async buffer ,source)))

(define-bookmarklet-command color-internal-external-links
  "Color internal links red, external links blue, and in-page links orange."
  nyxt/bookmarklets::color-internal-external-links)

