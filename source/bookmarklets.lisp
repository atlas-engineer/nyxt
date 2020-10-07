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


(in-package :nyxt)

(defmacro define-bookmarklet (name text)
  `(defparameter ,name ,text "Bookmarklet."))

(defmacro define-bookmarklet-command (name documentation source)
  (let ((namespaced-name (intern (string name) (find-package 'nyxt/bookmarklets))))
    `(progn
       (define-bookmarklet ,namespaced-name ,source)
       (define-command ,name (&optional (buffer (current-buffer)))
         ,documentation
         (ffi-buffer-evaluate-javascript-async buffer ,namespaced-name)))))

(define-bookmarklet-command color-internal-external-links
  "Color internal links red, external links blue, and in-page links orange."
  "(function(){var i,x; for (i=0;x=document.links[i];++i)x.style.color=['blue','red','orange'][sim(x,location)]; function sim(a,b) { if (a.hostname!=b.hostname) return 0; if (fixPath(a.pathname)!=fixPath(b.pathname) || a.search!=b.search) return 1; return 2; } function fixPath(p){ p = (p.charAt(0)=='/' ? '' : '/') + p;/*many browsers*/ p=p.split('?')[0];/*opera*/ return p; } })()")

(define-bookmarklet-command urls-as-link-text
  "Changes the text of links to match their absolute URLs."
  "javascript:(function(){var i,c,x,h; for(i=0;x=document.links[i];++i) { h=x.href; x.title+=\" \" + x.innerHTML; while(c=x.firstChild)x.removeChild(c); x.appendChild(document.createTextNode(h)); } })()")

(define-bookmarklet-command hide-visited-urls
  "Hide visited URLs."
  "javascript:(function(){var newSS, styles=':visited {display: none}'; if(document.createStyleSheet) { document.createStyleSheet(\"javascript:'\"+styles+\"'\"); } else { newSS=document.createElement('link'); newSS.rel='stylesheet'; newSS.href='data:text/css,'+escape(styles); document.getElementsByTagName(\"head\")[0].appendChild(newSS); } })();")

(define-bookmarklet-command toggle-checkboxes
  "Toggle all checkboxes."
  "javascript:(function(){ function toggle(box){ temp=box.onchange; box.onchange=null; box.checked=!box.checked; box.onchange=temp; } var x,k,f,j; x=document.forms; for (k=0; k<x.length; ++k) { f=x[k]; for (j=0;j<f.length;++j) if (f[j].type.toLowerCase() == \"checkbox\") toggle(f[j]); } })();")

(define-bookmarklet-command view-password-field-contents
  "View passwords on page."
  "javascript:(function(){var s,F,j,f,i; s = \"\"; F = document.forms; for(j=0; j<F.length; ++j) { f = F[j]; for (i=0; i<f.length; ++i) { if (f[i].type.toLowerCase() == \"password\") s += f[i].value + \"\n\"; } } if (s) alert(\"Passwords in forms on this page:\n\n\" + s); else alert(\"There are no passwords in forms on this page.\");})();")

(define-bookmarklet-command show-hidden-form-elements
  "Sohw hidden form elements."
  "javascript:(function(){var i,f,j,e,div,label,ne; for(i=0;f=document.forms[i];++i)for(j=0;e=f[j];++j)if(e.type==\"hidden\"){ D=document; function C(t){return D.createElement(t);} function A(a,b){a.appendChild(b);} div=C(\"div\"); label=C(\"label\"); A(div, label); A(label, D.createTextNode(e.name + \": \")); e.parentNode.insertBefore(div, e); e.parentNode.removeChild(e); ne=C(\"input\");/*for ie*/ ne.type=\"text\"; ne.value=e.value; A(label, ne); label.style.MozOpacity=\".6\"; --j;/*for moz*/}})()")
