;;;; This package and file serves as a source for bookmarklets that
;;;; originate outside of the Nyxt codebase. Eventually, the goal is
;;;; to translate these bookmarklets into their equivalent Parenscript
;;;; forms for easier interaction and editing.

;;;; The Bookmarklets in this file are copyright Jesse Ruderman and
;;;; are released into the public domain, per the license available
;;;; here: https://www.squarefree.com/bookmarklets/copyright.html

(nyxt:define-package :nyxt/bookmarklets-mode
  (:documentation "Easily create 'bookmarklets' (JavaScript snippets) to alter
the content of HTML pages."))
(in-package :nyxt/bookmarklets-mode)

(define-mode bookmarklets-mode ()
  "Mode for 'bookmarklets' commands.
By default, this mode does nothing but expose the default bookmarklets."
  ((visible-in-status-p nil)))

(defmacro define-bookmarklet-command (name documentation source) ; TODO: Should it really belong to the `nyxt' package?
  "Define a bookmarklet command, the source can either be
- a JavaScript string to evaluate,
- a `cl:pathname' to a JavaScript source file,
- or a form evaluating to a JavaScript string."
  `(define-command ,name (&optional (buffer (current-buffer)))
     ,documentation
     (let* ((source ,source)
            (source (etypecase source
                      (pathname (nfiles:content (make-instance 'nfiles:file :base-path source)))
                      (string source))))
       (ffi-buffer-evaluate-javascript-async buffer source))))
(sera:export-always 'define-bookmarklet-command)

(define-bookmarklet-command color-internal-external-links
  "Color internal links red, external links blue, and in-page links orange."
  "(function(){var i,x; for (i=0;x=document.links[i];++i)x.style.color=['blue','red','orange'][sim(x,location)]; function sim(a,b) { if (a.hostname!=b.hostname) return 0; if (fixPath(a.pathname)!=fixPath(b.pathname) || a.search!=b.search) return 1; return 2; } function fixPath(p){ p = (p.charAt(0)=='/' ? '' : '/') + p;/*many browsers*/ p=p.split('?')[0];/*opera*/ return p; } })()")

(define-bookmarklet-command urls-as-link-text
  "Changes the text of links to match their absolute URLs."
  "(function(){var i,c,x,h; for(i=0;x=document.links[i];++i) { h=x.href; x.title+=\" \" + x.innerHTML; while(c=x.firstChild)x.removeChild(c); x.appendChild(document.createTextNode(h)); } })()")

(define-bookmarklet-command hide-visited-urls
  "Hide visited URLs."
  "(function(){var newSS, styles=':visited {display: none}'; if(document.createStyleSheet) { document.createStyleSheet(\"javascript:'\"+styles+\"'\"); } else { newSS=document.createElement('link'); newSS.rel='stylesheet'; newSS.href='data:text/css,'+escape(styles); document.getElementsByTagName(\"head\")[0].appendChild(newSS); } })();")

(define-bookmarklet-command toggle-checkboxes
  "Toggle all checkboxes."
  "(function(){ function toggle(box){ temp=box.onchange; box.onchange=null; box.checked=!box.checked; box.onchange=temp; } var x,k,f,j; x=document.forms; for (k=0; k<x.length; ++k) { f=x[k]; for (j=0;j<f.length;++j) if (f[j].type.toLowerCase() == \"checkbox\") toggle(f[j]); } })();")

(define-bookmarklet-command view-password-field-contents
  "View passwords on page."
  "(function(){var s,F,j,f,i; s = \"\"; F = document.forms; for(j=0; j<F.length; ++j) { f = F[j]; for (i=0; i<f.length; ++i) { if (f[i].type.toLowerCase() == \"password\") s += f[i].value + \"\n\"; } } if (s) alert(\"Passwords in forms on this page:\n\n\" + s); else alert(\"There are no passwords in forms on this page.\");})();")

(define-bookmarklet-command show-hidden-form-elements
  "Show hidden form elements."
  "(function(){var i,f,j,e,div,label,ne; for(i=0;f=document.forms[i];++i)for(j=0;e=f[j];++j)if(e.type==\"hidden\"){ D=document; function C(t){return D.createElement(t);} function A(a,b){a.appendChild(b);} div=C(\"div\"); label=C(\"label\"); A(div, label); A(label, D.createTextNode(e.name + \": \")); e.parentNode.insertBefore(div, e); e.parentNode.removeChild(e); ne=C(\"input\");/*for ie*/ ne.type=\"text\"; ne.value=e.value; A(label, ne); label.style.MozOpacity=\".6\"; --j;/*for moz*/}})()")

(define-bookmarklet-command enlarge-textareas
  "Increase height of all text areas by 5 vertical lines."
  "(function(){var i,x; for(i=0;x=document.getElementsByTagName(\"textarea\")[i];++i) x.rows += 5; })()")

(define-bookmarklet-command show-textbox-character-count
  "Displays a running count of the characters in each textbox."
  "(function(){var D=document,i,f,j,e;for(i=0;f=D.forms[i];++i)for(j=0;e=f[j];++j)if(e.type==\"text\"||e.type==\"password\"||e.tagName.toLowerCase()==\"textarea\")S(e);function S(e){if(!e.N){var x=D.createElement(\"span\"),s=x.style;s.color=\"green\";s.background=\"white\";s.font=\"bold 10pt sans-serif\";s.verticalAlign=\"top\";e.parentNode.insertBefore(x,e.nextSibling);function u(){x.innerHTML=e.value.length;}u();e.onchange=u;e.onkeyup=u;e.oninput=u;e.N=x;}else{e.parentNode.removeChild(e.N);e.N=0;}}})()")

(define-bookmarklet-command highlight-regexp
  "Highlights each match for a regular expression."
  "(function(){var count=0, text, regexp;text=prompt(\"Search regexp:\", \"\");if(text==null || text.length==0)return;try{regexp=new RegExp(\"(\" + text +\")\", \"i\");}catch(er){alert(\"Unable to create regular expression using text '\"+text+\"'.\n\n\"+er);return;}function searchWithinNode(node, re){var pos, skip, spannode, middlebit, endbit, middleclone;skip=0;if( node.nodeType==3 ){pos=node.data.search(re);if(pos>=0){spannode=document.createElement(\"SPAN\");spannode.style.backgroundColor=\"yellow\";middlebit=node.splitText(pos);endbit=middlebit.splitText(RegExp.$1.length);middleclone=middlebit.cloneNode(true);spannode.appendChild(middleclone);middlebit.parentNode.replaceChild(spannode,middlebit);++count;skip=1;}}else if( node.nodeType==1 && node.childNodes && node.tagName.toUpperCase()!=\"SCRIPT\" && node.tagName.toUpperCase!=\"STYLE\"){for (var child=0; child < node.childNodes.length; ++child){child=child+searchWithinNode(node.childNodes[child], re);}}return skip;}window.status=\"Searching for \"+regexp+\"...\";searchWithinNode(document.body, regexp);window.status=\"Found \"+count+\" match\"+(count==1?\"\":\"es\")+\" for \"+regexp+\".\";})();")

(define-bookmarklet-command zoom-images
  "Zoom images in."
  "(function(){ function zoomImage(image, amt) { if(image.initialHeight == null) { /* avoid accumulating integer-rounding error */ image.initialHeight=image.height; image.initialWidth=image.width; image.scalingFactor=1; } image.scalingFactor*=amt; image.width=image.scalingFactor*image.initialWidth; image.height=image.scalingFactor*image.initialHeight; } var i,L=document.images.length; for (i=0;i<L;++i) zoomImage(document.images[i], 2); if (!L) alert(\"This page contains no images.\"); })();")

(define-bookmarklet-command unzoom-images
  "Zoom images out."
  "(function(){ function zoomImage(image, amt) { if(image.initialHeight == null) { /* avoid accumulating integer-rounding error */ image.initialHeight=image.height; image.initialWidth=image.width; image.scalingFactor=1; } image.scalingFactor*=amt; image.width=image.scalingFactor*image.initialWidth; image.height=image.scalingFactor*image.initialHeight; } var i,L=document.images.length; for (i=0;i<L;++i) zoomImage(document.images[i],.5); if (!L) alert(\"This page contains no images.\"); })();")

(define-bookmarklet-command sort-table
  "Sort a table alphabetically."
  "function toArray (c){var a, k;a=new Array;for (k=0; k<c.length; ++k)a[k]=c[k];return a;}function insAtTop(par,child){if(par.childNodes.length) par.insertBefore(child, par.childNodes[0]);else par.appendChild(child);}function countCols(tab){var nCols, i;nCols=0;for(i=0;i<tab.rows.length;++i)if(tab.rows[i].cells.length>nCols)nCols=tab.rows[i].cells.length;return nCols;}function makeHeaderLink(tableNo, colNo, ord){var link;link=document.createElement('a');link.href='javascript:sortTable('+tableNo+','+colNo+','+ord+');';link.appendChild(document.createTextNode((ord>0)?'a':'d'));return link;}function makeHeader(tableNo,nCols){var header, headerCell, i;header=document.createElement('tr');for(i=0;i<nCols;++i){headerCell=document.createElement('td');headerCell.appendChild(makeHeaderLink(tableNo,i,1));headerCell.appendChild(document.createTextNode('/'));headerCell.appendChild(makeHeaderLink(tableNo,i,-1));header.appendChild(headerCell);}return header;}g_tables=toArray(document.getElementsByTagName('table'));if(!g_tables.length) alert(\"This page doesn't contain any tables.\");(function(){var j, thead;for(j=0;j<g_tables.length;++j){thead=g_tables[j].createTHead();insAtTop(thead, makeHeader(j,countCols(g_tables[j])))}}) ();function compareRows(a,b){if(a.sortKey==b.sortKey)return 0;return (a.sortKey < b.sortKey) ? g_order : -g_order;}function sortTable(tableNo, colNo, ord){var table, rows, nR, bs, i, j, temp;g_order=ord;g_colNo=colNo;table=g_tables[tableNo];rows=new Array();nR=0;bs=table.tBodies;for(i=0; i<bs.length; ++i)for(j=0; j<bs[i].rows.length; ++j){rows[nR]=bs[i].rows[j];temp=rows[nR].cells[g_colNo];if(temp) rows[nR].sortKey=temp.innerHTML;else rows[nR].sortKey=\"\";++nR;}rows.sort(compareRows);for (i=0; i < rows.length; ++i)insAtTop(table.tBodies[0], rows[i]);}")

(define-bookmarklet-command number-table-rows
  "Add numbers to table rows."
  "(function(){function has(par,ctag){for(var k=0;k<par.childNodes.length;++k)if(par.childNodes[k].tagName==ctag)return true;} function add(par,ctag,text){var c=document.createElement(ctag); c.appendChild(document.createTextNode(text)); par.insertBefore(c,par.childNodes[0]);} var i,ts=document.getElementsByTagName(\"TABLE\"); for(i=0;i<ts.length;++i) { var n=0,trs=ts[i].rows,j,tr; for(j=0;j<trs.length;++j) {tr=trs[j]; if(has(tr,\"TD\"))add(tr,\"TD\",++n); else if(has(tr,\"TH\"))add(tr,\"TH\",\"Row\");}}})()")

(define-bookmarklet-command number-lines
  "Numberlines in plaintext documents and PRE tags."
  "(function(){var i,p,L,d,j,n; for(i=0; p=document.getElementsByTagName(\"pre\")[i]; ++i) { L=p.innerHTML.split(\"\r\n\"); d=\"\"+L.length; for(j=0;j<L.length;++j) { n = \"\"+(j+1)+\". \"; while(n.length<d.length+2) n=\"0\"+n; L[j] = n + L[j]; } p.innerHTML=L.join(\"<br>\");/*join with br for ie*/ } })()")

(define-bookmarklet-command transpose-tables
  "Transpose all table row and columns."
  "(function(){var d=document,q=\"table\",i,j,k,y,r,c,t;for(i=0;t=d.getElementsByTagName(q)[i];++i){var w=0,N=t.cloneNode(0);N.width=\"\";N.height=\"\";N.border=1;for(j=0;r=t.rows[j];++j)for(y=k=0;c=r.cells[k];++k){var z,a=c.rowSpan,b=c.colSpan,v=c.cloneNode(1);v.rowSpan=b;v.colSpan=a;v.width=\"\";v.height=\"\";if(!v.bgColor)v.bgColor=r.bgColor;while(w<y+b)N.insertRow(w++).p=0;while(N.rows[y].p>j)++y;N.rows[y].appendChild(v);for(z=0;z<b;++z)N.rows[y+z].p+=a;y+=b;}t.parentNode.replaceChild(N,t);}})()")

(define-bookmarklet-command remove-color
  "Remove color from web pages."
  "(function(){var newSS, styles='* { background: white ! important; color: black !important } :link, :link * { color: #0000EE !important } :visited, :visited * { color: #551A8B !important }'; if(document.createStyleSheet) { document.createStyleSheet(\"javascript:'\"+styles+\"'\"); } else { newSS=document.createElement('link'); newSS.rel='stylesheet'; newSS.href='data:text/css,'+escape(styles); document.getElementsByTagName(\"head\")[0].appendChild(newSS); } })();")

(define-bookmarklet-command remove-images
  "Remove images from web pages."
  "(function(){function toArray (c){var a, k;a=new Array;for (k=0; k < c.length; ++k)a[k]=c[k];return a;}var images, img, altText;images=toArray(document.images);for (var i=0; i < images.length; ++i){img=images[i];altText=document.createTextNode(img.alt);img.parentNode.replaceChild(altText, img)}})();")

(define-bookmarklet-command invert-color
  "Invert the color of the web page."
  ;; This bookmarklet was sourced from 'https://github.com/frontdevops/darkthemeswitcher-inline' with permission under free license.
  "(d=>{var css=`:root{background-color:#fefefe;filter:invert(100%)}*{background-color:inherit}img:not([src*=\".svg\"]),video{filter: invert(100%)}`,style,id=\"dark-theme-snippet\",ee=d.getElementById(id);if(null!=ee)ee.parentNode.removeChild(ee);else {style = d.createElement('style');style.type=\"text/css\";style.id=id;if(style.styleSheet)style.styleSheet.cssText=css;else style.appendChild(d.createTextNode(css));(d.head||d.querySelector('head')).appendChild(style)}})(document)")

(define-bookmarklet-command darken
  "Darken the page."
  ;; This bookmarklet was sourced form 'https://github.com/x08d/222' with permission under the GPL v3.0
  "javascript:document.querySelectorAll('*').forEach(e=>e.setAttribute('style','background-color:#222 !important;background-image:none !important;color:#'+(/^A|BU/.test(e.tagName)?'36c;text-decoration:underline;':'eee;')+e.getAttribute('style')))")
