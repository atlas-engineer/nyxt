;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class panel-buffer (web-buffer)
  ((window
    nil
    :type (maybe window)
    :documentation "The window this panel is attached to.")
   (width
    256
    :type integer
    :export nil
    :documentation "Width in pixels while the panel is shown.")
   (side
    :left
    :type (member :left :right)
    :documentation "Which edge of the page the panel occupies.")
   (visible-p
    nil
    :type boolean
    :documentation "Whether the panel is shown.")
   (drag-origin
    nil
    :export nil
    :type (or null integer)
    :documentation "Screen position used while the panel is being resized.")
   (link-url
    nil
    :export nil
    :type (or null string)
    :documentation "`lisp' URL that receives resize events from the panel."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Buffer fixed to the left or right of the page.
A window shows any number of them. Each is as tall as the page and shows one thing."))

(define-class buffer-listing-panel (panel-buffer)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Panel that lists the open buffers."))

(defmethod include-in-buffer-list-p ((buffer panel-buffer))
  nil)

(defmethod (setf width) :around (value (panel panel-buffer))
  (call-next-method (clamp-panel-width panel value) panel))

(defmethod (setf width) :after (value (panel panel-buffer))
  (declare (ignore value))
  (when (and (visible-p panel) (window panel))
    (apply-panel-geometry panel)))

(-> clamp-panel-width (panel-buffer integer) integer)
(defun clamp-panel-width (panel width)
  "Return WIDTH limited to the range the panel can occupy."
  (let* ((window-width (or (ignore-errors
                            (and (window panel)
                                 (plusp (ffi-width (window panel)))
                                 (ffi-width (window panel))))
                           800))
         (ceiling (max 160 (min 640 (floor window-width 2)))))
    (max 160 (min width ceiling))))

(-> apply-panel-geometry (panel-buffer) t)
(defun apply-panel-geometry (panel)
  "Place PANEL on its window at its current side and width."
  (when (window panel)
    (if (visible-p panel)
        (ffi-add-panel-buffer (window panel) panel (side panel) (width panel))
        (ffi-remove-panel-buffer (window panel) panel)))
  panel)

(-> focused-panel (&optional window) (maybe panel-buffer))
(defun focused-panel (&optional (window (current-window)))
  "Return the panel with keyboard focus, or the one most recently added."
  (when window
    (let ((keyboard (keyboard-buffer window)))
      (if (and (panel-buffer-p keyboard)
               (find keyboard (panel-buffers window)))
          keyboard
          (first (last (panel-buffers window)))))))

(export-always 'add-panel-buffer)
(-> add-panel-buffer (window panel-buffer) (values panel-buffer &optional))
(defun add-panel-buffer (window panel)
  "Show PANEL beside WINDOW's page."
  (setf (window panel) window)
  (unless (find panel (panel-buffers window))
    (setf (panel-buffers window) (append (panel-buffers window) (list panel))))
  (setf (visible-p panel) t)
  (apply-panel-geometry panel)
  panel)

(-> panel-event-param (string) (maybe string))
(defun panel-event-param (name)
  "Return the query value NAME from the `lisp' request in progress."
  (when *lisp-request-url*
    (assoc-value (quri:uri-query-params *lisp-request-url*) name :test #'equal)))

(-> panel-drag-begin (panel-buffer (maybe integer)) t)
(defun panel-drag-begin (panel x)
  (when x
    (setf (drag-origin panel)
          (if (eq (side panel) :left)
              (- x (width panel))
              (+ x (width panel))))
    (arm-panel-drag (and (window panel) (active-buffer (window panel)))
                    (link-url panel))))

(-> panel-drag-move (panel-buffer (maybe integer)) t)
(defun panel-drag-move (panel x)
  (when (and x (numberp (drag-origin panel)))
    (setf (width panel)
          (if (eq (side panel) :left)
              (- x (drag-origin panel))
              (- (drag-origin panel) x)))))

(-> panel-drag-end (panel-buffer) t)
(defun panel-drag-end (panel)
  (setf (drag-origin panel) nil))

(-> panel-handle-event (panel-buffer) t)
(defun panel-handle-event (panel)
  "Dispatch the resize event carried by `*lisp-request-url*'."
  (let ((op (panel-event-param "op"))
        (x (ignore-errors (parse-integer (or (panel-event-param "x") "")))))
    (cond ((equal op "down") (panel-drag-begin panel x))
          ((equal op "move") (panel-drag-move panel x))
          ((equal op "up") (panel-drag-end panel)))))

(-> ensure-panel-link-callback (panel-buffer) string)
(defun ensure-panel-link-callback (panel)
  "Return the `lisp' URL PANEL's document uses while it is resized."
  (or (link-url panel)
      (setf (link-url panel)
            (quri:render-uri
             (lisp-url :buffer panel
                       :title "panel-event"
                       :callback (lambda ()
                                   (panel-handle-event panel)))))))

(-> arm-panel-drag ((maybe buffer) (maybe string)) t)
(defun arm-panel-drag (page callback-url)
  "Listen for the pointer on PAGE so a drag can leave the panel."
  (when (and page callback-url (not (panel-buffer-p page)))
    (ffi-buffer-evaluate-javascript-async
     page
     (format nil "(function(){
var callback = ~a;
function send(op, x) {
  fetch(callback + '&op=' + encodeURIComponent(op) + '&x=' + encodeURIComponent(x), {mode: 'no-cors'});
}
if (window.__nyxtPanelDragMove) {
  document.removeEventListener('mousemove', window.__nyxtPanelDragMove, true);
  document.removeEventListener('mouseup', window.__nyxtPanelDragUp, true);
}
window.__nyxtPanelDragMove = function(event) { send('move', event.screenX); };
window.__nyxtPanelDragUp = function(event) {
  document.removeEventListener('mousemove', window.__nyxtPanelDragMove, true);
  document.removeEventListener('mouseup', window.__nyxtPanelDragUp, true);
  window.__nyxtPanelDragMove = null;
  window.__nyxtPanelDragUp = null;
  send('up', event.screenX);
};
document.addEventListener('mousemove', window.__nyxtPanelDragMove, true);
document.addEventListener('mouseup', window.__nyxtPanelDragUp, true);
})();"
             (cl-json:encode-json-to-string callback-url)))))

(-> install-panel-chrome (panel-buffer) t)
(defun install-panel-chrome (panel)
  "Add the resize handle on PANEL's outer edge."
  (let ((callback (cl-json:encode-json-to-string (ensure-panel-link-callback panel)))
        (right (if (eq (side panel) :right) "true" "false"))
        (line (cl-json:encode-json-to-string (theme:background-color- (theme *browser*)))))
    (ffi-buffer-evaluate-javascript-async
     panel
     (format nil "(function(){
var callback = ~a;
var right = ~a;
var line = ~a;
function send(op, extra) {
  var url = callback + '&op=' + encodeURIComponent(op);
  if (extra !== undefined && extra !== null)
    url += '&x=' + encodeURIComponent(String(extra));
  fetch(url, {mode: 'no-cors'});
}
var old = document.getElementById('nyxt-panel-chrome');
if (old) old.remove();
var chrome = document.createElement('div');
chrome.id = 'nyxt-panel-chrome';
var handle = document.createElement('div');
handle.id = 'nyxt-panel-handle';
handle.style.cssText = 'position:fixed;top:0;bottom:0;width:6px;z-index:5;cursor:ew-resize;background:' + line + ';'
  + (right ? 'left:0;' : 'right:0;');
handle.addEventListener('mousedown', function(event) {
  event.preventDefault();
  send('down', event.screenX);
  function move(ev) { send('move', ev.screenX); }
  function up(ev) {
    document.removeEventListener('mousemove', move, true);
    document.removeEventListener('mouseup', up, true);
    send('up', ev.screenX);
  }
  document.addEventListener('mousemove', move, true);
  document.addEventListener('mouseup', up, true);
});
chrome.appendChild(handle);
(document.body || document.documentElement).appendChild(chrome);
})();"
             callback right line))))

(defmethod on-signal-load-finished :after ((buffer panel-buffer) url title)
  (declare (ignore url title))
  (install-panel-chrome buffer))

(define-command toggle-panel-buffer (&optional (panel (focused-panel)))
  "Show or hide PANEL."
  (when panel
    (setf (visible-p panel) (not (visible-p panel)))
    (apply-panel-geometry panel))
  panel)

(define-command toggle-panel-buffer-side (&optional (panel (focused-panel)))
  "Move PANEL to the other side of the page."
  (when panel
    (setf (side panel) (if (eq (side panel) :left) :right :left))
    (setf (visible-p panel) t)
    (apply-panel-geometry panel)
    (install-panel-chrome panel))
  panel)

(define-command delete-panel-buffer (&optional (panel (focused-panel)))
  "Remove PANEL from its window."
  (when panel
    (let ((window (window panel)))
      (when window
        (setf (panel-buffers window) (remove panel (panel-buffers window))))
      (setf (visible-p panel) nil)
      (apply-panel-geometry panel)
      (ignore-errors (ffi-buffer-delete panel))
      (buffer-delete (id panel))))
  nil)

(setf *lisp-callback-buffer-hook*
      (lambda (buffer)
        (when (panel-buffer-p buffer)
          (let ((window (window buffer)))
            (and window (active-buffer window))))))
