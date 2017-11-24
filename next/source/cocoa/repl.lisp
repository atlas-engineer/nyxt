;;;; repl.lisp --- adds support for keybindings in repl loaded system

(in-package :gui)

;; When using Cocoa from the Objective-C bridge in CCL, an instance of
;; ide-application is created as the NSApplication. This NSApplication
;; extends from ccl::ccl-application. By implementing the sendEvent
;; method we can capture all key events and if they are applicable to
;; a nEXT keybinding we can execute them in place of the Cocoa IDE
;; keybinding.

(objc:defmethod (#/sendEvent: :void) ((self ide-application) event)
  (if (eql #$NSKeyDown (#/type event))
      (unless (interface:process-event event)
	(call-next-method event))
      (call-next-method event)))
