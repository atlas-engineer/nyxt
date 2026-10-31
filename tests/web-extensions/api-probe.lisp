;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:unlock-package :nyxt/renderer/cef))

(in-package :nyxt/renderer/cef)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun javascript-expression (form)
    (string-right-trim '(#\; #\Newline #\Space #\Tab #\Return) (ps:ps* form)))
  (defun compile-javascript-await (form)
    `(ps-js:escape ,(concatenate 'string "await " (javascript-expression form))))
  (defun compile-javascript-async-lambda (arguments &rest body)
    `(ps-js:escape ,(concatenate 'string "async " (javascript-expression `(lambda ,arguments ,@body)))))
  (setf (gethash 'await ps::*special-expression-operators*)
        (lambda (&rest whole)
          (destructuring-bind (form) whole
            (compile-javascript-await form))))
  (setf (gethash 'async-lambda ps::*special-expression-operators*)
        (lambda (&rest whole)
          (destructuring-bind (arguments &rest body) whole
            (apply #'compile-javascript-async-lambda arguments body))))
  (setf (gethash 'js-return ps::*special-statement-operators*)
        (lambda (&rest whole)
          (destructuring-bind (&optional (value nil value?)) whole
            (if value?
                `(ps-js:return ,(ps::compile-expression value))
                '(ps-js:return))))))

(defvar *api-probe-assets*
  (uiop:merge-pathnames* #p"tests/web-extensions/api-probe/"
                         (asdf:system-source-directory :nyxt)))

(-> write-api-probe (pathname) pathname)
(defun write-api-probe (directory)
  "Write a runnable copy of the API probe extension into DIRECTORY."
  (let ((root (uiop:ensure-directory-pathname directory)))
    (ensure-directories-exist root)
    (dolist (name '("manifest.json" "page.html" "icon.png"))
      (uiop:copy-file (uiop:merge-pathnames* name *api-probe-assets*)
                      (uiop:merge-pathnames* name root)))
    (ensure-directories-exist (uiop:merge-pathnames* #p"apis/" root))

(-> api-probe-background-javascript () string)
(defun api-probe-background-javascript ()
  "JavaScript for the API probe file background.js."
  (ps:ps
    (setf
        (ps:@ |SELF| |PROBE|)
        (ps:create
          |TESTS|
          (ps:array)
          |CONTEXT|
          (ps:create)
          |DELIVERED|
          (ps:create |ALARM| ps:false |ACTION| ps:false |MENU| ps:false)))
    (setf
        (ps:@ |SELF| |PROBE| |TEST|)
        (lambda
          (|API| |NAME| |FN|)
          (ps:chain
            |SELF|
            |PROBE|
            |TESTS|
            (|PUSH| (ps:create |API| |API| |NAME| |NAME| |FN| |FN|)))
          (js-return)))
    (setf
        (ps:@ |SELF| |PROBE| |CHECK|)
        (lambda
          (|VALUE| |MESSAGE|)
          (if
            (not |VALUE|)
            (throw (ps:new (|Error| |MESSAGE|))))
          (js-return)))
    (setf
        (ps:@ |SELF| |PROBE| |PAUSE|)
        (lambda
          (|MS|)
          (js-return
            (ps:new
              (|Promise|
                (lambda
                  (|RESOLVE|)
                  (|setTimeout| |RESOLVE| |MS|)
                  (js-return)))))))
    (setf
        (ps:@ |SELF| |PROBE| |QUIET|)
        (async-lambda
          (|PROMISE|)
          (ps:try
            (await |PROMISE|)
            (:catch (|ERROR|) nil))
          (js-return)))
    (ps:chain
        |CHROME|
        |RUNTIME|
        |onInstalled|
        (|addListener|
          (lambda
            (|DETAILS|)
            (ps:chain
              |CHROME|
              |STORAGE|
              |LOCAL|
              (|SET|
                (ps:create
                  |nyxtProbeInstalled|
                  (if
                    (and |DETAILS| (ps:@ |DETAILS| |REASON|))
                    (ps:@ |DETAILS| |REASON|)
                    ""))))
            (js-return))))
    (ps:chain
        |CHROME|
        |ALARMS|
        |onAlarm|
        (|addListener|
          (lambda
            (|ALARM|)
            (if
              (and |ALARM| (eql (ps:@ |ALARM| |NAME|) "nyxt-probe"))
              (setf (ps:@ |SELF| |PROBE| |DELIVERED| |ALARM|) t))
            (js-return))))
    (ps:chain
        |CHROME|
        |ACTION|
        |onClicked|
        (|addListener|
          (lambda
            ()
            (setf (ps:@ |SELF| |PROBE| |DELIVERED| |ACTION|) t)
            (js-return))))
    (ps:chain
        |CHROME|
        |contextMenus|
        |onClicked|
        (|addListener|
          (lambda
            (|INFO|)
            (if
              (and |INFO| (eql (ps:@ |INFO| |menuItemId|) "nyxt-probe"))
              (setf (ps:@ |SELF| |PROBE| |DELIVERED| |MENU|) t))
            (js-return))))
    (defun
        |withTimeout|
        (|PROMISE| |MS| |NAME|)
        (js-return
          (ps:new
            (|Promise|
              (lambda
                (|RESOLVE| |REJECT|)
                (ps:var
                  |TIMER|
                  (|setTimeout|
                    (lambda
                      ()
                      (|REJECT| (ps:new (|Error| (+ |NAME| " timed out"))))
                      (js-return))
                    |MS|))
                (ps:chain
                  (ps:chain
                    |Promise|
                    (|RESOLVE| |PROMISE|))
                  (|THEN|
                    (lambda
                      (|VALUE|)
                      (|clearTimeout| |TIMER|)
                      (|RESOLVE| |VALUE|)
                      (js-return))
                    (lambda
                      (|ERROR|)
                      (|clearTimeout| |TIMER|)
                      (|REJECT| |ERROR|)
                      (js-return))))
                (js-return))))))
    (ps:var
        |runAll|
        (async-lambda
          (|ECHO|)
          (setf (ps:@ |SELF| |PROBE| |CONTEXT|) (ps:create |ECHO| |ECHO|))
          (ps:var |RESULTS| (ps:array))
          (ps:var |nyxtOf1| (ps:@ |SELF| |PROBE| |TESTS|))
          (ps:for
            ((|nyxtFor1| 0))
            ((< |nyxtFor1| (ps:@ |nyxtOf1| |LENGTH|)))
            ((incf |nyxtFor1|))
            (ps:var |ENTRY| (aref |nyxtOf1| |nyxtFor1|))
            (ps:var |STARTED| (ps:chain
                |Date|
                (|NOW|)))
            (ps:try
              (progn
                (ps:var
                  |DETAIL|
                  (await
                    (|withTimeout|
                      (ps:chain
                        |ENTRY|
                        (|FN| (ps:@ |SELF| |PROBE| |CONTEXT|)))
                      15000
                      (+ (+ (ps:@ |ENTRY| |API|) ".") (ps:@ |ENTRY| |NAME|)))))
                (ps:chain
                  |RESULTS|
                  (|PUSH|
                    (ps:create
                      |API|
                      (ps:@ |ENTRY| |API|)
                      |NAME|
                      (ps:@ |ENTRY| |NAME|)
                      |OK|
                      t
                      |DETAIL|
                      (if
                        (equal |DETAIL| nil)
                        ""
                        (|String| |DETAIL|))
                      |MS|
                      (- (ps:chain
                          |Date|
                          (|NOW|)) |STARTED|)))))
              (:catch
                (|ERROR|)
                (ps:chain
                  |RESULTS|
                  (|PUSH|
                    (ps:create
                      |API|
                      (ps:@ |ENTRY| |API|)
                      |NAME|
                      (ps:@ |ENTRY| |NAME|)
                      |OK|
                      ps:false
                      |DETAIL|
                      (if
                        (and |ERROR| (ps:@ |ERROR| |MESSAGE|))
                        (ps:@ |ERROR| |MESSAGE|)
                        (|String| |ERROR|))
                      |MS|
                      (- (ps:chain
                          |Date|
                          (|NOW|)) |STARTED|)))))))
          (if
            (not (equal (ps:@ |SELF| |PROBE| |CONTEXT| |createdTabId|) nil))
            (await
              (ps:chain
                |SELF|
                |PROBE|
                (|QUIET|
                  (ps:chain
                    |CHROME|
                    |TABS|
                    (|REMOVE| (ps:@ |SELF| |PROBE| |CONTEXT| |createdTabId|)))))))
          (js-return
            (ps:create
              |ID|
              (ps:@ |CHROME| |RUNTIME| |ID|)
              |RESULTS|
              |RESULTS|))))
    (ps:chain
        |CHROME|
        |RUNTIME|
        |onMessage|
        (|addListener|
          (lambda
            (|MESSAGE| |SENDER| |sendResponse|)
            (if
              (or (not |MESSAGE|) (not (ps:@ |MESSAGE| |TYPE|)))
              (js-return))
            (if
              (eql (ps:@ |MESSAGE| |TYPE|) "echo")
              (progn
                (|sendResponse| (ps:create
                    |TEXT|
                    (ps:@ |MESSAGE| |TEXT|)))
                (js-return t)))
            (if
              (eql (ps:@ |MESSAGE| |TYPE|) "run")
              (progn
                (ps:chain
                  (|runAll| (ps:@ |MESSAGE| |ECHO|))
                  (|THEN| |sendResponse|))
                (js-return t)))
            (js-return))))
    (|importScripts|
        "apis/runtime.js"
        "apis/storage.js"
        "apis/tabs.js"
        "apis/scripting.js"
        "apis/action.js"
        "apis/alarms.js"
        "apis/context-menus.js"
        "apis/notifications.js"
        "apis/downloads.js"
        "apis/cookies.js"
        "apis/surface.js")))

(-> api-probe-content-javascript () string)
(defun api-probe-content-javascript ()
  "JavaScript for the API probe file content.js."
  (ps:ps
    (ps:chain
        |CHROME|
        |RUNTIME|
        |onMessage|
        (|addListener|
          (lambda
            (|MESSAGE| |SENDER| |sendResponse|)
            (if
              (and |MESSAGE| (eql (ps:@ |MESSAGE| |TYPE|) "content-ping"))
              (|sendResponse|
                (ps:create
                  |OK|
                  t
                  |HREF|
                  (ps:@ |LOCATION| |HREF|))))
            (js-return))))))

(-> api-probe-page-javascript () string)
(defun api-probe-page-javascript ()
  "JavaScript for the API probe file page.js."
  (ps:ps
    (ps:var
        |ASK|
        (async-lambda
          (|MESSAGE|)
          (ps:var |LAST|)
          (ps:for
            ((|ATTEMPT| 0))
            ((< |ATTEMPT| 25))
            ((incf |ATTEMPT| 1))
            (ps:try
              (js-return
                (await
                  (ps:chain
                    |CHROME|
                    |RUNTIME|
                    (|sendMessage| |MESSAGE|))))
              (:catch
                (|ERROR|)
                (setf |LAST| |ERROR|)
                (await
                  (ps:new
                    (|Promise|
                      (lambda
                        (|RESOLVE|)
                        (|setTimeout| |RESOLVE| 200)
                        (js-return))))))))
          (throw |LAST|)))
    (defun
        |RENDER|
        (|REPORT|)
        (ps:var |LINES| (ps:array))
        (ps:var |API| "")
        (ps:var
          |RESULTS|
          (if
            (and |REPORT| (ps:@ |REPORT| |RESULTS|))
            (ps:@ |REPORT| |RESULTS|)
            (ps:array)))
        (ps:var
          |FAILED|
          (ps:@
            (ps:chain
              |RESULTS|
              (|FILTER|
                (lambda
                  (|RESULT|)
                  (js-return (not (ps:@ |RESULT| |OK|))))))
            |LENGTH|))
        (ps:chain
          |LINES|
          (|PUSH| (+ (+ (+ (ps:@ |RESULTS| |LENGTH|) " checks, ") |FAILED|) " failed")))
        (ps:var |nyxtOf1| |RESULTS|)
        (ps:for
          ((|nyxtFor1| 0))
          ((< |nyxtFor1| (ps:@ |nyxtOf1| |LENGTH|)))
          ((incf |nyxtFor1|))
          (ps:var |RESULT| (aref |nyxtOf1| |nyxtFor1|))
          (if
            (/= (ps:@ |RESULT| |API|) |API|)
            (progn
              (setf |API| (ps:@ |RESULT| |API|))
              (ps:chain
                |LINES|
                (|PUSH| ""))
              (ps:chain
                |LINES|
                (|PUSH| |API|))))
          (ps:chain
            |LINES|
            (|PUSH|
              (+
                (+
                  (+
                    (+
                      (if
                        (ps:@ |RESULT| |OK|)
                        "pass"
                        "FAIL")
                      "  ")
                    (ps:@ |RESULT| |NAME|))
                  "  ")
                (ps:@ |RESULT| |DETAIL|)))))
        (setf
          (ps:@ (ps:chain
              |DOCUMENT|
              (|getElementById| "out")) |textContent|)
          (ps:chain
            |LINES|
            (|JOIN| #.(format nil "~%"))))
        (setf (ps:@ |DOCUMENT| |TITLE|) (if |FAILED| "PROBE-FAIL" "PROBE-PASS"))
        (js-return))
    (ps:chain
        (ps:chain
          (ps:chain
            (|ASK| (ps:create |TYPE| "echo" |TEXT| "nyxt"))
            (|THEN|
              (lambda
                (|ECHO|)
                (js-return (|ASK| (ps:create |TYPE| "run" |ECHO| |ECHO|))))))
          (|THEN| |RENDER|))
        (|CATCH|
          (lambda
            (|ERROR|)
            (setf
              (ps:@ (ps:chain
                  |DOCUMENT|
                  (|getElementById| "out")) |textContent|)
              (|String|
                (if
                  (and |ERROR| (ps:@ |ERROR| |MESSAGE|))
                  (ps:@ |ERROR| |MESSAGE|)
                  |ERROR|)))
            (setf (ps:@ |DOCUMENT| |TITLE|) "PROBE-FAIL")
            (js-return))))))

(-> api-probe-apis-action-javascript () string)
(defun api-probe-apis-action-javascript ()
  "JavaScript for the API probe file apis/action.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "action"
          "title"
          (async-lambda
            ()
            (await
              (ps:chain
                |CHROME|
                |ACTION|
                (|setTitle| (ps:create |TITLE| "Nyxt probe"))))
            (ps:var
              |TITLE|
              (await
                (ps:chain
                  |CHROME|
                  |ACTION|
                  (|getTitle| (ps:create)))))
            (ps:chain
              |PROBE|
              (|CHECK| (eql |TITLE| "Nyxt probe") (|String| |TITLE|)))
            (js-return |TITLE|))))
    (ps:chain
        |PROBE|
        (|TEST|
          "action"
          "badge"
          (async-lambda
            ()
            (await
              (ps:chain
                |CHROME|
                |ACTION|
                (|setBadgeBackgroundColor| (ps:create |COLOR| "#112233"))))
            (await
              (ps:chain
                |CHROME|
                |ACTION|
                (|setBadgeText| (ps:create |TEXT| "9"))))
            (ps:var
              |TEXT|
              (await
                (ps:chain
                  |CHROME|
                  |ACTION|
                  (|getBadgeText| (ps:create)))))
            (ps:chain
              |PROBE|
              (|CHECK| (eql |TEXT| "9") (|String| |TEXT|)))
            (await
              (ps:chain
                |CHROME|
                |ACTION|
                (|setBadgeText| (ps:create |TEXT| ""))))
            (js-return |TEXT|))))
    (ps:chain
        |PROBE|
        (|TEST|
          "action"
          "onClicked"
          (async-lambda
            ()
            (ps:for
              ((|ATTEMPT| 0))
              ((< |ATTEMPT| 25))
              ((incf |ATTEMPT| 1))
              (if
                (ps:@ |PROBE| |DELIVERED| |ACTION|)
                (js-return "clicked"))
              (await (ps:chain
                  |PROBE|
                  (|PAUSE| 200))))
            (throw (ps:new (|Error| "toolbar click was not delivered"))))))))

(-> api-probe-apis-alarms-javascript () string)
(defun api-probe-apis-alarms-javascript ()
  "JavaScript for the API probe file apis/alarms.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "alarms"
          "fire"
          (async-lambda
            ()
            (setf (ps:@ |PROBE| |DELIVERED| |ALARM|) ps:false)
            (await
              (ps:chain
                |CHROME|
                |ALARMS|
                (|CREATE| "nyxt-probe" (ps:create
                    |delayInMinutes|
                    (/ 1 60)))))
            (ps:var
              |LISTED|
              (await
                (ps:chain
                  |CHROME|
                  |ALARMS|
                  (|GET| "nyxt-probe"))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |LISTED| (eql (ps:@ |LISTED| |NAME|) "nyxt-probe"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |LISTED|))))
            (ps:for
              ((|ATTEMPT| 0))
              ((< |ATTEMPT| 40))
              ((incf |ATTEMPT| 1))
              (if
                (ps:@ |PROBE| |DELIVERED| |ALARM|)
                (js-return "fired"))
              (await (ps:chain
                  |PROBE|
                  (|PAUSE| 200))))
            (throw (ps:new (|Error| "alarm did not fire"))))))))

(-> api-probe-apis-context-menus-javascript () string)
(defun api-probe-apis-context-menus-javascript ()
  "JavaScript for the API probe file apis/context-menus.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "contextMenus"
          "create"
          (lambda
            ()
            (js-return
              (ps:new
                (|Promise|
                  (lambda
                    (|RESOLVE| |REJECT|)
                    (ps:chain
                      |CHROME|
                      |contextMenus|
                      (|removeAll|
                        (lambda
                          ()
                          (ps:chain
                            |CHROME|
                            |contextMenus|
                            (|CREATE|
                              (ps:create
                                |ID|
                                "nyxt-probe"
                                |TITLE|
                                "Nyxt probe"
                                |CONTEXTS|
                                (ps:array "page"))
                              (lambda
                                ()
                                (ps:var |ERROR| (ps:@ |CHROME| |RUNTIME| |lastError|))
                                (if
                                  |ERROR|
                                  (progn
                                    (|REJECT| (ps:new (|Error| (ps:@ |ERROR| |MESSAGE|))))
                                    (js-return)))
                                (|RESOLVE| "created")
                                (js-return))))
                          (js-return))))
                    (js-return))))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "contextMenus"
          "onClicked"
          (async-lambda
            ()
            (if
              (and (ps:@ |SELF| |NYXT|) (ps:@ |SELF| |NYXT| |activateMenu|))
              (await
                (ps:chain
                  |SELF|
                  |NYXT|
                  (|activateMenu| "nyxt-probe"))))
            (ps:chain
              |PROBE|
              (|CHECK| (ps:@ |PROBE| |DELIVERED| |MENU|) "menu click was not delivered"))
            (js-return "clicked"))))))

(-> api-probe-apis-cookies-javascript () string)
(defun api-probe-apis-cookies-javascript ()
  "JavaScript for the API probe file apis/cookies.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "cookies"
          "set-get"
          (async-lambda
            ()
            (ps:var
              |SET|
              (await
                (ps:chain
                  |CHROME|
                  |COOKIES|
                  (|SET| (ps:create |URL| "https://example.com/" |NAME| "nyxt_probe" |VALUE| "1")))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |SET| (eql (ps:@ |SET| |VALUE|) "1"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |SET|))))
            (ps:var
              |GOT|
              (await
                (ps:chain
                  |CHROME|
                  |COOKIES|
                  (|GET| (ps:create |URL| "https://example.com/" |NAME| "nyxt_probe")))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |GOT| (eql (ps:@ |GOT| |VALUE|) "1"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |GOT|))))
            (await
              (ps:chain
                |CHROME|
                |COOKIES|
                (|REMOVE| (ps:create |URL| "https://example.com/" |NAME| "nyxt_probe"))))
            (js-return (or (ps:@ |GOT| |DOMAIN|) "example.com")))))))

(-> api-probe-apis-downloads-javascript () string)
(defun api-probe-apis-downloads-javascript ()
  "JavaScript for the API probe file apis/downloads.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "downloads"
          "download"
          (async-lambda
            ()
            (ps:var
              |ID|
              (await
                (ps:chain
                  |CHROME|
                  |DOWNLOADS|
                  (|DOWNLOAD|
                    (ps:create
                      |URL|
                      "https://example.com/"
                      |FILENAME|
                      "nyxt-probe-download.html"
                      |conflictAction|
                      "overwrite"
                      |saveAs|
                      ps:false)))))
            (ps:chain
              |PROBE|
              (|CHECK| (eql (ps:typeof |ID|) "number") (|String| |ID|)))
            (ps:var
              |FOUND|
              (await
                (ps:chain
                  |CHROME|
                  |DOWNLOADS|
                  (|SEARCH| (ps:create |ID| |ID|)))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |FOUND| (eql (ps:@ |FOUND| |LENGTH|) 1))
                (ps:chain
                  |json|
                  (|STRINGIFY| |FOUND|))))
            (await
              (ps:chain
                |PROBE|
                (|QUIET|
                  (ps:chain
                    |CHROME|
                    |DOWNLOADS|
                    (|CANCEL| |ID|)))))
            (await
              (ps:chain
                |PROBE|
                (|QUIET|
                  (ps:chain
                    |CHROME|
                    |DOWNLOADS|
                    (|removeFile| |ID|)))))
            (await
              (ps:chain
                |PROBE|
                (|QUIET|
                  (ps:chain
                    |CHROME|
                    |DOWNLOADS|
                    (|ERASE| (ps:create |ID| |ID|))))))
            (js-return (|String| |ID|)))))))

(-> api-probe-apis-notifications-javascript () string)
(defun api-probe-apis-notifications-javascript ()
  "JavaScript for the API probe file apis/notifications.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "notifications"
          "create"
          (async-lambda
            ()
            (ps:var
              |ID|
              (await
                (ps:chain
                  |CHROME|
                  |NOTIFICATIONS|
                  (|CREATE|
                    "nyxt-probe"
                    (ps:create
                      |TYPE|
                      "basic"
                      |iconUrl|
                      (ps:chain
                        |CHROME|
                        |RUNTIME|
                        (|getURL| "icon.png"))
                      |TITLE|
                      "Nyxt API probe"
                      |MESSAGE|
                      "notification")))))
            (ps:chain
              |PROBE|
              (|CHECK| (and (eql (ps:typeof |ID|) "string") |ID|) (|String| |ID|)))
            (ps:var
              |ALL|
              (await (ps:chain
                  |CHROME|
                  |NOTIFICATIONS|
                  (|getAll|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |ALL| (aref |ALL| |ID|))
                (ps:chain
                  |json|
                  (|STRINGIFY| |ALL|))))
            (await (ps:chain
                |CHROME|
                |NOTIFICATIONS|
                (|CLEAR| |ID|)))
            (js-return |ID|))))))

(-> api-probe-apis-runtime-javascript () string)
(defun api-probe-apis-runtime-javascript ()
  "JavaScript for the API probe file apis/runtime.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "runtime"
          "id"
          (lambda
            ()
            (ps:chain
              |PROBE|
              (|CHECK|
                (ps:chain
                  (ps:new (funcall |RegExp| "^[a-p]{32}$"))
                  (|TEST| (ps:@ |CHROME| |RUNTIME| |ID|)))
                (ps:@ |CHROME| |RUNTIME| |ID|)))
            (js-return (ps:@ |CHROME| |RUNTIME| |ID|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "runtime"
          "getManifest"
          (lambda
            ()
            (ps:var
              |MANIFEST|
              (ps:chain
                |CHROME|
                |RUNTIME|
                (|getManifest|)))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |MANIFEST| (eql (ps:@ |MANIFEST| |NAME|) "Nyxt API probe"))
                (and |MANIFEST| (ps:@ |MANIFEST| |NAME|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (eql (ps:@ |MANIFEST| |MANIFEST_VERSION|) 3)
                (|String| (ps:@ |MANIFEST| |MANIFEST_VERSION|))))
            (js-return (ps:@ |MANIFEST| |VERSION|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "runtime"
          "getURL"
          (lambda
            ()
            (ps:var
              |URL|
              (ps:chain
                |CHROME|
                |RUNTIME|
                (|getURL| "page.html")))
            (ps:var |EXPECTED| (+ (+ "chrome-extension://" (ps:@ |CHROME| |RUNTIME| |ID|)) "/page.html"))
            (ps:chain
              |PROBE|
              (|CHECK| (eql |URL| |EXPECTED|) |URL|))
            (js-return |URL|))))
    (ps:chain
        |PROBE|
        (|TEST|
          "runtime"
          "sendMessage"
          (lambda
            (|CONTEXT|)
            (ps:chain
              |PROBE|
              (|CHECK|
                (and (ps:@ |CONTEXT| |ECHO|) (eql (ps:@ |CONTEXT| |ECHO| |TEXT|) "nyxt"))
                (ps:chain
                  |json|
                  (|STRINGIFY| (ps:@ |CONTEXT| |ECHO|)))))
            (js-return (ps:@ |CONTEXT| |ECHO| |TEXT|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "runtime"
          "onInstalled"
          (async-lambda
            ()
            (ps:for
              ((|ATTEMPT| 0))
              ((< |ATTEMPT| 20))
              ((incf |ATTEMPT| 1))
              (ps:var
                |STORED|
                (await
                  (ps:chain
                    |CHROME|
                    |STORAGE|
                    |LOCAL|
                    (|GET| "nyxtProbeInstalled"))))
              (if
                (and |STORED| (ps:@ |STORED| |nyxtProbeInstalled|))
                (js-return (ps:@ |STORED| |nyxtProbeInstalled|)))
              (await (ps:chain
                  |PROBE|
                  (|PAUSE| 200))))
            (throw (ps:new (|Error| "onInstalled did not record a reason"))))))))

(-> api-probe-apis-scripting-javascript () string)
(defun api-probe-apis-scripting-javascript ()
  "JavaScript for the API probe file apis/scripting.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "scripting"
          "executeScript"
          (async-lambda
            (|CONTEXT|)
            (ps:chain
              |PROBE|
              (|CHECK| (eql (ps:typeof (ps:@ |CONTEXT| |tabId|)) "number") "no tab id"))
            (ps:var
              |INJECTED|
              (await
                (ps:chain
                  |CHROME|
                  |SCRIPTING|
                  (|executeScript|
                    (ps:create
                      |TARGET|
                      (ps:create
                        |tabId|
                        (ps:@ |CONTEXT| |tabId|))
                      |FUNC|
                      (lambda
                        ()
                        (js-return (ps:@ |LOCATION| |HREF|))))))))
            (ps:var |HREF| (and |INJECTED| (aref |INJECTED| 0) (ps:@ (aref |INJECTED| 0) |RESULT|)))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (eql (ps:typeof |HREF|) "string")
                  (/= (ps:chain
                      |HREF|
                      (|indexOf| "example.com")) (- 1)))
                (ps:chain
                  |json|
                  (|STRINGIFY| |INJECTED|))))
            (js-return |HREF|))))
    (ps:chain
        |PROBE|
        (|TEST|
          "scripting"
          "insertCSS"
          (async-lambda
            (|CONTEXT|)
            (ps:chain
              |PROBE|
              (|CHECK| (eql (ps:typeof (ps:@ |CONTEXT| |tabId|)) "number") "no tab id"))
            (await
              (ps:chain
                |CHROME|
                |SCRIPTING|
                (|insertCSS|
                  (ps:create
                    |TARGET|
                    (ps:create
                      |tabId|
                      (ps:@ |CONTEXT| |tabId|))
                    |CSS|
                    "html { outline: 2px solid rgb(1, 2, 3) !important; }"))))
            (ps:var
              |INJECTED|
              (await
                (ps:chain
                  |CHROME|
                  |SCRIPTING|
                  (|executeScript|
                    (ps:create
                      |TARGET|
                      (ps:create
                        |tabId|
                        (ps:@ |CONTEXT| |tabId|))
                      |FUNC|
                      (lambda
                        ()
                        (js-return (ps:@ (|getComputedStyle| (ps:@ |DOCUMENT| |documentElement|)) |outlineColor|))))))))
            (ps:var |COLOR| (and |INJECTED| (aref |INJECTED| 0) (ps:@ (aref |INJECTED| 0) |RESULT|)))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (eql (ps:typeof |COLOR|) "string")
                  (/= (ps:chain
                      |COLOR|
                      (|indexOf| "1, 2, 3")) (- 1)))
                (|String| |COLOR|)))
            (js-return |COLOR|))))))

(-> api-probe-apis-storage-javascript () string)
(defun api-probe-apis-storage-javascript ()
  "JavaScript for the API probe file apis/storage.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "storage"
          "local"
          (async-lambda
            ()
            (await
              (ps:chain
                |CHROME|
                |STORAGE|
                |LOCAL|
                (|SET| (ps:create |nyxtProbe| "set"))))
            (ps:var
              |STORED|
              (await
                (ps:chain
                  |CHROME|
                  |STORAGE|
                  |LOCAL|
                  (|GET| "nyxtProbe"))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |STORED| (eql (ps:@ |STORED| |nyxtProbe|) "set"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |STORED|))))
            (await
              (ps:chain
                |CHROME|
                |STORAGE|
                |LOCAL|
                (|REMOVE| "nyxtProbe")))
            (js-return (ps:@ |STORED| |nyxtProbe|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "storage"
          "onChanged"
          (lambda
            ()
            (js-return
              (ps:new
                (|Promise|
                  (lambda
                    (|RESOLVE| |REJECT|)
                    (ps:var
                      |TIMER|
                      (|setTimeout|
                        (lambda
                          ()
                          (ps:chain
                            |CHROME|
                            |STORAGE|
                            |onChanged|
                            (|removeListener| |LISTENER|))
                          (|REJECT| (ps:new (|Error| "onChanged timed out")))
                          (js-return))
                        4000))
                    (defun
                      |LISTENER|
                      (|CHANGES| |AREA|)
                      (if
                        (and
                          (eql |AREA| "local")
                          (ps:@ |CHANGES| |nyxtProbeChange|)
                          (eql (ps:@ |CHANGES| |nyxtProbeChange| |newValue|) "changed"))
                        (progn
                          (|clearTimeout| |TIMER|)
                          (ps:chain
                            |CHROME|
                            |STORAGE|
                            |onChanged|
                            (|removeListener| |LISTENER|))
                          (ps:chain
                            |CHROME|
                            |STORAGE|
                            |LOCAL|
                            (|REMOVE| "nyxtProbeChange"))
                          (|RESOLVE| (ps:@ |CHANGES| |nyxtProbeChange| |newValue|))))
                      (js-return))
                    (ps:chain
                      |CHROME|
                      |STORAGE|
                      |onChanged|
                      (|addListener| |LISTENER|))
                    (ps:chain
                      |CHROME|
                      |STORAGE|
                      |LOCAL|
                      (|SET| (ps:create |nyxtProbeChange| "changed")))
                    (js-return))))))))))

(-> api-probe-apis-surface-javascript () string)
(defun api-probe-apis-surface-javascript ()
  "JavaScript for the API probe file apis/surface.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "windows"
          "getCurrent"
          (async-lambda
            ()
            (ps:var
              |WINDOW|
              (await
                (ps:chain
                  |CHROME|
                  |WINDOWS|
                  (|getCurrent| (ps:create |POPULATE| t)))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |WINDOW| (eql (ps:typeof (ps:@ |WINDOW| |ID|)) "number"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |WINDOW|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (ps:chain
                    |Array|
                    (|isArray| (ps:@ |WINDOW| |TABS|)))
                  (> (ps:@ |WINDOW| |TABS| |LENGTH|) 0))
                "no tabs"))
            (js-return (|String| (ps:@ |WINDOW| |ID|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "tabs"
          "update"
          (async-lambda
            (|CONTEXT|)
            (ps:chain
              |PROBE|
              (|CHECK| (eql (ps:typeof (ps:@ |CONTEXT| |tabId|)) "number") "no tab id"))
            (ps:var
              |TAB|
              (await
                (ps:chain
                  |CHROME|
                  |TABS|
                  (|UPDATE| (ps:@ |CONTEXT| |tabId|) (ps:create |URL| "https://example.com/")))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |TAB| (eql (ps:@ |TAB| |ID|) (ps:@ |CONTEXT| |tabId|)))
                (ps:chain
                  |json|
                  (|STRINGIFY| |TAB|))))
            (js-return (or (ps:@ |TAB| |URL|) (|String| (ps:@ |TAB| |ID|)))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "tabs"
          "zoom"
          (async-lambda
            (|CONTEXT|)
            (ps:var
              |BEFORE|
              (await
                (ps:chain
                  |CHROME|
                  |TABS|
                  (|getZoom| (ps:@ |CONTEXT| |tabId|)))))
            (await
              (ps:chain
                |CHROME|
                |TABS|
                (|setZoom| (ps:@ |CONTEXT| |tabId|) 1)))
            (ps:var
              |AFTER|
              (await
                (ps:chain
                  |CHROME|
                  |TABS|
                  (|getZoom| (ps:@ |CONTEXT| |tabId|)))))
            (ps:chain
              |PROBE|
              (|CHECK| (and (eql (ps:typeof |AFTER|) "number") (> |AFTER| 0)) (|String| |AFTER|)))
            (if
              (and (eql (ps:typeof |BEFORE|) "number") (> |BEFORE| 0) (/= |BEFORE| 1))
              (await
                (ps:chain
                  |CHROME|
                  |TABS|
                  (|setZoom| (ps:@ |CONTEXT| |tabId|) |BEFORE|))))
            (js-return (|String| |AFTER|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "webNavigation"
          "getAllFrames"
          (async-lambda
            (|CONTEXT|)
            (ps:var
              |FRAMES|
              (await
                (ps:chain
                  |CHROME|
                  |webNavigation|
                  (|getAllFrames|
                    (ps:create
                      |tabId|
                      (ps:@ |CONTEXT| |tabId|))))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (ps:chain
                    |Array|
                    (|isArray| |FRAMES|))
                  (aref |FRAMES| 0)
                  (eql (ps:@ (aref |FRAMES| 0) |frameId|) 0))
                (ps:chain
                  |json|
                  (|STRINGIFY| |FRAMES|))))
            (js-return (|String| (ps:@ |FRAMES| |LENGTH|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "webNavigation"
          "onCompleted"
          (async-lambda
            (|CONTEXT|)
            (ps:var
              |SEEN|
              (await
                (ps:new
                  (|Promise|
                    (lambda
                      (|RESOLVE|)
                      (ps:var
                        |TIMER|
                        (|setTimeout|
                          (lambda
                            ()
                            (ps:chain
                              |CHROME|
                              |webNavigation|
                              |onCompleted|
                              (|removeListener| |LISTENER|))
                            (|RESOLVE| ps:false)
                            (js-return))
                          8000))
                      (defun
                        |LISTENER|
                        (|DETAILS|)
                        (if
                          (and |DETAILS| (eql (ps:@ |DETAILS| |tabId|) (ps:@ |CONTEXT| |tabId|)))
                          (progn
                            (|clearTimeout| |TIMER|)
                            (ps:chain
                              |CHROME|
                              |webNavigation|
                              |onCompleted|
                              (|removeListener| |LISTENER|))
                            (|RESOLVE| t)))
                        (js-return))
                      (ps:chain
                        |CHROME|
                        |webNavigation|
                        |onCompleted|
                        (|addListener| |LISTENER|))
                      (ps:chain
                        (ps:chain
                          |CHROME|
                          |TABS|
                          (|RELOAD| (ps:@ |CONTEXT| |tabId|)))
                        (|CATCH| (lambda
                            ()
                            (js-return))))
                      (js-return))))))
            (ps:chain
              |PROBE|
              (|CHECK| |SEEN| "onCompleted did not fire"))
            (js-return "completed"))))
    (ps:chain
        |PROBE|
        (|TEST|
          "history"
          "add-search-delete"
          (async-lambda
            ()
            (ps:var |URL| "https://nyxt-probe.invalid/history")
            (await
              (ps:chain
                |CHROME|
                |HISTORY|
                (|addUrl| (ps:create |URL| |URL| |TITLE| "Nyxt probe"))))
            (ps:var
              |FOUND|
              (await
                (ps:chain
                  |CHROME|
                  |HISTORY|
                  (|SEARCH| (ps:create |TEXT| "nyxt-probe.invalid/history")))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (ps:chain
                    |Array|
                    (|isArray| |FOUND|))
                  (ps:chain
                    |FOUND|
                    (|SOME|
                      (lambda
                        (|ITEM|)
                        (js-return (and |ITEM| (eql (ps:@ |ITEM| |URL|) |URL|)))))))
                (ps:chain
                  |json|
                  (|STRINGIFY| |FOUND|))))
            (await
              (ps:chain
                |CHROME|
                |HISTORY|
                (|deleteUrl| (ps:create |URL| |URL|))))
            (ps:var
              |GONE|
              (await
                (ps:chain
                  |CHROME|
                  |HISTORY|
                  (|SEARCH| (ps:create |TEXT| "nyxt-probe.invalid/history")))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (not
                  (ps:chain
                    |GONE|
                    (|SOME|
                      (lambda
                        (|ITEM|)
                        (js-return (and |ITEM| (eql (ps:@ |ITEM| |URL|) |URL|)))))))
                (ps:chain
                  |json|
                  (|STRINGIFY| |GONE|))))
            (js-return (|String| (ps:@ |FOUND| |LENGTH|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "bookmarks"
          "create-search-remove"
          (async-lambda
            ()
            (ps:var |URL| "https://nyxt-probe.invalid/bookmark")
            (ps:var
              |CREATED|
              (await
                (ps:chain
                  |CHROME|
                  |BOOKMARKS|
                  (|CREATE| (ps:create |URL| |URL| |TITLE| "Nyxt probe bookmark")))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |CREATED| (ps:@ |CREATED| |ID|))
                (ps:chain
                  |json|
                  (|STRINGIFY| |CREATED|))))
            (ps:var
              |FOUND|
              (await
                (ps:chain
                  |CHROME|
                  |BOOKMARKS|
                  (|SEARCH| (ps:create |QUERY| "Nyxt probe bookmark")))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (ps:chain
                    |Array|
                    (|isArray| |FOUND|))
                  (ps:chain
                    |FOUND|
                    (|SOME|
                      (lambda
                        (|ITEM|)
                        (js-return (and |ITEM| (eql (ps:@ |ITEM| |URL|) |URL|)))))))
                (ps:chain
                  |json|
                  (|STRINGIFY| |FOUND|))))
            (await
              (ps:chain
                |CHROME|
                |BOOKMARKS|
                (|REMOVE| (ps:@ |CREATED| |ID|))))
            (js-return (|String| (ps:@ |CREATED| |ID|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "permissions"
          "contains"
          (async-lambda
            ()
            (ps:var
              |ALLOWED|
              (await
                (ps:chain
                  |CHROME|
                  |PERMISSIONS|
                  (|CONTAINS| (ps:create
                      |PERMISSIONS|
                      (ps:array "storage"))))))
            (ps:chain
              |PROBE|
              (|CHECK| (eql |ALLOWED| t) (|String| |ALLOWED|)))
            (js-return "storage"))))
    (ps:chain
        |PROBE|
        (|TEST|
          "commands"
          "getAll"
          (async-lambda
            ()
            (ps:var
              |COMMANDS|
              (await (ps:chain
                  |CHROME|
                  |COMMANDS|
                  (|getAll|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (ps:chain
                    |Array|
                    (|isArray| |COMMANDS|))
                  (ps:chain
                    |COMMANDS|
                    (|SOME|
                      (lambda
                        (|COMMAND|)
                        (js-return (and |COMMAND| (eql (ps:@ |COMMAND| |NAME|) "nyxt-probe")))))))
                (ps:chain
                  |json|
                  (|STRINGIFY| |COMMANDS|))))
            (js-return (|String| (ps:@ |COMMANDS| |LENGTH|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "idle"
          "queryState"
          (async-lambda
            ()
            (ps:var
              |STATE|
              (await (ps:chain
                  |CHROME|
                  |IDLE|
                  (|queryState| 60))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (or (eql |STATE| "active") (eql |STATE| "idle") (eql |STATE| "locked"))
                (|String| |STATE|)))
            (js-return |STATE|))))
    (ps:chain
        |PROBE|
        (|TEST|
          "runtime"
          "getPlatformInfo"
          (async-lambda
            ()
            (ps:var
              |INFO|
              (await
                (ps:chain
                  |CHROME|
                  |RUNTIME|
                  (|getPlatformInfo|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |INFO| (eql (ps:typeof (ps:@ |INFO| |OS|)) "string"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |INFO|))))
            (js-return (ps:@ |INFO| |OS|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "management"
          "getSelf"
          (async-lambda
            ()
            (ps:var
              |EXTENSION|
              (await (ps:chain
                  |CHROME|
                  |MANAGEMENT|
                  (|getSelf|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |EXTENSION| (eql (ps:@ |EXTENSION| |ID|) (ps:@ |CHROME| |RUNTIME| |ID|)))
                (ps:chain
                  |json|
                  (|STRINGIFY| |EXTENSION|))))
            (js-return (or (ps:@ |EXTENSION| |NAME|) (ps:@ |EXTENSION| |ID|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "topSites"
          "get"
          (async-lambda
            ()
            (ps:var
              |SITES|
              (await (ps:chain
                  |CHROME|
                  |topSites|
                  (|GET|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (ps:chain
                  |Array|
                  (|isArray| |SITES|))
                (ps:chain
                  |json|
                  (|STRINGIFY| |SITES|))))
            (js-return (|String| (ps:@ |SITES| |LENGTH|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "sessions"
          "getRecentlyClosed"
          (async-lambda
            ()
            (ps:var
              |CLOSED|
              (await
                (ps:chain
                  |CHROME|
                  |SESSIONS|
                  (|getRecentlyClosed|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (ps:chain
                  |Array|
                  (|isArray| |CLOSED|))
                (ps:chain
                  |json|
                  (|STRINGIFY| |CLOSED|))))
            (js-return (|String| (ps:@ |CLOSED| |LENGTH|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "tabGroups"
          "group"
          (async-lambda
            (|CONTEXT|)
            (ps:var
              |groupId|
              (await
                (ps:chain
                  |CHROME|
                  |TABS|
                  (|GROUP|
                    (ps:create
                      |tabIds|
                      (ps:array (ps:@ |CONTEXT| |tabId|)))))))
            (ps:chain
              |PROBE|
              (|CHECK| (eql (ps:typeof |groupId|) "number") (|String| |groupId|)))
            (ps:var
              |GROUPS|
              (await
                (ps:chain
                  |CHROME|
                  |tabGroups|
                  (|QUERY| (ps:create)))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (ps:chain
                  |GROUPS|
                  (|SOME|
                    (lambda
                      (|GROUP|)
                      (js-return (and |GROUP| (eql (ps:@ |GROUP| |ID|) |groupId|))))))
                (ps:chain
                  |json|
                  (|STRINGIFY| |GROUPS|))))
            (await
              (ps:chain
                |CHROME|
                |TABS|
                (|UNGROUP| (ps:array (ps:@ |CONTEXT| |tabId|)))))
            (js-return (|String| |groupId|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "identity"
          "getRedirectURL"
          (lambda
            ()
            (ps:var
              |URL|
              (ps:chain
                |CHROME|
                |IDENTITY|
                (|getRedirectURL|)))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (eql (ps:typeof |URL|) "string")
                  (>=
                    (ps:chain
                      |URL|
                      (|indexOf| (ps:@ |CHROME| |RUNTIME| |ID|)))
                    0))
                (|String| |URL|)))
            (js-return |URL|))))
    (ps:chain
        |PROBE|
        (|TEST|
          "storage"
          "session"
          (async-lambda
            ()
            (await
              (ps:chain
                |CHROME|
                |STORAGE|
                |SESSION|
                (|SET| (ps:create |nyxtProbeSession| "1"))))
            (ps:var
              |STORED|
              (await
                (ps:chain
                  |CHROME|
                  |STORAGE|
                  |SESSION|
                  (|GET| "nyxtProbeSession"))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |STORED| (eql (ps:@ |STORED| |nyxtProbeSession|) "1"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |STORED|))))
            (await
              (ps:chain
                |CHROME|
                |STORAGE|
                |SESSION|
                (|REMOVE| "nyxtProbeSession")))
            (js-return "session"))))
    (ps:chain
        |PROBE|
        (|TEST|
          "scripting"
          "registerContentScripts"
          (async-lambda
            ()
            (await
              (ps:chain
                |CHROME|
                |SCRIPTING|
                (|registerContentScripts|
                  (ps:array
                    (ps:create
                      |ID|
                      "nyxt-probe-registered"
                      |MATCHES|
                      (ps:array "https://example.com/*")
                      |JS|
                      (ps:array "content.js"))))))
            (ps:var
              |REGISTERED|
              (await
                (ps:chain
                  |CHROME|
                  |SCRIPTING|
                  (|getRegisteredContentScripts|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (ps:chain
                  |REGISTERED|
                  (|SOME|
                    (lambda
                      (|SCRIPT|)
                      (js-return (and |SCRIPT| (eql (ps:@ |SCRIPT| |ID|) "nyxt-probe-registered"))))))
                (ps:chain
                  |json|
                  (|STRINGIFY| |REGISTERED|))))
            (await
              (ps:chain
                |CHROME|
                |SCRIPTING|
                (|unregisterContentScripts|
                  (ps:create
                    |IDS|
                    (ps:array "nyxt-probe-registered")))))
            (js-return (|String| (ps:@ |REGISTERED| |LENGTH|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "declarativeNetRequest"
          "dynamicRules"
          (async-lambda
            ()
            (await
              (ps:chain
                |CHROME|
                |declarativeNetRequest|
                (|updateDynamicRules|
                  (ps:create
                    |removeRuleIds|
                    (ps:array 900001)
                    |addRules|
                    (ps:array
                      (ps:create
                        |ID|
                        900001
                        |PRIORITY|
                        1
                        |ACTION|
                        (ps:create |TYPE| "block")
                        |CONDITION|
                        (ps:create
                          |urlFilter|
                          "||nyxt-probe.invalid^"
                          |resourceTypes|
                          (ps:array "main_frame"))))))))
            (ps:var
              |RULES|
              (await
                (ps:chain
                  |CHROME|
                  |declarativeNetRequest|
                  (|getDynamicRules|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (ps:chain
                    |Array|
                    (|isArray| |RULES|))
                  (ps:chain
                    |RULES|
                    (|SOME|
                      (lambda
                        (|RULE|)
                        (js-return (and |RULE| (eql (ps:@ |RULE| |ID|) 900001)))))))
                (ps:chain
                  |json|
                  (|STRINGIFY| |RULES|))))
            (await
              (ps:chain
                |CHROME|
                |declarativeNetRequest|
                (|updateDynamicRules|
                  (ps:create
                    |removeRuleIds|
                    (ps:array 900001)))))
            (js-return (|String| (ps:@ |RULES| |LENGTH|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "downloads"
          "onCreated"
          (async-lambda
            ()
            (ps:var |CREATED| ps:false)
            (defun
              |LISTENER|
              (|ITEM|)
              (if
                |ITEM|
                (setf |CREATED| t))
              (js-return))
            (ps:chain
              |CHROME|
              |DOWNLOADS|
              |onCreated|
              (|addListener| |LISTENER|))
            (ps:var
              |ID|
              (await
                (ps:chain
                  |CHROME|
                  |DOWNLOADS|
                  (|DOWNLOAD|
                    (ps:create
                      |URL|
                      "https://example.com/"
                      |FILENAME|
                      "nyxt-probe-event.html"
                      |conflictAction|
                      "overwrite"
                      |saveAs|
                      ps:false)))))
            (await (ps:chain
                |CHROME|
                |DOWNLOADS|
                (|PAUSE| |ID|)))
            (await (ps:chain
                |CHROME|
                |DOWNLOADS|
                (|RESUME| |ID|)))
            (ps:for
              ((|ATTEMPT| 0))
              ((and (< |ATTEMPT| 20) (not |CREATED|)))
              ((incf |ATTEMPT| 1))
              (await (ps:chain
                  |PROBE|
                  (|PAUSE| 100))))
            (ps:chain
              |CHROME|
              |DOWNLOADS|
              |onCreated|
              (|removeListener| |LISTENER|))
            (await
              (ps:chain
                |PROBE|
                (|QUIET|
                  (ps:chain
                    |CHROME|
                    |DOWNLOADS|
                    (|ERASE| (ps:create |ID| |ID|))))))
            (ps:chain
              |PROBE|
              (|CHECK| |CREATED| "onCreated did not fire"))
            (js-return (|String| |ID|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "sidePanel"
          "setOptions"
          (async-lambda
            ()
            (await
              (ps:chain
                |CHROME|
                |sidePanel|
                (|setOptions| (ps:create |PATH| "page.html"))))
            (ps:var
              |OPTIONS|
              (await
                (ps:chain
                  |CHROME|
                  |sidePanel|
                  (|getOptions| (ps:create)))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |OPTIONS| (eql (ps:@ |OPTIONS| |PATH|) "page.html"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |OPTIONS|))))
            (js-return (ps:@ |OPTIONS| |PATH|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "offscreen"
          "hasDocument"
          (async-lambda
            ()
            (ps:var
              |PRESENT|
              (await
                (ps:chain
                  |CHROME|
                  |OFFSCREEN|
                  (|hasDocument|))))
            (ps:chain
              |PROBE|
              (|CHECK| (eql |PRESENT| ps:false) (|String| |PRESENT|)))
            (js-return "none"))))
    (ps:chain
        |PROBE|
        (|TEST|
          "proxy"
          "get"
          (async-lambda
            ()
            (ps:var
              |SETTINGS|
              (await
                (ps:chain
                  |CHROME|
                  |PROXY|
                  |SETTINGS|
                  (|GET| (ps:create)))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  |SETTINGS|
                  (ps:@ |SETTINGS| |VALUE|)
                  (eql (ps:typeof (ps:@ |SETTINGS| |VALUE| |MODE|)) "string"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |SETTINGS|))))
            (js-return (ps:@ |SETTINGS| |VALUE| |MODE|)))))
    (ps:chain
        |PROBE|
        (|TEST|
          "browsingData"
          "settings"
          (async-lambda
            ()
            (ps:var
              |SETTINGS|
              (await
                (ps:chain
                  |CHROME|
                  |browsingData|
                  (|SETTINGS|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |SETTINGS| (eql (ps:typeof |SETTINGS|) "object"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |SETTINGS|))))
            (js-return "settings"))))
    (ps:chain
        |PROBE|
        (|TEST|
          "userScripts"
          "getScripts"
          (async-lambda
            ()
            (ps:var
              |SCRIPTS|
              (await
                (ps:chain
                  |CHROME|
                  |userScripts|
                  (|getScripts|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (ps:chain
                  |Array|
                  (|isArray| |SCRIPTS|))
                (ps:chain
                  |json|
                  (|STRINGIFY| |SCRIPTS|))))
            (js-return (|String| (ps:@ |SCRIPTS| |LENGTH|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "tts"
          "getVoices"
          (async-lambda
            ()
            (ps:var
              |VOICES|
              (await (ps:chain
                  |CHROME|
                  |TTS|
                  (|getVoices|))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (ps:chain
                    |Array|
                    (|isArray| |VOICES|))
                  (> (ps:@ |VOICES| |LENGTH|) 0))
                (ps:chain
                  |json|
                  (|STRINGIFY| |VOICES|))))
            (js-return (or (ps:@ (aref |VOICES| 0) |voiceName|) "voice")))))
    (ps:chain
        |PROBE|
        (|TEST|
          "i18n"
          "getUILanguage"
          (lambda
            ()
            (ps:var
              |LANGUAGE|
              (ps:chain
                |CHROME|
                |I18N|
                (|getUILanguage|)))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and (eql (ps:typeof |LANGUAGE|) "string") (> (ps:@ |LANGUAGE| |LENGTH|) 0))
                (|String| |LANGUAGE|)))
            (js-return |LANGUAGE|))))
    (ps:chain
        |PROBE|
        (|TEST|
          "cookies"
          "getAll"
          (async-lambda
            ()
            (ps:var
              |COOKIES|
              (await
                (ps:chain
                  |CHROME|
                  |COOKIES|
                  (|getAll| (ps:create |URL| "https://example.com/")))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (ps:chain
                  |Array|
                  (|isArray| |COOKIES|))
                (ps:chain
                  |json|
                  (|STRINGIFY| |COOKIES|))))
            (js-return (|String| (ps:@ |COOKIES| |LENGTH|))))))))

(-> api-probe-apis-tabs-javascript () string)
(defun api-probe-apis-tabs-javascript ()
  "JavaScript for the API probe file apis/tabs.js."
  (ps:ps
    (ps:chain
        |PROBE|
        (|TEST|
          "tabs"
          "query"
          (async-lambda
            (|CONTEXT|)
            (ps:var
              |TABS|
              (await
                (ps:chain
                  |CHROME|
                  |TABS|
                  (|QUERY| (ps:create)))))
            (setf
              (ps:@ |CONTEXT| |TABS|)
              (if
                (ps:chain
                  |Array|
                  (|isArray| |TABS|))
                |TABS|
                (ps:array)))
            (ps:var
              |WEB|
              (ps:chain
                |CONTEXT|
                |TABS|
                (|FIND|
                  (lambda
                    (|TAB|)
                    (js-return
                      (and
                        |TAB|
                        (eql (ps:typeof (ps:@ |TAB| |URL|)) "string")
                        (ps:chain
                          (ps:new (funcall |RegExp| "^https?:"))
                          (|TEST| (ps:@ |TAB| |URL|)))))))))
            (if
              |WEB|
              (setf (ps:@ |CONTEXT| |tabId|) (ps:@ |WEB| |ID|)))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and
                  (ps:chain
                    |Array|
                    (|isArray| |TABS|))
                  (> (ps:@ |TABS| |LENGTH|) 0))
                "0 tabs"))
            (js-return (|String| (ps:@ |TABS| |LENGTH|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "tabs"
          "create"
          (async-lambda
            (|CONTEXT|)
            (ps:var
              |CREATED|
              (await
                (ps:chain
                  |CHROME|
                  |TABS|
                  (|CREATE| (ps:create |URL| "https://example.com/" |ACTIVE| ps:false)))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |CREATED| (eql (ps:typeof (ps:@ |CREATED| |ID|)) "number"))
                (ps:chain
                  |json|
                  (|STRINGIFY| |CREATED|))))
            (setf (ps:@ |CONTEXT| |tabId|) (ps:@ |CREATED| |ID|))
            (setf (ps:@ |CONTEXT| |createdTabId|) (ps:@ |CREATED| |ID|))
            (ps:var
              |COMPLETED|
              (await
                (ps:new
                  (|Promise|
                    (lambda
                      (|RESOLVE|)
                      (ps:var
                        |TIMER|
                        (|setTimeout|
                          (lambda
                            ()
                            (ps:chain
                              |CHROME|
                              |TABS|
                              |onUpdated|
                              (|removeListener| |LISTENER|))
                            (|RESOLVE| ps:false)
                            (js-return))
                          8000))
                      (defun
                        |LISTENER|
                        (|ID| |INFO|)
                        (if
                          (and (eql |ID| (ps:@ |CREATED| |ID|)) (eql (ps:@ |INFO| |STATUS|) "complete"))
                          (progn
                            (|clearTimeout| |TIMER|)
                            (ps:chain
                              |CHROME|
                              |TABS|
                              |onUpdated|
                              (|removeListener| |LISTENER|))
                            (|RESOLVE| t)))
                        (js-return))
                      (ps:chain
                        |CHROME|
                        |TABS|
                        |onUpdated|
                        (|addListener| |LISTENER|))
                      (ps:chain
                        (ps:chain
                          (ps:chain
                            |CHROME|
                            |TABS|
                            (|GET| (ps:@ |CREATED| |ID|)))
                          (|THEN|
                            (lambda
                              (|TAB|)
                              (if
                                (and |TAB| (eql (ps:@ |TAB| |STATUS|) "complete"))
                                (progn
                                  (|clearTimeout| |TIMER|)
                                  (ps:chain
                                    |CHROME|
                                    |TABS|
                                    |onUpdated|
                                    (|removeListener| |LISTENER|))
                                  (|RESOLVE| t)))
                              (js-return))))
                        (|CATCH| (lambda
                            ()
                            (js-return))))
                      (js-return))))))
            (ps:chain
              |PROBE|
              (|CHECK| |COMPLETED| "navigation did not finish"))
            (js-return (|String| (ps:@ |CREATED| |ID|))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "tabs"
          "get"
          (async-lambda
            (|CONTEXT|)
            (ps:chain
              |PROBE|
              (|CHECK| (eql (ps:typeof (ps:@ |CONTEXT| |tabId|)) "number") "no tab id"))
            (ps:var
              |TAB|
              (await
                (ps:chain
                  |CHROME|
                  |TABS|
                  (|GET| (ps:@ |CONTEXT| |tabId|)))))
            (ps:chain
              |PROBE|
              (|CHECK|
                (and |TAB| (eql (ps:@ |TAB| |ID|) (ps:@ |CONTEXT| |tabId|)))
                (ps:chain
                  |json|
                  (|STRINGIFY| |TAB|))))
            (js-return (or (ps:@ |TAB| |URL|) (|String| (ps:@ |TAB| |ID|)))))))
    (ps:chain
        |PROBE|
        (|TEST|
          "tabs"
          "sendMessage"
          (async-lambda
            (|CONTEXT|)
            (ps:chain
              |PROBE|
              (|CHECK| (eql (ps:typeof (ps:@ |CONTEXT| |tabId|)) "number") "no tab id"))
            (ps:var |LAST|)
            (ps:for
              ((|ATTEMPT| 0))
              ((< |ATTEMPT| 25))
              ((incf |ATTEMPT| 1))
              (ps:try
                (progn
                  (ps:var
                    |REPLY|
                    (await
                      (ps:chain
                        |CHROME|
                        |TABS|
                        (|sendMessage| (ps:@ |CONTEXT| |tabId|) (ps:create |TYPE| "content-ping")))))
                  (if
                    (and |REPLY| (ps:@ |REPLY| |OK|))
                    (js-return (ps:@ |REPLY| |HREF|)))
                  (setf
                    |LAST|
                    (ps:new
                      (|Error| (ps:chain
                          |json|
                          (|STRINGIFY| |REPLY|))))))
                (:catch (|ERROR|) (setf |LAST| |ERROR|)))
              (await (ps:chain
                  |PROBE|
                  (|PAUSE| 200))))
            (throw (or |LAST| (ps:new (|Error| "content script did not answer")))))))))

    (write-api-probe-text (uiop:merge-pathnames* #p"background.js" root) (api-probe-background-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"content.js" root) (api-probe-content-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"page.js" root) (api-probe-page-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/action.js" root) (api-probe-apis-action-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/alarms.js" root) (api-probe-apis-alarms-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/context-menus.js" root) (api-probe-apis-context-menus-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/cookies.js" root) (api-probe-apis-cookies-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/downloads.js" root) (api-probe-apis-downloads-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/notifications.js" root) (api-probe-apis-notifications-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/runtime.js" root) (api-probe-apis-runtime-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/scripting.js" root) (api-probe-apis-scripting-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/storage.js" root) (api-probe-apis-storage-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/surface.js" root) (api-probe-apis-surface-javascript))
    (write-api-probe-text (uiop:merge-pathnames* #p"apis/tabs.js" root) (api-probe-apis-tabs-javascript))
    root))

(-> write-api-probe-text (pathname string) pathname)
(defun write-api-probe-text (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
    (write-string text out))
  path)
