;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

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

(-> extension-shim-javascript () string)
(defun extension-shim-javascript ()
  "JavaScript for the extension API shim."
  (ps:ps
    (funcall
        (lambda
          ()
          (if
            (ps:@ |globalThis| |__nyxtShim|)
            (js-return))
          (setf (ps:@ |globalThis| |__nyxtShim|) t)
          (ps:var |BRIDGE| "http://127.0.0.1:__NYXT_PORT__")
          (ps:var |TOKEN| "__NYXT_TOKEN__")
          (ps:var |PENDING| (ps:new (|Map|)))
          (ps:var |SEQUENCE| 0)
          (ps:var |BAGS| (ps:create))
          (ps:var |clickGates| (ps:create))
          (ps:var |registeredScripts| (ps:array))
          (ps:var |userScripts| (ps:array))
          (ps:var |offscreenTab| nil)
          (ps:var |sidePanelPath| "")
          (ps:var |grantedOrigins| (ps:array))
          (defun
            |RPC|
            (|OP| |PAYLOAD|)
            (ps:var
              |BODY|
              (ps:create
                |OP|
                |OP|
                |extensionId|
                (and (ps:@ |CHROME| |RUNTIME|) (ps:@ |CHROME| |RUNTIME| |ID|))))
            (if
              |PAYLOAD|
              (ps:chain
                (ps:chain
                  |Object|
                  (|KEYS| |PAYLOAD|))
                (|forEach|
                  (lambda
                    (|KEY|)
                    (setf (aref |BODY| |KEY|) (aref |PAYLOAD| |KEY|))
                    (js-return)))))
            (js-return
              (ps:chain
                (ps:chain
                  (|FETCH|
                    (+ |BRIDGE| "/rpc")
                    (ps:create
                      |METHOD|
                      "POST"
                      |HEADERS|
                      (ps:create "content-type" "application/json" "x-nyxt-token" |TOKEN|)
                      |BODY|
                      (ps:chain
                        |json|
                        (|STRINGIFY| |BODY|))))
                  (|THEN|
                    (lambda
                      (|RESPONSE|)
                      (js-return (ps:chain
                          |RESPONSE|
                          (|JSON|))))))
                (|THEN|
                  (lambda
                    (|DECODED|)
                    (if
                      (or (not |DECODED|) (eql (ps:@ |DECODED| |OK|) ps:false))
                      (throw
                        (ps:new
                          (|Error|
                            (if
                              (and |DECODED| (ps:@ |DECODED| |ERROR|))
                              (ps:@ |DECODED| |ERROR|)
                              "Nyxt bridge failed")))))
                    (if
                      (ps:@ |DECODED| |EVENT|)
                      (|DISPATCH| (ps:@ |DECODED| |EVENT|)))
                    (js-return (ps:@ |DECODED| |RESULT|)))))))
          (defun
            |BAG|
            (|TYPE|)
            (if
              (not (aref |BAGS| |TYPE|))
              (setf (aref |BAGS| |TYPE|) (ps:array)))
            (js-return (aref |BAGS| |TYPE|)))
          (defun
            |EMIT|
            (|TYPE| |ARGS|)
            (ps:chain
              (ps:chain
                (|BAG| |TYPE|)
                (|SLICE|))
              (|forEach|
                (lambda
                  (|LISTENER|)
                  (ps:try
                    (ps:chain
                      |LISTENER|
                      (|APPLY| nil (or |ARGS| (ps:array))))
                    (:catch (|ERROR|) nil))
                  (js-return))))
            (js-return))
          (defun
            |decideRequest|
            (|EVENT|)
            (ps:var |CANCEL| ps:false)
            (ps:chain
              (ps:chain
                (|BAG| (ps:@ |EVENT| |TYPE|))
                (|SLICE|))
              (|forEach|
                (lambda
                  (|LISTENER|)
                  (ps:try
                    (progn
                      (ps:var |RESULT| (|LISTENER| (or (ps:@ |EVENT| |DETAILS|) (ps:create))))
                      (if
                        (and |RESULT| (ps:@ |RESULT| |CANCEL|))
                        (setf |CANCEL| t)))
                    (:catch (|ERROR|) nil))
                  (js-return))))
            (ps:chain
              (|RPC|
                "webRequest.decision"
                (ps:create
                  |decisionId|
                  (ps:@ |EVENT| |decisionId|)
                  |CANCEL|
                  |CANCEL|))
              (|CATCH| (lambda
                  ()
                  (js-return))))
            (js-return))
          (defun
            |DISPATCH|
            (|EVENT|)
            (if
              (or (not |EVENT|) (not (ps:@ |EVENT| |TYPE|)))
              (js-return))
            (if
              (ps:@ |EVENT| |decisionId|)
              (progn
                (|decideRequest| |EVENT|)
                (js-return)))
            (if
              (not (ps:@ |EVENT| |ARGS|))
              (js-return))
            (|EMIT| (ps:@ |EVENT| |TYPE|) (ps:@ |EVENT| |ARGS|))
            (if
              (eql (ps:@ |EVENT| |TYPE|) "tabs.onUpdated")
              (|applyRegistered|
                (aref (ps:@ |EVENT| |ARGS|) 0)
                (aref (ps:@ |EVENT| |ARGS|) 1)
                (aref (ps:@ |EVENT| |ARGS|) 2)))
            (js-return))
          (defun
            |POLL|
            ()
            (ps:chain
              (ps:chain
                (ps:chain
                  (|RPC| "events")
                  (|THEN|
                    (lambda
                      (|EVENTS|)
                      (ps:chain
                        (or |EVENTS| (ps:array))
                        (|forEach| |DISPATCH|))
                      (js-return))))
                (|CATCH| (lambda
                    ()
                    (js-return))))
              (|THEN|
                (lambda
                  ()
                  (|setTimeout| |POLL| 50)
                  (js-return))))
            (js-return))
          (if
            (or
              (eql (ps:typeof |importScripts|) "function")
              (and
                (eql (ps:typeof |ServiceWorkerGlobalScope|) "function")
                (ps:instanceof |SELF| |ServiceWorkerGlobalScope|)))
            (|setTimeout| |POLL| 0))
          (defun
            |BOTH|
            (|PROMISE| |CALLBACK|)
            (if
              (eql (ps:typeof |CALLBACK|) "function")
              (ps:chain
                |PROMISE|
                (|THEN|
                  (lambda
                    (|VALUE|)
                    (ps:try
                      (setf (ps:@ |CHROME| |RUNTIME| |lastError|) |UNDEFINED|)
                      (:catch (|ERROR|) nil))
                    (|CALLBACK| |VALUE|)
                    (js-return))
                  (lambda
                    (|ERROR|)
                    (ps:var
                      |MESSAGE|
                      (if
                        (and |ERROR| (ps:@ |ERROR| |MESSAGE|))
                        (ps:@ |ERROR| |MESSAGE|)
                        (|String| |ERROR|)))
                    (ps:try
                      (setf (ps:@ |CHROME| |RUNTIME| |lastError|) (ps:create |MESSAGE| |MESSAGE|))
                      (:catch (|IGNORED|) nil))
                    (ps:try
                      (|CALLBACK|)
                      (:finally
                        (ps:try
                          (setf (ps:@ |CHROME| |RUNTIME| |lastError|) |UNDEFINED|)
                          (:catch (|IGNORED|) nil))))
                    (js-return)))))
            (js-return |PROMISE|))
          (defun
            |REPLACE|
            (|OBJECT| |NAME| |FN|)
            (if
              (not |OBJECT|)
              (js-return))
            (ps:try
              (progn
                (setf (aref |OBJECT| |NAME|) |FN|)
                (js-return))
              (:catch (|ERROR|) nil))
            (ps:try
              (ps:chain
                |Object|
                (|defineProperty| |OBJECT| |NAME| (ps:create |CONFIGURABLE| t |WRITABLE| t |VALUE| |FN|)))
              (:catch (|ERROR|) nil))
            (js-return))
          (defun
            |LISTEN|
            (|EVENT| |TYPE|)
            (if
              (not |EVENT|)
              (js-return))
            (ps:var |LIST| (|BAG| |TYPE|))
            (|REPLACE|
              |EVENT|
              "addListener"
              (lambda
                (|LISTENER|)
                (if
                  (eql (ps:typeof |LISTENER|) "function")
                  (progn
                    (ps:var
                      |WRAPPED|
                      (if
                        (or
                          (eql |TYPE| "action.onClicked")
                          (eql |TYPE| "contextMenus.onClicked"))
                        (lambda
                          ()
                          (ps:var |NOW| (ps:chain |Date| (|now|)))
                          (ps:var |KEY| |TYPE|)
                          (if
                            (eql |TYPE| "contextMenus.onClicked")
                            (progn
                              (ps:var
                                |INFO|
                                (and
                                  |ARGUMENTS|
                                  (plusp (ps:@ |ARGUMENTS| |LENGTH|))
                                  (aref |ARGUMENTS| 0)))
                              (setf
                                |KEY|
                                (+
                                  |TYPE|
                                  " "
                                  (if
                                    |INFO|
                                    (+ "" (or (ps:@ |INFO| |menuItemId|) ""))
                                    "")))))
                          (ps:var |PREVIOUS| (aref |clickGates| |KEY|))
                          (if
                            (and |PREVIOUS| (< (- |NOW| |PREVIOUS|) 1000))
                            (js-return))
                          (setf (aref |clickGates| |KEY|) |NOW|)
                          (js-return
                            (ps:chain |LISTENER| (|APPLY| nil |ARGUMENTS|))))
                        |LISTENER|))
                    (if
                      (< (ps:chain |LIST| (|indexOf| |WRAPPED|)) 0)
                      (ps:chain |LIST| (|PUSH| |WRAPPED|)))))
                (js-return)))
            (|REPLACE|
              |EVENT|
              "removeListener"
              (lambda
                (|LISTENER|)
                (ps:var |INDEX| (ps:chain
                    |LIST|
                    (|indexOf| |LISTENER|)))
                (if
                  (>= |INDEX| 0)
                  (ps:chain
                    |LIST|
                    (|SPLICE| |INDEX| 1)))
                (js-return)))
            (|REPLACE|
              |EVENT|
              "hasListener"
              (lambda
                (|LISTENER|)
                (js-return (>= (ps:chain
                      |LIST|
                      (|indexOf| |LISTENER|)) 0))))
            (js-return))
          (defun
            |HAS|
            (|OBJECT| |NAME|)
            (js-return (not (not (and |OBJECT| (eql (ps:typeof (aref |OBJECT| |NAME|)) "function"))))))
          (defun
            |ENSURE|
            (|OBJECT| |NAME| |FN|)
            (if
              (or (not |OBJECT|) (|HAS| |OBJECT| |NAME|))
              (js-return))
            (|REPLACE| |OBJECT| |NAME| |FN|)
            (js-return))
          (defun
            |ensureEvent|
            (|OBJECT| |NAME| |TYPE|)
            (if
              (not |OBJECT|)
              (js-return))
            (if
              (not (aref |OBJECT| |NAME|))
              (setf (aref |OBJECT| |NAME|) (ps:create)))
            (if
              (/= (ps:typeof (ps:@ (aref |OBJECT| |NAME|) |addListener|)) "function")
              (|LISTEN| (aref |OBJECT| |NAME|) |TYPE|))
            (js-return))
          (setf (ps:@ |CHROME| |TABS|) (or (ps:@ |CHROME| |TABS|) (ps:create)))
          (setf (ps:@ |CHROME| |TABS| |onUpdated|) (or (ps:@ |CHROME| |TABS| |onUpdated|) (ps:create)))
          (if
            (ps:@ |CHROME| |TABS|)
            (progn
              (|REPLACE|
                (ps:@ |CHROME| |TABS|)
                "query"
                (lambda
                  (|QUERY| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (ps:chain
                        (|RPC|
                          "tabs.query"
                          (ps:create
                            |QUERY|
                            (or |QUERY| (ps:create))))
                        (|THEN|
                          (lambda
                            (|TABS|)
                            (js-return (or |TABS| (ps:array))))))
                      |CALLBACK|))))
              (|REPLACE|
                (ps:@ |CHROME| |TABS|)
                "create"
                (lambda
                  (|PROPERTIES| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (|RPC|
                        "tabs.create"
                        (ps:create
                          |PROPERTIES|
                          (or |PROPERTIES| (ps:create))))
                      |CALLBACK|))))
              (|REPLACE|
                (ps:@ |CHROME| |TABS|)
                "get"
                (lambda
                  (|tabId| |CALLBACK|)
                  (js-return (|BOTH| (|RPC| "tabs.get" (ps:create |tabId| |tabId|)) |CALLBACK|))))
              (|REPLACE|
                (ps:@ |CHROME| |TABS|)
                "remove"
                (lambda
                  (|tabId| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (ps:chain
                        (|RPC| "tabs.remove" (ps:create |tabId| |tabId|))
                        (|THEN| (lambda
                            ()
                            (js-return))))
                      |CALLBACK|))))
              (|REPLACE|
                (ps:@ |CHROME| |TABS|)
                "sendMessage"
                (lambda
                  (|tabId| |MESSAGE| |OPTIONS| |CALLBACK|)
                  (if
                    (eql (ps:typeof |OPTIONS|) "function")
                    (progn
                      (setf |CALLBACK| |OPTIONS|)
                      (setf |OPTIONS| |UNDEFINED|)))
                  (ps:var |TICKET| (|String| (incf |SEQUENCE|)))
                  (ps:var
                    |PROMISE|
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
                                  |PENDING|
                                  (|DELETE| |TICKET|))
                                (|REJECT| (ps:new (|Error| "Could not establish connection. Receiving end does not exist.")))
                                (js-return))
                              1200))
                          (ps:chain
                            |PENDING|
                            (|SET| |TICKET| (ps:create |RESOLVE| |RESOLVE| |REJECT| |REJECT| |TIMER| |TIMER|)))
                          (js-return)))))
                  (ps:chain
                    (|RPC| "tabs.sendMessage" (ps:create |tabId| |tabId| |MESSAGE| |MESSAGE| |TOKEN| |TICKET|))
                    (|CATCH|
                      (lambda
                        (|ERROR|)
                        (ps:var
                          |JOB|
                          (ps:chain
                            |PENDING|
                            (|GET| |TICKET|)))
                        (if
                          (not |JOB|)
                          (js-return))
                        (|clearTimeout| (ps:@ |JOB| |TIMER|))
                        (ps:chain
                          |PENDING|
                          (|DELETE| |TICKET|))
                        (ps:chain
                          |JOB|
                          (|REJECT| |ERROR|))
                        (js-return))))
                  (js-return (|BOTH| |PROMISE| |CALLBACK|))))
              (|LISTEN| (ps:@ |CHROME| |TABS| |onUpdated|) "tabs.onUpdated")))
          (ps:var
            |addRuntimeListener|
            (ps:chain
              |CHROME|
              |RUNTIME|
              |onMessage|
              |addListener|
              (|BIND| (ps:@ |CHROME| |RUNTIME| |onMessage|))))
          (defun
            |listenerWithNyxtTab|
            (|LISTENER|)
            (js-return
              (lambda
                (|MESSAGE| |SENDER| |sendResponse|)
                (if
                  (or (not |MESSAGE|) (/= (ps:@ |MESSAGE| |PATH|) "page-to-background"))
                  (js-return (|LISTENER| |MESSAGE| |SENDER| |sendResponse|)))
                (ps:chain
                  (ps:chain
                    (|RPC| "tabs.query" (ps:create
                        |QUERY|
                        (ps:create)))
                    (|THEN|
                      (lambda
                        (|TABS|)
                        (ps:var
                          |PAGE|
                          (or
                            (and
                              |SENDER|
                              (or (ps:@ |SENDER| |URL|) (and (ps:@ |SENDER| |TAB|) (ps:@ |SENDER| |TAB| |URL|))))
                            ""))
                        (ps:var |MATCH| nil)
                        (ps:chain
                          (or |TABS| (ps:array))
                          (|forEach|
                            (lambda
                              (|TAB|)
                              (if
                                (and (not |MATCH|) |PAGE| (eql (ps:@ |TAB| |URL|) |PAGE|))
                                (setf |MATCH| |TAB|))
                              (js-return))))
                        (if
                          (not |MATCH|)
                          (ps:chain
                            (or |TABS| (ps:array))
                            (|forEach|
                              (lambda
                                (|TAB|)
                                (if
                                  (and (not |MATCH|) (ps:@ |TAB| |ACTIVE|))
                                  (setf |MATCH| |TAB|))
                                (js-return)))))
                        (if
                          |MATCH|
                          (setf
                            |SENDER|
                            (ps:chain
                              |Object|
                              (|ASSIGN|
                                (ps:create)
                                |SENDER|
                                (ps:create
                                  |TAB|
                                  (ps:chain
                                    |Object|
                                    (|ASSIGN|
                                      (ps:create)
                                      (and |SENDER| (ps:@ |SENDER| |TAB|))
                                      (ps:create
                                        |ID|
                                        (ps:@ |MATCH| |ID|)
                                        |URL|
                                        (or (ps:@ |MATCH| |URL|) (and |SENDER| (ps:@ |SENDER| |TAB|) (ps:@ |SENDER| |TAB| |URL|)) "")
                                        |TITLE|
                                        (or
                                          (ps:@ |MATCH| |TITLE|)
                                          (and |SENDER| (ps:@ |SENDER| |TAB|) (ps:@ |SENDER| |TAB| |TITLE|))
                                          "")))))))))
                        (|LISTENER| |MESSAGE| |SENDER| |sendResponse|)
                        (js-return))))
                  (|CATCH|
                    (lambda
                      ()
                      (|LISTENER| |MESSAGE| |SENDER| |sendResponse|)
                      (js-return))))
                (js-return t))))
          (setf
            (ps:@ |CHROME| |RUNTIME| |onMessage| |addListener|)
            (lambda
              (|LISTENER|)
              (js-return (|addRuntimeListener| (|listenerWithNyxtTab| |LISTENER|)))))
          (|addRuntimeListener|
            (lambda
              (|MESSAGE| |SENDER| |sendResponse|)
              (if
                (or (not |MESSAGE|) (/= (ps:@ |MESSAGE| |__NYXT|) "reply"))
                (js-return))
              (ps:var |JOB| (ps:chain
                  |PENDING|
                  (|GET| (ps:@ |MESSAGE| |TOKEN|))))
              (if
                (not |JOB|)
                (js-return))
              (|clearTimeout| (ps:@ |JOB| |TIMER|))
              (ps:chain
                |PENDING|
                (|DELETE| (ps:@ |MESSAGE| |TOKEN|)))
              (ps:chain
                |JOB|
                (|RESOLVE| (ps:@ |MESSAGE| |RESULT|)))
              (|sendResponse| (ps:create |OK| t))
              (js-return t)))
          (setf (ps:@ |CHROME| |SCRIPTING|) (or (ps:@ |CHROME| |SCRIPTING|) (ps:create)))
          (|REPLACE|
            (ps:@ |CHROME| |SCRIPTING|)
            "executeScript"
            (lambda
              (|INJECTION|)
              (ps:var |TARGET| (or (and |INJECTION| (ps:@ |INJECTION| |TARGET|)) (ps:create)))
              (if
                (and |INJECTION| (eql (ps:typeof (ps:@ |INJECTION| |FUNC|)) "function"))
                (progn
                  (ps:var
                    |SOURCE|
                    (+
                      (+
                        (+
                          (+
                            "("
                            (ps:chain
                              |INJECTION|
                              |FUNC|
                              (|toString|)))
                          ").apply(null, ")
                        (ps:chain
                          |json|
                          (|STRINGIFY| (or (ps:@ |INJECTION| |ARGS|) (ps:array)))))
                      ")"))
                  (js-return
                    (ps:chain
                      (|RPC|
                        "scripting.executeScript"
                        (ps:create
                          |tabId|
                          (ps:@ |TARGET| |tabId|)
                          |CODE|
                          |SOURCE|))
                      (|THEN|
                        (lambda
                          (|VALUE|)
                          (js-return (ps:array (ps:create |RESULT| |VALUE|)))))))))
              (ps:var |FILES| (or (and |INJECTION| (ps:@ |INJECTION| |FILES|)) (ps:array)))
              (js-return
                (ps:chain
                  (ps:chain
                    (ps:chain
                      |Promise|
                      (|ALL|
                        (ps:chain
                          |FILES|
                          (|MAP|
                            (lambda
                              (|FILE|)
                              (js-return
                                (ps:chain
                                  (|FETCH|
                                    (ps:chain
                                      |CHROME|
                                      |RUNTIME|
                                      (|getURL| |FILE|)))
                                  (|THEN|
                                    (lambda
                                      (|RESPONSE|)
                                      (js-return
                                        (ps:chain
                                          |RESPONSE|
                                          (|TEXT|))))))))))))
                    (|THEN|
                      (lambda
                        (|PARTS|)
                        (js-return
                          (|RPC|
                            "scripting.executeScript"
                            (ps:create
                              |tabId|
                              (ps:@ |TARGET| |tabId|)
                              |CODE|
                              (ps:chain
                                |PARTS|
                                (|JOIN| #.(format nil "~%")))
                              |STATEMENTS|
                              t))))))
                  (|THEN|
                    (lambda
                      (|VALUE|)
                      (js-return (ps:array (ps:create |RESULT| |VALUE|)))))))))
          (|REPLACE|
            (ps:@ |CHROME| |SCRIPTING|)
            "insertCSS"
            (lambda
              (|INJECTION|)
              (ps:var |TARGET| (or (and |INJECTION| (ps:@ |INJECTION| |TARGET|)) (ps:create)))
              (ps:var |CSS| (or (and |INJECTION| (or (ps:@ |INJECTION| |CSS|) (ps:@ |INJECTION| |CODE|))) ""))
              (ps:var |TOKEN| (+ "nyxt-" (incf |SEQUENCE|)))
              (ps:var
                |CODE|
                (ps:chain
                  #.(ps:ps
                     (funcall
                      (lambda ()
                        (ps:var css |__nyxt_css__|)
                        (ps:var id |__nyxt_id__|)
                        (ps:var node (ps:chain document (create-element "style")))
                        (ps:chain node (set-attribute "data-nyxt-css" id))
                        (setf (ps:@ node text-content) css)
                        (ps:chain (or (ps:@ document head)
                                      (ps:@ document document-element))
                                  (append-child node)))))
                  (|REPLACE| "__NYXT_CSS__" (ps:chain |json| (|STRINGIFY| |CSS|)))
                  (|REPLACE| "__NYXT_ID__" (ps:chain |json| (|STRINGIFY| |TOKEN|)))))
              (js-return
                (ps:chain
                  (|RPC|
                    "scripting.executeScript"
                    (ps:create
                      |tabId|
                      (ps:@ |TARGET| |tabId|)
                      |CODE|
                      |CODE|
                      |STATEMENTS|
                      t))
                  (|THEN| (lambda
                      ()
                      (js-return |TOKEN|)))))))
          (|REPLACE|
            (ps:@ |CHROME| |SCRIPTING|)
            "removeCSS"
            (lambda
              (|INJECTION|)
              (ps:var |TARGET| (or (and |INJECTION| (ps:@ |INJECTION| |TARGET|)) (ps:create)))
              (ps:var |CSS| (or (and |INJECTION| (or (ps:@ |INJECTION| |CSS|) (ps:@ |INJECTION| |CODE|))) ""))
              (ps:var
                |CODE|
                (ps:chain
                  #.(ps:ps
                     (funcall
                      (lambda ()
                        (ps:var css |__nyxt_css__|)
                        (ps:chain
                         (ps:chain document (query-selector-all "style[data-nyxt-css]"))
                         (for-each
                          (lambda (node)
                            (if (eql (ps:@ node text-content) css)
                                (ps:chain node (remove)))))))))
                  (|REPLACE| "__NYXT_CSS__" (ps:chain |json| (|STRINGIFY| |CSS|)))))
              (js-return
                (ps:chain
                  (|RPC|
                    "scripting.executeScript"
                    (ps:create
                      |tabId|
                      (ps:@ |TARGET| |tabId|)
                      |CODE|
                      |CODE|
                      |STATEMENTS|
                      t))
                  (|THEN| (lambda
                      ()
                      (js-return)))))))
          (setf (ps:@ |CHROME| |ACTION|) (or (ps:@ |CHROME| |ACTION|) (ps:create)))
          (setf
            (ps:@ |CHROME| |ACTION| |onClicked|)
            (or (ps:@ |CHROME| |ACTION| |onClicked|) (ps:create)))
          (|LISTEN| (ps:@ |CHROME| |ACTION| |onClicked|) "action.onClicked")
          (if
            (ps:@ |CHROME| |DOWNLOADS|)
            (progn
              (|REPLACE|
                (ps:@ |CHROME| |DOWNLOADS|)
                "download"
                (lambda
                  (|OPTIONS| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (|RPC|
                        "downloads.download"
                        (ps:create
                          |OPTIONS|
                          (or |OPTIONS| (ps:create))))
                      |CALLBACK|))))
              (|REPLACE|
                (ps:@ |CHROME| |DOWNLOADS|)
                "search"
                (lambda
                  (|QUERY| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (ps:chain
                        (|RPC|
                          "downloads.search"
                          (ps:create
                            |QUERY|
                            (or |QUERY| (ps:create))))
                        (|THEN|
                          (lambda
                            (|ITEMS|)
                            (js-return (or |ITEMS| (ps:array))))))
                      |CALLBACK|))))
              (|REPLACE|
                (ps:@ |CHROME| |DOWNLOADS|)
                "cancel"
                (lambda
                  (|ID| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (ps:chain
                        (|RPC| "downloads.cancel" (ps:create |ID| |ID|))
                        (|THEN| (lambda
                            ()
                            (js-return))))
                      |CALLBACK|))))
              (|REPLACE|
                (ps:@ |CHROME| |DOWNLOADS|)
                "removeFile"
                (lambda
                  (|ID| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (ps:chain
                        (|RPC| "downloads.removeFile" (ps:create |ID| |ID|))
                        (|THEN| (lambda
                            ()
                            (js-return))))
                      |CALLBACK|))))
              (|REPLACE|
                (ps:@ |CHROME| |DOWNLOADS|)
                "erase"
                (lambda
                  (|QUERY| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (|RPC|
                        "downloads.erase"
                        (ps:create
                          |QUERY|
                          (or |QUERY| (ps:create))))
                      |CALLBACK|))))))
          (setf (ps:@ |CHROME| |TABS| |tab_id_none|) (- 1))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "update"
            (lambda
              (|tabId| |PROPERTIES| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "tabs.update"
                    (ps:create
                      |tabId|
                      |tabId|
                      |PROPERTIES|
                      (or |PROPERTIES| (ps:create))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "reload"
            (lambda
              (|tabId| |PROPERTIES| |CALLBACK|)
              (if
                (eql (ps:typeof |PROPERTIES|) "function")
                (setf |CALLBACK| |PROPERTIES|))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "tabs.reload" (ps:create |tabId| |tabId|))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "duplicate"
            (lambda
              (|tabId| |CALLBACK|)
              (js-return (|BOTH| (|RPC| "tabs.duplicate" (ps:create |tabId| |tabId|)) |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "getCurrent"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "tabs.query"
                      (ps:create
                        |QUERY|
                        (ps:create |ACTIVE| t)))
                    (|THEN|
                      (lambda
                        (|TABS|)
                        (js-return (and |TABS| (aref |TABS| 0))))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "move"
            (lambda
              (|tabId| |PROPERTIES| |CALLBACK|)
              (js-return (|BOTH| (|RPC| "tabs.get" (ps:create |tabId| |tabId|)) |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "highlight"
            (lambda
              (|INFO| |CALLBACK|)
              (ps:var |tabIds| (or (and |INFO| (or (ps:@ |INFO| |TABS|) (ps:@ |INFO| |tabIds|))) (ps:array)))
              (ps:var |tabId| (aref |tabIds| 0))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "tabs.update"
                      (ps:create
                        |tabId|
                        |tabId|
                        |PROPERTIES|
                        (ps:create |ACTIVE| t)))
                    (|THEN|
                      (lambda
                        ()
                        (js-return (ps:create |windowId| 1 |tabIds| |tabIds|)))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "discard"
            (lambda
              (|tabId| |CALLBACK|)
              (js-return (|BOTH| (|RPC| "tabs.get" (ps:create |tabId| |tabId|)) |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "goBack"
            (lambda
              (|tabId| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "tabs.go"
                      (ps:create
                        |tabId|
                        |tabId|
                        |DIRECTION|
                        (- 1)))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "goForward"
            (lambda
              (|tabId| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "tabs.go" (ps:create |tabId| |tabId| |DIRECTION| 1))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "getZoom"
            (lambda
              (|tabId| |CALLBACK|)
              (js-return (|BOTH| (|RPC| "tabs.zoom" (ps:create |tabId| |tabId|)) |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "setZoom"
            (lambda
              (|tabId| |FACTOR| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "tabs.zoom" (ps:create |tabId| |tabId| |FACTOR| |FACTOR|))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "detectLanguage"
            (lambda
              (|tabId| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "scripting.executeScript"
                    (ps:create |tabId| |tabId| |CODE| "(document.documentElement.lang||navigator.language||\"en\")"))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "group"
            (lambda
              (|OPTIONS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "tabs.group"
                    (ps:create
                      |PROPERTIES|
                      (or |OPTIONS| (ps:create))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "ungroup"
            (lambda
              (|tabIds| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "tabs.ungroup" (ps:create |tabIds| |tabIds|))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "captureVisibleTab"
            (lambda
              (|windowId| |OPTIONS| |CALLBACK|)
              (if
                (eql (ps:typeof |windowId|) "function")
                (setf |CALLBACK| |windowId|)
                (if
                  (eql (ps:typeof |OPTIONS|) "function")
                  (setf |CALLBACK| |OPTIONS|)))
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|REJECT| (ps:new (|Error| "Taking screenshots is not available."))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "executeScript"
            (lambda
              (|tabId| |DETAILS| |CALLBACK|)
              (ps:var |CODE| (and |DETAILS| (or (ps:@ |DETAILS| |CODE|) "")))
              (if
                (and |DETAILS| (ps:@ |DETAILS| |FILE|))
                (js-return
                  (|BOTH|
                    (ps:chain
                      (ps:chain
                        (|FETCH|
                          (ps:chain
                            |CHROME|
                            |RUNTIME|
                            (|getURL| (ps:@ |DETAILS| |FILE|))))
                        (|THEN|
                          (lambda
                            (|RESPONSE|)
                            (js-return
                              (ps:chain
                                |RESPONSE|
                                (|TEXT|))))))
                      (|THEN|
                        (lambda
                          (|TEXT|)
                          (js-return
                            (|RPC| "scripting.executeScript" (ps:create |tabId| |tabId| |CODE| |TEXT| |STATEMENTS| t))))))
                    |CALLBACK|)))
              (js-return
                (|BOTH| (|RPC| "scripting.executeScript" (ps:create |tabId| |tabId| |CODE| |CODE|)) |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |TABS|)
            "insertCSS"
            (lambda
              (|tabId| |DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |CHROME|
                    |SCRIPTING|
                    (|insertCSS|
                      (ps:create
                        |TARGET|
                        (ps:create |tabId| |tabId|)
                        |CSS|
                        (or (and |DETAILS| (ps:@ |DETAILS| |CODE|)) ""))))
                  |CALLBACK|))))
          (ps:chain
            (ps:array
              "onCreated"
              "onRemoved"
              "onActivated"
              "onHighlighted"
              "onMoved"
              "onAttached"
              "onDetached"
              "onReplaced")
            (|forEach|
              (lambda
                (|NAME|)
                (setf
                  (aref (ps:@ |CHROME| |TABS|) |NAME|)
                  (or (aref (ps:@ |CHROME| |TABS|) |NAME|) (ps:create)))
                (|LISTEN| (aref (ps:@ |CHROME| |TABS|) |NAME|) (+ "tabs." |NAME|))
                (js-return))))
          (setf (ps:@ |CHROME| |WINDOWS|) (or (ps:@ |CHROME| |WINDOWS|) (ps:create)))
          (setf (ps:@ |CHROME| |WINDOWS| |window_id_none|) (- 1))
          (setf (ps:@ |CHROME| |WINDOWS| |window_id_current|) (- 2))
          (|REPLACE|
            (ps:@ |CHROME| |WINDOWS|)
            "get"
            (lambda
              (|windowId| |QUERY| |CALLBACK|)
              (if
                (eql (ps:typeof |QUERY|) "function")
                (progn
                  (setf |CALLBACK| |QUERY|)
                  (setf |QUERY| (ps:create))))
              (js-return
                (|BOTH|
                  (|RPC|
                    "windows.get"
                    (ps:create
                      |windowId|
                      |windowId|
                      |POPULATE|
                      (not (not (and |QUERY| (ps:@ |QUERY| |POPULATE|))))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |WINDOWS|)
            "getCurrent"
            (lambda
              (|QUERY| |CALLBACK|)
              (if
                (eql (ps:typeof |QUERY|) "function")
                (progn
                  (setf |CALLBACK| |QUERY|)
                  (setf |QUERY| (ps:create))))
              (js-return
                (|BOTH|
                  (|RPC|
                    "windows.get"
                    (ps:create
                      |windowId|
                      (- 2)
                      |POPULATE|
                      (not (not (and |QUERY| (ps:@ |QUERY| |POPULATE|))))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |WINDOWS|)
            "getLastFocused"
            (lambda
              (|QUERY| |CALLBACK|)
              (js-return
                (ps:chain
                  |CHROME|
                  |WINDOWS|
                  (|getCurrent| |QUERY| |CALLBACK|)))))
          (|REPLACE|
            (ps:@ |CHROME| |WINDOWS|)
            "getAll"
            (lambda
              (|QUERY| |CALLBACK|)
              (if
                (eql (ps:typeof |QUERY|) "function")
                (progn
                  (setf |CALLBACK| |QUERY|)
                  (setf |QUERY| (ps:create))))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "windows.getAll"
                      (ps:create
                        |POPULATE|
                        (not (not (and |QUERY| (ps:@ |QUERY| |POPULATE|))))))
                    (|THEN|
                      (lambda
                        (|WINDOWS|)
                        (js-return (or |WINDOWS| (ps:array))))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |WINDOWS|)
            "create"
            (lambda
              (|PROPERTIES| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "windows.create"
                    (ps:create
                      |PROPERTIES|
                      (or |PROPERTIES| (ps:create))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |WINDOWS|)
            "update"
            (lambda
              (|windowId| |PROPERTIES| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "windows.update"
                    (ps:create
                      |windowId|
                      |windowId|
                      |PROPERTIES|
                      (or |PROPERTIES| (ps:create))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |WINDOWS|)
            "remove"
            (lambda
              (|windowId| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "windows.remove" (ps:create |windowId| |windowId|))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (ps:chain
            (ps:array "onCreated" "onRemoved" "onFocusChanged")
            (|forEach|
              (lambda
                (|NAME|)
                (setf
                  (aref (ps:@ |CHROME| |WINDOWS|) |NAME|)
                  (or (aref (ps:@ |CHROME| |WINDOWS|) |NAME|) (ps:create)))
                (|LISTEN| (aref (ps:@ |CHROME| |WINDOWS|) |NAME|) (+ "windows." |NAME|))
                (js-return))))
          (setf (ps:@ |CHROME| |webNavigation|) (or (ps:@ |CHROME| |webNavigation|) (ps:create)))
          (ps:chain
            (ps:array
              "onBeforeNavigate"
              "onCommitted"
              "onDOMContentLoaded"
              "onCompleted"
              "onErrorOccurred"
              "onHistoryStateUpdated"
              "onCreatedNavigationTarget"
              "onReferenceFragmentUpdated")
            (|forEach|
              (lambda
                (|NAME|)
                (setf
                  (aref (ps:@ |CHROME| |webNavigation|) |NAME|)
                  (or (aref (ps:@ |CHROME| |webNavigation|) |NAME|) (ps:create)))
                (|LISTEN| (aref (ps:@ |CHROME| |webNavigation|) |NAME|) (+ "webNavigation." |NAME|))
                (js-return))))
          (|REPLACE|
            (ps:@ |CHROME| |webNavigation|)
            "getAllFrames"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "webNavigation.getAllFrames"
                    (ps:create
                      |tabId|
                      (and |DETAILS| (ps:@ |DETAILS| |tabId|))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |webNavigation|)
            "getFrame"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "webNavigation.getFrame"
                    (ps:create
                      |tabId|
                      (and |DETAILS| (ps:@ |DETAILS| |tabId|))))
                  |CALLBACK|))))
          (defun
            |matchPattern|
            (|PATTERN| |URL|)
            (if
              (or (not |PATTERN|) (not |URL|))
              (js-return ps:false))
            (if
              (or (eql |PATTERN| "<all_urls>") (eql |PATTERN| "*://*/*"))
              (js-return
                (ps:chain
                  (ps:new (funcall |RegExp| "^(https?|file|ftp):" "i"))
                  (|TEST| |URL|))))
            (ps:var
              |MATCH|
              (ps:chain
                (ps:new (funcall |RegExp| "^(\\*|https?|file|ftp):\\/\\/(\\*|\\*\\.[^/]+|[^/]+)(\\/.*)$"))
                (|EXEC| |PATTERN|)))
            (if
              (not |MATCH|)
              (js-return ps:false))
            (ps:var |PARSED|)
            (ps:try
              (setf |PARSED| (ps:new (|url| |URL|)))
              (:catch (|ERROR|) (js-return ps:false)))
            (if
              (eql (aref |MATCH| 1) "*")
              (if
                (and (/= (ps:@ |PARSED| |PROTOCOL|) "http:") (/= (ps:@ |PARSED| |PROTOCOL|) "https:"))
                (js-return ps:false))
              (if
                (/= (ps:@ |PARSED| |PROTOCOL|) (+ (aref |MATCH| 1) ":"))
                (js-return ps:false)))
            (ps:var |HOST| (aref |MATCH| 2))
            (if
              (/= |HOST| "*")
              (if
                (eql (ps:chain
                    |HOST|
                    (|SLICE| 0 2)) "*.")
                (progn
                  (ps:var |DOMAIN| (ps:chain
                      |HOST|
                      (|SLICE| 2)))
                  (if
                    (and
                      (/= (ps:@ |PARSED| |HOSTNAME|) |DOMAIN|)
                      (/=
                        (ps:chain
                          |PARSED|
                          |HOSTNAME|
                          (|SLICE| (- (+ (ps:@ |DOMAIN| |LENGTH|) 1))))
                        (+ "." |DOMAIN|)))
                    (js-return ps:false)))
                (if
                  (/= (ps:@ |PARSED| |HOSTNAME|) |HOST|)
                  (js-return ps:false))))
            (ps:var
              |PATH|
              (ps:chain
                (ps:chain
                  (aref |MATCH| 3)
                  (|REPLACE| (ps:new (funcall |RegExp| "[.+?^${}()|[\\]\\\\]" "g")) "\\$&"))
                (|REPLACE| (ps:new (funcall |RegExp| "\\\\\\*" "g")) ".*")))
            (js-return
              (ps:chain
                (ps:new (|RegExp| (+ (+ "^" |PATH|) "$")))
                (|TEST| (ps:@ |PARSED| |PATHNAME|)))))
          (defun
            |applyRegistered|
            (|tabId| |CHANGE| |TAB|)
            (if
              (or (not |CHANGE|) (/= (ps:@ |CHANGE| |STATUS|) "complete"))
              (js-return))
            (ps:var |URL| (or (ps:@ |CHANGE| |URL|) (and |TAB| (ps:@ |TAB| |URL|)) ""))
            (if
              (or (not |URL|) (equal |tabId| nil))
              (js-return))
            (defun
              |RUN|
              (|SCRIPT|)
              (ps:var |MATCHES| (or (ps:@ |SCRIPT| |MATCHES|) (ps:array "<all_urls>")))
              (if
                (not
                  (ps:chain
                    |MATCHES|
                    (|SOME|
                      (lambda
                        (|PATTERN|)
                        (js-return (|matchPattern| |PATTERN| |URL|))))))
                (js-return))
              (ps:chain
                (or (ps:@ |SCRIPT| |JS|) (ps:array))
                (|forEach|
                  (lambda
                    (|FILE|)
                    (ps:chain
                      (ps:chain
                        (ps:chain
                          (|FETCH|
                            (ps:chain
                              |CHROME|
                              |RUNTIME|
                              (|getURL| |FILE|)))
                          (|THEN|
                            (lambda
                              (|RESPONSE|)
                              (js-return
                                (ps:chain
                                  |RESPONSE|
                                  (|TEXT|))))))
                        (|THEN|
                          (lambda
                            (|CODE|)
                            (js-return
                              (|RPC| "scripting.executeScript" (ps:create |tabId| |tabId| |CODE| |CODE| |STATEMENTS| t))))))
                      (|CATCH| (lambda
                          ()
                          (js-return))))
                    (js-return))))
              (ps:chain
                (or (ps:@ |SCRIPT| |CSS|) (ps:array))
                (|forEach|
                  (lambda
                    (|FILE|)
                    (ps:chain
                      (ps:chain
                        (ps:chain
                          (|FETCH|
                            (ps:chain
                              |CHROME|
                              |RUNTIME|
                              (|getURL| |FILE|)))
                          (|THEN|
                            (lambda
                              (|RESPONSE|)
                              (js-return
                                (ps:chain
                                  |RESPONSE|
                                  (|TEXT|))))))
                        (|THEN|
                          (lambda
                            (|CSS|)
                            (js-return
                              (ps:chain
                                |CHROME|
                                |SCRIPTING|
                                (|insertCSS|
                                  (ps:create
                                    |TARGET|
                                    (ps:create |tabId| |tabId|)
                                    |CSS|
                                    |CSS|)))))))
                      (|CATCH| (lambda
                          ()
                          (js-return))))
                    (js-return))))
              (js-return))
            (ps:chain
              |registeredScripts|
              (|forEach| |RUN|))
            (ps:chain
              |userScripts|
              (|forEach| |RUN|))
            (js-return))
          (|REPLACE|
            (ps:@ |CHROME| |SCRIPTING|)
            "registerContentScripts"
            (lambda
              (|SCRIPTS|)
              (ps:chain
                (or |SCRIPTS| (ps:array))
                (|forEach|
                  (lambda
                    (|SCRIPT|)
                    (setf
                      |registeredScripts|
                      (ps:chain
                        |registeredScripts|
                        (|FILTER|
                          (lambda
                            (|ITEM|)
                            (js-return (/= (ps:@ |ITEM| |ID|) (ps:@ |SCRIPT| |ID|)))))))
                    (ps:chain
                      |registeredScripts|
                      (|PUSH| |SCRIPT|))
                    (js-return))))
              (js-return (ps:chain
                  |Promise|
                  (|RESOLVE|)))))
          (|REPLACE|
            (ps:@ |CHROME| |SCRIPTING|)
            "getRegisteredContentScripts"
            (lambda
              (|FILTER|)
              (ps:var |IDS| (and |FILTER| (ps:@ |FILTER| |IDS|)))
              (js-return
                (ps:chain
                  |Promise|
                  (|RESOLVE|
                    (ps:chain
                      |registeredScripts|
                      (|FILTER|
                        (lambda
                          (|SCRIPT|)
                          (js-return
                            (or
                              (not |IDS|)
                              (>=
                                (ps:chain
                                  |IDS|
                                  (|indexOf| (ps:@ |SCRIPT| |ID|)))
                                0)))))))))))
          (|REPLACE|
            (ps:@ |CHROME| |SCRIPTING|)
            "unregisterContentScripts"
            (lambda
              (|FILTER|)
              (ps:var |IDS| (and |FILTER| (ps:@ |FILTER| |IDS|)))
              (setf
                |registeredScripts|
                (if
                  |IDS|
                  (ps:chain
                    |registeredScripts|
                    (|FILTER|
                      (lambda
                        (|SCRIPT|)
                        (js-return
                          (<
                            (ps:chain
                              |IDS|
                              (|indexOf| (ps:@ |SCRIPT| |ID|)))
                            0)))))
                  (ps:array)))
              (js-return (ps:chain
                  |Promise|
                  (|RESOLVE|)))))
          (|REPLACE|
            (ps:@ |CHROME| |SCRIPTING|)
            "updateContentScripts"
            (lambda
              (|SCRIPTS|)
              (js-return
                (ps:chain
                  |CHROME|
                  |SCRIPTING|
                  (|registerContentScripts| |SCRIPTS|)))))
          (setf (ps:@ |CHROME| |DOWNLOADS|) (or (ps:@ |CHROME| |DOWNLOADS|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |DOWNLOADS|)
            "download"
            (lambda
              (|OPTIONS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "downloads.download"
                    (ps:create
                      |OPTIONS|
                      (or |OPTIONS| (ps:create))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |DOWNLOADS|)
            "search"
            (lambda
              (|QUERY| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "downloads.search"
                      (ps:create
                        |QUERY|
                        (or |QUERY| (ps:create))))
                    (|THEN|
                      (lambda
                        (|ITEMS|)
                        (js-return (or |ITEMS| (ps:array))))))
                  |CALLBACK|))))
          (ps:chain
            (ps:array "pause" "resume")
            (|forEach|
              (lambda
                (|NAME|)
                (|REPLACE|
                  (ps:@ |CHROME| |DOWNLOADS|)
                  |NAME|
                  (lambda
                    (|ID| |CALLBACK|)
                    (js-return
                      (|BOTH|
                        (ps:chain
                          (|RPC| (+ "downloads." |NAME|) (ps:create |ID| |ID|))
                          (|THEN| (lambda
                              ()
                              (js-return))))
                        |CALLBACK|))))
                (js-return))))
          (ps:chain
            (ps:array "open" "show" "showDefaultFolder" "setShelfEnabled" "setUiOptions" "acceptDanger")
            (|forEach|
              (lambda
                (|NAME|)
                (|ENSURE|
                  (ps:@ |CHROME| |DOWNLOADS|)
                  |NAME|
                  (lambda
                    (|FIRST| |CALLBACK|)
                    (if
                      (eql (ps:typeof |FIRST|) "function")
                      (setf |CALLBACK| |FIRST|))
                    (js-return
                      (|BOTH| (ps:chain
                          |Promise|
                          (|RESOLVE|)) |CALLBACK|))))
                (js-return))))
          (setf
            (ps:@ |CHROME| |DOWNLOADS| |onCreated|)
            (or (ps:@ |CHROME| |DOWNLOADS| |onCreated|) (ps:create)))
          (setf
            (ps:@ |CHROME| |DOWNLOADS| |onChanged|)
            (or (ps:@ |CHROME| |DOWNLOADS| |onChanged|) (ps:create)))
          (setf
            (ps:@ |CHROME| |DOWNLOADS| |onDeterminingFilename|)
            (or (ps:@ |CHROME| |DOWNLOADS| |onDeterminingFilename|) (ps:create)))
          (|LISTEN| (ps:@ |CHROME| |DOWNLOADS| |onCreated|) "downloads.onCreated")
          (|LISTEN| (ps:@ |CHROME| |DOWNLOADS| |onChanged|) "downloads.onChanged")
          (|LISTEN| (ps:@ |CHROME| |DOWNLOADS| |onDeterminingFilename|) "downloads.onDeterminingFilename")
          (setf (ps:@ |CHROME| |contextMenus|) (or (ps:@ |CHROME| |contextMenus|) (ps:create)))
          (ps:var |menuMirror| (ps:array))
          (ps:var |menuRevision| 0)
          (ps:var |menuFlushScheduled| ps:false)
          (defun
            |scheduleMenuFlush|
            ()
            (if
              |menuFlushScheduled|
              (js-return))
            (setf |menuFlushScheduled| t)
            (ps:chain
              (ps:chain |Promise| (|RESOLVE|))
              (|THEN|
                (lambda
                  ()
                  (setf |menuFlushScheduled| ps:false)
                  (ps:var |REVISION| (incf |menuRevision|))
                  (|RPC|
                    "contextMenus.replace"
                    (ps:create
                      |REVISION|
                      |REVISION|
                      |ITEMS|
                      (ps:chain |menuMirror| (|SLICE|))))
                  (js-return))))
            (js-return))
          (defun
            |menuIdText|
            (|VALUE|)
            (js-return
              (if
                (or (eql |VALUE| |UNDEFINED|) (eql |VALUE| nil))
                ""
                (+ "" |VALUE|))))
          (defun
            |rememberMenu|
            (|PROPERTIES|)
            (ps:var |ID| (|menuIdText| (and |PROPERTIES| (ps:@ |PROPERTIES| |ID|))))
            (ps:var
              |TITLE|
              (if
                (and |PROPERTIES| (ps:@ |PROPERTIES| |TITLE|))
                (+ "" (ps:@ |PROPERTIES| |TITLE|))
                |ID|))
            (ps:var |NEXT| (ps:array))
            (ps:var |FOUND| ps:false)
            (ps:chain
              |menuMirror|
              (|forEach|
                (lambda
                  (|ITEM|)
                  (if
                    (and (not (equal |ID| "")) (equal (|menuIdText| (ps:@ |ITEM| |ID|)) |ID|))
                    (progn
                      (setf |FOUND| t)
                      (ps:chain |NEXT| (|PUSH| (ps:create |ID| |ID| |TITLE| |TITLE|))))
                    (ps:chain |NEXT| (|PUSH| |ITEM|)))
                  (js-return))))
            (if
              (not |FOUND|)
              (ps:chain |NEXT| (|PUSH| (ps:create |ID| |ID| |TITLE| |TITLE|))))
            (setf |menuMirror| |NEXT|)
            (js-return))
          (|REPLACE|
            (ps:@ |CHROME| |contextMenus|)
            "create"
            (lambda
              (|PROPERTIES| |CALLBACK|)
              (|rememberMenu| (or |PROPERTIES| (ps:create)))
              (|scheduleMenuFlush|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (and |PROPERTIES| (ps:@ |PROPERTIES| |ID|))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |contextMenus|)
            "remove"
            (lambda
              (|menuId| |CALLBACK|)
              (ps:var |ID| (|menuIdText| |menuId|))
              (setf
                |menuMirror|
                (ps:chain
                  |menuMirror|
                  (|FILTER|
                    (lambda
                      (|ITEM|)
                      (js-return (not (equal (|menuIdText| (ps:@ |ITEM| |ID|)) |ID|)))))))
              (if
                (eql (ps:typeof |CALLBACK|) "function")
                (|CALLBACK|))
              (|scheduleMenuFlush|)
              (js-return (ps:chain |Promise| (|RESOLVE|)))))
          (|REPLACE|
            (ps:@ |CHROME| |contextMenus|)
            "removeAll"
            (lambda
              (|CALLBACK|)
              (setf |menuMirror| (ps:array))
              (if
                (eql (ps:typeof |CALLBACK|) "function")
                (|CALLBACK|))
              (|scheduleMenuFlush|)
              (js-return (ps:chain |Promise| (|RESOLVE|)))))
          (|REPLACE|
            (ps:@ |CHROME| |contextMenus|)
            "update"
            (lambda
              (|menuId| |PROPERTIES| |CALLBACK|)
              (ps:var |ITEM| (or |PROPERTIES| (ps:create)))
              (if
                (eql (ps:@ |ITEM| |ID|) |UNDEFINED|)
                (setf (ps:@ |ITEM| |ID|) |menuId|))
              (|rememberMenu| |ITEM|)
              (if
                (eql (ps:typeof |CALLBACK|) "function")
                (|CALLBACK|))
              (|scheduleMenuFlush|)
              (js-return (ps:chain |Promise| (|RESOLVE|)))))
          (setf
            (ps:@ |CHROME| |contextMenus| |onClicked|)
            (or (ps:@ |CHROME| |contextMenus| |onClicked|) (ps:create)))
          (|LISTEN| (ps:@ |CHROME| |contextMenus| |onClicked|) "contextMenus.onClicked")
          (|ensureEvent| (ps:@ |CHROME| |contextMenus|) "onShown" "contextMenus.onShown")
          (ps:var
            |actionState|
            (ps:create
              |TITLE|
              ""
              |TEXT|
              ""
              |COLOR|
              (ps:array 0 0 0 0)
              |POPUP|
              ""))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "setTitle"
            (lambda
              (|DETAILS| |CALLBACK|)
              (setf (ps:@ |actionState| |TITLE|) (or (and |DETAILS| (ps:@ |DETAILS| |TITLE|)) ""))
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "getTitle"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:@ |actionState| |TITLE|)))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "setBadgeText"
            (lambda
              (|DETAILS| |CALLBACK|)
              (setf (ps:@ |actionState| |TEXT|) (or (and |DETAILS| (ps:@ |DETAILS| |TEXT|)) ""))
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "getBadgeText"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:@ |actionState| |TEXT|)))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "setBadgeBackgroundColor"
            (lambda
              (|DETAILS| |CALLBACK|)
              (setf
                (ps:@ |actionState| |COLOR|)
                (or (and |DETAILS| (ps:@ |DETAILS| |COLOR|)) (ps:@ |actionState| |COLOR|)))
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "getBadgeBackgroundColor"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:@ |actionState| |COLOR|)))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "setBadgeTextColor"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "setIcon"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "setPopup"
            (lambda
              (|DETAILS| |CALLBACK|)
              (setf (ps:@ |actionState| |POPUP|) (or (and |DETAILS| (ps:@ |DETAILS| |POPUP|)) ""))
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "getPopup"
            (lambda
              (|DETAILS| |CALLBACK|)
              (ps:var
                |MANIFEST|
                (or
                  (and
                    (ps:@ |CHROME| |RUNTIME| |getManifest|)
                    (ps:chain
                      |CHROME|
                      |RUNTIME|
                      (|getManifest|)))
                  (ps:create)))
              (ps:var
                |POPUP|
                (or
                  (ps:@ |actionState| |POPUP|)
                  (and (ps:@ |MANIFEST| |ACTION|) (ps:@ |MANIFEST| |ACTION| |DEFAULT_POPUP|))
                  ""))
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| |POPUP|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "enable"
            (lambda
              (|tabId| |CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "disable"
            (lambda
              (|tabId| |CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "isEnabled"
            (lambda
              (|tabId| |CALLBACK|)
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| t)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "getUserSettings"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:create |isOnToolbar| ps:false)))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |ACTION|)
            "openPopup"
            (lambda
              (|OPTIONS| |CALLBACK|)
              (ps:var |POPUP| (ps:@ |actionState| |POPUP|))
              (if
                (and (not |POPUP|) (ps:@ |CHROME| |RUNTIME| |getManifest|))
                (progn
                  (ps:var
                    |MANIFEST|
                    (ps:chain
                      |CHROME|
                      |RUNTIME|
                      (|getManifest|)))
                  (setf |POPUP| (and (ps:@ |MANIFEST| |ACTION|) (ps:@ |MANIFEST| |ACTION| |DEFAULT_POPUP|)))))
              (if
                (not |POPUP|)
                (js-return
                  (|BOTH| (ps:chain
                      |Promise|
                      (|RESOLVE|)) |CALLBACK|)))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "tabs.create"
                      (ps:create
                        |PROPERTIES|
                        (ps:create
                          |URL|
                          (ps:chain
                            |CHROME|
                            |RUNTIME|
                            (|getURL| |POPUP|))
                          |ACTIVE|
                          t)))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |ACTION|) "onUserSettingsChanged" "action.onUserSettingsChanged")
          (if
            (not (ps:@ |CHROME| |browserAction|))
            (setf (ps:@ |CHROME| |browserAction|) (ps:@ |CHROME| |ACTION|)))
          (if
            (not (ps:@ |CHROME| |pageAction|))
            (setf (ps:@ |CHROME| |pageAction|) (ps:@ |CHROME| |ACTION|)))
          (defun
            |storageArea|
            (|AREA|)
            (ps:var |DATA| (ps:create))
            (defun
              |PICK|
              (|KEYS|)
              (if
                (equal |KEYS| nil)
                (js-return (ps:chain
                    |Object|
                    (|ASSIGN| (ps:create) |DATA|))))
              (if
                (eql (ps:typeof |KEYS|) "string")
                (setf |KEYS| (ps:array |KEYS|)))
              (if
                (ps:chain
                  |Array|
                  (|isArray| |KEYS|))
                (progn
                  (ps:var |RESULT| (ps:create))
                  (ps:chain
                    |KEYS|
                    (|forEach|
                      (lambda
                        (|KEY|)
                        (if
                          (ps:in |KEY| |DATA|)
                          (setf (aref |RESULT| |KEY|) (aref |DATA| |KEY|)))
                        (js-return))))
                  (js-return |RESULT|)))
              (ps:var |FILLED| (ps:chain
                  |Object|
                  (|ASSIGN| (ps:create) |KEYS|)))
              (ps:chain
                (ps:chain
                  |Object|
                  (|KEYS| |DATA|))
                (|forEach|
                  (lambda
                    (|KEY|)
                    (if
                      (ps:chain
                        |Object|
                        |PROTOTYPE|
                        |hasOwnProperty|
                        (|CALL| |KEYS| |KEY|))
                      (setf (aref |FILLED| |KEY|) (aref |DATA| |KEY|)))
                    (js-return))))
              (js-return |FILLED|))
            (js-return
              (ps:create
                |GET|
                (lambda
                  (|KEYS| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (ps:chain
                        |Promise|
                        (|RESOLVE| (|PICK| |KEYS|)))
                      |CALLBACK|)))
                |SET|
                (lambda
                  (|ITEMS| |CALLBACK|)
                  (ps:var |CHANGES| (ps:create))
                  (ps:chain
                    (ps:chain
                      |Object|
                      (|KEYS| (or |ITEMS| (ps:create))))
                    (|forEach|
                      (lambda
                        (|KEY|)
                        (setf
                          (aref |CHANGES| |KEY|)
                          (ps:create
                            |oldValue|
                            (aref |DATA| |KEY|)
                            |newValue|
                            (aref |ITEMS| |KEY|)))
                        (setf (aref |DATA| |KEY|) (aref |ITEMS| |KEY|))
                        (js-return))))
                  (|EMIT| "storage.onChanged" (ps:array |CHANGES| (or |AREA| "session")))
                  (js-return
                    (|BOTH| (ps:chain
                        |Promise|
                        (|RESOLVE|)) |CALLBACK|)))
                |REMOVE|
                (lambda
                  (|KEYS| |CALLBACK|)
                  (ps:chain
                    (if
                      (ps:chain
                        |Array|
                        (|isArray| |KEYS|))
                      |KEYS|
                      (ps:array |KEYS|))
                    (|forEach|
                      (lambda
                        (|KEY|)
                        (delete (aref |DATA| |KEY|))
                        (js-return))))
                  (js-return
                    (|BOTH| (ps:chain
                        |Promise|
                        (|RESOLVE|)) |CALLBACK|)))
                |CLEAR|
                (lambda
                  (|CALLBACK|)
                  (setf |DATA| (ps:create))
                  (js-return
                    (|BOTH| (ps:chain
                        |Promise|
                        (|RESOLVE|)) |CALLBACK|)))
                |getBytesInUse|
                (lambda
                  (|KEYS| |CALLBACK|)
                  (js-return
                    (|BOTH|
                      (ps:chain
                        |Promise|
                        (|RESOLVE|
                          (ps:@
                            (ps:chain
                              |json|
                              (|STRINGIFY| (|PICK| |KEYS|)))
                            |LENGTH|)))
                      |CALLBACK|))))))
          (setf (ps:@ |CHROME| |STORAGE|) (or (ps:@ |CHROME| |STORAGE|) (ps:create)))
          (if
            (not (ps:@ |CHROME| |STORAGE| |SESSION|))
            (setf (ps:@ |CHROME| |STORAGE| |SESSION|) (|storageArea| "session")))
          (if
            (not (ps:@ |CHROME| |STORAGE| |SYNC|))
            (setf
              (ps:@ |CHROME| |STORAGE| |SYNC|)
              (or (ps:@ |CHROME| |STORAGE| |LOCAL|) (|storageArea| "sync"))))
          (if
            (not (ps:@ |CHROME| |STORAGE| |MANAGED|))
            (setf (ps:@ |CHROME| |STORAGE| |MANAGED|) (|storageArea| "managed")))
          (|ensureEvent| (ps:@ |CHROME| |STORAGE|) "onChanged" "storage.onChanged")
          (if
            (and
              (ps:@ |CHROME| |STORAGE| |LOCAL|)
              (eql (ps:typeof (ps:@ |CHROME| |STORAGE| |LOCAL| |GET|)) "function"))
            (progn
              (ps:var
                |nativeStorageGet|
                (ps:chain
                  |CHROME|
                  |STORAGE|
                  |LOCAL|
                  |GET|
                  (|BIND| (ps:@ |CHROME| |STORAGE| |LOCAL|))))
              (defun
                |onceCallback|
                (|FN|)
                (ps:var |DONE| ps:false)
                (js-return
                  (lambda
                    ()
                    (if
                      |DONE|
                      (js-return))
                    (setf |DONE| t)
                    (if
                      (eql (ps:typeof |FN|) "function")
                      (js-return (ps:chain |FN| (|APPLY| nil |ARGUMENTS|))))
                    (js-return))))
              (|REPLACE|
                (ps:@ |CHROME| |STORAGE| |LOCAL|)
                "get"
                (lambda
                  (|KEYS| |CALLBACK|)
                  (if
                    (eql (ps:typeof |CALLBACK|) "function")
                    (js-return (|nativeStorageGet| |KEYS| (|onceCallback| |CALLBACK|))))
                  (js-return (|nativeStorageGet| |KEYS|))))))
          (defun
            |manifestPermissions|
            ()
            (ps:var
              |MANIFEST|
              (if
                (ps:@ |CHROME| |RUNTIME| |getManifest|)
                (ps:chain
                  |CHROME|
                  |RUNTIME|
                  (|getManifest|))
                (ps:create)))
            (js-return
              (ps:create
                |PERMISSIONS|
                (ps:chain
                  (or (ps:@ |MANIFEST| |PERMISSIONS|) (ps:array))
                  (|CONCAT| (or (ps:@ |grantedOrigins| |_PERMISSIONS|) (ps:array))))
                |ORIGINS|
                (ps:chain
                  (ps:chain
                    (ps:chain
                      (or (ps:@ |MANIFEST| |HOST_PERMISSIONS|) (ps:array))
                      (|CONCAT| (or (ps:@ |MANIFEST| |PERMISSIONS|) (ps:array))))
                    (|FILTER|
                      (lambda
                        (|ITEM|)
                        (js-return
                          (or
                            (>=
                              (ps:chain
                                |ITEM|
                                (|indexOf| "://"))
                              0)
                            (eql |ITEM| "<all_urls>"))))))
                  (|CONCAT| |grantedOrigins|)))))
          (setf (ps:@ |CHROME| |PERMISSIONS|) (or (ps:@ |CHROME| |PERMISSIONS|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |PERMISSIONS|)
            "getAll"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (|manifestPermissions|)))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |PERMISSIONS|)
            "contains"
            (lambda
              (|REQUESTED| |CALLBACK|)
              (ps:var |HAVE| (|manifestPermissions|))
              (ps:var
                |PERMISSIONS|
                (ps:chain
                  (or (and |REQUESTED| (ps:@ |REQUESTED| |PERMISSIONS|)) (ps:array))
                  (|EVERY|
                    (lambda
                      (|ITEM|)
                      (js-return
                        (>=
                          (ps:chain
                            |HAVE|
                            |PERMISSIONS|
                            (|indexOf| |ITEM|))
                          0))))))
              (ps:var
                |ORIGINS|
                (ps:chain
                  (or (and |REQUESTED| (ps:@ |REQUESTED| |ORIGINS|)) (ps:array))
                  (|EVERY|
                    (lambda
                      (|ORIGIN|)
                      (js-return
                        (or
                          (>=
                            (ps:chain
                              |HAVE|
                              |ORIGINS|
                              (|indexOf| "<all_urls>"))
                            0)
                          (>=
                            (ps:chain
                              |HAVE|
                              |ORIGINS|
                              (|indexOf| |ORIGIN|))
                            0)
                          (>=
                            (ps:chain
                              |HAVE|
                              |ORIGINS|
                              (|indexOf| "*://*/*"))
                            0)))))))
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (and |PERMISSIONS| |ORIGINS|)))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |PERMISSIONS|)
            "request"
            (lambda
              (|REQUESTED| |CALLBACK|)
              (setf
                (ps:@ |grantedOrigins| |_PERMISSIONS|)
                (or (ps:@ |grantedOrigins| |_PERMISSIONS|) (ps:array)))
              (ps:chain
                (or (and |REQUESTED| (ps:@ |REQUESTED| |PERMISSIONS|)) (ps:array))
                (|forEach|
                  (lambda
                    (|ITEM|)
                    (if
                      (<
                        (ps:chain
                          |grantedOrigins|
                          |_PERMISSIONS|
                          (|indexOf| |ITEM|))
                        0)
                      (ps:chain
                        |grantedOrigins|
                        |_PERMISSIONS|
                        (|PUSH| |ITEM|)))
                    (js-return))))
              (ps:chain
                (or (and |REQUESTED| (ps:@ |REQUESTED| |ORIGINS|)) (ps:array))
                (|forEach|
                  (lambda
                    (|ORIGIN|)
                    (if
                      (<
                        (ps:chain
                          |grantedOrigins|
                          (|indexOf| |ORIGIN|))
                        0)
                      (ps:chain
                        |grantedOrigins|
                        (|PUSH| |ORIGIN|)))
                    (js-return))))
              (|EMIT| "permissions.onAdded" (ps:array (or |REQUESTED| (ps:create))))
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| t)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |PERMISSIONS|)
            "remove"
            (lambda
              (|REQUESTED| |CALLBACK|)
              (|EMIT| "permissions.onRemoved" (ps:array (or |REQUESTED| (ps:create))))
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| t)) |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |PERMISSIONS|) "onAdded" "permissions.onAdded")
          (|ensureEvent| (ps:@ |CHROME| |PERMISSIONS|) "onRemoved" "permissions.onRemoved")
          (setf (ps:@ |CHROME| |COMMANDS|) (or (ps:@ |CHROME| |COMMANDS|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |COMMANDS|)
            "getAll"
            (lambda
              (|CALLBACK|)
              (ps:var
                |COMMANDS|
                (or
                  (and
                    (ps:@ |CHROME| |RUNTIME| |getManifest|)
                    (ps:@
                      (ps:chain
                        |CHROME|
                        |RUNTIME|
                        (|getManifest|))
                      |COMMANDS|))
                  (ps:create)))
              (ps:var
                |LIST|
                (ps:chain
                  (ps:chain
                    |Object|
                    (|KEYS| |COMMANDS|))
                  (|MAP|
                    (lambda
                      (|NAME|)
                      (ps:var |ITEM| (or (aref |COMMANDS| |NAME|) (ps:create)))
                      (ps:var |KEY| (or (ps:@ |ITEM| |SUGGESTED_KEY|) (ps:create)))
                      (js-return
                        (ps:create
                          |NAME|
                          |NAME|
                          |DESCRIPTION|
                          (or (ps:@ |ITEM| |DESCRIPTION|) "")
                          |SHORTCUT|
                          (or (ps:@ |KEY| |DEFAULT|) (ps:@ |KEY| |MAC|) "")))))))
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| |LIST|)) |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |COMMANDS|) "onCommand" "commands.onCommand")
          (setf (ps:@ |CHROME| |IDLE|) (or (ps:@ |CHROME| |IDLE|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |IDLE|)
            "queryState"
            (lambda
              (|detectionInterval| |CALLBACK|)
              (if
                (eql (ps:typeof |detectionInterval|) "function")
                (setf |CALLBACK| |detectionInterval|))
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| "active")) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |IDLE|)
            "setDetectionInterval"
            (lambda
              (|INTERVAL| |CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |IDLE|)
            "getAutoLockDelay"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| 0)) |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |IDLE|) "onStateChanged" "idle.onStateChanged")
          (|ENSURE|
            (ps:@ |CHROME| |RUNTIME|)
            "getPlatformInfo"
            (lambda
              (|CALLBACK|)
              (ps:var
                |PLATFORM|
                (ps:chain
                  (or (ps:@ |NAVIGATOR| |PLATFORM|) "")
                  (|toLowerCase|)))
              (ps:var
                |OS|
                (if
                  (>= (ps:chain
                      |PLATFORM|
                      (|indexOf| "mac")) 0)
                  "mac"
                  (if
                    (>= (ps:chain
                        |PLATFORM|
                        (|indexOf| "win")) 0)
                    "win"
                    "linux")))
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:create |OS| |OS| |ARCH| "x86-64" |NACL_ARCH| "x86-64")))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |RUNTIME|)
            "getBrowserInfo"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:create |NAME| "Nyxt" |VENDOR| "Atlas Engineer" |VERSION| "4.0.0")))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |RUNTIME|)
            "openOptionsPage"
            (lambda
              (|CALLBACK|)
              (ps:var
                |MANIFEST|
                (if
                  (ps:@ |CHROME| |RUNTIME| |getManifest|)
                  (ps:chain
                    |CHROME|
                    |RUNTIME|
                    (|getManifest|))
                  (ps:create)))
              (ps:var
                |PAGE|
                (or
                  (and (ps:@ |MANIFEST| |OPTIONS_UI|) (ps:@ |MANIFEST| |OPTIONS_UI| |PAGE|))
                  (ps:@ |MANIFEST| |OPTIONS_PAGE|)))
              (if
                (not |PAGE|)
                (js-return
                  (|BOTH| (ps:chain
                      |Promise|
                      (|RESOLVE|)) |CALLBACK|)))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "tabs.create"
                      (ps:create
                        |PROPERTIES|
                        (ps:create
                          |URL|
                          (ps:chain
                            |CHROME|
                            |RUNTIME|
                            (|getURL| |PAGE|))
                          |ACTIVE|
                          t)))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |RUNTIME|)
            "setUninstallURL"
            (lambda
              (|URL| |CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE| (ps:@ |CHROME| |RUNTIME|) "reload" (lambda
              ()
              (js-return)))
          (|ENSURE|
            (ps:@ |CHROME| |RUNTIME|)
            "requestUpdateCheck"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:array "no_update" (ps:create))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |RUNTIME|)
            "getContexts"
            (lambda
              (|FILTER| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE|
                      (ps:array
                        (ps:create |contextType| "BACKGROUND" |documentUrl| "" |documentId| "" |documentOrigin| ""))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |RUNTIME|)
            "sendNativeMessage"
            (lambda
              (|HOST| |MESSAGE| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|REJECT| (ps:new (|Error| "Specified native messaging host not found."))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |RUNTIME|)
            "connectNative"
            (lambda
              ()
              (throw (ps:new (|Error| "Specified native messaging host not found.")))))
          (setf
            (ps:@ |CHROME| |RUNTIME| |OnInstalledReason|)
            (or
              (ps:@ |CHROME| |RUNTIME| |OnInstalledReason|)
              (ps:create
                |install|
                "install"
                |update|
                "update"
                |chrome_update|
                "chrome_update"
                |shared_module_update|
                "shared_module_update")))
          (setf
            (ps:@ |CHROME| |RUNTIME| |ContextType|)
            (or (ps:@ |CHROME| |RUNTIME| |ContextType|) (ps:create |background| "BACKGROUND" |tab| "TAB")))
          (|ensureEvent| (ps:@ |CHROME| |RUNTIME|) "onStartup" "runtime.onStartup")
          (|ensureEvent| (ps:@ |CHROME| |RUNTIME|) "onSuspend" "runtime.onSuspend")
          (|ensureEvent| (ps:@ |CHROME| |RUNTIME|) "onUpdateAvailable" "runtime.onUpdateAvailable")
          (setf (ps:@ |CHROME| |EXTENSION|) (or (ps:@ |CHROME| |EXTENSION|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |EXTENSION|)
            "getURL"
            (lambda
              (|PATH|)
              (js-return
                (ps:chain
                  |CHROME|
                  |RUNTIME|
                  (|getURL| |PATH|)))))
          (|ENSURE|
            (ps:@ |CHROME| |EXTENSION|)
            "isAllowedFileSchemeAccess"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| ps:false)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |EXTENSION|)
            "isAllowedIncognitoAccess"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| ps:false)) |CALLBACK|))))
          (if
            (/= (ps:typeof (ps:@ |CHROME| |EXTENSION| |inIncognitoContext|)) "boolean")
            (setf (ps:@ |CHROME| |EXTENSION| |inIncognitoContext|) ps:false))
          (|ENSURE|
            (ps:@ |CHROME| |EXTENSION|)
            "getViews"
            (lambda
              ()
              (js-return (ps:array))))
          (|ENSURE|
            (ps:@ |CHROME| |EXTENSION|)
            "getBackgroundPage"
            (lambda
              ()
              (js-return nil)))
          (setf (ps:@ |CHROME| |IDENTITY|) (or (ps:@ |CHROME| |IDENTITY|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |IDENTITY|)
            "getRedirectURL"
            (lambda
              (|PATH|)
              (js-return
                (+ (+ (+ "https://" (ps:@ |CHROME| |RUNTIME| |ID|)) ".chromiumapp.org/") (or |PATH| "")))))
          (|ENSURE|
            (ps:@ |CHROME| |IDENTITY|)
            "launchWebAuthFlow"
            (lambda
              (|DETAILS| |CALLBACK|)
              (ps:var |INTERACTIVE| (or (not |DETAILS|) (/= (ps:@ |DETAILS| |INTERACTIVE|) ps:false)))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "tabs.create"
                      (ps:create
                        |PROPERTIES|
                        (ps:create
                          |URL|
                          (and |DETAILS| (ps:@ |DETAILS| |URL|))
                          |ACTIVE|
                          |INTERACTIVE|)))
                    (|THEN|
                      (lambda
                        (|TAB|)
                        (js-return
                          (ps:new
                            (|Promise|
                              (lambda
                                (|RESOLVE| |REJECT|)
                                (ps:var
                                  |REDIRECT|
                                  (ps:chain
                                    |CHROME|
                                    |IDENTITY|
                                    (|getRedirectURL|)))
                                (defun
                                  |LISTENER|
                                  (|ID| |INFO| |UPDATED|)
                                  (if
                                    (or (not |TAB|) (/= |ID| (ps:@ |TAB| |ID|)))
                                    (js-return))
                                  (ps:var |URL| (or (and |INFO| (ps:@ |INFO| |URL|)) (and |UPDATED| (ps:@ |UPDATED| |URL|)) ""))
                                  (if
                                    (and
                                      |URL|
                                      (or
                                        (eql
                                          (ps:chain
                                            |URL|
                                            (|indexOf| |REDIRECT|))
                                          0)
                                        (>=
                                          (ps:chain
                                            |URL|
                                            (|indexOf| ".chromiumapp.org/"))
                                          0)))
                                    (progn
                                      (ps:chain
                                        |CHROME|
                                        |TABS|
                                        |onUpdated|
                                        (|removeListener| |LISTENER|))
                                      (ps:chain
                                        (|RPC|
                                          "tabs.remove"
                                          (ps:create
                                            |tabId|
                                            (ps:@ |TAB| |ID|)))
                                        (|CATCH|
                                          (lambda
                                            ()
                                            (js-return))))
                                      (|RESOLVE| |URL|)))
                                  (js-return))
                                (ps:chain
                                  |CHROME|
                                  |TABS|
                                  |onUpdated|
                                  (|addListener| |LISTENER|))
                                (|setTimeout|
                                  (lambda
                                    ()
                                    (ps:chain
                                      |CHROME|
                                      |TABS|
                                      |onUpdated|
                                      (|removeListener| |LISTENER|))
                                    (|REJECT| (ps:new (|Error| "Authorization page could not be loaded.")))
                                    (js-return))
                                  180000)
                                (js-return))))))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |IDENTITY|)
            "getAuthToken"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|REJECT| (ps:new (|Error| "The user is not signed in to the browser."))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |IDENTITY|)
            "removeCachedAuthToken"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |IDENTITY|)
            "clearAllCachedAuthTokens"
            (lambda
              (|CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |IDENTITY|)
            "getProfileUserInfo"
            (lambda
              (|DETAILS| |CALLBACK|)
              (if
                (eql (ps:typeof |DETAILS|) "function")
                (setf |CALLBACK| |DETAILS|))
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:create |EMAIL| "" |ID| "")))
                  |CALLBACK|))))
          (setf
            (ps:@ |CHROME| |IDENTITY| |AccountStatus|)
            (or (ps:@ |CHROME| |IDENTITY| |AccountStatus|) (ps:create |any| "ANY" |sync| "SYNC")))
          (|ensureEvent| (ps:@ |CHROME| |IDENTITY|) "onSignInChanged" "identity.onSignInChanged")
          (setf (ps:@ |CHROME| |MANAGEMENT|) (or (ps:@ |CHROME| |MANAGEMENT|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |MANAGEMENT|)
            "getSelf"
            (lambda
              (|CALLBACK|)
              (ps:var
                |MANIFEST|
                (if
                  (ps:@ |CHROME| |RUNTIME| |getManifest|)
                  (ps:chain
                    |CHROME|
                    |RUNTIME|
                    (|getManifest|))
                  (ps:create)))
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE|
                      (ps:create
                        |ID|
                        (ps:@ |CHROME| |RUNTIME| |ID|)
                        |NAME|
                        (or (ps:@ |MANIFEST| |NAME|) "")
                        |DESCRIPTION|
                        (or (ps:@ |MANIFEST| |DESCRIPTION|) "")
                        |VERSION|
                        (or (ps:@ |MANIFEST| |VERSION|) "")
                        |ENABLED|
                        t
                        |mayDisable|
                        t
                        |installType|
                        "development"
                        |TYPE|
                        "extension"
                        |offlineEnabled|
                        ps:false)))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |MANAGEMENT|)
            "getAll"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "management.getAll")
                    (|THEN|
                      (lambda
                        (|ITEMS|)
                        (js-return (or |ITEMS| (ps:array))))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |MANAGEMENT|)
            "get"
            (lambda
              (|ID| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (ps:chain
                      |CHROME|
                      |MANAGEMENT|
                      (|getAll|))
                    (|THEN|
                      (lambda
                        (|ITEMS|)
                        (ps:var
                          |FOUND|
                          (aref
                            (ps:chain
                              (or |ITEMS| (ps:array))
                              (|FILTER|
                                (lambda
                                  (|ITEM|)
                                  (js-return (eql (ps:@ |ITEM| |ID|) |ID|)))))
                            0))
                        (if
                          (not |FOUND|)
                          (throw (ps:new (|Error| (+ (+ "No extension with id " |ID|) ".")))))
                        (js-return |FOUND|))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |MANAGEMENT|)
            "setEnabled"
            (lambda
              (|ID| |ENABLED| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "management.setEnabled" (ps:create |ID| |ID| |ENABLED| |ENABLED|))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |MANAGEMENT|)
            "uninstallSelf"
            (lambda
              (|OPTIONS| |CALLBACK|)
              (if
                (eql (ps:typeof |OPTIONS|) "function")
                (setf |CALLBACK| |OPTIONS|))
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|REJECT| (ps:new (|Error| "This browser does not allow an extension to uninstall itself."))))
                  |CALLBACK|))))
          (setf (ps:@ |CHROME| |sidePanel|) (or (ps:@ |CHROME| |sidePanel|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |sidePanel|)
            "setOptions"
            (lambda
              (|OPTIONS| |CALLBACK|)
              (if
                (and |OPTIONS| (ps:@ |OPTIONS| |PATH|))
                (setf |sidePanelPath| (ps:@ |OPTIONS| |PATH|)))
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |sidePanel|)
            "getOptions"
            (lambda
              (|OPTIONS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:create |PATH| |sidePanelPath| |ENABLED| t)))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |sidePanel|)
            "setPanelBehavior"
            (lambda
              (|BEHAVIOR| |CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |sidePanel|)
            "open"
            (lambda
              (|OPTIONS| |CALLBACK|)
              (ps:var
                |PATH|
                (or
                  |sidePanelPath|
                  (and
                    (ps:@
                      (ps:chain
                        |CHROME|
                        |RUNTIME|
                        (|getManifest|))
                      |SIDE_PANEL|)
                    (ps:@
                      (ps:chain
                        |CHROME|
                        |RUNTIME|
                        (|getManifest|))
                      |SIDE_PANEL|
                      |DEFAULT_PATH|))
                  ""))
              (if
                (not |PATH|)
                (js-return
                  (|BOTH|
                    (ps:chain
                      |Promise|
                      (|REJECT| (ps:new (|Error| "No side panel is configured."))))
                    |CALLBACK|)))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "tabs.create"
                      (ps:create
                        |PROPERTIES|
                        (ps:create
                          |URL|
                          (ps:chain
                            |CHROME|
                            |RUNTIME|
                            (|getURL| |PATH|))
                          |ACTIVE|
                          t)))
                    (|THEN|
                      (lambda
                        ()
                        (|EMIT| "sidePanel.onOpened" (ps:array (ps:create)))
                        (js-return))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |sidePanel|)
            "close"
            (lambda
              (|OPTIONS| |CALLBACK|)
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |sidePanel|) "onOpened" "sidePanel.onOpened")
          (|ensureEvent| (ps:@ |CHROME| |sidePanel|) "onClosed" "sidePanel.onClosed")
          (setf (ps:@ |CHROME| |OFFSCREEN|) (or (ps:@ |CHROME| |OFFSCREEN|) (ps:create)))
          (setf
            (ps:@ |CHROME| |OFFSCREEN| |Reason|)
            (or
              (ps:@ |CHROME| |OFFSCREEN| |Reason|)
              (ps:create
                |testing|
                "TESTING"
                |audio_playback|
                "AUDIO_PLAYBACK"
                |dom_scraping|
                "DOM_SCRAPING"
                |blobs|
                "BLOBS"
                |clipboard|
                "CLIPBOARD"
                |iframe_scripting|
                "IFRAME_SCRIPTING")))
          (|ENSURE|
            (ps:@ |CHROME| |OFFSCREEN|)
            "createDocument"
            (lambda
              (|PARAMETERS| |CALLBACK|)
              (if
                |offscreenTab|
                (js-return
                  (|BOTH|
                    (ps:chain
                      |Promise|
                      (|REJECT| (ps:new (|Error| "Only a single offscreen document may be created."))))
                    |CALLBACK|)))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "tabs.create"
                      (ps:create
                        |PROPERTIES|
                        (ps:create
                          |URL|
                          (ps:chain
                            |CHROME|
                            |RUNTIME|
                            (|getURL| (or (and |PARAMETERS| (ps:@ |PARAMETERS| |URL|)) "")))
                          |ACTIVE|
                          ps:false)))
                    (|THEN|
                      (lambda
                        (|TAB|)
                        (setf |offscreenTab| |TAB|)
                        (js-return))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |OFFSCREEN|)
            "closeDocument"
            (lambda
              (|CALLBACK|)
              (ps:var |TAB| |offscreenTab|)
              (setf |offscreenTab| nil)
              (if
                (not |TAB|)
                (js-return
                  (|BOTH| (ps:chain
                      |Promise|
                      (|RESOLVE|)) |CALLBACK|)))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "tabs.remove"
                      (ps:create
                        |tabId|
                        (ps:@ |TAB| |ID|)))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |OFFSCREEN|)
            "hasDocument"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (not (not |offscreenTab|))))
                  |CALLBACK|))))
          (setf (ps:@ |CHROME| |NOTIFICATIONS|) (or (ps:@ |CHROME| |NOTIFICATIONS|) (ps:create)))
          (ps:var |NOTIFICATIONS| (ps:create))
          (|ENSURE|
            (ps:@ |CHROME| |NOTIFICATIONS|)
            "create"
            (lambda
              (|ID| |OPTIONS| |CALLBACK|)
              (if
                (eql (ps:typeof |ID|) "object")
                (progn
                  (setf |CALLBACK| |OPTIONS|)
                  (setf |OPTIONS| |ID|)
                  (setf |ID| (+ "nyxt-" (incf |SEQUENCE|)))))
              (setf (aref |NOTIFICATIONS| |ID|) (or |OPTIONS| (ps:create)))
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| |ID|)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |NOTIFICATIONS|)
            "clear"
            (lambda
              (|ID| |CALLBACK|)
              (delete (aref |NOTIFICATIONS| |ID|))
              (|EMIT| "notifications.onClosed" (ps:array |ID| t))
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| t)) |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |NOTIFICATIONS|)
            "getAll"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE|
                      (ps:chain
                        |Object|
                        (|ASSIGN| (ps:create) |NOTIFICATIONS|))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |NOTIFICATIONS|)
            "update"
            (lambda
              (|ID| |OPTIONS| |CALLBACK|)
              (setf
                (aref |NOTIFICATIONS| |ID|)
                (ps:chain
                  |Object|
                  (|ASSIGN| (or (aref |NOTIFICATIONS| |ID|) (ps:create)) (or |OPTIONS| (ps:create)))))
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (not (not (aref |NOTIFICATIONS| |ID|)))))
                  |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |NOTIFICATIONS|) "onClicked" "notifications.onClicked")
          (|ensureEvent| (ps:@ |CHROME| |NOTIFICATIONS|) "onClosed" "notifications.onClosed")
          (|ensureEvent|
            (ps:@ |CHROME| |NOTIFICATIONS|)
            "onButtonClicked"
            "notifications.onButtonClicked")
          (defun
            |historyApi|
            (|NAME| |PAYLOAD| |CALLBACK| |EMPTY|)
            (js-return
              (|BOTH|
                (ps:chain
                  (|RPC| (+ "history." |NAME|) |PAYLOAD|)
                  (|THEN|
                    (lambda
                      (|VALUE|)
                      (js-return
                        (if
                          (and (equal |VALUE| nil) |EMPTY|)
                          |EMPTY|
                          |VALUE|)))))
                |CALLBACK|)))
          (setf (ps:@ |CHROME| |HISTORY|) (or (ps:@ |CHROME| |HISTORY|) (ps:create)))
          (|REPLACE|
            (ps:@ |CHROME| |HISTORY|)
            "search"
            (lambda
              (|QUERY| |CALLBACK|)
              (js-return
                (|historyApi|
                  "search"
                  (ps:create
                    |QUERY|
                    (or |QUERY| (ps:create)))
                  |CALLBACK|
                  (ps:array)))))
          (|REPLACE|
            (ps:@ |CHROME| |HISTORY|)
            "addUrl"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|historyApi|
                  "addUrl"
                  (ps:create
                    |DETAILS|
                    (or |DETAILS| (ps:create)))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |HISTORY|)
            "deleteUrl"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|historyApi|
                  "deleteUrl"
                  (ps:create
                    |URL|
                    (and |DETAILS| (ps:@ |DETAILS| |URL|)))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |HISTORY|)
            "deleteRange"
            (lambda
              (|RANGE| |CALLBACK|)
              (js-return
                (|historyApi|
                  "deleteRange"
                  (ps:create
                    |RANGE|
                    (or |RANGE| (ps:create)))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |HISTORY|)
            "getVisits"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|historyApi|
                  "getVisits"
                  (ps:create
                    |URL|
                    (and |DETAILS| (ps:@ |DETAILS| |URL|)))
                  |CALLBACK|
                  (ps:array)))))
          (setf
            (ps:@ |CHROME| |HISTORY| |onVisited|)
            (or (ps:@ |CHROME| |HISTORY| |onVisited|) (ps:create)))
          (setf
            (ps:@ |CHROME| |HISTORY| |onVisitRemoved|)
            (or (ps:@ |CHROME| |HISTORY| |onVisitRemoved|) (ps:create)))
          (|LISTEN| (ps:@ |CHROME| |HISTORY| |onVisited|) "history.onVisited")
          (|LISTEN| (ps:@ |CHROME| |HISTORY| |onVisitRemoved|) "history.onVisitRemoved")
          (setf (ps:@ |CHROME| |BOOKMARKS|) (or (ps:@ |CHROME| |BOOKMARKS|) (ps:create)))
          (|REPLACE|
            (ps:@ |CHROME| |BOOKMARKS|)
            "getTree"
            (lambda
              (|CALLBACK|)
              (js-return (|BOTH| (|RPC| "bookmarks.getTree") |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |BOOKMARKS|)
            "get"
            (lambda
              (|idOrIds| |CALLBACK|)
              (ps:var
                |IDS|
                (if
                  (ps:chain
                    |Array|
                    (|isArray| |idOrIds|))
                  |idOrIds|
                  (ps:array |idOrIds|)))
              (js-return (|BOTH| (|RPC| "bookmarks.get" (ps:create |IDS| |IDS|)) |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |BOOKMARKS|)
            "getChildren"
            (lambda
              (|ID| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "bookmarks.getTree")
                    (|THEN|
                      (lambda
                        (|TREE|)
                        (ps:var
                          |ROOT|
                          (and
                            |TREE|
                            (aref |TREE| 0)
                            (ps:@ (aref |TREE| 0) |CHILDREN|)
                            (aref (ps:@ (aref |TREE| 0) |CHILDREN|) 0)))
                        (js-return (or (and |ROOT| (ps:@ |ROOT| |CHILDREN|)) (ps:array))))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |BOOKMARKS|)
            "getSubTree"
            (lambda
              (|ID| |CALLBACK|)
              (js-return
                (ps:chain
                  |CHROME|
                  |BOOKMARKS|
                  (|getTree| |CALLBACK|)))))
          (|REPLACE|
            (ps:@ |CHROME| |BOOKMARKS|)
            "search"
            (lambda
              (|QUERY| |CALLBACK|)
              (ps:var
                |BODY|
                (if
                  (eql (ps:typeof |QUERY|) "string")
                  (ps:create |QUERY| |QUERY|)
                  (or |QUERY| (ps:create))))
              (js-return (|BOTH| (|RPC| "bookmarks.search" (ps:create |QUERY| |BODY|)) |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |BOOKMARKS|)
            "create"
            (lambda
              (|BOOKMARK| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "bookmarks.create"
                    (ps:create
                      |PROPERTIES|
                      (or |BOOKMARK| (ps:create))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |BOOKMARKS|)
            "update"
            (lambda
              (|ID| |CHANGES| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "bookmarks.update"
                    (ps:create
                      |ID|
                      |ID|
                      |PROPERTIES|
                      (or |CHANGES| (ps:create))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |BOOKMARKS|)
            "remove"
            (lambda
              (|ID| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "bookmarks.remove" (ps:create |ID| |ID|))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |BOOKMARKS|) "onCreated" "bookmarks.onCreated")
          (|ensureEvent| (ps:@ |CHROME| |BOOKMARKS|) "onRemoved" "bookmarks.onRemoved")
          (setf (ps:@ |CHROME| |topSites|) (or (ps:@ |CHROME| |topSites|) (ps:create)))
          (|REPLACE|
            (ps:@ |CHROME| |topSites|)
            "get"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "topSites.get")
                    (|THEN|
                      (lambda
                        (|SITES|)
                        (js-return (or |SITES| (ps:array))))))
                  |CALLBACK|))))
          (setf (ps:@ |CHROME| |SESSIONS|) (or (ps:@ |CHROME| |SESSIONS|) (ps:create)))
          (|REPLACE|
            (ps:@ |CHROME| |SESSIONS|)
            "getRecentlyClosed"
            (lambda
              (|FILTER| |CALLBACK|)
              (if
                (eql (ps:typeof |FILTER|) "function")
                (setf |CALLBACK| |FILTER|))
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "sessions.getRecentlyClosed")
                    (|THEN|
                      (lambda
                        (|ITEMS|)
                        (js-return (or |ITEMS| (ps:array))))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |SESSIONS|)
            "restore"
            (lambda
              (|sessionId| |CALLBACK|)
              (if
                (eql (ps:typeof |sessionId|) "function")
                (setf |CALLBACK| |sessionId|))
              (js-return (|BOTH| (|RPC| "sessions.restore") |CALLBACK|))))
          (setf (ps:@ |CHROME| |tabGroups|) (or (ps:@ |CHROME| |tabGroups|) (ps:create)))
          (setf (ps:@ |CHROME| |tabGroups| |tab_group_id_none|) (- 1))
          (|REPLACE|
            (ps:@ |CHROME| |tabGroups|)
            "query"
            (lambda
              (|INFO| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "tabGroups.query")
                    (|THEN|
                      (lambda
                        (|GROUPS|)
                        (js-return (or |GROUPS| (ps:array))))))
                  |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |tabGroups|)
            "get"
            (lambda
              (|groupId| |CALLBACK|)
              (js-return (|BOTH| (|RPC| "tabGroups.get" (ps:create |groupId| |groupId|)) |CALLBACK|))))
          (|REPLACE|
            (ps:@ |CHROME| |tabGroups|)
            "update"
            (lambda
              (|groupId| |PROPERTIES| |CALLBACK|)
              (js-return
                (|BOTH|
                  (|RPC|
                    "tabGroups.update"
                    (ps:create
                      |groupId|
                      |groupId|
                      |PROPERTIES|
                      (or |PROPERTIES| (ps:create))))
                  |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |tabGroups|) "onUpdated" "tabGroups.onUpdated")
          (setf (ps:@ |CHROME| |browsingData|) (or (ps:@ |CHROME| |browsingData|) (ps:create)))
          (|REPLACE|
            (ps:@ |CHROME| |browsingData|)
            "remove"
            (lambda
              (|OPTIONS| |dataToRemove| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "browsingData.remove"
                      (ps:create
                        |OPTIONS|
                        (or |OPTIONS| (ps:create))
                        |dataToRemove|
                        (or |dataToRemove| (ps:create))))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (ps:chain
            (ps:array "removeHistory" "removeCache" "removeCookies" "removePasswords" "removeDownloads")
            (|forEach|
              (lambda
                (|NAME|)
                (|ENSURE|
                  (ps:@ |CHROME| |browsingData|)
                  |NAME|
                  (lambda
                    (|OPTIONS| |CALLBACK|)
                    (ps:var |DATA| (ps:create))
                    (setf
                      (aref
                        |DATA|
                        (ps:chain
                          (ps:chain
                            |NAME|
                            (|REPLACE| "remove" ""))
                          (|toLowerCase|)))
                      t)
                    (if
                      (eql |NAME| "removeHistory")
                      (setf |DATA| (ps:create |HISTORY| t)))
                    (js-return
                      (ps:chain
                        |CHROME|
                        |browsingData|
                        (|REMOVE| |OPTIONS| |DATA| |CALLBACK|)))))
                (js-return))))
          (|ENSURE|
            (ps:@ |CHROME| |browsingData|)
            "settings"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE|
                      (ps:create
                        |OPTIONS|
                        (ps:create)
                        |dataToRemove|
                        (ps:create)
                        |dataRemovalPermitted|
                        (ps:create))))
                  |CALLBACK|))))
          (setf (ps:@ |CHROME| |SEARCH|) (or (ps:@ |CHROME| |SEARCH|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |SEARCH|)
            "query"
            (lambda
              (|QUERY| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "search.query"
                      (ps:create
                        |QUERY|
                        (or |QUERY| (ps:create))))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (setf (ps:@ |CHROME| |PRIVACY|) (or (ps:@ |CHROME| |PRIVACY|) (ps:create)))
          (defun
            |settingGroup|
            ()
            (ps:var |VALUES| (ps:create))
            (js-return
              (ps:new
                (|Proxy|
                  (ps:create)
                  (ps:create
                    |GET|
                    (lambda
                      (|TARGET| |NAME|)
                      (if
                        (/= (ps:typeof |NAME|) "string")
                        (js-return |UNDEFINED|))
                      (if
                        (not (aref |VALUES| |NAME|))
                        (setf
                          (aref |VALUES| |NAME|)
                          (ps:create
                            |GET|
                            (lambda
                              (|DETAILS| |CALLBACK|)
                              (js-return
                                (|BOTH|
                                  (ps:chain
                                    |Promise|
                                    (|RESOLVE|
                                      (ps:create
                                        |VALUE|
                                        (ps:@ (aref |VALUES| |NAME|) |_VALUE|)
                                        |levelOfControl|
                                        "controllable_by_this_extension")))
                                  |CALLBACK|)))
                            |SET|
                            (lambda
                              (|DETAILS| |CALLBACK|)
                              (setf (ps:@ (aref |VALUES| |NAME|) |_VALUE|) (and |DETAILS| (ps:@ |DETAILS| |VALUE|)))
                              (js-return
                                (|BOTH|
                                  (ps:chain
                                    |Promise|
                                    (|RESOLVE|))
                                  |CALLBACK|)))
                            |CLEAR|
                            (lambda
                              (|DETAILS| |CALLBACK|)
                              (setf (ps:@ (aref |VALUES| |NAME|) |_VALUE|) |UNDEFINED|)
                              (js-return
                                (|BOTH|
                                  (ps:chain
                                    |Promise|
                                    (|RESOLVE|))
                                  |CALLBACK|))))))
                      (js-return (aref |VALUES| |NAME|))))))))
          (if
            (not (ps:@ |CHROME| |PRIVACY| |NETWORK|))
            (setf (ps:@ |CHROME| |PRIVACY| |NETWORK|) (|settingGroup|)))
          (if
            (not (ps:@ |CHROME| |PRIVACY| |SERVICES|))
            (setf (ps:@ |CHROME| |PRIVACY| |SERVICES|) (|settingGroup|)))
          (if
            (not (ps:@ |CHROME| |PRIVACY| |WEBSITES|))
            (setf (ps:@ |CHROME| |PRIVACY| |WEBSITES|) (|settingGroup|)))
          (setf (ps:@ |CHROME| |PROXY|) (or (ps:@ |CHROME| |PROXY|) (ps:create)))
          (setf (ps:@ |CHROME| |PROXY| |SETTINGS|) (or (ps:@ |CHROME| |PROXY| |SETTINGS|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |PROXY| |SETTINGS|)
            "get"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return (|BOTH| (|RPC| "proxy.get") |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |PROXY| |SETTINGS|)
            "set"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC| "proxy.set" (or |DETAILS| (ps:create)))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |PROXY| |SETTINGS|)
            "clear"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (|RPC|
                      "proxy.set"
                      (ps:create
                        |VALUE|
                        (ps:create |MODE| "system")))
                    (|THEN| (lambda
                        ()
                        (js-return))))
                  |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |PROXY|) "onProxyError" "proxy.onProxyError")
          (defun
            |systemInfo|
            (|CALLBACK| |VALUE|)
            (js-return
              (|BOTH| (ps:chain
                  |Promise|
                  (|RESOLVE| |VALUE|)) |CALLBACK|)))
          (setf (ps:@ |CHROME| |SYSTEM|) (or (ps:@ |CHROME| |SYSTEM|) (ps:create)))
          (setf (ps:@ |CHROME| |SYSTEM| |DISPLAY|) (or (ps:@ |CHROME| |SYSTEM| |DISPLAY|) (ps:create)))
          (setf (ps:@ |CHROME| |SYSTEM| |CPU|) (or (ps:@ |CHROME| |SYSTEM| |CPU|) (ps:create)))
          (setf (ps:@ |CHROME| |SYSTEM| |MEMORY|) (or (ps:@ |CHROME| |SYSTEM| |MEMORY|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |SYSTEM| |DISPLAY|)
            "getInfo"
            (lambda
              (|CALLBACK|)
              (js-return
                (|systemInfo|
                  |CALLBACK|
                  (ps:array
                    (ps:create
                      |ID|
                      "primary"
                      |NAME|
                      "Primary"
                      |isPrimary|
                      t
                      |isEnabled|
                      t
                      |ROTATION|
                      0
                      |BOUNDS|
                      (ps:create
                        |LEFT|
                        0
                        |TOP|
                        0
                        |WIDTH|
                        (or (and (/= (ps:typeof |SCREEN|) "undefined") (ps:@ |SCREEN| |WIDTH|)) 1)
                        |HEIGHT|
                        (or (and (/= (ps:typeof |SCREEN|) "undefined") (ps:@ |SCREEN| |HEIGHT|)) 1))
                      |workArea|
                      (ps:create
                        |LEFT|
                        0
                        |TOP|
                        0
                        |WIDTH|
                        (or (and (/= (ps:typeof |SCREEN|) "undefined") (ps:@ |SCREEN| |WIDTH|)) 1)
                        |HEIGHT|
                        (or (and (/= (ps:typeof |SCREEN|) "undefined") (ps:@ |SCREEN| |HEIGHT|)) 1))))))))
          (|ENSURE|
            (ps:@ |CHROME| |SYSTEM| |CPU|)
            "getInfo"
            (lambda
              (|CALLBACK|)
              (js-return
                (|systemInfo|
                  |CALLBACK|
                  (ps:create
                    |numOfProcessors|
                    (or (ps:@ |NAVIGATOR| |hardwareConcurrency|) 1)
                    |archName|
                    "x86_64"
                    |modelName|
                    ""
                    |FEATURES|
                    (ps:array)
                    |PROCESSORS|
                    (ps:array))))))
          (|ENSURE|
            (ps:@ |CHROME| |SYSTEM| |MEMORY|)
            "getInfo"
            (lambda
              (|CALLBACK|)
              (js-return
                (|systemInfo|
                  |CALLBACK|
                  (ps:create
                    |CAPACITY|
                    (* (* (* (or (ps:@ |NAVIGATOR| |deviceMemory|) 0) 1024) 1024) 1024)
                    |availableCapacity|
                    0)))))
          (setf (ps:@ |CHROME| |userScripts|) (or (ps:@ |CHROME| |userScripts|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |userScripts|)
            "register"
            (lambda
              (|SCRIPTS|)
              (ps:chain
                (or |SCRIPTS| (ps:array))
                (|forEach|
                  (lambda
                    (|SCRIPT|)
                    (setf
                      |userScripts|
                      (ps:chain
                        |userScripts|
                        (|FILTER|
                          (lambda
                            (|ITEM|)
                            (js-return (/= (ps:@ |ITEM| |ID|) (ps:@ |SCRIPT| |ID|)))))))
                    (ps:chain
                      |userScripts|
                      (|PUSH| |SCRIPT|))
                    (js-return))))
              (js-return (ps:chain
                  |Promise|
                  (|RESOLVE|)))))
          (|ENSURE|
            (ps:@ |CHROME| |userScripts|)
            "getScripts"
            (lambda
              (|FILTER|)
              (ps:var |IDS| (and |FILTER| (ps:@ |FILTER| |IDS|)))
              (js-return
                (ps:chain
                  |Promise|
                  (|RESOLVE|
                    (ps:chain
                      |userScripts|
                      (|FILTER|
                        (lambda
                          (|SCRIPT|)
                          (js-return
                            (or
                              (not |IDS|)
                              (>=
                                (ps:chain
                                  |IDS|
                                  (|indexOf| (ps:@ |SCRIPT| |ID|)))
                                0)))))))))))
          (|ENSURE|
            (ps:@ |CHROME| |userScripts|)
            "unregister"
            (lambda
              (|FILTER|)
              (ps:var |IDS| (and |FILTER| (ps:@ |FILTER| |IDS|)))
              (setf
                |userScripts|
                (if
                  |IDS|
                  (ps:chain
                    |userScripts|
                    (|FILTER|
                      (lambda
                        (|SCRIPT|)
                        (js-return
                          (<
                            (ps:chain
                              |IDS|
                              (|indexOf| (ps:@ |SCRIPT| |ID|)))
                            0)))))
                  (ps:array)))
              (js-return (ps:chain
                  |Promise|
                  (|RESOLVE|)))))
          (|ENSURE|
            (ps:@ |CHROME| |userScripts|)
            "update"
            (lambda
              (|SCRIPTS|)
              (js-return
                (ps:chain
                  |CHROME|
                  |userScripts|
                  (|REGISTER| |SCRIPTS|)))))
          (|ENSURE|
            (ps:@ |CHROME| |userScripts|)
            "configureWorld"
            (lambda
              ()
              (js-return (ps:chain
                  |Promise|
                  (|RESOLVE|)))))
          (setf (ps:@ |CHROME| |TTS|) (or (ps:@ |CHROME| |TTS|) (ps:create)))
          (ps:var
            |nativeGetVoices|
            (and
              (|HAS| (ps:@ |CHROME| |TTS|) "getVoices")
              (ps:chain
                |CHROME|
                |TTS|
                |getVoices|
                (|BIND| (ps:@ |CHROME| |TTS|)))))
          (|REPLACE|
            (ps:@ |CHROME| |TTS|)
            "getVoices"
            (lambda
              (|CALLBACK|)
              (ps:var
                |FALLBACK|
                (ps:array
                  (ps:create
                    |voiceName|
                    "Default"
                    |LANG|
                    (or (ps:@ |NAVIGATOR| |LANGUAGE|) "en-US")
                    |REMOTE|
                    ps:false
                    |eventTypes|
                    (ps:array "end"))))
              (ps:var
                |PROMISE|
                (if
                  |nativeGetVoices|
                  (ps:chain
                    (ps:chain
                      (ps:chain
                        |Promise|
                        (|RESOLVE| (|nativeGetVoices|)))
                      (|THEN|
                        (lambda
                          (|VOICES|)
                          (js-return
                            (if
                              (and |VOICES| (ps:@ |VOICES| |LENGTH|))
                              |VOICES|
                              |FALLBACK|)))))
                    (|CATCH| (lambda
                        ()
                        (js-return |FALLBACK|))))
                  (ps:chain
                    |Promise|
                    (|RESOLVE| |FALLBACK|))))
              (js-return (|BOTH| |PROMISE| |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |TTS|)
            "speak"
            (lambda
              (|UTTERANCE| |OPTIONS| |CALLBACK|)
              (if
                (eql (ps:typeof |OPTIONS|) "function")
                (setf |CALLBACK| |OPTIONS|))
              (js-return (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE|)) |CALLBACK|))))
          (|ENSURE| (ps:@ |CHROME| |TTS|) "stop" (lambda
              ()
              (js-return)))
          (|ENSURE|
            (ps:@ |CHROME| |TTS|)
            "isSpeaking"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| ps:false)) |CALLBACK|))))
          (setf (ps:@ |CHROME| |OMNIBOX|) (or (ps:@ |CHROME| |OMNIBOX|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |OMNIBOX|)
            "setDefaultSuggestion"
            (lambda
              (|SUGGESTION|)
              (js-return)))
          (|ensureEvent| (ps:@ |CHROME| |OMNIBOX|) "onInputChanged" "omnibox.onInputChanged")
          (|ensureEvent| (ps:@ |CHROME| |OMNIBOX|) "onInputEntered" "omnibox.onInputEntered")
          (|ensureEvent| (ps:@ |CHROME| |OMNIBOX|) "onInputCancelled" "omnibox.onInputCancelled")
          (defun
            |mirrorRules|
            (|KIND| |OPTIONS|)
            (js-return
              (ps:chain
                (|RPC|
                  "dnr.update"
                  (ps:create
                    |KIND|
                    |KIND|
                    |OPTIONS|
                    (or |OPTIONS| (ps:create))))
                (|CATCH| (lambda
                    ()
                    (js-return))))))
          (setf
            (ps:@ |CHROME| |declarativeNetRequest|)
            (or (ps:@ |CHROME| |declarativeNetRequest|) (ps:create)))
          (ps:var
            |nativeDynamic|
            (and
              (|HAS| (ps:@ |CHROME| |declarativeNetRequest|) "updateDynamicRules")
              (ps:chain
                |CHROME|
                |declarativeNetRequest|
                |updateDynamicRules|
                (|BIND| (ps:@ |CHROME| |declarativeNetRequest|)))))
          (ps:var
            |nativeSession|
            (and
              (|HAS| (ps:@ |CHROME| |declarativeNetRequest|) "updateSessionRules")
              (ps:chain
                |CHROME|
                |declarativeNetRequest|
                |updateSessionRules|
                (|BIND| (ps:@ |CHROME| |declarativeNetRequest|)))))
          (defun
            |callNative|
            (|FN| |OPTIONS|)
            (if
              (not |FN|)
              (js-return (ps:chain
                  |Promise|
                  (|RESOLVE|))))
            (ps:try
              (js-return
                (ps:chain
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (|FN| |OPTIONS|)))
                  (|CATCH| (lambda
                      ()
                      (js-return)))))
              (:catch (|ERROR|) (js-return (ps:chain
                    |Promise|
                    (|RESOLVE|)))))
            (js-return))
          (|REPLACE|
            (ps:@ |CHROME| |declarativeNetRequest|)
            "updateDynamicRules"
            (lambda
              (|OPTIONS|)
              (js-return
                (ps:chain
                  (ps:chain
                    (|callNative| |nativeDynamic| |OPTIONS|)
                    (|THEN|
                      (lambda
                        ()
                        (js-return (|mirrorRules| "dynamic" |OPTIONS|)))))
                  (|THEN| (lambda
                      ()
                      (js-return)))))))
          (|REPLACE|
            (ps:@ |CHROME| |declarativeNetRequest|)
            "updateSessionRules"
            (lambda
              (|OPTIONS|)
              (js-return
                (ps:chain
                  (ps:chain
                    (|callNative| |nativeSession| |OPTIONS|)
                    (|THEN|
                      (lambda
                        ()
                        (js-return (|mirrorRules| "session" |OPTIONS|)))))
                  (|THEN| (lambda
                      ()
                      (js-return)))))))
          (|REPLACE|
            (ps:@ |CHROME| |declarativeNetRequest|)
            "getDynamicRules"
            (lambda
              ()
              (js-return
                (ps:chain
                  (|RPC| "dnr.get" (ps:create |KIND| "dynamic"))
                  (|THEN|
                    (lambda
                      (|RULES|)
                      (js-return (or |RULES| (ps:array)))))))))
          (|REPLACE|
            (ps:@ |CHROME| |declarativeNetRequest|)
            "getSessionRules"
            (lambda
              ()
              (js-return
                (ps:chain
                  (|RPC| "dnr.get" (ps:create |KIND| "session"))
                  (|THEN|
                    (lambda
                      (|RULES|)
                      (js-return (or |RULES| (ps:array)))))))))
          (|ENSURE|
            (ps:@ |CHROME| |declarativeNetRequest|)
            "getEnabledRulesets"
            (lambda
              ()
              (js-return (ps:chain
                  |Promise|
                  (|RESOLVE| (ps:array))))))
          (|ENSURE|
            (ps:@ |CHROME| |declarativeNetRequest|)
            "updateEnabledRulesets"
            (lambda
              ()
              (js-return (ps:chain
                  |Promise|
                  (|RESOLVE|)))))
          (|ENSURE|
            (ps:@ |CHROME| |declarativeNetRequest|)
            "isRegexSupported"
            (lambda
              (|regexOptions|)
              (js-return
                (|RPC|
                  "dnr.regex"
                  (ps:create
                    |REGEX|
                    (or (and |regexOptions| (ps:@ |regexOptions| |REGEX|)) ""))))))
          (setf
            (ps:@ |CHROME| |declarativeNetRequest| |max_number_of_dynamic_and_session_rules|)
            (or (ps:@ |CHROME| |declarativeNetRequest| |max_number_of_dynamic_and_session_rules|) 30000))
          (setf
            (ps:@ |CHROME| |declarativeNetRequest| |dynamic_ruleset_id|)
            (or (ps:@ |CHROME| |declarativeNetRequest| |dynamic_ruleset_id|) "_dynamic"))
          (setf
            (ps:@ |CHROME| |declarativeNetRequest| |RuleActionType|)
            (or
              (ps:@ |CHROME| |declarativeNetRequest| |RuleActionType|)
              (ps:create
                |block|
                "block"
                |allow|
                "allow"
                |redirect|
                "redirect"
                |modify_headers|
                "modifyHeaders"
                |upgrade_scheme|
                "upgradeScheme")))
          (setf
            (ps:@ |CHROME| |declarativeNetRequest| |ResourceType|)
            (or
              (ps:@ |CHROME| |declarativeNetRequest| |ResourceType|)
              (ps:create
                |main_frame|
                "main_frame"
                |sub_frame|
                "sub_frame"
                |stylesheet|
                "stylesheet"
                |script|
                "script"
                |image|
                "image"
                |font|
                "font"
                |object|
                "object"
                |xmlhttprequest|
                "xmlhttprequest"
                |ping|
                "ping"
                |media|
                "media"
                |websocket|
                "websocket"
                |other|
                "other")))
          (setf
            (ps:@ |CHROME| |declarativeNetRequest| |HeaderOperation|)
            (or
              (ps:@ |CHROME| |declarativeNetRequest| |HeaderOperation|)
              (ps:create |append| "append" |set| "set" |remove| "remove")))
          (|ensureEvent|
            (ps:@ |CHROME| |declarativeNetRequest|)
            "onRuleMatchedDebug"
            "declarativeNetRequest.onRuleMatchedDebug")
          (if
            (or
              (not (ps:@ |CHROME| |webRequest|))
              (eql (ps:typeof (ps:@ |CHROME| |webRequest| |onBeforeRequest|)) "undefined"))
            (progn
              (setf (ps:@ |CHROME| |webRequest|) (or (ps:@ |CHROME| |webRequest|) (ps:create)))
              (ps:chain
                (ps:array
                  "onBeforeRequest"
                  "onBeforeSendHeaders"
                  "onSendHeaders"
                  "onHeadersReceived"
                  "onAuthRequired"
                  "onResponseStarted"
                  "onBeforeRedirect"
                  "onCompleted"
                  "onErrorOccurred")
                (|forEach|
                  (lambda
                    (|NAME|)
                    (setf (aref (ps:@ |CHROME| |webRequest|) |NAME|) (ps:create))
                    (ps:var |LIST| (|BAG| (+ "webRequest." |NAME|)))
                    (|REPLACE|
                      (aref (ps:@ |CHROME| |webRequest|) |NAME|)
                      "addListener"
                      (lambda
                        (|LISTENER| |FILTER| |EXTRA|)
                        (if
                          (eql (ps:typeof |LISTENER|) "function")
                          (ps:chain
                            |LIST|
                            (|PUSH| |LISTENER|)))
                        (if
                          (and
                            (eql |NAME| "onBeforeRequest")
                            |EXTRA|
                            (>=
                              (ps:chain
                                |EXTRA|
                                (|indexOf| "blocking"))
                              0))
                          (ps:chain
                            (|RPC| "webRequest.blocking" (ps:create |ON| t))
                            (|CATCH| (lambda
                                ()
                                (js-return)))))
                        (js-return)))
                    (|REPLACE|
                      (aref (ps:@ |CHROME| |webRequest|) |NAME|)
                      "removeListener"
                      (lambda
                        (|LISTENER|)
                        (ps:var
                          |INDEX|
                          (ps:chain
                            |LIST|
                            (|indexOf| |LISTENER|)))
                        (if
                          (>= |INDEX| 0)
                          (ps:chain
                            |LIST|
                            (|SPLICE| |INDEX| 1)))
                        (js-return)))
                    (|REPLACE|
                      (aref (ps:@ |CHROME| |webRequest|) |NAME|)
                      "hasListener"
                      (lambda
                        (|LISTENER|)
                        (js-return
                          (>=
                            (ps:chain
                              |LIST|
                              (|indexOf| |LISTENER|))
                            0))))
                    (js-return))))))
          (setf
            (ps:@ |CHROME| |declarativeContent|)
            (or (ps:@ |CHROME| |declarativeContent|) (ps:create)))
          (|ensureEvent|
            (ps:@ |CHROME| |declarativeContent|)
            "onPageChanged"
            "declarativeContent.onPageChanged")
          (setf (ps:@ |CHROME| |readingList|) (or (ps:@ |CHROME| |readingList|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |readingList|)
            "query"
            (lambda
              (|QUERY| |CALLBACK|)
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:array))) |CALLBACK|))))
          (setf (ps:@ |CHROME| |fontSettings|) (or (ps:@ |CHROME| |fontSettings|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |fontSettings|)
            "getFontList"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:array (ps:create |fontId| "sans-serif" |displayName| "Sans"))))
                  |CALLBACK|))))
          (setf (ps:@ |CHROME| |DNS|) (or (ps:@ |CHROME| |DNS|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |DNS|)
            "resolve"
            (lambda
              (|HOSTNAME| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:create |resultCode| 0 |ADDRESS| "")))
                  |CALLBACK|))))
          (setf (ps:@ |CHROME| |THEME|) (or (ps:@ |CHROME| |THEME|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |THEME|)
            "getCurrent"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH| (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:create))) |CALLBACK|))))
          (|ENSURE| (ps:@ |CHROME| |THEME|) "update" (lambda
              ()
              (js-return)))
          (|ENSURE| (ps:@ |CHROME| |THEME|) "reset" (lambda
              ()
              (js-return)))
          (setf (ps:@ |CHROME| |GCM|) (or (ps:@ |CHROME| |GCM|) (ps:create)))
          (|ensureEvent| (ps:@ |CHROME| |GCM|) "onMessage" "gcm.onMessage")
          (setf (ps:@ |CHROME| |instanceID|) (or (ps:@ |CHROME| |instanceID|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |instanceID|)
            "getID"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:@ |CHROME| |RUNTIME| |ID|)))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |instanceID|)
            "getToken"
            (lambda
              (|DETAILS| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|REJECT| (ps:new (|Error| "Instance ID is not available."))))
                  |CALLBACK|))))
          (|ensureEvent| (ps:@ |CHROME| |instanceID|) "onTokenRefresh" "instanceID.onTokenRefresh")
          (setf (ps:@ |CHROME| |printerProvider|) (or (ps:@ |CHROME| |printerProvider|) (ps:create)))
          (ps:chain
            (ps:array "onGetPrintersRequested" "onGetCapabilityRequested" "onPrintRequested")
            (|forEach|
              (lambda
                (|NAME|)
                (|ensureEvent| (ps:@ |CHROME| |printerProvider|) |NAME| (+ "printerProvider." |NAME|))
                (js-return))))
          (defun
            |UNAVAILABLE|
            (|NAME|)
            (js-return
              (lambda
                ()
                (ps:var |CALLBACK| (aref |ARGUMENTS| (- (ps:@ |ARGUMENTS| |LENGTH|) 1)))
                (js-return
                  (|BOTH|
                    (ps:chain
                      |Promise|
                      (|REJECT| (ps:new (|Error| (+ |NAME| " is not available.")))))
                    (if
                      (eql (ps:typeof |CALLBACK|) "function")
                      |CALLBACK|
                      |UNDEFINED|))))))
          (setf (ps:@ |CHROME| |DEBUGGER|) (or (ps:@ |CHROME| |DEBUGGER|) (ps:create)))
          (ps:chain
            (ps:array "attach" "detach" "sendCommand" "getTargets")
            (|forEach|
              (lambda
                (|NAME|)
                (|ENSURE| (ps:@ |CHROME| |DEBUGGER|) |NAME| (|UNAVAILABLE| (+ "debugger." |NAME|)))
                (js-return))))
          (|ensureEvent| (ps:@ |CHROME| |DEBUGGER|) "onEvent" "debugger.onEvent")
          (|ensureEvent| (ps:@ |CHROME| |DEBUGGER|) "onDetach" "debugger.onDetach")
          (setf (ps:@ |CHROME| |tabCapture|) (or (ps:@ |CHROME| |tabCapture|) (ps:create)))
          (|ENSURE| (ps:@ |CHROME| |tabCapture|) "capture" (|UNAVAILABLE| "tabCapture.capture"))
          (|ENSURE|
            (ps:@ |CHROME| |tabCapture|)
            "getMediaStreamId"
            (|UNAVAILABLE| "tabCapture.getMediaStreamId"))
          (setf (ps:@ |CHROME| |desktopCapture|) (or (ps:@ |CHROME| |desktopCapture|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |desktopCapture|)
            "chooseDesktopMedia"
            (lambda
              (|SOURCES| |CALLBACK|)
              (if
                |CALLBACK|
                (|CALLBACK| ""))
              (js-return 0)))
          (setf (ps:@ |CHROME| |pageCapture|) (or (ps:@ |CHROME| |pageCapture|) (ps:create)))
          (|ENSURE| (ps:@ |CHROME| |pageCapture|) "saveAsMHTML" (|UNAVAILABLE| "pageCapture.saveAsMHTML"))
          (setf (ps:@ |CHROME| |fileSystem|) (or (ps:@ |CHROME| |fileSystem|) (ps:create)))
          (|ENSURE| (ps:@ |CHROME| |fileSystem|) "chooseEntry" (|UNAVAILABLE| "fileSystem.chooseEntry"))
          (setf (ps:@ |CHROME| |DEVTOOLS|) (or (ps:@ |CHROME| |DEVTOOLS|) (ps:create)))
          (setf
            (ps:@ |CHROME| |DEVTOOLS| |inspectedWindow|)
            (or (ps:@ |CHROME| |DEVTOOLS| |inspectedWindow|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |DEVTOOLS| |inspectedWindow|)
            "eval"
            (lambda
              (|CODE| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    (ps:chain
                      (|RPC|
                        "tabs.query"
                        (ps:create
                          |QUERY|
                          (ps:create |ACTIVE| t)))
                      (|THEN|
                        (lambda
                          (|TABS|)
                          (ps:var |TAB| (and |TABS| (aref |TABS| 0)))
                          (if
                            (not |TAB|)
                            (throw (ps:new (|Error| "No inspected window."))))
                          (js-return
                            (|RPC|
                              "scripting.executeScript"
                              (ps:create
                                |tabId|
                                (ps:@ |TAB| |ID|)
                                |CODE|
                                |CODE|))))))
                    (|THEN|
                      (lambda
                        (|VALUE|)
                        (if
                          |CALLBACK|
                          (|CALLBACK| |VALUE| nil))
                        (js-return |VALUE|))))
                  |CALLBACK|))))
          (setf
            (ps:@ |CHROME| |DEVTOOLS| |NETWORK|)
            (or (ps:@ |CHROME| |DEVTOOLS| |NETWORK|) (ps:create)))
          (|ensureEvent|
            (ps:@ |CHROME| |DEVTOOLS| |NETWORK|)
            "onNavigated"
            "devtools.network.onNavigated")
          (setf (ps:@ |CHROME| |DEVTOOLS| |PANELS|) (or (ps:@ |CHROME| |DEVTOOLS| |PANELS|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |DEVTOOLS| |PANELS|)
            "create"
            (lambda
              (|TITLE| |ICON| |PAGE| |CALLBACK|)
              (ps:var
                |PANEL|
                (ps:create
                  |onShown|
                  (ps:create)
                  |onHidden|
                  (ps:create)))
              (|ensureEvent| |PANEL| "onShown" "devtools.panels.onShown")
              (|ensureEvent| |PANEL| "onHidden" "devtools.panels.onHidden")
              (if
                |CALLBACK|
                (|CALLBACK| |PANEL|))
              (js-return |PANEL|)))
          (setf (ps:@ |CHROME| |I18N|) (or (ps:@ |CHROME| |I18N|) (ps:create)))
          (|ENSURE|
            (ps:@ |CHROME| |I18N|)
            "getMessage"
            (lambda
              (|NAME|)
              (js-return (or |NAME| ""))))
          (|ENSURE|
            (ps:@ |CHROME| |I18N|)
            "getUILanguage"
            (lambda
              ()
              (js-return (or (ps:@ |NAVIGATOR| |LANGUAGE|) "en"))))
          (|ENSURE|
            (ps:@ |CHROME| |I18N|)
            "getAcceptLanguages"
            (lambda
              (|CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE| (ps:array (or (ps:@ |NAVIGATOR| |LANGUAGE|) "en"))))
                  |CALLBACK|))))
          (|ENSURE|
            (ps:@ |CHROME| |I18N|)
            "detectLanguage"
            (lambda
              (|TEXT| |CALLBACK|)
              (js-return
                (|BOTH|
                  (ps:chain
                    |Promise|
                    (|RESOLVE|
                      (ps:create
                        |isReliable|
                        ps:false
                        |LANGUAGES|
                        (ps:array (ps:create |LANGUAGE| "en" |PERCENTAGE| 100)))))
                  |CALLBACK|))))
          (setf
            (ps:@ |globalThis| |NYXT|)
            (ps:create
              |activateMenu|
              (lambda
                (|menuItemId|)
                (js-return (|RPC| "contextMenus.activate" (ps:create |menuItemId| |menuItemId|))))))
          (js-return)))))

(-> extension-content-javascript () string)
(defun extension-content-javascript ()
  "JavaScript for the content-script bridge."
  (ps:ps
    (funcall
        (lambda
          ()
          (if
            (ps:@ |globalThis| |__nyxtContentBridge|)
            (js-return))
          (setf (ps:@ |globalThis| |__nyxtContentBridge|) t)
          (ps:var |LISTENERS| (ps:array))
          (ps:var
            |ORIGINAL|
            (ps:chain
              |CHROME|
              |RUNTIME|
              |onMessage|
              |addListener|
              (|BIND| (ps:@ |CHROME| |RUNTIME| |onMessage|))))
          (setf
            (ps:@ |CHROME| |RUNTIME| |onMessage| |addListener|)
            (lambda
              (|LISTENER|)
              (if
                (eql (ps:typeof |LISTENER|) "function")
                (ps:chain
                  |LISTENERS|
                  (|PUSH| |LISTENER|)))
              (js-return (|ORIGINAL| |LISTENER|))))
          (defun
            |DELIVER|
            (|PAYLOAD|)
            (if
              (not |PAYLOAD|)
              (js-return))
            (ps:var |REPLIED| ps:false)
            (defun
              |sendResponse|
              (|RESPONSE|)
              (if
                |REPLIED|
                (js-return))
              (setf |REPLIED| t)
              (ps:chain
                |CHROME|
                |RUNTIME|
                (|sendMessage|
                  (ps:create
                    |__NYXT|
                    "reply"
                    |TOKEN|
                    (ps:@ |PAYLOAD| |TOKEN|)
                    |RESULT|
                    |RESPONSE|)))
              (js-return))
            (ps:var |WAITING| ps:false)
            (ps:chain
              (ps:chain
                |LISTENERS|
                (|SLICE|))
              (|forEach|
                (lambda
                  (|LISTENER|)
                  (ps:var
                    |KEEP|
                    (|LISTENER|
                      (ps:@ |PAYLOAD| |MESSAGE|)
                      (or (ps:@ |PAYLOAD| |SENDER|) (ps:create))
                      |sendResponse|))
                  (if
                    (eql |KEEP| t)
                    (setf |WAITING| t))
                  (js-return))))
            (if
              (and (not |WAITING|) (not |REPLIED|))
              (|sendResponse| nil))
            (js-return))
          (ps:chain
            |WINDOW|
            (|addEventListener|
              "message"
              (lambda
                (|EVENT|)
                (if
                  (or
                    (/= (ps:@ |EVENT| |SOURCE|) |WINDOW|)
                    (not (ps:@ |EVENT| |DATA|))
                    (/= (ps:@ |EVENT| |DATA| |__NYXT|) "deliver"))
                  (js-return))
                (|DELIVER| (ps:@ |EVENT| |DATA|))
                (js-return))))
          (js-return)))))
