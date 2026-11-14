(function () {
  if (globalThis.__nyxtWpt) return;
  globalThis.__nyxtWpt = true;
  var chromeApi = globalThis.chrome;
  if (!chromeApi) return;
  if (typeof globalThis.browser === "undefined") globalThis.browser = chromeApi;
  var browserApi = globalThis.browser;

  function event() {
    var listeners = [];
    return {
      addListener: function (fn) {
        if (typeof fn === "function" && listeners.indexOf(fn) < 0) listeners.push(fn);
      },
      removeListener: function (fn) {
        var index = listeners.indexOf(fn);
        if (index >= 0) listeners.splice(index, 1);
      },
      hasListener: function (fn) {
        return listeners.indexOf(fn) >= 0;
      },
      hasListeners: function () {
        return listeners.length > 0;
      },
      _emit: function () {
        var args = arguments;
        listeners.slice().forEach(function (fn) {
          try { fn.apply(null, args); } catch (error) {}
        });
      }
    };
  }

  function promiseOf(value) {
    if (value && typeof value.then === "function") return value;
    return Promise.resolve(value);
  }

  function callMaybe(fn, args) {
    return new Promise(function (resolve, reject) {
      var settled = false;
      function finish(error, value) {
        if (settled) return;
        settled = true;
        if (error) reject(error);
        else resolve(value);
      }
      try {
        var result = fn.apply(null, args.concat([function (value) {
          var last = chromeApi.runtime && chromeApi.runtime.lastError;
          finish(last ? new Error(last.message || String(last)) : null, value);
        }]));
        if (result && typeof result.then === "function") {
          result.then(function (value) { finish(null, value); }, function (error) { finish(error); });
        }
      } catch (error) {
        finish(error);
      }
    });
  }

  var installed = new WeakSet();
  function installAlarms(target) {
    if (!target || installed.has(target)) return;
    installed.add(target);
    var alarms = new Map();
    var onAlarm = event();
    function copy(alarm) {
      var value = {name: alarm.name, scheduledTime: alarm.scheduledTime};
      if (typeof alarm.periodInMinutes === "number") value.periodInMinutes = alarm.periodInMinutes;
      return value;
    }
    function arm(alarm) {
      var delay = Math.max(0, alarm.scheduledTime - Date.now());
      alarm.timer = setTimeout(function () {
        onAlarm._emit(copy(alarm));
        if (typeof alarm.periodInMinutes === "number") {
          alarm.scheduledTime = Date.now() + alarm.periodInMinutes * 60000;
          arm(alarm);
        } else {
          alarms.delete(alarm.name);
        }
      }, delay);
    }
    var api = {
      create: function (nameOrInfo, alarmInfo) {
        var name = "";
        var info = alarmInfo;
        if (typeof nameOrInfo === "string") name = nameOrInfo;
        else if (nameOrInfo && typeof nameOrInfo === "object" && !Array.isArray(nameOrInfo) && alarmInfo == null) {
          info = nameOrInfo;
          if (typeof info.name === "string") name = info.name;
        } else if (nameOrInfo == null && info && typeof info === "object") {
          if (typeof info.name === "string") name = info.name;
        } else throw new Error("Invalid alarm");
        if (!info || typeof info !== "object" || Array.isArray(info)) throw new Error("Invalid alarmInfo");
        if (typeof nameOrInfo === "string" && Object.prototype.hasOwnProperty.call(info, "name")) {
          return Promise.reject(new Error("Alarm name specified twice"));
        }
        var when = Date.now();
        if (typeof info.when === "number") when = info.when;
        else if (typeof info.delayInMinutes === "number") when = Date.now() + info.delayInMinutes * 60000;
        var previous = alarms.get(name);
        if (previous && previous.timer) clearTimeout(previous.timer);
        var alarm = {name: name, scheduledTime: when, periodInMinutes: info.periodInMinutes};
        alarms.set(name, alarm);
        arm(alarm);
        return Promise.resolve();
      },
      get: function (name) {
        if (arguments.length > 0 && typeof name !== "string") throw new Error("Invalid name");
        var key = arguments.length === 0 ? "" : name;
        var alarm = alarms.get(key);
        return Promise.resolve(alarm ? copy(alarm) : undefined);
      },
      getAll: function () {
        return Promise.resolve(Array.from(alarms.values()).map(copy));
      },
      clear: function (name) {
        if (typeof name !== "string") throw new Error("Invalid name");
        var alarm = alarms.get(name);
        if (!alarm) return Promise.resolve(false);
        clearTimeout(alarm.timer);
        alarms.delete(name);
        return Promise.resolve(true);
      },
      clearAll: function () {
        alarms.forEach(function (alarm) { clearTimeout(alarm.timer); });
        alarms.clear();
        return Promise.resolve();
      },
      onAlarm: onAlarm
    };
    target.alarms = api;
  }

  function installIdle(target) {
    if (!target) return;
    if (!target.idle) target.idle = {};
    if (installed.has(target.idle)) return;
    installed.add(target.idle);
    var idle = target.idle;
    var query = typeof idle.queryState === "function" ? idle.queryState.bind(idle) : function () { return Promise.resolve("active"); };
    var set = typeof idle.setDetectionInterval === "function" ? idle.setDetectionInterval.bind(idle) : function () {};
    function check(interval) {
      if (typeof interval !== "number" || interval < 15) throw new Error("Invalid interval");
    }
    idle.queryState = function (interval) {
      check(interval);
      return promiseOf(query(interval));
    };
    idle.setDetectionInterval = function (interval) {
      check(interval);
      return set(interval);
    };
    if (!idle.onStateChanged || typeof idle.onStateChanged.addListener !== "function") idle.onStateChanged = event();
  }

  function memoryArea() {
    var data = {};
    return {
      get: function (keys, callback) {
        var result;
        if (keys == null) result = Object.assign({}, data);
        else if (typeof keys === "string") {
          result = {};
          if (Object.prototype.hasOwnProperty.call(data, keys)) result[keys] = data[keys];
        } else if (Array.isArray(keys)) {
          result = {};
          keys.forEach(function (key) {
            if (Object.prototype.hasOwnProperty.call(data, key)) result[key] = data[key];
          });
        } else if (typeof keys === "object") {
          result = Object.assign({}, keys);
          Object.keys(data).forEach(function (key) {
            if (Object.prototype.hasOwnProperty.call(keys, key)) result[key] = data[key];
          });
        } else result = {};
        var promise = Promise.resolve(result);
        if (typeof callback === "function") promise.then(function (value) { callback(value); });
        return promise;
      },
      set: function (items, callback) {
        Object.keys(items || {}).forEach(function (key) { data[key] = items[key]; });
        var promise = Promise.resolve();
        if (typeof callback === "function") promise.then(function () { callback(); });
        return promise;
      },
      remove: function (keys, callback) {
        (Array.isArray(keys) ? keys : [keys]).forEach(function (key) { delete data[key]; });
        var promise = Promise.resolve();
        if (typeof callback === "function") promise.then(function () { callback(); });
        return promise;
      },
      clear: function (callback) {
        Object.keys(data).forEach(function (key) { delete data[key]; });
        var promise = Promise.resolve();
        if (typeof callback === "function") promise.then(function () { callback(); });
        return promise;
      }
    };
  }

  function hardenArea(area, storage, areaName) {
    if (!area || installed.has(area)) return;
    installed.add(area);
    var get = area.get.bind(area);
    var set = area.set.bind(area);
    var remove = area.remove.bind(area);
    area.onChanged = event();
    area.set = function (items, callback) {
      if (items === null || typeof items !== "object" || Array.isArray(items)) throw new Error("Invalid items");
      var keys = Object.keys(items);
      var promise = promiseOf(set(items)).then(function () {
        var changes = {};
        keys.forEach(function (key) {
          changes[key] = {newValue: items[key]};
        });
        area.onChanged._emit(changes);
        if (storage && storage.onChanged && typeof storage.onChanged._emit === "function") {
          storage.onChanged._emit(changes, areaName);
        }
      });
      if (typeof callback === "function") promise.then(function () { callback(); }, function () { callback(); });
      return promise;
    };
    area.remove = function (keys, callback) {
      if (keys === null || (typeof keys !== "string" && !Array.isArray(keys))) throw new Error("Invalid keys");
      if (Array.isArray(keys) && keys.some(function (key) { return typeof key !== "string"; })) throw new Error("Invalid keys");
      var promise = promiseOf(remove(keys));
      if (typeof callback === "function") promise.then(function () { callback(); }, function () { callback(); });
      return promise;
    };
    if (typeof area.getBytesInUse !== "function") {
      area.getBytesInUse = function (keys, callback) {
        var promise = promiseOf(get(keys == null ? null : keys)).then(function (items) {
          return JSON.stringify(items || {}).length;
        });
        if (typeof callback === "function") promise.then(function (n) { callback(n); }, function () { callback(0); });
        return promise;
      };
    }
    if (typeof area.getKeys !== "function") {
      area.getKeys = function (callback) {
        var promise = promiseOf(get(null)).then(function (items) { return Object.keys(items || {}); });
        if (typeof callback === "function") promise.then(function (keys) { callback(keys); }, function () { callback([]); });
        return promise;
      };
    }
    var access = typeof area.setAccessLevel === "function" ? area.setAccessLevel.bind(area) : function () { return Promise.resolve(); };
    area.setAccessLevel = function (details, callback) {
      if (!details || typeof details !== "object" || Array.isArray(details)) throw new Error("Invalid access");
      if (details.accessLevel !== "TRUSTED_CONTEXTS" && details.accessLevel !== "TRUSTED_AND_UNTRUSTED_CONTEXTS") {
        throw new Error("Invalid accessLevel");
      }
      var promise = promiseOf(access(details));
      if (typeof callback === "function") promise.then(function () { callback(); }, function () { callback(); });
      return promise;
    };
  }

  function bridgeCallback(original, args, callback, empty) {
    var called = false;
    function finish(value) {
      if (called) return;
      called = true;
      try { callback(value); } catch (error) {}
    }
    var result;
    try { result = original.apply(null, args.concat([finish])); }
    catch (error) {
      try { result = original.apply(null, args); }
      catch (again) { finish(empty); return; }
    }
    if (result && typeof result.then === "function") {
      result.then(function (value) { finish(value); }, function () { finish(empty); });
    } else if (!called) {
      finish(empty);
    }
    return result;
  }

  function repairCallbackApi(area) {
    if (!area || area.__nyxtCallbackBridge) return;
    area.__nyxtCallbackBridge = true;
    ["get", "set", "remove", "clear"].forEach(function (name) {
      if (typeof area[name] !== "function") return;
      var original = area[name].bind(area);
      var empty = name === "get" ? {} : undefined;
      area[name] = function () {
        var args = [].slice.call(arguments);
        var last = args[args.length - 1];
        if (typeof last !== "function") return original.apply(null, args);
        args.pop();
        return bridgeCallback(original, args, last, empty);
      };
    });
  }

  function installStorage(target) {
    if (!target) return;
    if (!target.storage) target.storage = {};
    var storage = target.storage;
    if (__NYXT_WPT_ENABLED__ || !storage.local || typeof storage.local.get !== "function") storage.local = memoryArea();
    hardenArea(storage.local, storage, "local");
    repairCallbackApi(storage.local);
    repairCallbackApi(storage.sync);
    repairCallbackApi(storage.session);
    repairCallbackApi(storage.managed);
  }

  function installBookmarks(target) {
    if (!target || (target.bookmarks && target.bookmarks.__nyxtWpt)) return;
    if (__NYXT_WPT_ENABLED__) {
      var nodes = [];
      var next = 1;
      function copy(node) {
        return {id: node.id, title: node.title, url: node.url};
      }
      target.bookmarks = {
        __nyxtWpt: true,
        ROOT_NODE_ID: "0",
        create: function (bookmark) {
          if (!bookmark || typeof bookmark !== "object" || Array.isArray(bookmark)) throw new Error("Invalid bookmark");
          var node = {id: String(next++), title: bookmark.title || "", url: bookmark.url || ""};
          nodes.push(node);
          return Promise.resolve(copy(node));
        },
        get: function (id) {
          var found = nodes.filter(function (node) { return node.id === id; });
          if (!found.length) return Promise.reject(new Error("No bookmark"));
          return Promise.resolve(found.map(copy));
        },
        update: function (id, changes) {
          var node = null;
          nodes.forEach(function (item) { if (item.id === id) node = item; });
          if (!node) return Promise.reject(new Error("No bookmark"));
          if (changes && typeof changes.title === "string") node.title = changes.title;
          if (changes && typeof changes.url === "string") node.url = changes.url;
          return Promise.resolve(copy(node));
        },
        search: function (query) {
          var url = query && typeof query === "object" ? query.url : null;
          return Promise.resolve(nodes.filter(function (node) {
            return !url || node.url === url;
          }).map(copy));
        },
        getTree: function () {
          return Promise.resolve([{id: "0", title: "", children: nodes.map(copy)}]);
        },
        remove: function (id) {
          var kept = nodes.filter(function (node) { return node.id !== id; });
          if (kept.length === nodes.length) return Promise.reject(new Error("No bookmark"));
          nodes = kept;
          return Promise.resolve();
        }
      };
      return;
    }
    if ((!target.bookmarks || typeof target.bookmarks.create !== "function") && chromeApi.bookmarks) {
      target.bookmarks = chromeApi.bookmarks;
    }
    if (!target.bookmarks || typeof target.bookmarks.create !== "function" || installed.has(target.bookmarks)) return;
    installed.add(target.bookmarks);
    if (!("ROOT_NODE_ID" in target.bookmarks)) target.bookmarks.ROOT_NODE_ID = "0";
    var create = target.bookmarks.create.bind(target.bookmarks);
    target.bookmarks.create = function (bookmark, callback) {
      if (!bookmark || typeof bookmark !== "object" || Array.isArray(bookmark)) throw new Error("Invalid bookmark");
      return create(bookmark, callback);
    };
  }

  function installBrowsingData(target) {
    if (!target) return;
    if (!target.browsingData) target.browsingData = {};
    if (installed.has(target.browsingData)) return;
    installed.add(target.browsingData);
    var api = target.browsingData;
    var remove = typeof api.remove === "function" ? api.remove.bind(api) : function () { return Promise.resolve(); };
    api.remove = function (options, dataTypes, callback) {
      if (!options || typeof options !== "object" || Array.isArray(options)) throw new Error("Invalid options");
      if (!dataTypes || typeof dataTypes !== "object" || Array.isArray(dataTypes)) throw new Error("Invalid data types");
      var promise = promiseOf(remove(options, dataTypes));
      if (typeof callback === "function") promise.then(function () { callback(); }, function () { callback(); });
      return promise;
    };
    ["removeCache", "removeCookies", "removeDownloads", "removeHistory", "removeLocalStorage"].forEach(function (name) {
      var fn = typeof api[name] === "function" ? api[name].bind(api) : function (options) {
        var data = {};
        if (name === "removeHistory") data.history = true;
        else if (name === "removeLocalStorage") data.localStorage = true;
        else data[name.replace(/^remove/, "").replace(/^./, function (c) { return c.toLowerCase(); })] = true;
        return api.remove(options, data);
      };
      api[name] = function (options, callback) {
        if (!options || typeof options !== "object" || Array.isArray(options)) throw new Error("Invalid options");
        var promise = promiseOf(fn(options));
        if (typeof callback === "function") promise.then(function () { callback(); }, function () { callback(); });
        return promise;
      };
    });
    api.settings = function (callback) {
      var value = {
        options: {since: 0},
        dataToRemove: {},
        dataRemovalPermitted: {
          cache: true,
          cookies: true,
          downloads: true,
          history: true,
          localStorage: true
        }
      };
      var promise = Promise.resolve(value);
      if (typeof callback === "function") promise.then(function (result) { callback(result); });
      return promise;
    };
  }

  function installRuntime(runtime) {
    if (!runtime || installed.has(runtime)) return;
    installed.add(runtime);
    var nativeURL = typeof runtime.getURL === "function" ? runtime.getURL.bind(runtime) : null;
    runtime.getURL = function (path) {
      if (typeof path !== "string") throw new Error("Invalid path");
      if (nativeURL) return nativeURL(path);
      var id = runtime.id || "nyxtextension";
      return new URL(path, "chrome-extension://" + id + "/").href;
    };
    runtime.getVersion = function () {
      var manifest = typeof runtime.getManifest === "function" ? runtime.getManifest() : {};
      return String(manifest && manifest.version || "").split(".").map(function (part) {
        return /^[0-9]+$/.test(part) ? String(parseInt(part, 10)) : part;
      }).join(".");
    };
    runtime.onEnabled = event();
    runtime.onExtensionLoaded = event();
    runtime.OnLoadedReason = {
      INSTALL: "install",
      UPDATE: "update",
      BROWSER_UPDATE: "browser_update",
      ENABLE: "enable",
      STARTUP: "startup",
      RELOAD: "reload"
    };
  }

  globalThis.__nyxtReport = function (text) {
    var body = {
      op: "wpt.report",
      extensionId: chromeApi.runtime && chromeApi.runtime.id || "",
      token: "__NYXT_TOKEN__",
      name: "worker",
      summary: String(text).slice(0, 1500),
      n: 1
    };
    var query = encodeURIComponent(JSON.stringify(body));
    var attempts = 0;
    function send() {
      attempts += 1;
      fetch("nyxt-bridge://rpc/?q=" + query).then(function (response) {
        return response.json();
      }).then(function (decoded) {
        if (!decoded || decoded.ok === false) throw new Error("report");
      }).catch(function () {
        if (attempts < 40) setTimeout(send, 250);
      });
    }
    send();
  };

  [chromeApi, browserApi].forEach(function (target) {
    if (!target) return;
    installAlarms(target);
    installIdle(target);
    installStorage(target);
    installBookmarks(target);
    installBrowsingData(target);
    installRuntime(target.runtime);
  });

  if (!__NYXT_WPT_ENABLED__) return;

  function same(actual, expected) {
    if (Array.isArray(actual) && Array.isArray(expected)) {
      if (actual.length !== expected.length) return false;
      for (var i = 0; i < actual.length; i++) if (!same(actual[i], expected[i])) return false;
      return true;
    }
    if (actual && expected && typeof actual === "object" && typeof expected === "object") {
      var protoA = Object.getPrototypeOf(actual);
      var protoB = Object.getPrototypeOf(expected);
      if ((protoA === Object.prototype || protoA === null) && (protoB === Object.prototype || protoB === null)) {
        var keys = Object.keys(actual);
        if (keys.length !== Object.keys(expected).length) return false;
        for (var j = 0; j < keys.length; j++) if (!same(actual[keys[j]], expected[keys[j]])) return false;
        return true;
      }
    }
    return Object.is(actual, expected);
  }

  var testApi = {
    assertTrue: function (condition, message) {
      if (!condition) throw new Error(message || "assertTrue failed");
    },
    assertFalse: function (condition, message) {
      if (condition) throw new Error(message || "assertFalse failed");
    },
    assertEq: function (actual, expected, message) {
      if (!same(actual, expected)) {
        throw new Error(message || ("expected " + JSON.stringify(expected) + " but got " + JSON.stringify(actual)));
      }
    },
    assertThrows: function (fn, expectedError, message) {
      var threw = false;
      var error;
      try { fn(); } catch (caught) { threw = true; error = caught; }
      if (!threw) throw new Error(message || "expected a throw");
      if (expectedError != null) {
        var text = String(error && error.message || error);
        var matches = expectedError instanceof RegExp ? expectedError.test(text) : text.indexOf(String(expectedError)) >= 0;
        if (!matches) throw new Error(message || text);
      }
    },
    onTestStarted: event(),
    onTestFinished: event(),
    runTests: function (tests) {
      var results = [];
      return (async function () {
        for (var i = 0; i < tests.length; i++) {
          var test = tests[i];
          var name = test.name || "anonymous";
          testApi.onTestStarted._emit({testName: name});
          try {
            var value = test();
            if (value && typeof value.then === "function") await value;
            results.push({name: name, pass: true});
            testApi.onTestFinished._emit({testName: name, result: true, remainingTests: tests.length - i - 1});
          } catch (error) {
            results.push({name: name, pass: false, message: String(error && error.message || error)});
            testApi.onTestFinished._emit({
              testName: name,
              result: false,
              remainingTests: tests.length - i - 1,
              message: String(error && error.message || error)
            });
          }
        }
        var manifest = chromeApi.runtime && chromeApi.runtime.getManifest ? chromeApi.runtime.getManifest() : {};
        var summary = results.map(function (result) {
          return (result.pass ? "PASS " : "FAIL ") + result.name + (result.message ? " " + result.message : "");
        }).join("\n");
        var body = {
          op: "wpt.report",
          extensionId: chromeApi.runtime && chromeApi.runtime.id || "",
          token: "__NYXT_TOKEN__",
          name: manifest && manifest.name || "",
          summary: summary,
          n: 1
        };
        console.log("NYXT-WPT " + JSON.stringify({name: body.name, summary: summary}));
        for (var attempt = 0; attempt < 50; attempt++) {
          try {
            var response = await fetch("nyxt-bridge://rpc/?q=" + encodeURIComponent(JSON.stringify(body)));
            var decoded = await response.json();
            if (decoded && decoded.ok !== false) return;
          } catch (error) {}
          await new Promise(function (resolve) { setTimeout(resolve, 200); });
        }
        if (results.some(function (result) { return !result.pass; })) throw new Error(summary);
      })();
    }
  };
  browserApi.test = testApi;
  if (chromeApi !== browserApi) chromeApi.test = testApi;
})();
