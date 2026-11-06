(function () {
  var id = __BUFFER_ID__;
  var assetRoot = __ASSET_ROOT__;
  var step = __CHUNK_BYTES__;

  function send(action, query) {
    fetch("nyxt-editor://session/" + action + "?" + query, {mode: "no-cors"});
  }

  function encodeData(bytes) {
    var binary = "";
    for (var index = 0; index < bytes.length; index++)
      binary += String.fromCharCode(bytes[index]);
    return btoa(binary).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/g, "");
  }

  function decodeUtf8(payload) {
    if (!payload) return "";
    var binary = atob(payload);
    var bytes = new Uint8Array(binary.length);
    for (var index = 0; index < binary.length; index++)
      bytes[index] = binary.charCodeAt(index);
    return new TextDecoder().decode(bytes);
  }

  function registerLisp() {
    var known = monaco.languages.getLanguages().some(function (language) {
      return language.id === "lisp";
    });
    if (known) return;
    monaco.languages.register({id: "lisp"});
    monaco.languages.setMonarchTokensProvider("lisp", {
      tokenizer: {
        root: [
          [/;.*/, "comment"],
          [/"([^"\\]|\\.)*"/, "string"],
          [/#\\./, "string"],
          [/'[^\s()\[\]{}]+/, "string"],
          [/\b(?:defun|defmacro|defpackage|define-mode|define-class|defmethod|defvar|defparameter|let|if|when|unless|cond|lambda|progn|loop|return|setf|setq)\b/,
           "keyword"],
          [/[+-]?\d+(?:\.\d+)?/, "number"]
        ]
      }
    });
  }

  function setup() {
    try {
    registerLisp();
    monaco.editor.defineTheme("nyxt", {
      base: __BASE_THEME__,
      inherit: true,
      rules: [],
      colors: {
        "editor.background": __BACKGROUND__,
        "editor.foreground": __FOREGROUND__,
        "editorCursor.foreground": __CURSOR__,
        "editor.selectionBackground": __SELECTION__,
        "editor.lineHighlightBackground": __LINE__,
        "editorLineNumber.foreground": __FOREGROUND__,
        "editorWidget.background": __BACKGROUND__,
        "editorWidget.foreground": __FOREGROUND__
      }
    });
    var editor = monaco.editor.create(document.getElementById("editor"), {
      value: "",
      language: "plaintext",
      theme: "nyxt",
      fontFamily: __FONT__,
      fontSize: 15,
      automaticLayout: true,
      scrollBeyondLastLine: false,
      minimap: {enabled: true},
      wordWrap: "off",
      readOnly: true,
      padding: {top: 8, bottom: 8}
    });
    var api = {
      editor: editor,
      applying: false,
      dirty: false,
      generation: 0,
      focus: function () { editor.focus(); },
      markClean: function (generation) {
        if (generation === api.generation) api.dirty = false;
      },
      open: function (payload, language, path) {
        api.applying = true;
        var text = decodeUtf8(payload);
        var uri = path ? monaco.Uri.file(path) : monaco.Uri.parse("inmemory://nyxt/" + id);
        var previous = editor.getModel();
        var model = monaco.editor.getModel(uri);
        if (model) {
          model.setValue(text);
          monaco.editor.setModelLanguage(model, language || "plaintext");
        } else {
          model = monaco.editor.createModel(text, language || "plaintext", uri);
        }
        if (previous !== model) {
          editor.setModel(model);
          if (previous) previous.dispose();
        }
        api.dirty = false;
        api.generation = 0;
        api.applying = false;
        editor.updateOptions({readOnly: false});
        editor.focus();
      },
      save: function () {
        var bytes = new TextEncoder().encode(editor.getValue());
        var count = Math.max(1, Math.ceil(bytes.length / step));
        var generation = api.generation;
        var index = 0;
        function next() {
          var end = Math.min(bytes.length, (index + 1) * step);
          var slice = bytes.subarray(index * step, end);
          var query = "buffer=" + id
            + "&index=" + index
            + "&count=" + count
            + "&generation=" + generation
            + "&data=" + encodeURIComponent(encodeData(slice));
          index += 1;
          fetch("nyxt-editor://session/chunk?" + query, {mode: "no-cors"})
            .then(function () {
              if (index < count) next();
            });
        }
        next();
      },
      dispose: function () { editor.dispose(); }
    };
    editor.onDidChangeModelContent(function () {
      if (api.applying) return;
      api.generation += 1;
      if (!api.dirty) {
        api.dirty = true;
        send("changed", "buffer=" + id + "&generation=" + api.generation);
      }
    });
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, function () {
      api.save();
    });
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyO, function () {
      send("open", "buffer=" + id);
    });
    if (window.nyxtEditor && window.nyxtEditor.dispose) window.nyxtEditor.dispose();
    window.nyxtEditor = api;
    api.focus();
    send("ready", "buffer=" + id);
    } catch (error) {
      send("failed", "buffer=" + id + "&message=" + encodeURIComponent(String(error)));
    }
  }

  function start() {
    var node = document.getElementById("editor");
    if (!node) {
      window.setTimeout(start, 30);
      return;
    }
    function load() {
      window.require.config({paths: {vs: assetRoot}});
      window.require(["vs/editor/editor.main"], setup, function () {
        send("failed", "buffer=" + id);
      });
    }
    if (window.require) {
      load();
      return;
    }
    var script = document.createElement("script");
    script.src = assetRoot + "/loader.js";
    script.onload = load;
    script.onerror = function () { send("failed", "buffer=" + id); };
    document.head.appendChild(script);
  }

  start();
})();
