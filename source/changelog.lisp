;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defparameter +changelog+ (make-hash-table :test #'equal)
  "A hash table mapping versions to release information.")

(defmacro define-version (version-string &body body)
  `(setf (gethash ,version-string +changelog+)
         (spinneret:with-html-string (:nsection :title ,version-string ,@body))))

(define-internal-page-command-global changelog ()
    (buffer "*Changelog*")
  "Show the changelog."
  (spinneret:with-html-string
    (:h1 "Change Log")
    (:p "Nyxt tries to use semantic versioning.")
    (:p "Given a version number MAJOR.MINOR.PATCH, we increment:")
    (:ul
     (:li "MAJOR when we make incompatible API changes.")
     (:li "MINOR when we add functionality in a backwards-compatible manner.")
     (:li "PATCH when we make backwards-compatible bug fixes."))
    (:p "See also the " (:code "migration-guide") ".")
    (loop for version being the hash-value of +changelog+
          collect (:raw version))))

(define-version "3.11.3"
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix prompt buffer freezes when invoking command " (:nxref :command 'set-url) "."))))

(define-version "3.11.2"
  (:nsection :title "Bug fixes"
    (:ul
     (:li "When enabling " (:code "vi") " keybindings via "
          (:a :href (nyxt-url 'common-settings) "common settings")
          ", start the prompt buffer with "
          (:nxref :mode 'nyxt/mode/vi:vi-insert-mode) "enabled.")
     (:li "The scroll position is remembered when issuing buffer reloading commands, such as "
          (:nxref :command 'reload-current-buffer) ".")
     (:li "Improve performance of " (:nxref :command 'nyxt/mode/editor:edit-file) ".")
     (:li "Fix bug that prevented listing buffers recovered via command "
          (:nxref :command 'reopen-buffer) "."))))

(define-version "3.11.1"
  (:nsection :title "UI/UX"
    (:ul
     (:li "Improve user experience of "
          (:nxref :class-name 'nyxt/mode/macro-edit:macro-edit-mode) ".")
     (:li "Improve UI of the interfaces bound to "
          (:nxref :class-name 'nyxt/mode/annotate:annotate-mode)
          " commands, such as "
          (:nxref :command 'nyxt/mode/annotate:show-annotations-for-current-url) ".")
     (:li "Review the appearance of all help system pages such as"
          (:nxref :command 'describe-class) ".")
     (:li "Review font sizes.")
     (:li "Add possibility to specify a monospace font via "
          (:nxref :class-name 'theme:theme :slot 'theme:monospace-font-family) "."
          "By default, it is set to DejaVu.")
     (:li "Review usage of proportional and monospace fonts.")
     (:li "Use monospace fonts in "
          (:nxref :class-name 'nyxt/mode/editor:plaintext-editor-mode) ".")
     (:li "Replace source heading buttons in the "
          (:nxref :class-name 'prompt-buffer) ".")
     (:li "Review styling of the " (:nxref :class-name 'status-buffer) ", "
          (:nxref :command 'nyxt/mode/repl:repl) " and " (:code "migration-guide") ".")
     (:li "Replace glyph that collapses sections, for instance in the "
          (:a :href (nyxt-url 'manual) "manual") ".")
     (:li "Improve UI of the interfaces bound to commands:"
          (:ul
           (:li (:nxref :command 'nyxt/mode/download:list-downloads))
           (:li (:nxref :command 'nyxt/mode/history:list-history))
           (:li (:nxref :command 'nyxt/mode/buffer-listing:list-buffers))
           (:li (:nxref :command 'nyxt/mode/bookmark:list-bookmarks))
           (:li (:nxref :command 'nyxt/mode/buffer-listing:buffers-panel))
           (:li (:nxref :command 'nyxt/mode/bookmark:bookmarks-panel)))))))

(define-version "3.11.0"
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix commands "
          (:nxref :command 'reopen-last-buffer) " and "
          (:nxref :command 'reopen-buffer) ".")
     (:li "Fix performance issues related to "
          (:nxref :class-name 'nyxt/mode/hint:hint-mode) ".")
     (:li "Fix logic behind "
          (:nxref :class-name 'prompter:source :slot 'prompter:actions-on-marks)
          ".")))
  (:nsection :title "UI/UX"
    (:ul
     (:li "Built-in modes and special pages have a dedicated menu area now. This menu
allows access to common functions that are defined within the mode."))))

(define-version "3.10.0"
  (:nsection :title "UI/UX"
    (:ul
     (:li "Improve source heading buttons, layout and interactions in the "
          (:nxref :class-name 'prompt-buffer) ".")
     (:li "Add checkboxes for suggestions within the "
          (:nxref :class-name 'prompt-buffer) ".")))
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix command "
          (:nxref :command 'nyxt/mode/annotate:show-annotations-for-current-url) ".")
     (:li "Command " (:code "nyxt/renderer/gtk:make-buffer-with-context")
          " now lists previously defined contexts.")
     (:li "Improve architecture of " (:nxref :class-name 'nyxt/mode/hint:hint-mode)
          " as to ensure that hints are shown for arbitrary URLs." ))))

(define-version "3.9.2"
  (:nsection :title "UI/UX"
    (:ul
     (:li "Review of the status buffer UI.  The URL area is now clickable.")))
  (:nsection :title "Bug fixes"
    (:ul
     (:li (:nxref :mode 'nyxt/mode/blocker:blocker-mode)
          " ensures that hostlist files are loaded when missing.")
     (:li "Fix connection to Nyxt via commands "
          (:code "start-swank") " and " (:code "start-slynk") ".")))
  (:nsection :title "Programming interface"
    (:ul
     (:li "When running Nyxt as a Flatpak, programs available on the host can be
invoked via " (:code "flatpak-spawn --host <command> <command-args>") "."))))

(define-version "3.9.1"
  (:nsection :title "UI/UX"
    (:ul
     (:li "UI improvement of interfaces bound to commands "
          (:nxref :command 'nyxt/mode/bookmark:bookmarks-panel) ", "
          (:nxref :command 'nyxt/mode/bookmark:list-bookmarks) " and "
          (:nxref :command 'nyxt/mode/buffer-listing:buffers-panel) ".")))
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix command " (:nxref :command 'nyxt/mode/bookmark:bookmark-url) ".")
     (:li "Fix commands that rely on "
          (:nxref :class-name 'browser :slot 'external-editor-program)
          ".")
     (:li "Fix command "
          (:nxref :command 'nyxt/mode/prompt-buffer:toggle-attributes-display)
          " and behavior of UI elements relying on it."))))

(define-version "3.9.0"
  (:nsection :title "Features"
    (:ul
     (:li "Complete refactor of the "
          (:a :href (nyxt-url 'common-settings) "Common Settings Menu") ".")
     (:li "Refactor the " (:nxref :package :theme)
	  " API to allow for more nuanced themes and richer interfaces.")))
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix the functionality of the download cancellation button.")
     (:li "Fix duplicated suggestions when issuing command "
          (:nxref :command 'describe-slot) ".")
     (:li "Fix accepted values of "
          (:nxref :class-name 'browser :slot 'external-editor-program)
          "."))))

(define-version "3.8.0"
  (:nsection :title "Features"
    (:ul
     (:li "Add new slots "
          (:code "nyxt/mode/hint:hints-offset-x")
          " and "
          (:code "nyxt/mode/hint:hints-offset-y")
          ", to change the position of hint overlays.")))
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix bug with subsequent invocations of "
          (:nxref :macro 'define-configuration) " on the same class and slot
being overwritten by the first.")))
  (:nsection :title "UI/UX"
    (:ul
     (:li "Keybindings are shown in a more user-friendly way, when using the CUA
keyscheme.")
     (:li "Modes in the status area are shown vertically in their tooltip to
improve readability.")
     (:li "Stylistic review of the help system pages to improve readability.")))
  (:nsection :title "Programming interface"
    (:ul
     (:li "Fix Nyxt connection via commands " (:code "start-slynk") "
and " (:code "start-swank") " on the Flatpak distribution.")
     (:li "Fix warning signaling when a mismatch between the history file and the current
Nyxt version exists.  It is only raised when the major version differs.")
     (:li "Buffers of type " (:nxref :class-name 'nyxt/mode/editor:editor-buffer) " have  "
          (:nxref :class-name 'nyxt/mode/editor:plaintext-editor-mode)
          "enabled by default."))))

(define-version "3.7.0"
  (:nsection :title "UI/UX"
    (:ul
     (:li "Minor UI improvements on the status and prompt buffer.")
     (:li "Stylistic review of the manual to improve readability.")))
  (:nsection :title "Features"
    (:ul
     (:li "Add possibility to open PDF files via command "
          (:nxref :command 'nyxt/mode/file-manager:open-file) ".")
     (:li "The installation process now takes into account the Appdata file and
a scalable icon."))))

(define-version "3.6.1"
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix clipboard facilities on X11 and Wayland.")
     (:li "Fix source code location on Flatpak.")
     (:li "Fix " (:nxref :mode 'nyxt/mode/style:dark-mode)
        " and the " (:nxref :command 'nyxt/mode/bookmarklets:darken)
        " bookmarklet command."))))

(define-version "3.6.0"
  (:nsection :title "Features"
    (:ul
     (:li "Add commands for importing history from Firefox, Google Chrome,
Chromium, Brave and Vivaldi. For instance, "
          (:nxref :command 'nyxt/mode/history-migration:import-history-from-firefox) ".")))
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix keyscheme configuration via "
          (:a :href (nyxt-url 'common-settings) "Common Settings Menu") ".")))
  (:nsection :title "UI/UX"
    (:ul
     (:li "Add Public Sans font and set it as the default.")
     (:li "Redesign status buffer buttons.")
     (:li "Minor review of " (:a :href (nyxt-url 'new) "start page") "."))))

(define-version "3.5.0"
  (:nsection :title "Features"
    (:ul
     (:li "Add new quick start tutorial, accesible via command "
            (:nxref :command 'nyxt:quick-start) ".")
     (:li "Add hinting support for pages using Shadow DOMs.")
     (:li "Add keybinding for command " (:nxref :command 'describe-any) ".")))
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix status buffer history buttons.")
     (:li "Fix bug in command " (:nxref :command 'nyxt/mode/repeat:repeat-key) ".")))
  (:nsection :title "UI/UX"
    (:ul
     (:li "Redesign " (:a :href (nyxt-url 'new) "start page") ".")
     (:li "Fix styling of progress bar.")
     (:li "Fix styling of prompt buffer's input area."))))

(define-version "3.4.0"
  (:nsection :title "Features"
    (:ul
     (:li "Enable native spell-checking from WebKitGTK. The language can be set via
command "
          (:code "set-spell-check-languages")
          ". The list of dictionaries is provided by Enchant.")
     (:li "Add prompt buffer keybindings for commands "
          (:nxref :command 'nyxt/mode/prompt-buffer:previous-page) " and "
          (:nxref :command 'nyxt/mode/prompt-buffer:next-page) ".")))
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Fix theme configuration via "
          (:a :href (nyxt-url 'common-settings) "Common Settings Menu") ".")
     (:li "Honor the value of environment variables "
          (:code "$XDG_DATA_HOME") ", "
          (:code "$XDG_CONFIG_HOME") "and "
          (:code "$XDG_CACHE_HOME") "for the Flatpak.")))
  (:nsection :title "UI/UX"
    (:ul
     (:li "Add cursor change on hover for buttons and links in internal pages and panel
buffers.")
     (:li "Improve user experience of " (:nxref :command 'nyxt:pick-color) "."))))

(define-version "3.3.0"
  (:nsection :title "Features"
    (:ul
     (:li "Change default search engine to "
          (:a :href "https://search.atlas.engineer/searxng"
              "Atlas' own SearXNG instance") ".")
     (:li "Add command " (:nxref :command 'nyxt:pick-color)
          ", to pick a color and copy its HEX, RGB or HSL code to clipboard.")
     (:li "Add keybindings for commands "
          (:nxref :command 'nyxt:delete-panel-buffer) " and "
          (:nxref :command 'nyxt:delete-all-panel-buffers) ".")
     (:li "Improve " (:nxref :command 'nyxt/mode/prompt-buffer:describe-prompt-buffer) ":")
     (:ul
      (:li "List available keybindings for enabled prompt buffer modes.")
      (:li "Reference current prompt buffer sources."))))
  (:nsection :title "Bug fixes"
    (:ul
     (:li "Honor the value of "
          (:nxref :slot 'zoom-ratio-default :class-name 'document-buffer) ".")
     (:li "Honor the value of the panel buffer's "
          (:nxref :slot 'style :class-name 'panel-buffer) ".")
     (:li "Fix bugs in the "
          (:a :href (nyxt-url 'common-settings) "common settings interface") ".")
     (:li "Fix clipboard support for the Flatpak on Wayland.")
     (:li "Fix behavior of "
          (:nxref :class-name 'nyxt/mode/hint:hint-mode) "when slots "
          (:nxref :class-name 'nyxt/mode/hint:hint-mode
            :slot 'nyxt/mode/hint:hinting-type)
          " and "
          (:nxref :class-name 'nyxt/mode/hint:hint-mode
            :slot 'nyxt/mode/hint:show-hint-scope-p)
          "are set to " (:code ":vi") " and " (:code "t") ", respectively.")))
  (:nsection :title "UI"
    (:ul
     (:li "Add border between panel buffer and main content area.")
     (:li "Apply nuanced background color to panel buffers.")))
  (:nsection :title "Programming interface"
    (:ul

     (:li (:nxref :class-name 'browser :slot 'external-editor-program)
          " no longer signals when the program is a string containing spaces.")
     (:li (:nxref :class-name 'browser :slot 'external-editor-program)
          " returns its value rather than returning a string value in a list."))))

(define-version "3.2.1"
  (:ul
   (:li "Fix clipboard support for the Flatpak.")))

(define-version "3.2.0"
  (:ul
   (:li "Add support for PDF.js.")
   (:li "Add command "
        (:nxref :command 'nyxt/mode/prompt-buffer:toggle-suggestions-display)
        ", that allows collapsing the prompt buffer to its input area.")
   (:li "Improve the UI of the REPL.")
   (:li "Output multiple values when evaluating Lisp expressions from "
        (:nxref :command 'nyxt:execute-command) ".")
   (:li "Bug fixes.")))

(define-version "3.1.0"
  (:ul
   (:li "The REPL provides symbol suggestions by issuing "
        (:nxref :command 'nyxt/mode/repl:suggest-into-cell) ", bound to "
        (:code "TAB (↹)") ".")
   (:li (:nxref :slot 'global-history-p :class-name 'buffer)
        " is enabled by default.  The old behavior can be recovered by setting
        it to " (:code "nil") ".")
   (:li "Bind " (:nxref :command 'nyxt:delete-current-buffer) " uniformly for
        all modes, when using the CUA keyscheme."))
  (:h3 "Programming interfaces")
  (:ul
   (:li (:code "conservative-history-movement-p") " is deprecated in favor of "
        (:nxref :slot 'global-history-p :class-name 'buffer) ".")))

(define-version "3.0.0"
  (:nsection
   :title "New features"
   (:ul
    (:li "Add Flatpak build.")
    (:li "New " (:a :href "nyxt:nyxt/migration:migration-guide" (:code "migration-guide"))
         " command to help the user migrate their configuration between major version
releases. Migration suggestions are automatically given on startup error.")
    (:li "The auto-config file is now suffixed with the major version number. This means
that upgrading Nyxt to a new major version will ignore the previous
auto-configuration (which probably wouldn't work anyways).")
    (:li "UserScript support (such as GreaseMonkey scripts).")
    (:li "Prompt buffer updated for intuitive matching with new algorithms and settings.")
    (:li "Revamp status buffer design for increased readability and aesthetics. Make it
fully customizable with " (:nxref :function 'format-status) " framework.")
    (:li "Status buffer placement can be changed with "
         (:nxref :slot 'status-buffer-position :class-name 'window) " (thanks to @mianmoreno)")
    (:li "Most help pages, including " (:a :href (nyxt-url 'manual) "the manual")
         " are more readable and interactive.")
    (:li "Add support for the Gopher and Gemini protocols.")
    (:li "Headless mode available through " (:code "--headless")
         " CLI switch. Config file (" (:code "--config")
         ") becomes the script to run in the headless instance on Nyxt.")
    (:li "Color-picker support when " (:nxref :slot 'native-dialogs :class-name 'browser) " are on.")
    (:li "New "
         (:nxref :slot 'nyxt/mode/hint:hinting-type :class-name 'nyxt/mode/hint:hint-mode)
         " setting to configure one's favorite link hints style (Vimium-style
vs. body-matching prompts). Thanks to @heiwiper!")
    (:li "Hinting now highlights hints by dimming the matched characters (thanks to
@heiwiper!)")
    (:li "Session is restored on startup by default. Slot "
         (:code "session-restore-prompt") " has been replaced by "
         (:nxref :slot 'restore-session-on-startup-p :class-name 'browser)
         ", a boolean.")
    (:li (:nxref :class-name 'nyxt/mode/reduce-tracking:reduce-tracking-mode)
         " clears widely known tracking query parameters.")
    (:li "Add a " (:nxref :variable '%slot-value%) " value to "
         (:nxref :macro 'define-configuration) " to allow configuration to compose from different "
         (:nxref :macro 'define-configuration) " forms.")
    (:li (:nxref :command 'execute-command)
         " evaluates arbitrary Lisp code and provides inline documentation for symbols.")
    (:li "New prediction capabilities. Nyxt can now predict your next command, it will
show up automatically in the execute-command menu. Nyxt uses a stochastic model
to generate predictions for what your next command will be. The model is stored
locally and is cleared after every session.")
    (:li "Support for key files and Yubikey locking in KeePassXC password interface.")
    (:li "History globality can be set on a per-buffer basis. "
         "See the " (:nxref :slot 'global-history-p :class-name 'context-buffer) ".")
    (:li (:nxref :slot 'backtrack-to-hubs-p :class-name 'nyxt/mode/history:history-mode)
         " allows to revisit the \"hub\" URLs you often visit, instead of adding them to
history anew.")
    (:li "When started with " (:code "--remote") " and without " (:code "--quit")
         ", Nyxt now reads s-expression from standard input and sends it to the remote
process. This avoids the performance penalty of a new process startup on each
iteration.")
    (:li (:nxref :function 'define-configuration)
         " automatically resolves class names and slot names even without package
prefix. For instance " (:code ":style-mode -> nyxt/mode/style:style-mode") ".")
    (:li "Keybindings are also resolved to existing commands when necessary. For instance "
         (:code ":jump-to-heading -> nyxt/mode/document:jump-to-heading") ".")
    (:li (:nxref :slot 'after-init-hook :class-name 'browser) " and "
         (:nxref :slot 'after-startup-hook :class-name 'browser)
         " are browser slots, instead of the global variables they used to be.")
    (:li "Universal describe-* commands have been replaced with new sources for the
regular commands, such as "
         (:nxref :class-name 'function-non-nyxt-source) ", "
         (:nxref :class-name 'function-internal-source) " and so on.")
    (:li "The " (:nxref :class-name 'browser) " class has a "
         (:nxref :slot 'profile :class-name 'browser) " slot.")
    (:li "With " (:nxref :slot 'dynamic-attribute-width-p :class-name 'prompt-buffer)
         " on, prompt buffer attribute columns adjust to their content, allowing
for a better overview of lengthy attributes.")
    (:li "Search engines are now listed with their full name (when available).")
    (:li "Internal buffers/pages are now rendered in the status area as " (:code "internal") ".")))

  (:nsection
   :title "Prompt buffer improvements"
   (:ul
    (:li "New prompt buffer fuzzy matching algorithm, hopefully offering more relevant
  results (thanks to @kchanqvq!)")
    (:li "Prompt buffer mouse support can be disabled with the "
         (:nxref :slot 'mouse-support-p :class-name 'prompt-buffer)
         " slot (thanks to @efimerspan!)")
    (:li "Add " (:nxref :class-name 'prompt-buffer :slot 'height) " slot.")
    (:li "Add "
         (:nxref :command 'nyxt/mode/prompt-buffer:toggle-actions-on-current-suggestion-enabled)
         ", bound to " (:code "C-c C-j") "by default.")
    (:li (:code "return-selection-over-action") " renamed to "
         (:nxref :command 'nyxt/mode/prompt-buffer:run-action-on-return)
         ".  The default keybinding is the same.")
    (:li "Add " (:nxref :command 'nyxt:toggle-prompt-buffer-focus) ".")
    (:li "Add " (:nxref :command 'nyxt/mode/prompt-buffer:first-suggestion-within-source) ".")
    (:li "Add " (:nxref :command 'nyxt/mode/prompt-buffer:last-suggestion-within-source) ".")))

  (:nsection
   :title "New modes"
   (:ul
    (:li "New " (:nxref :mode 'nyxt/mode/record-input-field:record-input-field-mode)
         " to record and restore input fields.")
    (:li (:nxref :command 'nyxt/mode/repl:repl "Lisp REPL")
         " is fully redesigned into a multiple-pane environment with debugging, value
inspection, convenient editing bindings, and full extensibility via "
         (:nxref :class-name 'nyxt/mode/repl:cell) " and " (:nxref :function 'value->html) ".")
    (:li "New " (:nxref :class-name 'nyxt/mode/remembrance:remembrance-mode)
         " to automatically cache the visited page content. The cache can be looked up
and the page textual content can be displayed even offline. See "
         (:nxref :function 'nyxt/mode/remembrance:recollect-visited-page) ".")))

  (:nsection
   :title "New commands"
   (:ul
    (:li "New " (:nxref :command 'nyxt/mode/document:next-heading) " and "
         (:nxref :command 'nyxt/mode/document:previous-heading)
         " commands to jump between neighboring headings.")
    (:li "New " (:nxref :command 'toggle-message-buffer)  " and "
         (:nxref :command 'toggle-status-buffer) " commands. And "
         (:nxref :command 'toggle-maximize)
         "command for maximizing a window, relying on these two (thanks to @maggiplant!)")
    (:li "New " (:nxref :command 'nyxt/mode/repeat:repeat-key)
         " command repeating the provided key as many times as you like.")
    (:li "Slynk is a new dependency and SLY users can now connect to a running Nyxt
instance using the " (:code "start-slynk") " command (thanks to @jgart!)")))

  (:nsection
   :title "Refactors"
   (:ul
    (:li (:code "auto-mode")
         " is incorporated into Nyxt core, with its settings residing in "
         (:nxref :class-name 'modable-buffer) ".")
    (:ul
     (:li "The new "
          (:nxref :slot 'apply-all-matching-auto-rules-p :class-name 'modable-buffer)
          " allows you to apply all the matching auto-rules instead of the most
specific one.")
     (:li "There are default rules for Gopher, Gemini, and Nyxt-internal-pages.")
     (:li "The rules file is now moved to" (:code "auto-rules.lisp") " (instead of the old "
          (:code "auto-mode-rules.lisp") ")."))
    (:li "Major "
         (:nxref :class-name 'nyxt/mode/search-buffer:search-buffer-mode)
         " refactor. The new implementation is more accurate and more flexible, as it
allows manipulating matches as Lisp objects.")
    (:li "Major improvement of " (:nxref :class-name 'nyxt/mode/editor:editor-mode) ".")))

  (:nsection
    :title "Moves and removals"
    (:ul
     (:li "Move the " (:code "prompter") " library to a separate repository.")
     (:li "Some of the Nyxt-internal logic was abstracted into separate libraries/systems:")
     (:ul
      (:li (:nxref :macro 'define-class) " macro — to " (:code "nclasses") ".")
      (:li "Portable GUI-friendly debugger — to " (:code "ndebug") ".")
      (:li "JSON parsing — to " (:code "njson") ".")
      (:li "Symbol search and listing — to " (:code "nsymbols") "."))
     (:li "Move " (:code "lisp-system") " to separate repository as extension. The functions "
          (:code "list-system") " and " (:code "load-system") " are no longer available.")
     (:li "Move " (:code "os-package-manager-mode") " to separate repository as
extension.")
     (:li (:code "diff-mode") " has been removed.")))

  (:nsection
    :title "Bug fixes"
    (:ul
     (:li "Lisp code run with the --script or --eval command line arguments now defaults to
the " (:code "nyxt-user") " package.")
     (:li "Various " (:nxref :mode 'nyxt/mode/spell-check:spell-check-mode)
          " fixes. (Thanks to @hendursaga!) ")
     (:li "All copying and pasting commands populate "
          (:nxref :class-name 'browser :slot 'clipboard-ring) " reliably, thus fixing the "
          (:nxref :command 'nyxt/mode/document:paste-from-clipboard-ring) " command.")
     (:li "Fix touchscreen gestures for VI mode.")
     (:li "Fix processing via relative paths when opening files.")
     (:li "Fix buffer re-attachment from the deleted window.")
     (:li "Fix " (:nxref :command 'nyxt/mode/history:history-backwards)
          " by gracefully handling pages that are not yet done loading.")
     (:li "Fix full-screening event handling — status buffer no longer goes off-sync with
the full-screened page/video.")
     (:li "Startup is more robust against corrupted history files.")
     (:li "VI insert mode is triggered in more cases where it should be triggered.")
     (:li "Invoke the right WebKit command when cutting text with "
          (:nxref :function 'ffi-buffer-cut) ".")
     (:li "Fix the display of history suggestions when going forward in history.")
     (:li "Security: all the non-ASCII domain names are shown as IDN punycodes in addition
to aesthetic display in the status buffer.")
     (:li "The canceled page requests are stored to history, making it more consistent.")
     (:li "Extensions directory is more carefully checked to avoid crashes.")
     (:li "Trying to delete a hanged buffer destroys it, instead of leaving it dangling
forever.")
     (:li "Switching focus away from Nyxt doesn't make it unfullscreen anymore (thanks to
@shaunsingh!)")
     (:li "Fix " (:nxref :command 'nyxt/mode/document:select-frame-new-buffer) " and "
          (:nxref :command 'nyxt/mode/expedition:select-frame-expedition) ".")
     (:li "Numerous documentation and functionality fixes thanks to @shamazmazum, @hendursaga,
@Gavinok, @mianmoreno, @edgar-vincent, @K1D77A, @kchanqvq, @tiberious726,
@createyourpersonalaccount, @khinsen, @aaron-tan, @chrisboeg, @taiju,
@odanoburu, @wasamasa, @fabian-thomas, @shakatoday, @grawlinson,
@kenranunderscore, @Uthar, @e0a6, @comradekingu, @whirm, and others!")))

  (:nsection
    :title "Bindings"
    (:ul
     (:li (:nxref :class-name 'nyxt/mode/editor:editor-mode)
          " now has an equally powerful set of bindings in all key schemes, allowing one
to open a file, save it, switch buffer or delete current buffer.")
     (:li (:nxref :mode 'nyxt/mode/visual:visual-mode)
          " now has more VI bindings (thanks to @CorruptedVor!)")
     (:li (:nxref :command 'nyxt/mode/document:paste-from-clipboard-ring)
          " is now conveniently bound to " (:code "M-y") " in Emacs scheme of "
          (:nxref :class-name 'nyxt/mode/document:document-mode) ".")
     (:li "Add Emacs/VI text editing bindings in "
          (:nxref :mode 'nyxt/mode/prompt-buffer:prompt-buffer-mode)
          " and " (:nxref :command 'nyxt/mode/repl:repl) ".")
     (:li "Rebind " (:nxref :command 'nyxt/mode/history:history-forwards) " to "
          (:nxref :command 'nyxt/mode/history:history-forwards-maybe-query)
          " in the Emacs and VI schemes.")
     (:li "Rebind " (:nxref :command 'nyxt/mode/bookmark:bookmark-url) " and "
          (:nxref :command 'copy-title) " to be more consistent with other bindings.")

     (:li "Rebind " (:nxref :command 'nyxt/mode/download:list-downloads)"."
          "When using the CUA keyscheme, the keybinding was previously shadowed by "
          (:nxref :command 'nyxt/mode/hint:follow-hint) ".")
     (:li "Rebind " (:nxref :command 'nyxt/mode/document:jump-to-heading)" for the
CUA keyscheme.")))

  (:nsection
    :title "Renamings"
    (:ul
     (:li "The buffer hierarchy has been redesigned.  Now " (:nxref :class-name 'buffer)
          " is a minimal class and instantiating such a buffer is only useful if you need
a dummy buffer. " (:nxref :class-name 'web-buffer)
          " inherits from a mix of specialized buffer subclasses, such as "
          (:nxref :class-name 'modable-buffer) " and " (:nxref :class-name 'input-buffer)
          ".  For the full list, see the "
          (:nxref :class-name 'buffer) " class documentation and browse its subclasses.")
     (:li (:code "application-mode") " is now "
          (:nxref :mode 'nyxt/mode/passthrough:passthrough-mode) ".")
     (:li (:code "web-mode") " is no more.  Instead much of its features have been moved to the new "
          (:nxref :mode 'nyxt/mode/document:document-mode)
          ".  The buffer history management is now handled in a separate mode, "
          (:nxref :mode 'nyxt/mode/history:history-mode) "."
          (:code "bookmarklets") " have they own mode too, "
          (:nxref :mode 'nyxt/mode/bookmarklets:bookmarklets-mode) ".")
     (:li "Rename " (:code "keep-search-hints-p") "slot to "
          (:nxref :slot 'keep-search-marks-p :class-name 'document-buffer)".")
     (:li "Rename " (:code "remove-search-hints") "command to "
          (:nxref :command 'nyxt/mode/search-buffer:remove-search-marks) ".")
     (:li (:code "load-after-system") " and " (:code "nyxt-config-file")
          "have been replaced with " (:nxref :macro 'define-nyxt-user-system)
          " and " (:nxref :macro 'define-nyxt-user-system-and-load) ".")
     (:li "Rename " (:code "nyxt/mode/hint:box-style") " to "
          (:nxref :class-name 'nyxt/mode/hint:hint-mode :slot 'style) ".")
     (:li "Deprecate " (:code "nyxt/mode/hint:highlighted-box-style") " and merge into "
          (:nxref :class-name 'nyxt/mode/hint:hint-mode :slot 'style) ".")
     (:li "New " (:nxref :function 'describe-mode) " command (an alias for "
          (:nxref :function 'describe-class) ").")
     (:li (:nxref :function 'describe-command) " became an alias for "
          (:nxref :function 'describe-function) ".")
     (:li (:code "nyxt/mode/prompt-buffer:return-selection") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:run-action-on-return) ".")
     (:li (:code "nyxt/mode/prompt-buffer:cancel-input") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:quit-prompt-buffer) ".")
     (:li (:code "nyxt/mode/prompt-buffer:toggle-toggle-mark-backwards") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:toggle-mark-backwards) ".")
     (:li (:code "nyxt/mode/prompt-buffer:toggle-mark") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:toggle-mark-forwards) ".")
     (:li (:code "nyxt/mode/prompt-buffer:select-next-source") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:next-source) ".")
     (:li (:code "nyxt/mode/prompt-buffer:select-previous-source") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:previous-source) ".")
     (:li (:code "nyxt/mode/prompt-buffer:select-next-page") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:next-page) ".")
     (:li (:code "nyxt/mode/prompt-buffer:select-previous-page") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:previous-page) ".")
     (:li (:code "nyxt/mode/prompt-buffer:select-last") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:last-suggestion) ".")
     (:li (:code "nyxt/mode/prompt-buffer:select-first") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:first-suggestion) ".")
     (:li (:code "nyxt/mode/prompt-buffer:select-next") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:next-suggestion) ".")
     (:li (:code "nyxt/mode/prompt-buffer:select-previous") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:previous-suggestion) ".")
     (:li (:code "nyxt/mode/prompt-buffer:set-selection-action") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:set-action-on-return) ".")
     (:li (:code "nyxt/mode/prompt-buffer:run-selection-action") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:run-action-on-current-suggestion) ".")
     (:li (:code "nyxt/mode/prompt-buffer:toggle-selection-actions-enabled") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:toggle-actions-on-current-suggestion-enabled) ".")
     (:li (:code "nyxt/mode/prompt-buffer:insert-selection") " renamed to "
          (:nxref :command 'nyxt/mode/prompt-buffer:insert-current-suggestion) ".")))

  (:nsection
    :title "Programming interface"
    (:ul
     (:li "Internal pages are now using the " (:code "nyxt") " URL scheme.  They support the "
          (:code "lisp") " protocol to allow evaluating arbitrary Lisp, for instance from a button click."
          "Internal pages also have a URL now, which means they have history support.")
     (:li "New " (:nxref :macro 'define-internal-page-command) "and"
          (:nxref :macro 'define-internal-page-command-global) " helpers to define internal pages.")
     (:li (:nxref :macro 'define-panel-command) " and "
          (:nxref :macro 'define-panel-command-global)
          " helpers to define new panels.")
     (:li "New " (:nxref :macro 'define-internal-scheme) " helper to define custom schemes.")
     (:li "Nyxt-native debugger available via "
          (:nxref :command 'toggle-debug-on-error) ".")

     (:li "Better Lisp values inspection in " (:code "describe-*")
          " commands and " (:nxref :command 'nyxt/mode/repl:repl) ", extensible through "
          (:nxref :function 'value->html) " methods.")
     (:li "Universal" (:code "describe-*")
          " commands describing things in any Nyxt-accessible package."
          "Available via " (:code "C-h u") " key prefix.")
     (:li (:nxref :slot 'after-startup-hook :class-name 'browser)
          " to attach headless mode actions or configuration to.")
     (:li "Thread name is now mandatory in " (:nxref :macro 'run-thread) ".")
     (:li "New " (:code "nyxt-unstable") " " (:code "*features*")
          " when built from source on an untagged commit.  A feature with the commit is
also added.")
     (:li "New " (:nxref :function 'prompt1) " helper.")
     (:li "New " (:nxref :package :theme) " library.")
     (:li "Input processing is now easier to customize with "
          (:nxref :slot 'command-dispatcher :class-name 'window)
          " and " (:nxref :slot 'input-skip-dispatcher :class-name 'window) ".")
     (:li "Rename buffer slot " (:code "load-status") " to "
          (:nxref :slot 'status :class-name 'network-buffer) ".")
     (:li "The core " (:code "nyxt")
          " packages are now locked to prevent against accidental clobbering from the user
side.")
     (:li "New " (:nxref :function 'ffi-buffer-load-html)
          " and " (:nxref :function 'ffi-buffer-load-alternate-html)
          ". This is useful to set the buffer content without resorting to expensive
JavaScript calls.")
     (:li "Removed " (:code "clipboard-text") " since it's redundant with "
          (:nxref :function 'ffi-buffer-copy)".")
     (:li "General purpose helpers can be found in the "
          (:nxref :package :nyxt/utilities) " package.")
     (:li "New " (:code "nxref") " Spinneret tag for cross-referencing.")
     (:li (:nxref :macro 'if-confirm)
          " now allows configuring its yes/no options and can return booleans.")
     (:li "Move download hooks to " (:nxref :class-name 'nyxt/mode/download:download)
          " enabling proper typing and adding handlers to them.")
     (:li "Spinneret tags like " (:code ":nxref") ", " (:code ":nbutton") ", " (:code ":ninput") ", "
          (:code ":nselect") ", " (:code ":ncode") ", " (:code ":nsection") ", " (:code ":ntoc")
          " for better help pages markup enhanced by the compiler data.")
     (:li "All mode packages have been renamed to " (:code "nyxt/mode/mode-name") ".")
     (:li "Renderers are now first class objects, see the " (:nxref :class-name 'renderer)
          " class.  It's possible to change renderer from a same REPL session.")
     (:li (:nxref :command 'set-url) " and " (:nxref :command 'set-url-new-buffer) " accept the "
          (:code ":URL") " keyword argument and load it when provided.")
     (:li "New " (:nxref :function 'ffi-height) " and " (:nxref :function 'ffi-width)
          " methods to unify most of the height & width methods used before.")
     (:li "Generate methods instead of functions in " (:nxref :function 'define-parenscript)
          " and " (:nxref :function 'define-parenscript-async)
          " to ease hooking into those with, for example, " (:code ":around") " methods.")
     (:li "Allow the command argument to " (:nxref :function 'ffi-add-context-menu-command)
          " to be an arbitrary function.")
     (:li "New package nicknames:"
          (:ul
           (:li  (:code "time") " for " (:code "local-time"))
           (:li  (:code "types") " for " (:code "trivial-types"))
           (:li  (:code "sym") " for " (:code "nsymbols"))))
     (:li "The third value in the " (:nxref :function 'prompter:object-attributes)
          " attribute list is interpreted as display HTML for the suggestion. See the
color-picker support as an example application for this feature.")
     (:li "New " (:nxref :function 'match-port) " URL designator predicate for auto-rules."))))

(define-version "2.2.4"
  (:ul
   (:li "New " (:code "forward-to-renderer") " command.  When a key is bound to
   it, the last keypress is forwarded to the renderer.")
   (:li "New " (:code "reset-asdf-registries") " helper function.  "
        "Call it in your initialization file to re-enable the default ASDF
search paths, "
        "e.g. to find your Common Lisp libraries in ~/common-lisp or your Emacs
SLY install.")
   (:li "Prompt buffer attributes can now be computed asynchronously."
        "This is used for instance to speed up " (:code "switch-buffer") ".")
   (:li "Non-prefixed completion is now enabled for the default search engine."
        "You can turn off this behavior it with the "
        (:code "search-always-auto-complete-p") " slot option.")
   (:li "New " (:code "delete-command") " command.  (Thanks to @BlueFlo0d!)")
   (:li "More bindings and some minor improvements to " (:code "visual-mode") ".  (Thanks to @hendursaga!)"))

  (:h3 "Platform support")
  (:p "Nyxt is compiled with SBCL set to use a dynamic-space-size of at least 3GiB."
      "This should mitigate memory errors since Nyxt is very memory intensive."
      "You can override this by setting the " (:code "LISP_FLAGS") " variable as
      per the installation instructions.")

  (:h3 "Bug fixes")
  (:ul
   (:li "Fix the frequent dead locks on multi-buffer deletion.")
   (:li "Fix some rare dead locks on page load.")
   (:li "Fix crash on unhandled condition in a prompt buffer thread.")
   (:li "Fix iframe support on redirects (for instance with "
        (:code "blocker-mode") ").")
   (:li "Keymap fixes:"
        (:ul
         (:li "Prefix keymaps can no longer be shadowed.")
         (:li "Remove shadowed bindings from the binding listing.")
         (:li "Don't list shadowed parent keymap bindings.")))
   (:li "Fix " (:code "resume-prompt") " to hide the resumed prompt buffer when done.")
   (:li "Fix freeze due to errors in " (:code "blocker-mode") ".")
   (:li "Fix formatting of web process crash report.")
   (:li "Fix some " (:code "auto-mode") " issues.  (Thanks to @efimerspan!)")))

(define-version "2.2.3"
  (:ul
   (:li "Speed up most network queries.  This may also prevent some hangs that
used to happen when loading resource-intensive pages.")
   (:li "Speed up " (:code "set-url") " and friends.  (Thanks to @shamazmazum!)"
        (:br)
        "The user input suggestion should now be instantaneously updated in the
        suggestion list.  Search engine completions no longer hold back the
        listing of other suggestions.")
   (:li "Nyxt now prompts for confirmation before deleting a buffer that has
   edited contents.")
   (:li "New common text editing bindings (select-all, undo, redo, cut, etc.).")
   (:li "Display source code in " (:code "describe-function") " whenever possible.")
   (:li "Allow for arbitrary HTML in the prompt buffer (both attributes and the prompt).")
   (:li "Permission requests are now handled (such as geolocation access).")
   (:li "Intelligent Tracking Prevention is no longer systematically enabled. "
        "This should fix some website incompatibilities. "
        "ITP can be selectively enabled with " (:code "reduce-tracking-mode") ".")
   (:li (:code "reduce-tracking-mode") " has a new options, "
        (:code "preferred-user-agent") " which is set to a generic value by default.")
   (:li "The Lisp REPL now highlights the input, displays the input package and
       displays the current package at the prompt.")
   (:li "New " (:code "m l") " VI binding to " (:code "list-bookmarks") "."))

  (:h3 "Bug fixes")
  (:ul
   (:li "Fix crash and hangs with WebKitGTK 2.34."
        (:br) (:b "Warning: ") "Sandboxing is no longer enforced.")
   (:li "Fix hangs in some cases when " (:code "blocker-mode") " hostlists were
out-of-date.")
   (:li "Work around load failures when going backward or forward in history.")
   (:li "Catch more errors in search completion.")))

(define-version "2.2.2"
  (:ul
   (:li "HTTP redirects are no longer stored to history.")
   (:li "Selecting hints in prompt buffer no longer scrolls the page automatically, press "
        (:code "C-l") " instead."))
  (:h3 "Build and compilation changes")
  (:ul
   (:li "The source tarball now embeds the Git submodules and thus fixes build errors about missing dependencies."))
  (:h3 "Platform support")
  (:ul
   (:li "Fix type errors when building with some unusual compiler. (Thanks to @lpaviets!)"))
  (:h3 "Bug fixes")
  (:ul
   (:li "Fix prompt buffer paging.")
   (:li (:code "switch-buffer") " is no longer triggered when there is no buffer to restore.")
   (:li "On various occasions, KeepassXC entries could be missing from the prompt, this is now fixed.")
   (:li (:code "lisp-repl") " now prints all results properly.")
   (:li "Onion URLs are now supported.  (Thanks to @hendursaga!)")))

(define-version "2.2.1"
  (:ul
   (:li "Re-added the inferred-keyword source to the bookmark prompt buffer.")
   (:li "Numerous documentation fixes and updates.")
   (:li (:code "define-scheme") "syntax has been extended to allow importing
   other schemes.  See the manual for an example.")
   (:li "Arbitrary HTML is now allowed in mode formatting.")
   (:li "*Bookmarks* buffer is automatically updated when deleting entries.  (Thanks to @shamazmazum!)")
   (:li "Processes can now be stopped in process-mode.")
   (:li "New " (:code "repeat-times") " command.")
   (:li "List methods in class descriptions."))
  (:h3 "Build and compilation changes")
  (:ul
   (:li "Quicklisp is no longer used to fetch dependencies.  Instead, all Lisp
   dependencies are pinned and fetched via Git submodules into the "
        (:code "_build") " subdirectory.  This should improve reproducibility.
   The " (:code "NYXT_QUICKLISP") " environment variable has been replaced by "
        (:code "NYXT_SUBMODULES") "."))
  (:h3 "Platform support")
  (:ul
   (:li "Quicklisp can now be properly loaded when Nyxt was installed via the .deb file."))
  (:h3 "Bug fixes")
  (:ul
   (:li "Properly handle cancellation in yes/no prompt buffers.")
   (:li "Fix sandboxing.  (Thanks to @tiberious726!)")
   (:li "Fix toggle-mark in visual-mode.  (Thanks to @hendursaga!)")
   (:li "Report load-after-system warnings.  (Thanks to @hendursaga!)")
   (:li "Properly scroll into view when in visual-mode.  (Thanks to @aaron-tan!)")
   (:li "Fix upload of files with wildcard characters in their name.  (Thanks to @shamazmazum!)")))

(define-version "2.2.0"
  (:ul
   (:li "New " (:code "changelog") " command.")
   (:li "New " (:code "show-qrcode-of-current-url") " command.  (Thanks to @ag91!)")
   (:li "New " (:code "view-source") " command.")
   (:li "New " (:code "edit-user-file-with-external-editor") " command.")
   (:li "New " (:code "summarize-buffer") " command.")
   (:li "Macro editor: interactively record command macros with " (:code "edit-macro") "."
        " Macros can be saved to the " (:code "auto-config.lisp") " file.")
   (:li "New action for the " (:code "switch-buffer") " command to delete selected buffers.")
   (:li "Support for GTK clipboard.  (Thanks to @shamazmazum!)")
   (:li "Deprecated commands no longer show in the "
        (:code "execute-command") " list.")
   (:li "New " (:code "titler") " window slot to allow customizing the window title.") (:li (:code "jump-to-heading") " now also lists content keywords.")
   (:li "Various manual and tutorial improvements and fixes.  (Thanks to @pdelfino!)")
   (:li "Deprecate " (:code "paste-or-set-url") ".")
   (:li "Replace " (:code "copy-system-information") " with " (:code "show-system-information") ".")
   (:li (:p "Don't forward printable characters in vi-normal-mode.")
        (:p "Concretely, pressing an unbound letter won't insert it in an HTML input."))
   (:li (:p "New VI status and prompt buffer indicator.  (Thanks to @edgar-vincent!)")
        (:p "Both the status buffer and the prompt buffer now display a colored  indicator for "
            (:code "vi-normal-mode") " and " (:code "vi-insert-mode") "."))
   (:li "New navigation commands: "
        (:code "go-previous") ", "
        (:code "go-next") ", "
        (:code "go-up") ", "
        (:code "go-to-homepage") ".")
   (:li "Password commands set the default input to the current host.")
   (:li "Anonymous commands are now allowed in keybindings (e.g. with "
        (:code "make-command") ").")
   (:li (:code "describe-variable")
        " can now describe compound values recursively, such as lists of variables.")
   (:li (:code "execute-extended-command") " performs type checking and
        prefills default values."))

  (:h3 "Backward-incompatible changes")
  (:ul
   (:li (:p "Updated " (:code "cl-webkit") " to version 3.0.0.")
        (:p (:code "webkit2:webkit-web-view-evaluate-javascript")
            " lambda list changed."))
   (:li (:p "We've introduced an API-breaking change to the " (:code "nyxt/history-tree")
            " library: now all its call points take an owner as parameter."
            "This has allowed us to fix the constant history corruptions.")))
  (:h3 "Element hinting overhaul")
  (:p "It should be now faster, better integrated and more exhaustive (such as drop-down buttons).")
  (:p
   "A new DOM parser has been developed.  As a result, much of the
       former Parenscript code has been replaced by pure Lisp code.")
  (:h3 "New 'panel' buffers.")
  (:p "Panel buffers can be used to display arbitrary information on the
       side of a window.  A few new commands are thus introduced:")
  (:ul
   (:li (:code "show-bookmarks-panel"))
   (:li (:code "show-buffers-panel"))
   (:li (:code "delete-panel-buffer")))
  (:h3 "Prompt buffer")
  (:ul
   (:li "Input area no longer stutters when updating the display.")
   (:li "Key press responsiveness has been dramatically improved.")
   (:li "The prompt is properly refreshed when pasting to it.")
   (:li (:p "Bind " (:code "M-space") " to " (:code "resume-prompt") " to make it more useful.")
        (:p "It's particularly useful to resume a search."))
   (:li (:p "Bind " (:code "C-M-space") " to " (:code "execute-extended-command") ".")
        (:p "It makes it easier for users to execute commands and supply parameters.")))

  (:h3 "Status buffer")
  (:ul
   (:li "Clicking on modes now describes them.")
   (:li "New '+' button to toggles modes.")
   (:li "Arbitrary HTML is now allowed for mode glyphs."))
  (:h3 "Support for buffer tree relationships")
  (:ul
   (:li (:code "switch-buffer-previous") " and " (:code "switch-buffer-next")
        " browse the buffer tree in a depth-first fashion.")
   (:li "New " (:code "switch-buffer-last")
        " command to easily switch back and forth between the 2 most recent buffers.")
   (:li (:code "list-buffers") " can now display the the buffers as a tree."))
  (:h3 "Platform support")
  (:ul
   (:li "Fix slow buffer and prompt buffer creation on FreeBSD.  (Thanks to
        @shamazmazum!)"))
  (:h3 "Bug fixes")
  (:ul
   (:li "Various fixes with the password manager commands.")
   (:li "Don't crash when an error occurs in a GTK signal.")
   (:li "Start up errors due to flawed user init files now lead to a restart
    of Nyxt without init file.")
   (:li (:p "Fix endless compilation of =tutorial.lisp= when SBCL dynamic space
    size to low.")
        (:p (:code "cl-markup") " was replaced by " (:code "spinneret") "."))
   (:li "Fix potential dead-lock in " (:code "restore-history-buffers") ".")
   (:li "Fix " (:code "buffer-history-tree") " that showed a truncated tree.")))

(define-version "2.1.1"
  (:ul
   (:li (:code "reopen-buffer") " restores the scroll position.")
   (:li "New " (:code "copy-username") " command for password managers."))
  (:h3 "Bug fixes")
  (:ul
   (:li "Fix history double-restore which led to many crashes.")
   (:li "Create file and parent directories of configuration files if they
        don't exist.")
   (:li "Fix " (:code "set-url-from-bookmark") " with marks.")
   (:li "Fix " (:code "process-mode") " to not run an action when it is nil.")))

(define-version "2.1.0"
  (:ul
   (:li (:code "expedition-mode") ". Expedition mode stores a set of links
        that you can then traverse forwards and backwards as if you are on an
        expedition through the Internet!")
   (:li (:code "select-frame") ". Select frame allows the user to
        drag-select multiple elements on screen. For example, the user can
        drag-select to open up all of the links of a search result.")
   (:li (:code "process-mode") ". Process mode enables you to automate the
        repetition of tedious tasks. For example, you can have process mode
        refresh a page whenever you save a file. This could be useful for
        previewing LaTeX documents, or working on websites!")
   (:li (:code "cruise-control-mode") ".  Cruise control mode allows you to
        scroll down the page at a fixed speed. This mode is particularly well
        suited for reading long articles.")
   (:li "Replace 'loading' text with animated spinner.")
   (:li "vi mode automatically switches to vi-insert-mode when logical.")
   (:li "The prompt buffer is used to interface with the user instead of
        GTK dialogs. This behavior can be controlled via
        the " (:code "native-dialogs") (:code "browser") "slot.")
   (:li "add raw-source
        to " (:code "add-domain-to-certificate-exceptions") " to allow for
        arbitrary URLs."))
  (:h3 "Bug fixes")
  (:ul
   (:li "Fix broken status buffer button to list-buffers.")
   (:li "Make bookmarklets callable from " (:code "execute-command") ".")
   (:li "Quit gracefully when " (:code "C-c") " is pressed in the parent shell.")
   (:li "Fix search engine queries with " (:code "&") " and " (:code "%") " characters.")
   (:li "Make " (:code "search-buffer-source") " customizable
        with " (:code "define-configuration") ". Thanks to @mariari.")
   (:li "Support command-line arguments in " (:code "external-editor-program") ".")
   (:li "Fix " (:code "proxy-mode") " configuration. Thanks to @Zwo1in.")
   (:li "Fix history corruptions when an error occurred while saving the history to disk.")
   (:li "Fix " (:code "history-all-query") ". Thanks to @franburstall.")
   (:li "Fix " (:code "search-buffer") "when input contains regexp-special characters.")))


(define-version "2.0.0"
  (:ul
   (:li "Search engine autocompletion. See the search engine suggestions in
        the prompt buffer as you type. By default, support for DuckDuckGo and
        Wikipedia is included.")
   (:li (:code "set-url") " and related commands now allow for multiple selection.")
   (:li "On downloads, display the number of downloaded bytes.")
   (:li "Buffer default modes can now be customized via
        the " (:code "default-modes") " generic function. This function uses
        the " (:code "append") " method
        combination." (:code "define-configuration") " now supports customizing
        these methods just as for slots.")
   (:li "Commands defined with " (:code "define-command") " are no longer
        listed in " (:code "execute-command") "
        use " (:code "define-command-global") " to define global commands that
        are always listed.")
   (:li "New " (:code "define-command-global") "function. Use this to
        define globally visible commands.")
   (:li "Removed the " (:code "notify") " function, also removing the
        dependency on notify-osd. This function was not generic enough and only
        used in one place. Notifications will be reintroduced with a more generic
        interface.")
   (:li "Remove " (:code "set-url-from-bookmark-new-buffer") " It is
        redundant with set-url and set-url-from-bookmark.")
   (:li "New system-information command line option.")
   (:li "Removed Common Lisp file-attributes dependency."))
  (:h3 "Bug fixes")
  (:ul
   (:li "Fixed crash when right-clicking on some elements, like the Slack
        composition area.")
   (:li "Fixed crashes on click and scroll in prompt buffer.")
   (:li "Removed duplicates from prompt buffer history.")
   (:li "Fixed some commands, like " (:code "describe-variable") "
        and " (:code "history-tree."))
   (:li "Fixed " (:code "url-dispatching-handler") " when handler returns NIL.")
   (:li "Fixed loading of local HTML files when passed as command line argument.")))
