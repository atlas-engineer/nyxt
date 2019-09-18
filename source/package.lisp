;;; package.lisp --- Definition of packages used by Next.

(in-package :cl-user)

;; Some compilers (e.g. SBCL) fail to reload the system with `defpackage' when
;; exports are spread around.  `uiop:define-package' does not have this problem.
(uiop:define-package next
    (:use :common-lisp :trivia :annot.class)
  ;; TODO: Ideally, we'd like to define exports at definition site.  Since we
  ;; have packages, ASDF fails to "compile-bundle-op" Next which is required by
  ;; Guix.
  ;; See https://gitlab.common-lisp.net/asdf/asdf/issues/11.
  (:export
   :define-command
   ;; Macros
   :define-parenscript
   :with-result
   :with-result*
   ;; Base
   :entry-point
   :start
   ;; Global
   :*interface*
   :+version+
   ;; Buffer
   :reload-current-buffer
   :set-url
   :buffer-get-url
   :buffer-get-title
   ;; Window class
   :window
   :id
   :active-buffer
   :active-minibuffers
   :status-buffer
   :status-buffer-height
   :minibuffer-callbacks
   :minibuffer-closed-height
   :minibuffer-open-height
   :history-db-path
   :bookmark-db-path
   :search-engines
   :window-set-active-buffer-hook
   :window-delete-hook
   :proxy
   :server-address
   :whitelist
   :proxied-downloads-p
   ;; Buffer class
   :buffer
   :name
   :title
   :modes
   :default-modes
   :current-keymap-scheme
   :override-map
   :forward-input-events-p
   :last-key-chords
   :view
   :resource-query-function
   :callbacks
   :default-new-buffer-url
   :scroll-distance
   :horizontal-scroll-distance
   :current-zoom-ratio
   :zoom-ratio-step
   :zoom-ratio-min
   :zoom-ratio-max
   :zoom-ratio-default
   :cookies-path
   :box-style
   :load-hook
   :buffer-delete-hook
   ;; Remote-interface class
   :remote-interface
   :port
   :platform-port-poll-duration
   :platform-port-poll-interval
   :active-connection
   :dbus-pid
   :clipboard-ring
   :minibuffer-generic-history
   :minibuffer-search-history
   :minibuffer-set-url-history
   :windows
   :total-window-count
   :last-active-window
   :buffers
   :total-buffer-count
   :start-page-url
   :open-external-link-in-new-window-p
   :key-chord-stack
   :downloads
   :download-watcher
   :download-directory
   :startup-timestamp
   :after-init-hook
   :before-exit-hook
   :window-make-hook
   :buffer-make-hook
   :buffer-download-hook
   :after-download-hook
   ;; bookmarks
   :bookmark-current-page
   :bookmark-hint
   :set-url-from-bookmark
   ;; keymaps
   :define-key
   :make-keymap
   ;; minibuffer
   :echo
   :echo-dismiss
   :erase-input
   :get-candidate
   :minibuffer
   :minibuffer-mode
   :read-from-minibuffer
   :ring-insert-clipboard
   :update-display
   ;; Modes
   :define-mode
   :find-mode
   :find-buffer
   :root-mode
   ;; Zoom
   :%zoom-in-page
   :%zoom-out-page
   :%unzoom-page
   :zoom-in-page
   :zoom-out-page
   :unzoom-page
   ;; Hints
   :copy-hint-url
   :follow-hint
   :follow-hint-new-buffer
   :follow-hint-new-buffer-focus
   ;; Utilities
   :object-string
   :fuzzy-match
   :xdg-data-home
   :xdg-config-home
   :get-default
   :member-string
   :file-size-human-readable
   ;; History
   :history-typed-add
   ;; Jump heading
   :jump-to-heading
   :paren-jump-to-heading
   :get-headings
   ;; Remote
   :rpc-window-make
   :rpc-window-set-title
   :rpc-window-delete
   :rpc-window-active
   :rpc-window-exists
   :rpc-window-set-active-buffer
   :set-window-title
   :window-set-active-buffer
   :rpc-window-set-minibuffer-height
   :rpc-buffer-make
   :rpc-buffer-delete
   :rpc-buffer-load
   :rpc-buffer-evaluate-javascript
   :rpc-minibuffer-evaluate-javascript
   :rpc-generate-input-event
   :rpc-set-proxy
   :rpc-get-proxy
   :rpc-buffer-set
   :resource-query-default
   :set-active-buffer
   ;; Search
   :next-search-hint
   :remove-search-hints
   :previous-search-hint
   :search-buffer
   ;; Scroll
   :scroll-to-top
   :scroll-to-bottom
   :scroll-down
   :scroll-up
   :scroll-left
   :scroll-right
   :scroll-page-down
   :scroll-page-up))
