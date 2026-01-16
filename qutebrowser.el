;;; qutebrowser.el --- Qutebrowser integration with Emacs and EXWM     -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Lars Rustand.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Lars Rustand
;; URL: https://github.com/lrustand/qutebrowser.el
;; Version: 0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package adds enhanced support for Qutebrowser under EXWM,
;; including integration with the Emacs bookmark system, buffer and
;; history sources for Consult, a minor mode for Qutebrowser EXWM
;; buffers, a minor mode providing theme synchronization between Emacs
;; and Qutebrowser, and facilities for sending arbitrary commands to
;; Qutebrowser from Emacs using IPC.

;;; Change Log:

;;; Code:

(require 'sqlite)
(require 'exwm)
(require 'json)
(require 'jsonrpc)
(require 'color)
(require 'cl-lib)

;;;; Customizable variables

(defgroup qutebrowser nil
  "EXWM enhancements for Qutebrowser."
  :group 'external)

(defcustom qutebrowser-theme-export-face-mappings
  '((completion.fg . default)
    (completion.odd.bg . default)
    (completion.even.bg . default)
    (completion.category.fg . font-lock-function-name-face)
    (completion.category.bg . default)
    (completion.category.border.top . mode-line)
    (completion.category.border.bottom . mode-line)
    (completion.item.selected.fg . highlight)
    (completion.item.selected.bg . highlight)
    (completion.item.selected.border.top . highlight)
    (completion.item.selected.border.bottom . highlight)
    (completion.match.fg . dired-directory)
    (completion.scrollbar.fg . scroll-bar)
    (completion.scrollbar.bg . scroll-bar)
    (contextmenu.disabled.bg . default)
    (contextmenu.disabled.fg . shadow)
    (contextmenu.menu.bg . default)
    (contextmenu.menu.fg . default)
    (contextmenu.selected.bg . highlight)
    (contextmenu.selected.fg . highlight)
    (downloads.bar.bg . mode-line)
    (downloads.start.fg . success)
    (downloads.start.bg . success)
    (downloads.stop.fg . error)
    (downloads.stop.bg . error)
    (downloads.error.fg . error)
    (hints.fg . isearch)
    (hints.bg . isearch)
    (hints.match.fg . isearch-group-1)
    (keyhint.fg . default)
    (keyhint.suffix.fg . font-lock-constant-face)
    (keyhint.bg . highlight)
    (messages.error.fg . error)
    (messages.error.bg . error)
    (messages.error.border . error)
    (messages.warning.fg . warning)
    (messages.warning.bg . warning)
    (messages.warning.border . warning)
    (messages.info.fg . success)
    (messages.info.bg . success)
    (messages.info.border . success)
    (prompts.fg . minibuffer-prompt)
    (prompts.bg . highlight)
    (prompts.border . minibuffer-prompt)
    (prompts.selected.fg . success)
    (prompts.selected.bg . success)
    (statusbar.normal.fg . mode-line)
    (statusbar.normal.bg . default)
    (statusbar.insert.fg . dired-header)
    (statusbar.insert.bg . dired-header)
    (statusbar.passthrough.fg . mode-line)
    (statusbar.passthrough.bg . mode-line)
    (statusbar.private.fg . mode-line)
    (statusbar.private.bg . mode-line)
    (statusbar.command.fg . mode-line)
    (statusbar.command.bg . mode-line)
    (statusbar.command.private.fg . mode-line)
    (statusbar.command.private.bg . mode-line)
    (statusbar.caret.fg . region)
    (statusbar.caret.bg . region)
    (statusbar.caret.selection.fg . region)
    (statusbar.caret.selection.bg . region)
    (statusbar.progress.bg . mode-line)
    (statusbar.url.fg . success)
    (statusbar.url.error.fg . error)
    (statusbar.url.hover.fg . link-visited)
    (statusbar.url.success.http.fg . success)
    (statusbar.url.success.https.fg . success)
    (statusbar.url.warn.fg . warning)
    (tabs.bar.bg . tab-bar)
    (tabs.indicator.start . success)
    (tabs.indicator.stop . mode-line)
    (tabs.indicator.error . error)
    (tabs.odd.fg . tab-bar)
    (tabs.odd.bg . tab-bar)
    (tabs.even.fg . tab-bar)
    (tabs.even.bg . tab-bar)
    (tabs.pinned.even.bg . tab-bar)
    (tabs.pinned.even.fg . tab-bar)
    (tabs.pinned.odd.bg . tab-bar)
    (tabs.pinned.odd.fg . tab-bar)
    (tabs.pinned.selected.even.fg . tab-line)
    (tabs.pinned.selected.even.bg . tab-line)
    (tabs.pinned.selected.odd.fg . tab-line)
    (tabs.pinned.selected.odd.bg . tab-line)
    (tabs.selected.odd.fg . tab-line)
    (tabs.selected.odd.bg . tab-line)
    (tabs.selected.even.fg . tab-line)
    (tabs.selected.even.bg . tab-line))
  ;;(webpage.bg . default))
  "Mapping between Emacs faces and Qutebrowser color settings."
  :type '(alist :key-type symbol
                :value-type face)
  :group 'qutebrowser)

(defcustom qutebrowser-default-open-target 'auto
  "The default open target for Qutebrowser."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Tab" tab)
                 (const :tag "Window" window)
                 (const :tag "Private Window" private-window))
  :group 'qutebrowser)

(defcustom qutebrowser-command-backend 'qutebrowser-rpc-send-commands
  "The default backend to use when sending commands to Qutebrowser.
If the selected one fails to be initialized, the next one is tried. RPC
is the most featureful one, but also requires some backend Python code
to be installed in Qutebrowser."
  :type '(choice (const :tag "RPC" qutebrowser-rpc-send-commands)
                 (const :tag "IPC" qutebrowser-ipc-send)
                 (const :tag "FIFO" qutebrowser-fifo-send)
                 (const :tag "Commandline" qutebrowser-commandline-send)
                 (function :tag "Custom command"))
  :risky t
  :group 'qutebrowser)

(defcustom qutebrowser-config-directory
  "~/.config/qutebrowser/"
  "Path to the Qutebrowser config directory."
  :type 'file
  :risky t
  :group 'qutebrowser)

(defcustom qutebrowser-history-database
  "~/.local/share/qutebrowser/history.sqlite"
  "Path to the Qutebrowser history database."
  :type 'file
  :risky t
  :group 'qutebrowser)

(defcustom qutebrowser-history-exclusion-patterns
  '("https://www.google.%/search?%"
    "https://www.google.com/sorry/%"
    "https://scholar.google.com/scholar?%&q=%"
    "https://%youtube.com/results?%"
    "https://%perplexity.ai/search/%"
    "https://%/search?%"
    "https://%?search=%"
    "https://%/search/?%"
    "https://%/search_result?%"
    "https://www.finn.no/%/search.html?%"
    "https://www.finn.no/globalsearchlander?%"
    "https://%ebay.%/sch/%"
    "https://%amazon.%/s?%"
    "https://%duckduckgo.com/?%q=%")

  "URL patterns to exclude from the Qutebrowser history list.
The patterns are SQlite wildcard patterns, and will be used to build up
the WHERE clause of the database query.  For more details on how the
query is built, see `qutebrowser--history-search'."
  :type '(repeat string)
  :group 'qutebrowser)

(defcustom qutebrowser-title-display-length 97
  "Max display length of Qutebrowser titles in completion lists."
  :type 'integer
  :group 'qutebrowser)

(defcustom qutebrowser-url-display-length 50
  "Max display length of Qutebrowser URLs in completion lists."
  :type 'integer
  :group 'qutebrowser)

(defcustom qutebrowser-launcher-show-icons t
  "Show favicons of Qutebrowser buffers in launcher."
  :type 'boolean
  :group 'qutebrowser)

(defcustom qutebrowser-launcher-backend #'qutebrowser-completing-read-launcher
  "Backend function for `qutebrowser-launcher'."
  :type '(choice (const :tag "Built-in" qutebrowser-completing-read-launcher)
		 (const :tag "Consult" qutebrowser-consult-launcher)
		 (function :tag "Custom command"))
  :group 'qutebrowser)

(defcustom qutebrowser-history-order-by "last_atime DESC"
  "How to sort the history entries in the completion lists."
  :type '(choice
          (const :tag "Unsorted" nil)
          (const :tag "Recency" "last_atime DESC")
          (string :tag "Custom ORDER BY clause"))
  :risky t
  :group 'qutebrowser)

(defcustom qutebrowser-dynamic-results 100
  "The amount of dynamic results to show from history."
  :type 'integer
  :group 'qutebrowser)

(defcustom qutebrowser-dwim-send-commands nil
  "If non-nil, `qutebrowser-dwim' may send commands."
  :type 'boolean
  :group 'qutebrowser)

(defgroup qutebrowser-faces nil
  "Faces used by qutebrowser.el."
  :group 'qutebrowser
  :group 'faces)

(defgroup qutebrowser-hooks nil
  "Hooks for various Qutebrowser events.
All the hooks having a name like qutebrowser-on-SOME-SIGNAL-functions
are ran when the Qt signal SOME-SIGNAL is emitted in Qutebrowser.  The
functions are called with a plist containing any information related to
the signal that was emitted.  This plist usually contains X11-WIN-ID which
is an X11 window ID of the window that emitted the signal.

If the plist contains a X11-WIN-ID that can be resolved to an EXWM buffer,
the hooks are run with that buffer as `current-buffer'.

The hooks are automatically dispatched from
`qutebrowser-rpc--notification-dispatcher' based on the name of the
signal received."
  :group 'qutebrowser)

(defcustom qutebrowser-on-entered-mode-functions '()
  "Functions run when receiving a `entered-mode` signal.
The functions are run with one argument, a plist containing X11-WIN-ID and
MODE.  See also `qutebrowser-on-left-mode-functions'."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-left-mode-functions '()
  "Functions run when receiving a `left-mode` signal.
The functions are run with one argument, a plist containing X11-WIN-ID,
LEFT-MODE, and MODE.  Where LEFT-MODE is the mode that was left, and
MODE is the new mode after leaving the mode.

See also `qutebrowser-on-entered-mode-functions'."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-new-window-functions '()
  "Functions run when receiving a `new-window` signal.
The functions are run with one argument, a plist containing X11-WIN-ID"
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-url-changed-functions '()
  "Functions run when receiving a `url-changed` signal.
The functions are run with one argument, a plist containing X11-WIN-ID and
URL.  See also `qutebrowser-on-link-hovered-functions'."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-link-hovered-functions '()
  "Functions run when receiving a `link-hovered` signal.
The functions are run with one argument, a plist containing X11-WIN-ID and
HOVER.  See also `qutebrowser-on-url-changed-functions'."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-icon-changed-functions '()
  "Functions run when receiving a `icon-changed` signal.
The functions are run with one argument, a plist containing X11-WIN-ID and
ICON-FILE."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-got-search-functions '()
  "Functions run when receiving a `got-search` signal.
The functions are run with one argument, a plist containing X11-WIN-ID and
SEARCH."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-load-started-functions '()
  "Functions run when receiving a `load-started` signal.
The functions are run with one argument, a plist containing X11-WIN-ID."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-load-finished-functions '()
  "Functions run when receiving a `load-finished` signal.
The functions are run with one argument, a plist containing X11-WIN-ID."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-scroll-perc-changed-functions '()
  "Functions run when receiving a `scroll-perc-changed` signal.
The functions are run with one argument, a plist containing X11-WIN-ID,
X-SCROLL-PERC, and Y-SCROLL-PERC."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-on-recently-audible-changed-functions '()
  "Functions run when receiving a `recently-audible-changed` signal.
The functions are run with one argument, a plist containing X11-WIN-ID and
RECENTLY-AUDIBLE."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-update-window-info-functions
  '(qutebrowser-exwm--update-window-info)
  "Functions to run with updated information about windows.
These functions should not be considered as hooks for any kind of event,
and can be triggered both manually and automatically by various functions
to refresh the local copy of window information.

The functions are called with whatever new window information was
received, whether that is a full list of window properties, or just a
single property for a single window.  Any time a signal is received from
Qutebrowser, this hook is triggered in addition to the corresponding
qutebrowser-on-SIGNAL-functions hook.

The window information plist contains (one or more of) the following keys:

  - `:x11-win-id' is the X11 window ID of the window the informations is about.
  - `:win-id' is the internal window ID of the window the informations is about.
  - `:url' is the currently visited URL.
  - `:title' is the title of the window.
  - `:icon-file' is a temp-file containing the favicon.
  - `:search' is the active search term.
  - `:hover' is the URL of the currently hovered link.
  - `:private' is t if window is private.
  - `:mode' is the KeyMode of the window.
  - `:recently-audible' is t if the window is currently or was recently audible.
  - `:x-scroll-perc' is the scroll percentage in the x direction.
  - `:y-scroll-perc' is the scroll percentage in the y direction."
  :group 'qutebrowser-hooks
  :type 'hook)

(defcustom qutebrowser-theme-export-functions
  (list #'qutebrowser-theme-export--face-mappings)
  "Functions that insert content into the theme file."
  :group 'qutebrowser-hooks
  :type 'hook)

;;;; Variables

(defvar qutebrowser-process-names
  '("qutebrowser"
    ".qutebrowser-real" ;; Process name on Guix
    ".qutebrowser-re"   ;; Process name on Guix, mangled by Emacs
    "QtWebEngineProcess"
    "QtWebEngineProc") ;; Mangled by emacs
  "List of possible names of the Qutebrowser process.
This list is used to identify running Qutebrowser processes.")

(defvar qutebrowser-history-matching-pattern
  "(url || title) LIKE ('%%' || ? || '%%')"
  "SQL matching pattern used for each input word.")

(defvar qutebrowser--db-object nil
  "Contains a reference to the database connection.")

(defvar-local qutebrowser-exwm-win-id nil
  "Contains the internal Qutebrowser window ID.")

(defvar-local qutebrowser-exwm-keymode nil)

(defvar-local qutebrowser-exwm-hovered-url nil
  "Contains the URL of the link currently hovered in Qutebrowser.")

(defvar-local qutebrowser-exwm-current-url nil
  "Contains the current URL of Qutebrowser.")

(defvar-local qutebrowser-exwm-favicon nil
  "Contains the favicon for each Qutebrowser buffer.")

(defvar-local qutebrowser-exwm-current-search nil
  "Contains the current search terms of Qutebrowser.")

(defvar-local qutebrowser-exwm-recently-audible nil
  "Contains the recently audible status of Qutebrowser.")

(defvar-local qutebrowser-exwm-private nil
  "Contains the private status of Qutebrowser.")

(defvar-local qutebrowser-exwm-x-scroll-perc nil
  "Contains the current x scroll percentage of Qutebrowser.")

(defvar-local qutebrowser-exwm-y-scroll-perc nil
  "Contains the current y scroll percentage of Qutebrowser.")

(defvar qutebrowser-exwm-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in `qutebrowser-exwm-mode' buffers.")

(defvar qutebrowser-config-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'qutebrowser-config-source-file)
    (define-key map (kbd "C-c C-b") #'qutebrowser-config-source-buffer)
    (define-key map (kbd "C-c C-r") #'qutebrowser-config-source-region)
    map)
  "Keymap used in `qutebrowser-config-mode' buffers.")


(defconst qutebrowser--package-directory (file-name-directory (or load-file-name
                                                                  buffer-file-name)))
;;;; Hook functions

(defun qutebrowser-exwm--update-favicon (icon-file)
  "Update the favicon.
ICON-FILE is a temp-file containing the favicon.  Any previous ICON-FILE
will be deleted."
  (if (and (file-regular-p icon-file)
           ;; Not empty
           (> (nth 7 (file-attributes icon-file)) 0))
      (when-let* ((image (create-image icon-file nil nil :height 16 :width 16 :ascent 'center)))
        (let ((old-icon-file (image-property qutebrowser-exwm-favicon :file)))
          (setq-local qutebrowser-exwm-favicon image)
          (when old-icon-file
            (delete-file old-icon-file))))
    ;; Delete invalid/empty icon files
    (delete-file icon-file)))

(defun qutebrowser-exwm--delete-favicon-tempfile ()
  "Deletes the tempfile associated with the favicon of current buffer."
  (when-let* ((icon-file (image-property qutebrowser-exwm-favicon :file)))
    (delete-file icon-file)))

(add-hook 'kill-buffer-hook #'qutebrowser-exwm--delete-favicon-tempfile)

(defmacro qutebrowser--with-plist-key (key plist &rest body)
  "Execute BODY if KEY exists in PLIST, with KEY's value bound.
KEY should be the name of a plist key without the colon.
PLIST is the property list to check.
BODY is one or more forms to execute if KEY is found in PLIST."
  (declare (indent defun))
  (let ((key-keyword (intern (concat ":" (symbol-name key)))))
    `(when (plist-member ,plist ,key-keyword)
       (let ((,key (plist-get ,plist ,key-keyword)))
         ,@body))))

(defmacro qutebrowser--with-plist (plist &rest clauses)
  "Execute forms based on the presence of keys in PLIST.
PLIST is the property list to check against.
CLAUSES are of the form (KEY BODY...), where KEY is a symbol
and BODY is one or more forms to execute if KEY is in PLIST."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (clause)
                 (let ((key (car clause))
                       (body (cdr clause)))
                   `(qutebrowser--with-plist-key ,key ,plist
                      ,@body)))
               clauses)))

(defun qutebrowser-exwm--update-window-info (window-info)
  "Update buffer-local variables from WINDOW-INFO."
  (when-let* ((x11-win-id (plist-get window-info :x11-win-id))
              (buffer (exwm--id->buffer x11-win-id)))
    (with-current-buffer buffer
      (qutebrowser--with-plist window-info
        (win-id (setq-local qutebrowser-exwm-win-id win-id))
        (mode (setq-local qutebrowser-exwm-keymode mode))
        (icon-file (qutebrowser-exwm--update-favicon icon-file))
        (search (setq-local qutebrowser-exwm-current-search search))
        (hover (when (string= hover "") (setq hover nil))
               (setq-local qutebrowser-exwm-hovered-url hover))
        (url (setq-local qutebrowser-exwm-current-url
                         (unless (string-empty-p url) url))
             (setq-local buffer-file-name
                         (when (string-prefix-p "file://" url)
                           (string-replace "file://" "" url))))
        (x-scroll-perc (setq-local qutebrowser-exwm-x-scroll-perc x-scroll-perc))
        (y-scroll-perc (setq-local qutebrowser-exwm-y-scroll-perc y-scroll-perc))
        (private (setq-local qutebrowser-exwm-private private))
        (recently-audible (setq-local qutebrowser-exwm-recently-audible recently-audible))))))


;;;; History database functions

(defun qutebrowser--get-db ()
  "Return the open database, or open it."
  (unless (sqlitep qutebrowser--db-object)
    (setq qutebrowser--db-object (sqlite-open qutebrowser-history-database)))
  qutebrowser--db-object)

(defun qutebrowser--history-search (&optional string)
  "Search the sqlite database for entries matching WORDS.
Return up to LIMIT results."
  (let* ((db (qutebrowser--get-db))
         (input (or string ""))
         (words (or (string-split input) '("")))
         (inclusion (string-join (make-list (length words)
                                            qutebrowser-history-matching-pattern)
                                 " AND "))
         (exclusion (mapconcat (apply-partially 'format " url LIKE '%s'")
                               qutebrowser-history-exclusion-patterns " OR "))
         (limit (if qutebrowser-dynamic-results
                    (format "LIMIT %d" qutebrowser-dynamic-results)
                  ""))
         (query (format "SELECT url,substr(title,0,%d)
                         FROM CompletionHistory
                         WHERE %s AND NOT (%s)
                         ORDER BY %s
                         %s"
                        qutebrowser-title-display-length
                        inclusion
                        exclusion
                        qutebrowser-history-order-by
                        limit))
         (rows (sqlite-select db query words))
         (matches (length rows)))
    (setq qutebrowser-heading-history--with-count
          (format qutebrowser-heading-history matches))
    ;; Return list of URLs propertized with title
    (mapcar (lambda (row)
              (let* ((url (car row))
                     (title (cadr row)))
                (propertize (qutebrowser--shorten-display-url url)
                            :qutebrowser-candidate-type 'url
                            :qutebrowser-title title)))
            rows)))

;;;; Utility functions

(defun qutebrowser--target-to-flag (target)
  "Return the :open flag corresponding to TARGET."
  (pcase target
    ('window "-w")
    ('tab "-t")
    ('private-window "-p")
    ('auto "")))

(defun qutebrowser-exwm-find-buffer (url)
  "Find the buffer showing URL."
  (seq-find (lambda (buffer)
              (string= url (qutebrowser-exwm-buffer-url buffer)))
            (qutebrowser-exwm-buffer-list)))

(defun qutebrowser-exwm--win-id->buffer (win-id)
  (seq-find (lambda (buffer)
              (eq win-id (buffer-local-value 'qutebrowser-exwm-win-id buffer)))
            (qutebrowser-exwm-buffer-list)))

(defun qutebrowser-exwm-p (&optional buffer)
  "Return t if BUFFER is a Qutebrowser EXWM buffer."
  (with-current-buffer (or buffer (current-buffer))
    (string-equal "qutebrowser"
                  exwm-class-name)))

(defun qutebrowser-exwm-audible-p (&optional buffer)
  "Return t if BUFFER is an audible Qutebrowser EXWM buffer."
  (eq t
      (buffer-local-value 'qutebrowser-exwm-recently-audible (or (get-buffer buffer) (current-buffer)))))

(defun qutebrowser-exwm-private-p (&optional buffer)
  "Return t if BUFFER is an audible Qutebrowser EXWM buffer."
  (eq t
      (buffer-local-value 'qutebrowser-exwm-private (or (get-buffer buffer) (current-buffer)))))

(defun qutebrowser-exwm-buffer-url (&optional buffer)
  "Return the URL of BUFFER or the current buffer."
  (buffer-local-value 'qutebrowser-exwm-current-url (or buffer (current-buffer))))

(defun qutebrowser-exwm-buffer-list ()
  "Return a list of all Qutebrowser buffers."
  (seq-filter #'qutebrowser-exwm-p (buffer-list)))

(defun qutebrowser--shorten-display-string (string max-length)
  "Shorten STRING by making the end invisible."
  (let ((string (substring string)) ; Copy it to avoid modifying the original
        (string-length (length string)))
    (when (> string-length max-length)
      (put-text-property max-length string-length 'invisible t string))
    string))

(defun qutebrowser--shorten-display-url (url)
  "Shorten URL by making the end invisible."
  (qutebrowser--shorten-display-string url qutebrowser-url-display-length))

(defun qutebrowser--shorten-display-title (title)
  "Shorten TITLE by making the end invisible."
  (qutebrowser--shorten-display-string title qutebrowser-title-display-length))

;;;; Bookmark functions

(defun qutebrowser-bookmark-make-record ()
  "Make a bookmark record for Qutebrowser buffers."
  `(,(buffer-name)
    (handler . qutebrowser-bookmark-jump)
    (url . ,(qutebrowser-exwm-buffer-url))))

(defun qutebrowser-bookmark-url (bookmark)
  "Return the URL that BOOKMARK is pointing to."
  (bookmark-prop-get bookmark 'url))

(defun qutebrowser-bookmark-jump (bookmark)
  "Jump to a Qutebrowser BOOKMARK."
  (let ((url (qutebrowser-bookmark-url bookmark)))
    (qutebrowser-open-url url)))

(defun qutebrowser-bookmark-p (bookmark)
  "Return t if BOOKMARK is a Qutebrowser bookmark."
  (eq 'qutebrowser-bookmark-jump
      (bookmark-get-handler bookmark)))

(defun qutebrowser-bookmarks-list ()
  "Return a list of Qutebrowser bookmarks."
  (seq-filter #'qutebrowser-bookmark-p
              (bookmark-all-names)))

;;;; Dynamic consult source

(defun qutebrowser--filter-list (input list &rest field-getters)
  "Generalized list filtering function.
INPUT will be split into a list of words to search for.
LIST is the list to be filtered.
FIELD-GETTERS is a list of functions for getting the fields from each
that should be matched against INPUT. The function is called with the
element to filter.

The elements in LIST are filtered to contain only elements that match
all the words in INPUT in any of the fields retrieved by FIELD-GETTERS."
  (let ((case-fold-search t)
        (words (string-split (or input ""))))
    (seq-filter
     (lambda (elem)
       ;; All search words matching
       (seq-every-p
        (lambda (word)
          ;; At least one field matches each word
          (seq-some
           (lambda (field)
             (cl-search word (funcall field elem) :test #'char-equal))
           field-getters))
        words))
     list)))

(defun qutebrowser-bookmark-search (&optional input)
  "Return a propertized list of Qutebrowser bookmarks matching INPUT."
  (let* ((bookmarks (qutebrowser-bookmarks-list))
         (matching-bookmarks
          (qutebrowser--filter-list input
                                    bookmarks
                                    #'identity
                                    #'qutebrowser-bookmark-url))
         (matches (length matching-bookmarks)))
    (setq qutebrowser-heading-bookmark--with-count
          (format qutebrowser-heading-bookmark matches))
    (mapcar (lambda (bookmark)
              (let* ((url (qutebrowser-bookmark-url bookmark)))
                (propertize (qutebrowser--shorten-display-url url)
                            :qutebrowser-candidate-type 'bookmark
                            :qutebrowser-title bookmark)))
            matching-bookmarks)))

(defun qutebrowser-exwm-buffer-search (&optional input predicate)
  "Return a propertized list of Qutebrowser buffers matching INPUT and PREDICATE."
  (let* ((buffers (qutebrowser-exwm-buffer-list))
         (input-matching-buffers
           (qutebrowser--filter-list input
                                     buffers
                                     #'buffer-name
                                     #'qutebrowser-exwm-buffer-url))
         (matching-buffers (if predicate
                               (seq-filter predicate input-matching-buffers)
                             input-matching-buffers))
         (matches (length matching-buffers)))
    (setq qutebrowser-heading-buffer--with-count
          (format qutebrowser-heading-buffer matches))
    (mapcar (lambda (buffer)
              (let* ((title (substring-no-properties (buffer-name buffer)))
                     (url (or (qutebrowser-exwm-buffer-url buffer) " "))
                     (win-id (buffer-local-value 'qutebrowser-exwm-win-id buffer))
                     (icon (buffer-local-value 'qutebrowser-exwm-favicon buffer))

                     (icon-string (propertize "" 'display icon)))
                (propertize (qutebrowser--shorten-display-url url)
			    :qutebrowser-candidate-type 'buffer
			    :qutebrowser-buffer buffer
                            :qutebrowser-title (if qutebrowser-launcher-show-icons
						   (concat icon-string " " title)
						 title))))
            matching-buffers)))


(defun qutebrowser-command-search (&optional input)
  "Return a propertized list of Qutebrowser commands matching INPUT."
  (when (string-prefix-p ":" input)
    (let* ((all-commands (seq-into (qutebrowser-rpc-request :list-commands) 'list))
           (matching-commands
            (qutebrowser--filter-list input
                                      all-commands
                                      (lambda (cmd)
                                        (concat ":" (plist-get cmd :command)))
                                      (lambda (cmd)
                                        (car (string-lines (plist-get cmd :description)))))))
      (setq qutebrowser-heading-command--with-count
            (format qutebrowser-heading-command (length matching-commands)))
      (mapcar
       (lambda (cmd)
         (let ((name (concat ":" (plist-get cmd :command)))
               (desc (car (string-lines (plist-get cmd :description)))))
           (propertize (qutebrowser--shorten-display-url name)
                       :qutebrowser-candidate-type 'command
                       :qutebrowser-title desc)))
       matching-commands))))

(defun qutebrowser--highlight-matches (words str)
  "Highlight all occurrences of words in WORDS in STR."
  (dolist (word words)
    (let ((pos 0)
          (case-fold-search t))
      (while-let ((start (cl-search word str :start2 pos :test #'char-equal))
                  (end (+ start (length word))))
        (setq pos end)
        (put-text-property start end 'face 'match str)))))

(defun qutebrowser-annotate (entry)
  "Return annotation for ENTRY.

ENTRY can be a bookmark, a buffer, a history item, or a command.  ENTRY
should be a string containing a URL/command, and it should be
propertized with `title'.

ENTRY will be modified to highlight any words contained in
`qutebrowser-launcher--current-input', and the end of the string will be
hidden by setting the `invisible' property.

If PAD is non-nil, add padding to the annotation if ENTRY is shorter
than `qutebrowser-url-display-length'."
  (let* ((title (get-text-property 0 :qutebrowser-title entry))
         (annotation (propertize title 'face 'completions-annotations))
         ;; When used in qutebrowser-completing-read-launcher, we need
         ;; to pad the annotations for alignment. This is not needed
         ;; when the annotations are used in qutebrowser-consult-launcher.
         (pad-length (max 0 (- qutebrowser-url-display-length
                               (length entry))))
         (padding (make-string pad-length ?\ )))
    (concat padding " "  (qutebrowser--shorten-display-title title))))

(defvar qutebrowser-heading-buffer "Buffer (%s)")
(defvar qutebrowser-heading-buffer--with-count nil)
(defvar qutebrowser-heading-bookmark "Bookmark (%s)")
(defvar qutebrowser-heading-bookmark--with-count nil)
(defvar qutebrowser-heading-history "History (%s)")
(defvar qutebrowser-heading-history--with-count nil)
(defvar qutebrowser-heading-command "Command (%s)")
(defvar qutebrowser-heading-command--with-count nil)

(defun qutebrowser-launcher--group-entries (entry transform)
  (if transform
      entry
    (pcase (get-text-property 0 :qutebrowser-candidate-type entry)
      ('buffer qutebrowser-heading-buffer--with-count)
      ('bookmark qutebrowser-heading-bookmark--with-count)
      ('command qutebrowser-heading-command--with-count)
      (_ qutebrowser-heading-history--with-count))))


(defalias 'qutebrowser--completion-table
  (let ((current-candidates nil))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata . ((category . url)
                        (display-sort-function . identity)
                        (annotation-function . qutebrowser-annotate)
                        (group-function . qutebrowser-launcher--group-entries)))
        ;; Only compute the results once per input.  The 'boundaries
        ;; action seems to be the only one that is passed the input, so we
        ;; save the results here, and return them for all other actions.
        (if (listp action)
            (setq current-candidates
                  (append
                   (qutebrowser-command-search string)
                   (qutebrowser-exwm-buffer-search string)
                   (qutebrowser-bookmark-search string)
                   (qutebrowser--history-search string))))
        current-candidates))))

;;;###autoload
(defun qutebrowser-delete-from-history (url)
  (let ((query "DELETE FROM %s WHERE url=?;"))
    (dolist (table '("CompletionHistory" "History"))
      (sqlite-execute qutebrowser--db-object (format query table) (list url)))))

(defun qutebrowser--candidate-type (item)
  "Return completion candidate type of ITEM."
  (get-text-property 0 :qutebrowser-candidate-type (or item "")))

(defun qutebrowser--candidate-buffer (item)
  "Return buffer referenced by ITEM."
  (get-text-property 0 :qutebrowser-buffer (or item "")))

(defun qutebrowser--switch-to-selected-buffer (item)
  (switch-to-buffer (get-text-property 0 :qutebrowser-buffer (or item ""))))

(defun qutebrowser-completing-read-launcher (&optional initial default target)
  "Backend for `qutebrowser-launcher' based on `completing-read'."
  (let* ((prompt (if default
		     (format "Select (default %s): " default)
                   "Select: "))
	 (minibuffer-allow-text-properties t)
	 (selection (completing-read prompt
                                     #'qutebrowser--completion-table
                                     nil       ; predicate
                                     nil       ; require-match
                                     initial   ; initial-input
                                     nil       ; history
                                     default)) ; default
         (type (qutebrowser--candidate-type selection)))
    (pcase type
      ('buffer (qutebrowser--switch-to-selected-buffer selection))
      ('command (qutebrowser-send-commands selection))
      (_ (qutebrowser-open-url selection target)))))

;;;; Launcher functions

;;;###autoload
(defun qutebrowser (thing &optional target)
  "Do THING in Qutebrowser.

THING can be one of: a URL, a Qutebrowser command with colon prefix,
or any string which will be searched with the default search engine.

TARGET is where to do the thing, and can be one of: 'auto, 'tab,
'window, 'private-window. If not specified, defaults to
`qutebrowser-default-open-target'. When THING is a buffer or a command,
TARGET is ignored.

If called interactively, prompts for input with dynamic completion from
Qutebrowser history, open Qutebrowser buffers, Qutebrowser bookmarks,
and, if input starts with a colon, known Qutebrowser commands.

With one universal argument, set TARGET to 'tab.
With two universal arguments, set TARGET to 'private-window."
  (interactive (list (let ((default
                            (or (and (region-active-p)
                                     (filter-buffer-substring
                                      (region-beginning)
                                      (region-end)))
                                (thing-at-point 'url t)
                                (thing-at-point 'symbol t)
                                (thing-at-point 'word t))))
                       (qutebrowser-launcher nil default))
                     (pcase current-prefix-arg
                       ('(4) 'tab)
                       ('(16) 'private-window)
                       (_ nil))))
  (unless (interactive-p)
    (if (string-prefix-p ":" thing)
	(qutebrowser-send-commands thing)
      (qutebrowser-open-url thing target))))

;;;###autoload
(defun qutebrowser-dwim (thing &optional target)
  (interactive (list (or (and (region-active-p)
                              (filter-buffer-substring
                               (region-beginning)
                               (region-end)))
                         (thing-at-point 'url t)
                         (thing-at-point 'symbol t)
                         (thing-at-point 'word t))
                     (pcase current-prefix-arg
                       ('(4) 'tab)
                       ('(16) 'private-window)
                       (_ nil))))
  (if (and (interactive-p) (not thing))
      (qutebrowser-launcher nil nil target)
    (if (and qutebrowser-dwim-send-commands
	     (string-prefix-p ":" thing))
	(qutebrowser-send-commands thing)
      (qutebrowser-open-url thing target))))

;;;###autoload
(defun qutebrowser-launcher (&optional initial default target)
  "Select a URL to open in Qutebrowser.
Set initial completion input to INITIAL.  Open the URL in TARGET or the
default target if nil."
  (interactive)
  (funcall qutebrowser-launcher-backend initial default target))

;;;###autoload
(defun qutebrowser-launcher-tab (&optional initial default)
  "Select a URL to open in a new tab.
Set initial completion input to INITIAL."
  (interactive)
  (qutebrowser-launcher initial default 'tab))

;;;###autoload
(defun qutebrowser-launcher-window (&optional initial default)
  "Select a URL to open in a new window.
Set initial completion input to INITIAL."
  (interactive)
  (qutebrowser-launcher initial default 'window))

;;;###autoload
(defun qutebrowser-launcher-private (&optional initial default)
  "Select a URL to open in a private window.
Set initial completion input to INITIAL."
  (interactive)
  (qutebrowser-launcher initial default 'private-window))

;;;; Advice

;; Prevent Prescient history from being clogged up by web pages.
(defun qutebrowser--advice-vertico-prescient (orig-fun &rest args)
  "Exclude Qutebrowser buffer names and URLs from prescient history.
The ORIG-FUN takes ARGS."
  (let* ((selected-candidate (minibuffer-contents-no-properties))
         (selected-buffer (get-buffer selected-candidate)))
    (unless (or (qutebrowser-exwm-p selected-buffer)
                (string-match-p "^https?://" selected-candidate))
      (apply orig-fun args))))

(with-eval-after-load 'vertico-prescient
  (advice-add 'vertico-prescient--remember-minibuffer-contents :around
              #'qutebrowser--advice-vertico-prescient))

;;;; RPC functions

(defun qutebrowser-rpc--bootstrap-server ()
  "Bootstrap the RPC server and hooks by sourcing the config files."
  (let ((rpc (expand-file-name "emacs_rpc.py"
                               qutebrowser-config-directory))
        (hooks (expand-file-name "emacs_hooks.py"
                                 qutebrowser-config-directory)))
    (if (and (file-regular-p rpc)
             (file-regular-p hooks))
        ;; TODO: Detect when it is necessary to do this
        (let ((qutebrowser-command-backend 'qutebrowser-ipc-send)) ;; Don't bootstrap RPC over RPC...
          (qutebrowser-config-source rpc)
          (qutebrowser-config-source hooks))
      (message "RPC Python backend not found. Did you install it? Tip: run `qutebrowser-rpc-ensure-installed'."))))

(defvar qutebrowser-rpc--socket-path "/tmp/emacs-rpc"
  "Path to the RPC socket.")

(defun qutebrowser-rpc--make-network-process ()
  "Make a network process connected to the RPC socket."
  (if (qutebrowser-is-running-p)
      (progn
        (unless (file-exists-p qutebrowser-rpc--socket-path)
          (qutebrowser-rpc--bootstrap-server)
          (sit-for 1))
        (when (file-exists-p qutebrowser-rpc--socket-path)
          (make-network-process
           :name "qutebrowser-rpc"
           :family 'local
           :service qutebrowser-rpc--socket-path
           :noquery t
           :sentinel (lambda (proc event)
                       (when (string= event "connection broken by remote peer\n")
                         (delete-process proc))))))
    ;; If Qutebrowser isn't running, remove any lingering RPC socket
    (when (file-exists-p qutebrowser-rpc--socket-path)
      (delete-file qutebrowser-rpc--socket-path))))

(defvar qutebrowser-rpc--connection nil
  "The jsonrpc-connection to the Qutebrowser RPC backend.")
(defvar qutebrowser-rpc-should-reconnect t
  "If non-nil, try to reconnect to RPC every 10 seconds if connction is lost.")
(defvar qutebrowser-rpc--reconnect-timer nil)

(defun qutebrowser-rpc-maybe-reconnect (&rest _)
  (when (timerp qutebrowser-rpc--reconnect-timer)
    (cancel-timer qutebrowser-rpc--reconnect-timer))
  (when qutebrowser-rpc-should-reconnect
    (setq qutebrowser-rpc--reconnect-timer
          (run-with-timer 1 10 #'qutebrowser-rpc-connect))))

(defun qutebrowser-rpc-connect (&optional flush)
  "Connect to RPC.
If FLUSH is non-nil, delete any existing connection before reconnecting."
  (interactive "p")
  (let ((process (get-process "qutebrowser-rpc")))
    (when (and flush process)
      (delete-process process)
      (setq process nil))
    (unless (qutebrowser-rpc-connected-p)
      (condition-case err
          (when-let* ((proc (qutebrowser-rpc--make-network-process)))
            (setq qutebrowser-rpc--connection
                  (qutebrowser-jsonrpc-process-connection
                   :name "qutebrowser-jsonrpc"
                   :process proc
                   :notification-dispatcher #'qutebrowser-rpc--notification-dispatcher
                   :request-dispatcher #'qutebrowser-rpc--request-dispatcher
                   :on-shutdown #'qutebrowser-rpc-maybe-reconnect))
            (qutebrowser-rpc-request-window-info)
            (qutebrowser-populate-commands)
            (qutebrowser-populate-rpcmethods)
            (when (timerp qutebrowser-rpc--reconnect-timer)
              (cancel-timer qutebrowser-rpc--reconnect-timer)))
        (file-error
         (message "Error connecting to Qutebrowser RPC socket: %s" (error-message-string err)))
        (error
         (message "Unexpected error when connecting jsonrpc: %s" (error-message-string err)))))))

(defun qutebrowser-rpc-get-connection ()
  "Return a `jsonrpc-connection' to the RPC socket."
    (unless (qutebrowser-rpc-connected-p)
      (qutebrowser-rpc-connect))
    qutebrowser-rpc--connection)

(defclass qutebrowser-jsonrpc-process-connection (jsonrpc-process-connection)
  nil)

(cl-defmethod initialize-instance ((conn qutebrowser-jsonrpc-process-connection) slots)
  (cl-call-next-method))

(cl-defmethod jsonrpc-connection-send
  ((connection qutebrowser-jsonrpc-process-connection) &rest args
   &key _id method _params _result _error _partial)
  "Send MESSAGE, a JSON object, to CONNECTION."
  (when method
    (plist-put args :method
               (cond ((keywordp method) (substring (symbol-name method) 1))
                     ((and method (symbolp method)) (symbol-name method))
                     ((stringp method) method))))
  (let* ((message `(:jsonrpc "2.0" ,@args))
         (json (jsonrpc--json-encode message)))
    ;; Qutebrowser reads until newline.
    ;; Need to add one to avoid hanging the process.
    (process-send-string (jsonrpc--process connection) (concat json "\n"))))

(defun qutebrowser-rpc-connected-p ()
  "Check if connected to the Qutebrowser RPC."
  (and (qutebrowser-jsonrpc-process-connection-p qutebrowser-rpc--connection)
       (jsonrpc-running-p qutebrowser-rpc--connection)))

(defun qutebrowser-rpc-ensure-installed ()
  "Ensure that the Python backend files for RPC and hooks are installed.
To make sure that these files are updated whenever the package is
updated it is recommended to run this function when loading the package."
  (interactive)
  (dolist (file '("emacs_rpc.py"
                  "emacs_hooks.py"))
    (copy-file (expand-file-name file qutebrowser--package-directory)
               (expand-file-name file qutebrowser-config-directory)
	       'overwrite)))

(defun qutebrowser-rpc--format-params (params)
  "Format PARAMS in a way that is JSON-serializable.
Tries to accept as many different types of parameter lists."
  (cond
   ((json-alist-p params) (cl--alist-to-plist params))
   ((json-plist-p params) params)
   ((listp params) (apply #'vector params))
   (t params)))

(defun qutebrowser-rpc-request (method &optional params)
  "Send an RPC request synchronously and wait for a response.
METHOD is the RPC method to call.
PARAMS are the arguments for the method, and should be a plist
containing keyword arguments."
  (let ((conn (qutebrowser-rpc-get-connection))
        (params (qutebrowser-rpc--format-params params)))
    (jsonrpc-request conn method params :timeout 1)))

(cl-defun qutebrowser-rpc-async-request
    (method &optional params &rest args &key success-fn error-fn timeout-fn)
  "Send an RPC request asynchronously.
METHOD is the RPC method to call.
PARAMS are the arguments for the method, and should be a plist
containing keyword arguments.
SUCCESS-FN, ERROR-FN and TIMEOUT-FN as in `jsonrpc-async-request'."
  (let ((conn (qutebrowser-rpc-get-connection))
        (params (qutebrowser-rpc--format-params params)))
    (if (and conn
             (qutebrowser-jsonrpc-process-connection-p conn)
             (process-live-p (jsonrpc--process conn)))
        (jsonrpc-async-request conn method params
                               :timeout 1
                               :timeout-fn timeout-fn
                               :success-fn success-fn
                               :error-fn error-fn)
      (when error-fn
        (funcall error-fn nil)))))

(defun qutebrowser-rpc-notify (method &optional params)
  "Send an RPC notification and do not expect a response.
METHOD is the RPC method to call.
PARAMS are the arguments for the method, and should be a plist
containing keyword arguments."
  (let ((conn (qutebrowser-rpc-get-connection))
        (params (qutebrowser-rpc--format-params params)))
    (jsonrpc-notify conn method params)))


;; TODO: Rename and move elsewhere
(defun qutebrowser-rpc-request-window-info ()
  "Request window-info from Qutebrowser.
Useful for initializing window information when first connecting to an
instance with existing windows."
  (qutebrowser-rpc-async-request
   :get-window-info nil
   :success-fn
   (lambda (resp)
     (seq-doseq (win resp)
       (run-hook-with-args 'qutebrowser-update-window-info-functions win)))))


(defun qutebrowser-rpc--notification-dispatcher (conn method params)
  "Dispatcher for RPC notifications received from Qutebrowser.
CONN is the `jsonrpc-connection' the request was received on.
METHOD is the method that was called.
PARAMS are the parameters given."
  (let* ((hook (intern-soft (format "qutebrowser-on-%s-functions" method)))
         (x11-win-id (plist-get params :x11-win-id))
         (buffer (exwm--id->buffer x11-win-id)))
    (with-current-buffer (or buffer (current-buffer))
      (run-hook-with-args 'qutebrowser-update-window-info-functions params)
      (run-hook-with-args hook params))))

;; TODO: Implement methods
(defun qutebrowser-rpc--request-dispatcher (conn method params)
  "Dispatcher for RPC requests received from Qutebrowser.
CONN is the `jsonrpc-connection' the request was received on.
METHOD is the method that was called.
PARAMS are the parameters given."
  (cl-case method
    (eval
     (with-current-buffer
         (or (and (fboundp 'exwm--id->buffer)
                  (exwm--id->buffer (plist-get params :x11-win-id)))
             (current-buffer))
       (eval (read (plist-get params :code)))))
    (t (message "Receive request from QB: %s, %s" method params)
       "Responding from Emacs!")))

;;;; Command sending functions

(defvar qutebrowser-ipc-protocol-version 1
  "The protocol version for Qutebrowser IPC.")

(defun qutebrowser-ipc-socket-path ()
  "Return the path to Qutebrowser's IPC socket."
  (expand-file-name
   (format "qutebrowser/ipc-%s" (md5 (user-login-name)))
   (or (getenv "XDG_RUNTIME_DIR")
       (format "/run/user/%d" (user-real-uid)))))

(defun qutebrowser-ipc-send (commands &optional start)
  "Send COMMANDS to Qutebrowser via IPC.
Falls back to sending over commandline if IPC fails."
  (condition-case err
      (let* ((socket-path (qutebrowser-ipc-socket-path))
             (data (json-encode `(("args" . ,commands)
                                  ("target_arg" . nil)
                                  ("protocol_version" . ,qutebrowser-ipc-protocol-version))))
             (process (make-network-process :name "qutebrowser-ipc"
                                            :family 'local
                                            :service socket-path
                                            :coding 'utf-8)))
        (process-send-string process (concat data "\n"))
        (delete-process process))
    (error
     (progn
       (message "IPC failed sending commands. Fallback to commandline.")
       (funcall #'qutebrowser-commandline-send commands start)))))

(defun qutebrowser-commandline-send (commands &optional start)
  "Send COMMANDS to Qutebrowser via commandline."
  (let ((running (qutebrowser-is-running-p)))
    (if (or start running)
        (progn
          (unless running
            (message "Starting new Qutebrowser instance."))
          (apply #'start-process "qutebrowser" nil "qutebrowser" commands))
      (message "Qutebrowser is not running, not going to send commands via commandline."))))

(defvar qutebrowser-fifo nil
  "Holds the path of the Qutebrowser FIFO when called as a userscript.")

(defun qutebrowser-fifo-send (commands &optional _)
  "Send COMMANDS to Qutebrowser via FIFO.
Expects to be called from Qutebrowser through a userscript that
let-binds the path to the Qutebrowser FIFO to the variable
`qutebrowser-fifo'."
  (dolist (cmd commands)
    (write-region (concat cmd "\n") nil qutebrowser-fifo t 'novisit)))

(defun qutebrowser-rpc-send-commands (commands &optional start)
  "Send COMMANDS to Qutebrowser via RPC.
If START is non-nil, start Qutebrowser if it is not running.

Supports `with-current-buffer', such that any commands are executed in
the Qutebrowser window associated with the current buffer.  Otherwise the
command is executed in the default place, which usually seem to be the
last visible window."
  (let ((params `(:commands ,(apply #'vector commands))))
    (when (and (numberp current-prefix-arg)
               (not (plist-member params :count)))
      (plist-put params :count current-prefix-arg))
    (when (and qutebrowser-exwm-win-id
               (not (plist-member params :win-id)))
      (plist-put params :win-id qutebrowser-exwm-win-id))
    (qutebrowser-rpc-async-request
     :command params
     :error-fn
     (lambda (_)
       (message "RPC failed sending commands. Fallback to IPC.")
       (funcall 'qutebrowser-ipc-send commands start))
     :timeout-fn
     (lambda ()
       (message "RPC timed out sending commands. Fallback to IPC.")
       (funcall 'qutebrowser-ipc-send commands start)))))

(defun qutebrowser-send-commands (&rest commands)
  "Send COMMANDS to Qutebrowser via the selected backend."
  (funcall qutebrowser-command-backend commands))

(defun qutebrowser-send-commands-or-start (&rest commands)
  "Send COMMANDS to running Qutebrowser instance, or start a new one."
  (funcall qutebrowser-command-backend commands t))

;;;; Qutebrowser command wrappers

;; This section contains the hardcoded wrappers for some of the
;; Qutebrowser commands that are used internally by this
;; package. These may have special handling of arguments or
;; similar. Not all Qutebrowser commands are wrapped here. To use any
;; Qutebrowser command directly, use either
;; 'qutebrowser-send-commands', or one of the autogenerated command
;; wrappers 'qutebrowser-cmd-COMMAND-NAME'.

(defun qutebrowser-open-url (url &optional target)
  "Open URL in Qutebrowser.
TARGET specifies where to open it, or `qutebrowser-default-open-target'
if nil."
  (let* ((target (cond
                  (target)
                  ((qutebrowser-exwm-p) qutebrowser-default-open-target)
                  (qutebrowser-fifo qutebrowser-default-open-target)
                  ;; We don't want to accidentally replace an existing
                  ;; window/tab when opening a URL in 'auto target
                  ;; when the current buffer is not a Qutebrowser
                  ;; window. We use 'tab as the default in this case,
                  ;; since it is most likely to do the right
                  ;; thing. When the 'tabs_are_windows' option is set
                  ;; in Qutebrowser it will open windows as expected.
                  ((eq 'auto qutebrowser-default-open-target) 'tab)
                  (t qutebrowser-default-open-target)))
         (flag (qutebrowser--target-to-flag target)))
    (qutebrowser-send-commands-or-start (format ":open %s %s" flag url))))

(defun qutebrowser-config-source (&optional config-file)
  "Source CONFIG-FILE in running Qutebrowser instance."
  (interactive)
  (qutebrowser-send-commands (concat ":config-source " config-file)))

(defun qutebrowser-fake-keys--escape (text)
  "Escape any special characters from TEXT to be sent to :fake-keys."
  (apply #'concat
   (mapcar (lambda (chr)
             (pcase chr
               (?< "<less>")
               (?> "<greater>")
               (?\" "\\\"")
               (?\' "'")
               (?\\ "\\\\")
               (_ (char-to-string chr))))
           text)))

(defun qutebrowser-fake-keys--raw (raw-keys)
  "Send RAW-KEYS without escaping special characters."
  (qutebrowser-send-commands (format ":fake-key %s" raw-keys)))

(defun qutebrowser-fake-keys (text)
  "Send TEXT as input to Qutebrowser."
  (let* ((escaped-text (qutebrowser-fake-keys--escape text)))
    (funcall #'qutebrowser-fake-keys--raw (format "\"%s\"" escaped-text))))

;; This is a legacy function from before the implementation of the RPC
;; backend. It was supposed to be used as a convenience function for
;; executing arbitrary Python code in Qutebrowser by sourcing it as a
;; config file, but it is rather limited. Prefer to use either the
;; autogenerated function 'qutebrowser-rpcmethod-eval', or use
;; 'qutebrowser-rpc-request' directly with the ':eval' method.
(defun qutebrowser-execute-python (python-code)
  "Execute PYTHON-CODE in running Qutebrowser instance.
Creates a temporary file and sources it in Qutebrowser using the
:config-source command."
  (let ((temp-conf-file (make-temp-file "qutebrowser-temp-config"
                                        nil nil python-code)))
    (qutebrowser-config-source temp-conf-file)))

(defun qutebrowser-execute-js (js-code)
  "Execute JS-CODE in running Qutebrowser instance."
  (qutebrowser-send-commands (format ":jseval -w main %s" js-code)))

;;;###autoload
(defun qutebrowser-undo-window ()
  "Undo closing Qutebrowser window."
  (interactive)
  (qutebrowser-send-commands ":undo --window"))

(defun qutebrowser-revert-buffer-function (&rest _)
  "Revert buffer function for Qutebrowser EXWM buffers.
This function is used as the `revert-buffer-function' in
`qutebrowser-exwm-mode', so that `revert-buffer' will reload the
webpage."
  (qutebrowser-send-commands ":reload"))

(defun qutebrowser--list-anchors ()
  "List webpage anchors."
  (let* ((js "Array.from(document.querySelectorAll('[href^=\"#\"]'))
                   .map((elem)=> elem.hash);")
         (params `(:js-code ,js :win-id ,qutebrowser-exwm-win-id)))

    (seq-filter (lambda (elem) (and elem (not (string-blank-p elem))))
                (seq-uniq
                 (qutebrowser-rpc-request :js params)))))

;;;###autoload
(defun qutebrowser-goto-anchor (anchor)
  "Scroll to a webpage anchor.
Interactively prompts for an anchor name with completion.
ANCHOR is the anchor to go to."
  (interactive (list (completing-read "Select: " (qutebrowser--list-anchors))))
  (qutebrowser-send-commands (format ":scroll-to-anchor %s"
                                     (string-remove-prefix "#" anchor))))


;;;; Autogenerated functions and commands

;; This section contains functions that autogenerate interactive
;; commands and lisp functions for Qutebrowser commands and RPC
;; methods. These are populated automatically when first connecting to
;; the RPC. Commands are created with the prefix 'qutebrowser-cmd-*',
;; and RPC methods are created with the prefix 'qutebrowser-rpcmethod-*'.

(defun qutebrowser-cmd--parse-args (args)
  "Parse keyword arguments given to qutebrowser-cmd-*.

ARGS is the list of arguments the command was called with.

Translate keywords from :KEYWORD to --KEYWORD and concatenate all args
to a string.  Otherwise keep everything in place and let Qutebrowser sort
it out."
  (string-join
   (cl-loop for arg in args
            collect (if (keywordp arg)
                        (format "--%s" (substring (symbol-name arg) 1))
                      (format "%s" arg)))
   " "))

(defun qutebrowser-cmd--generate-arg-docs (args heading)
  "Generate documentation for a list of arguments.
Generates the arguments section of the documentation for a Qutebrowser command.

HEADING is the heading of the section.
ARGS is the list of arguments where each element has the form
 (:name NAME :description DESCRIPTION)."
  (unless (seq-empty-p args)
    (format
     "%s:\n\n%s\n"
     heading
     (mapconcat (lambda (arg)
                  (let* ((arg-name (plist-get arg :name))
                         (arg-desc (string-replace "\n" "\n    " (or (plist-get arg :description) ""))))
                    (format " - `:%s':  %s\n" arg-name arg-desc)))
                args))))

(defun qutebrowser-populate-commands ()
  "Generate interactive commands for all Qutebrowser commands.

All commands that exist in Qutebrowser are made available as interactive
Emacs commands called qutebrowser-cmd-COMMAND, where COMMAND is the name
of the command in Qutebrowser."
  (qutebrowser-rpc-async-request
   :list-commands nil
   :success-fn
   (lambda (resp)
     (seq-doseq (command resp)
       (let* ((name (plist-get command :command))
              (desc (plist-get command :description))
              (args (plist-get command :arguments))
              (keywords (plist-get command :keywords))
              (takes-count (plist-get command :takes-count))
              (func-name (intern (concat "qutebrowser-cmd-" name)))
              (func-args '(&rest args))
              (func-body `((interactive nil qutebrowser-exwm-mode)
                           (let ((args (qutebrowser-cmd--parse-args args)))
                             (qutebrowser-send-commands (format ":%s %s" ,name args)))))
              (doc-string
               (string-join
                (list desc "\n\n"
                      (format "This function was autogenerated by `qutebrowser-populate-commands'.
It runs the `:%s' command in Qutebrowser.\n\n" name)
                      (qutebrowser-cmd--generate-arg-docs args "Positional arguments")
                      (qutebrowser-cmd--generate-arg-docs keywords "Keyword arguments")
                      (when takes-count (format "Count: %s\n\n" takes-count))
                      (format "Function body:\n\n%s\n"
                              (pp-to-string `(lambda ,func-args
                                               ,@func-body)))))))
         (fset func-name
               (eval `(lambda ,func-args
                        ,doc-string
                        ,@func-body))))))))

(defun qutebrowser-populate-rpcmethods ()
  "Generate functions for all Qutebrowser RPC methods."
  (qutebrowser-rpc-async-request
   :list-rpc-methods nil
   :success-fn
   (lambda (resp)
     (seq-doseq (method resp)
       (let* ((method-name (plist-get method :method))
              (method-desc (plist-get method :description))
              (method-args (plist-get method :arguments))
              (interactive (plist-get method :interactive))
              (takes-count (plist-get method :takes-count))
              (func-name (intern (concat "qutebrowser-rpcmethod-" method-name)))
              (func-args '(&rest args))
              (func-body `(,@(when interactive '((interactive)))
                           (qutebrowser-rpc-request ,method-name args)))
              (doc-string
               (string-join
                (list method-desc "\n\n"
                      (format "This function was autogenerated by `qutebrowser-populate-rpcmethods'.
It runs the `%s' RPC method in Qutebrowser.\n\n" method-name)
                      (when takes-count (format "Count: %s\n\n" takes-count))
                      (format "Function body:\n\n%s\n"
                              (pp-to-string `(lambda ,func-args
                                               ,@func-body)))))))
         (fset func-name
               (eval `(lambda ,func-args
                        ,doc-string
                        ,@func-body))))))))

;;;; Modes

;;;###autoload
(define-minor-mode qutebrowser-exwm-mode
  "Minor mode for Qutebrowser buffers in EXWM."
  :lighter nil
  :global nil
  :keymap qutebrowser-exwm-mode-map
  (if qutebrowser-exwm-mode
      (progn
        (qutebrowser-rpc-get-connection)
        (setq-local revert-buffer-function
                    #'qutebrowser-revert-buffer-function)
        (setq-local bookmark-make-record-function
                    #'qutebrowser-bookmark-make-record))
    (kill-local-variable 'bookmark-make-record-function)
    (kill-local-variable 'revert-buffer-function)))

(defun qutebrowser-exwm-mode-maybe-enable ()
  "Enable `qutebrowser-exwm-mode' if the buffer is a Qutebrowser buffer."
  (when (qutebrowser-exwm-p)
    (qutebrowser-exwm-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-qutebrowser-exwm-mode
  qutebrowser-exwm-mode
  qutebrowser-exwm-mode-maybe-enable
  (if global-qutebrowser-exwm-mode
      (add-hook 'exwm-manage-finish-hook #'qutebrowser-exwm-mode-maybe-enable)
    (remove-hook 'exwm-manage-finish-hook #'qutebrowser-exwm-mode-maybe-enable)))

;;;; Theme export mode
(defun qutebrowser-theme-export--face-mappings ()
  "Write `qutebrowser-theme-export-face-mappings' values."
  (dolist (mapping qutebrowser-theme-export-face-mappings)
    (let* ((qute-face (symbol-name (car mapping)))
           (emacs-face (cdr mapping))
           (is-fg (string-match-p "\\.fg$" qute-face))
           (attribute (if is-fg :foreground :background))
           (color (face-attribute emacs-face attribute nil 'default))
           (hex-color (apply #'color-rgb-to-hex
                             (append (color-name-to-rgb color) '(2)))))
      (insert (format "c.colors.%s = '%s'\n" qute-face hex-color)))))

;;;###autoload
(defun qutebrowser-theme-export ()
  "Export selected Emacs faces to Qutebrowser theme format."
  (interactive)
  (with-temp-file (expand-file-name "emacs_theme.py"
                                    qutebrowser-config-directory)
    (insert "# Qutebrowser theme exported from Emacs\n\n")
    (run-hooks 'qutebrowser-theme-export-functions)))

;;;###autoload
(defun qutebrowser-theme-export-and-apply (&rest _)
  "Export and apply theme to running Qutebrowser instance."
  (interactive)
  (qutebrowser-theme-export)
  (qutebrowser-config-source (expand-file-name "emacs_theme.py"
                                               qutebrowser-config-directory)))

;;;###autoload
(define-minor-mode qutebrowser-theme-export-mode
  "Minor mode to automatically export Emacs theme to Qutebrowser."
  :lighter nil
  :global t
  (if qutebrowser-theme-export-mode
      (progn
        (qutebrowser-theme-export-and-apply)
        (advice-add 'enable-theme :after #'qutebrowser-theme-export-and-apply))
    (advice-remove 'enable-theme #'qutebrowser-theme-export-and-apply)))


;;;; Process utilities

(defun qutebrowser--get-process-pid ()
  "Return a list of PIDs for Qutebrowser processes."
  (cl-remove-if-not
   (lambda (pid)
     (let* ((attrs (process-attributes pid))
            (cmd (alist-get 'comm attrs))
            (state (alist-get 'state attrs)))
       (and (member cmd qutebrowser-process-names)
            ;; Sometimes a zombie process sticks around
            (not (string= "Z" state)))))
   (list-system-processes)))

(defun qutebrowser--get-process-attribute (attr)
  "Return process attribute ATTR of Qutebrowser process."
  (mapcar (lambda (pid)
            (alist-get attr (process-attributes pid)))
          (qutebrowser--get-process-pid)))

(defun qutebrowser--get-process-uptime ()
  "Return uptime in seconds of Qutebrowser process."
  (mapcar (lambda (pid)
            (time-convert (alist-get 'etime (process-attributes pid))
                          'integer))
          (qutebrowser--get-process-pid)))

(defun qutebrowser-is-running-p ()
  "Return non-nil if Qutebrowser is running."
  (when (or (qutebrowser-rpc-connected-p)
            (qutebrowser--get-process-pid)
            (qutebrowser-exwm-buffer-list))
    t))

;;;; Config mode

;;;###autoload
(define-minor-mode qutebrowser-config-mode
  "Minor mode for editing Qutebrowser config files."
  :lighter nil
  :global nil
  :keymap qutebrowser-config-mode-map)

(defun qutebrowser-config-source-buffer (&optional buffer)
  "Source the contents of BUFFER."
  (interactive)
  (let ((temp (make-temp-file "qutebrowser-temp-config")))
    (with-current-buffer (or buffer (current-buffer))
      (write-region (point-min) (point-max) temp nil 'novisit))
    (qutebrowser-config-source temp)))

(defun qutebrowser-config-source-region ()
  "Source the current region."
  (interactive)
  (let ((temp (make-temp-file "qutebrowser-temp-config")))
    (write-region (region-beginning) (region-end) temp nil 'novisit)
    (qutebrowser-config-source temp)))

(defun qutebrowser-config-source-file ()
  "Source the file associated with the current buffer."
  (interactive)
  (qutebrowser-config-source (buffer-file-name)))

;;;; Footer

(provide 'qutebrowser)

;;; qutebrowser.el ends here
