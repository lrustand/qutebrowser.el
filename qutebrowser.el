;;; qutebrowser.el --- Qutebrowser integration with Emacs and EXWM     -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Lars Rustand.

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
;; Package-Requires: ((emacs "29.1") (consult "0.34"))

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
(require 'consult)
(require 'exwm)
(require 'json)
(require 'jsonrpc)
(require 'color)
(require 'cl-lib)
(require 'dash)
(require 'evil)

(declare-function evil-emacs-state "ext:evil-states" t t)
(declare-function evil-motion-state "ext:evil-states" t t)
(declare-function evil-visual-state "ext:evil-states" t t)
(declare-function evil-insert-state "ext:evil-states" t t)
(declare-function evil-normal-state "ext:evil-states" t t)

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
           (hints.fg . avy-lead-face)
           (hints.bg . avy-lead-face)
           (hints.match.fg . avy-lead-face-0)
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

(defcustom qutebrowser-command-backend 'qutebrowser-ipc-send
  "The backend to use when sending commands to Qutebrowser."
  :type '(choice (const :tag "IPC" qutebrowser-ipc-send)
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

(defcustom qutebrowser-title-display-length 100
  "Max display length of Qutebrowser titles in completion lists."
  :type 'integer
  :group 'qutebrowser)

(defcustom qutebrowser-url-display-length 50
  "Max display length of Qutebrowser URLs in completion lists."
  :type 'integer
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

(defgroup qutebrowser-faces nil
  "Faces used by qutebrowser.el."
  :group 'qutebrowser
  :group 'faces)

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
  "(url || title) LIKE '%%%s%%'"
  "SQL matching pattern used for each input word.")

(defvar qutebrowser-on-entered-mode-functions '()
  "Functions run when receiving a `entered-mode` signal.")

(defvar qutebrowser-on-left-mode-functions '()
  "Functions run when receiving a `left-mode` signal.")

(defvar qutebrowser-on-new-window-functions '()
  "Functions run when receiving a `new-window` signal.")

;; This triggers ~300 times (maybe once per line?)
(defvar qutebrowser-on-config-changed-functions '()
  "Functions run when receiving a `config-changed` signal.")

(defvar qutebrowser-on-url-changed-functions '()
  "Functions run when receiving a `url-changed` signal.")

(defvar qutebrowser-on-link-hovered-functions '()
  "Functions run when receiving a `link-hovered` signal.")

(defvar qutebrowser-on-icon-changed-functions '()
  "Functions run when receiving a `icon-changed` signal.")

(defvar qutebrowser-on-got-search-functions '()
  "Functions run when receiving a `got-search` signal.")

(defvar qutebrowser--db-object nil
  "Contains a reference to the database connection.")

(defvar-local qutebrowser-exwm-keymode "KeyMode.normal")

(defvar-local qutebrowser-exwm-hovered-url nil
  "Contains the URL of the link currently hovered in Qutebrowser.")

(defvar-local qutebrowser-exwm-current-url nil
  "Contains the current URL of Qutebrowser.")

(defvar-local qutebrowser-exwm-favicon nil
  "Contains the favicon for each Qutebrowser buffer.")

(defvar-local qutebrowser-exwm-current-search nil
  "Contains the current search terms of Qutebrowser.")

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

(defun qutebrowser-exwm-update-current-url (buffer url)
  "Update the buffer-local variable `qutebrowser-exwm-current-url'."
  (with-current-buffer buffer
    (setq-local qutebrowser-exwm-current-url (unless (string-empty-p url)
                                               url))))

(defun qutebrowser-exwm-update-hovered-url (buffer hover)
  "Update the currently hovered URL."
  (with-current-buffer buffer
    (when (string= hover "") (setq hover nil))
    (setq-local qutebrowser-exwm-hovered-url hover)))

(defun qutebrowser-exwm-update-favicon (buffer icon-file)
  "Update the favicon."
  (if (and (file-regular-p icon-file)
           ;; Not empty
           (> (nth 7 (file-attributes icon-file)) 0))
      (with-current-buffer buffer
        (when-let ((image (create-image icon-file nil nil :height 16 :width 16 :ascent 'center)))
          (let ((old-icon-file (image-property qutebrowser-exwm-favicon :file)))
            (setq-local qutebrowser-exwm-favicon image)
            (when old-icon-file
              (delete-file old-icon-file)))))
    ;; Delete invalid/empty icon files
    (delete-file icon-file)))

(defun qutebrowser-exwm-delete-favicon-tempfile ()
  "Deletes the tempfile associated with the favicon of current buffer."
  (when-let ((icon-file (image-property qutebrowser-exwm-favicon :file)))
    (delete-file icon-file)))

(add-hook 'kill-buffer-hook #'qutebrowser-exwm-delete-favicon-tempfile)

(defun qutebrowser-exwm-update-evil-state (buffer mode)
  "Set evil state to match Qutebrowser keymode."
  (with-current-buffer buffer
    (setq-local qutebrowser-exwm-keymode mode)
    (pcase mode
      ("KeyMode.insert" (evil-insert-state))
      ("KeyMode.caret" (evil-visual-state))
      ("KeyMode.hint" (evil-motion-state))
      ("KeyMode.command" (evil-emacs-state))
      ("KeyMode.normal" (evil-normal-state)))))

(defun qutebrowser-exwm-update-search (buffer search)
  "Update the variable `qutebrowser-exwm-current-search'."
  (with-current-buffer buffer
    (setq-local qutebrowser-exwm-current-search search)))

(defun qutebrowser-exwm-update-window-info (window-info &optional accept-nil)
  (when-let* ((win-id (plist-get window-info :win-id))
              (buffer (exwm--id->buffer win-id)))
    (let* ((url (plist-get window-info :url))
           (icon-file (plist-get window-info :icon-file))
           (hover (plist-get window-info :hover))
           (search (plist-get window-info :search))
           (mode (plist-get window-info :mode)))

      (when (or mode accept-nil)
        (qutebrowser-exwm-update-evil-state buffer mode))

      (when (or icon-file accept-nil)
        (qutebrowser-exwm-update-favicon buffer icon-file))

      (when (or search accept-nil)
        (qutebrowser-exwm-update-search buffer search))

      (when (or hover accept-nil)
        (qutebrowser-exwm-update-hovered-url buffer hover))

      (when (or url accept-nil)
        (qutebrowser-exwm-update-current-url buffer url)))))

;;;; History database functions

(defun qutebrowser--get-db ()
  "Return the open database, or open it."
  (unless (sqlitep qutebrowser--db-object)
    (setq qutebrowser--db-object (sqlite-open qutebrowser-history-database)))
  qutebrowser--db-object)

(defun qutebrowser--history-search (&optional input limit)
  "Search the sqlite database for INPUT.
Return up to LIMIT results."
  (let* ((db (qutebrowser--get-db))
         ;; Safeguarding to avoid nil value
         (words (or (string-split (or input "")) '("")))
         (inclusion (mapconcat (apply-partially 'format qutebrowser-history-matching-pattern)
                               words " AND "))
         (exclusion (mapconcat (apply-partially 'format " url LIKE '%s'")
                               qutebrowser-history-exclusion-patterns " OR "))
         (limit (if limit (format "LIMIT %s" limit) ""))
         (query (format "SELECT url,substr(title,0,%d)
                         FROM CompletionHistory
                         WHERE %s AND NOT (%s)
                         ORDER BY %s
                         %s"
                        (1- qutebrowser-title-display-length)
                        inclusion
                        exclusion
                        qutebrowser-history-order-by
                        limit)))
    ;; Return list of URLs propertized with input and title
    (mapcar (lambda (row)
              (let* ((url (car row))
                     (title (cadr row)))
                (propertize (consult--tofu-append url ?h)
                            'input input
                            'title title)))
            (sqlite-select db query))))

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

(defun qutebrowser-exwm-p (&optional buffer)
  "Return t if BUFFER is a Qutebrowser EXWM buffer."
  (with-current-buffer (or buffer (current-buffer))
    (string-equal "qutebrowser"
                  exwm-class-name)))

(defun qutebrowser-exwm-buffer-url (&optional buffer)
  "Return the URL of BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or qutebrowser-exwm-current-url
        ;; Keep backward compatibility for now
        (get-text-property 0 'url (buffer-name buffer)))))

(defun qutebrowser-exwm-buffer-list ()
  "Return a list of all Qutebrowser buffers."
  (seq-filter #'qutebrowser-exwm-p (buffer-list)))

(defun qutebrowser--shorten-display-url (url)
  "Shorten URL by making the end invisible."
  (let ((url-length (length url))
        (max-length qutebrowser-url-display-length))
    (when (> url-length max-length)
      (put-text-property max-length url-length 'invisible t url))
    url))

(defun qutebrowser--strip-tofus (str)
  (let* ((end (length str)))
    (while (and (> end 0) (consult--tofu-p (aref str (1- end))))
      (cl-decf end))
    (substring str 0 end)))


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

(defun qutebrowser-exwm-buffer-filter (words buffers)
  "Filter BUFFERS to find those matching WORDS.
Both buffer names and URLs are used for matching."
  (seq-filter
   (lambda (buffer)
     ;; All search words matching
     (-all-p (lambda (word)
               (let ((title (or (buffer-name buffer) ""))
                     (url (or (qutebrowser-exwm-buffer-url buffer) "")))
               (or (string-match-p word title)
                   (string-match-p word url))))
             words))
   buffers))

(defun qutebrowser-bookmark-filter (words bookmarks)
  "Filter BOOKMARKS to find those matching WORDS.
Both bookmark name and URLs are used for matching."
  (seq-filter
   (lambda (bookmark)
     ;; All search words matching
     (-all-p (lambda (word)
               (or (string-match-p word bookmark)
                   (string-match-p word (qutebrowser-bookmark-url bookmark))))
             words))
   bookmarks))

(defun qutebrowser-bookmark-search (&optional input)
  "Return a propertized list of Qutebrowser bookmarks matching INPUT."
  (let* ((words (string-split (or input "")))
         (bookmarks (qutebrowser-bookmarks-list))
         (matching-bookmarks (qutebrowser-bookmark-filter words bookmarks)))
    (mapcar (lambda (bookmark)
              (let* ((url (qutebrowser-bookmark-url bookmark)))
                (propertize (consult--tofu-append url ?m)
                            'input input
                            'title bookmark
                            'bookmark t)))
            matching-bookmarks)))

(defun qutebrowser-exwm-buffer-search (&optional input)
  "Return a propertized list of Qutebrowser buffers matching INPUT."
  (let* ((words (string-split (or input "")))
         (buffers (qutebrowser-exwm-buffer-list))
         (matching-buffers (qutebrowser-exwm-buffer-filter words buffers)))
    (mapcar (lambda (buffer)
              (let* ((title (substring-no-properties (buffer-name buffer)))
                     (url (qutebrowser-exwm-buffer-url buffer)))
                (propertize (consult--tofu-append url ?b)
                            'input input
                            'title title
                            'buffer buffer)))
            matching-buffers)))

(defun qutebrowser-highlight-matches (input str)
  "Highlight all occurrences of words in INPUT in STR."
  (dolist (word (string-split input))
    (if-let* ((start (string-match word str))
              (end (+ start (length word))))
        (put-text-property start end 'face 'link str))))

(defun qutebrowser-annotate (entry &optional pad)
  "Return annotation for ENTRY.
ENTRY can be a bookmark, a buffer, or a history item.  ENTRY should be a
string containing a URL, and it should be propertized with at least some
of `input', `url', and/or `title'.

ENTRY will be modified to highlight any words contained in the `input'
property, and the end of the string will be hidden by setting the
`invisible' property.

If PAD is non-nil, add padding to the annotation if ENTRY is shorter
than `qutebrowser-url-display-length'."
  (let ((input (get-text-property 0 'input entry))
        (url (substring-no-properties entry))
        (title (get-text-property 0 'title entry)))
    ;; Set main face of annotation (title)
    (put-text-property 0 (length title) 'face 'completions-annotations title)
    ;; Highlight all matching words (both in url and title)
    (when input
      (qutebrowser-highlight-matches input entry)
      (qutebrowser-highlight-matches input title))
    (qutebrowser--shorten-display-url entry)
    (let* ((pad-length (max 0 (- qutebrowser-url-display-length
                                 (1- (length url)))))
           ;; When used in the dynamic qutebrowser-select-url, we need
           ;; to pad the annotations for alignment. This is not needed
           ;; when the annotations are used in non-dynamic buffer
           ;; sources.
           (padding (when pad (make-string pad-length ?\ ))))
      (concat padding " "  (truncate-string-to-width title qutebrowser-title-display-length)))))

;; TODO: Duplicate URL buffers seem to only show once
(defun qutebrowser-select-url (&optional initial)
  "Dynamically select a URL from Qutebrowser history.
INITIAL sets the initial input in the minibuffer."
  (let ((consult-async-min-input 0))
    (consult--read
     (consult--dynamic-collection
      (lambda (input)
        (append
         (qutebrowser-exwm-buffer-search input)
         (qutebrowser-bookmark-search input)
         (qutebrowser--history-search input qutebrowser-dynamic-results))))
     :group (lambda (entry transform)
              (if transform
                  entry
                (cond
                 ((get-text-property 0 'buffer entry) "Buffer")
                 ((get-text-property 0 'bookmark entry) "Bookmark")
                 (t "History"))))
     :sort nil
     :annotate (lambda (entry) (qutebrowser-annotate entry t))
     :initial initial
     :require-match nil)))

;;;; Static consult buffer sources

(defvar qutebrowser--exwm-buffer-source
  (list :name "Qutebrowser buffers"
        :hidden nil
        :narrow ?q
        :history nil
        :category 'other
        :action (lambda (entry)
                  (switch-to-buffer (get-text-property 0 'buffer entry)))
        :annotate #'qutebrowser-annotate
        :items #'qutebrowser-exwm-buffer-search)
  "`consult-buffer' source for open Qutebrowser windows.")

(defvar qutebrowser--bookmark-source
  (list :name "Qutebrowser bookmarks"
        :hidden nil
        :narrow ?m
        :history nil
        :category 'other
        :face 'consult-bookmark
        :action #'qutebrowser-bookmark-jump
        :items #'qutebrowser-bookmarks-list)
  "`consult-buffer' source for Qutebrowser bookmarks.")

;;;; Launcher functions

;;;###autoload
(defun qutebrowser-launcher (&optional initial target)
  "Select a URL to open in Qutebrowser.
Set initial completion input to INITIAL.  Open the URL in TARGET or the
default target if nil."
  (interactive)
  (when-let* ((qutebrowser-default-open-target
               (or target qutebrowser-default-open-target))
              (selected (qutebrowser-select-url initial)))
    ;; FIXME: This way of dispatching is a temporary workaround
    ;; because consult currently doesn't support mixing dynamic and
    ;; static sources, so we can't set up individual consult sources
    ;; with :action functions.
    (let ((source-id (consult--tofu-get selected))
          (url (qutebrowser--strip-tofus selected)))
    (if (eq ?b source-id)
        (let ((buffer (qutebrowser-exwm-find-buffer url)))
         (switch-to-buffer buffer))
      (qutebrowser-open-url url)))))

;;;###autoload
(defun qutebrowser-launcher-tab (&optional initial)
  "Select a URL to open in a new tab.
Set initial completion input to INITIAL."
  (interactive)
  (qutebrowser-launcher initial 'tab))

;;;###autoload
(defun qutebrowser-launcher-window (&optional initial)
  "Select a URL to open in a new window.
Set initial completion input to INITIAL."
  (interactive)
  (qutebrowser-launcher initial 'window))

;;;###autoload
(defun qutebrowser-launcher-private (&optional initial)
  "Select a URL to open in a private window.
Set initial completion input to INITIAL."
  (interactive)
  (qutebrowser-launcher initial 'private-window))

;;;; Advice

;; Prevent Prescient history from being clogged up by web pages.
(defun qutebrowser-advice-vertico-prescient (orig-fun &rest args)
  "Exclude Qutebrowser buffer names and URLs from prescient history.
The ORIG-FUN takes ARGS."
  (let* ((selected-candidate (qutebrowser--strip-tofus
                              (minibuffer-contents-no-properties)))
         (selected-buffer (get-buffer selected-candidate)))
    (unless (or (qutebrowser-exwm-p selected-buffer)
                (string-match-p "^https?://" selected-candidate))
      (apply orig-fun args))))

(with-eval-after-load 'vertico-prescient
  (advice-add 'vertico-prescient--remember-minibuffer-contents :around
              #'qutebrowser-advice-vertico-prescient))

;;;; RPC functions

(defun qutebrowser-rpc--bootstrap-server ()
  "Bootstrap the RPC server and hooks by sourcing the config files."
  (let ((ipc (expand-file-name "emacs_ipc.py"
                               qutebrowser-config-directory))
        (hooks (expand-file-name "emacs_hooks.py"
                                 qutebrowser-config-directory)))
    (if (and (file-regular-p ipc)
             (file-regular-p hooks))
        ;; TODO: Detect when it is necessary to do this
        (progn
          (qutebrowser-config-source ipc)
          (qutebrowser-config-source hooks))
      (message "RPC Python backend not found. Did you install it? Tip: run `qutebrowser-rpc-ensure-installed'."))))

(defun qutebrowser-rpc--make-network-process ()
  "Make a network process connected to the RPC socket."
  (unless (file-exists-p "/tmp/emacs-ipc")
    (qutebrowser-rpc--bootstrap-server)
    (sit-for 1))
  (when (file-exists-p "/tmp/emacs-ipc")
    (make-network-process
     :name "qutebrowser-rpc"
     :family 'local
     :service "/tmp/emacs-ipc"
     :sentinel (lambda (proc event)
                 (when (string= event "connection broken by remote peer\n")
                   (delete-process proc))))))

(defvar qutebrowser-rpc-connection nil)

(defun qutebrowser-rpc-get-connection (&optional flush)
  "Return a `jsonrpc-connection' to the RPC socket.
If FLUSH is non-nil, delete any existing connection before reconnecting."
  (interactive)
  (let ((process (get-process "qutebrowser-rpc")))
    (when (and flush process)
      (delete-process process)
      (setq process nil))
    (unless (qutebrowser-rpc-connected-p)
      (if-let ((proc (qutebrowser-rpc--make-network-process)))
          (progn
            (setq qutebrowser-rpc-connection
                  (jsonrpc-process-connection
                   :name "qutebrowser-jsonrpc"
                   :process proc
                   :notification-dispatcher
                   #'qutebrowser-rpc--notification-dispatcher
                   :request-dispatcher
                   #'qutebrowser-rpc--request-dispatcher))
            (qutebrowser-rpc-request-window-info))
        (message "Could not connect jsonrpc")))
    qutebrowser-rpc-connection))

(defun qutebrowser-rpc-connected-p ()
  "Check if connected to the Qutebrowser RPC."
  (and (jsonrpc-process-connection-p qutebrowser-rpc-connection)
       (jsonrpc-running-p qutebrowser-rpc-connection)))

(defun qutebrowser-rpc-ensure-installed ()
  "Ensure that the Python backend files for RPC and hooks are installed.
To make sure that these files are updated whenever the package is
updated it is recommended to run this function when loading the package."
  (interactive)
  (dolist (file '("emacs_ipc.py"
                  "emacs_hooks.py"))
    (copy-file (expand-file-name file qutebrowser--package-directory)
               (expand-file-name file qutebrowser-config-directory)
	       'overwrite)))

(defun qutebrowser-rpc-request (method &optional params)
  (let ((conn (qutebrowser-rpc-get-connection)))
    ;; Qutebrowser reads until newline.
    ;; Need to add one to avoid hanging the process.
    (cl-letf (((symbol-function 'jsonrpc--json-encode)
               (lambda (object)
                 (concat
                  (json-serialize object
                                  :false-object :json-false
                                  :null-object nil)
                  "\n"))))
      (jsonrpc-request conn method params))))


;; TODO: Rename and move elsewhere
(defun qutebrowser-rpc-request-window-info ()
  "Request window-info from Qutebrowser.
Useful for initializing window information when first connecting to an
instance with existing windows."
  (seq-doseq (win (qutebrowser-rpc-request "window-info" nil))
    (qutebrowser-exwm-update-window-info win nil)))


(defun qutebrowser-rpc--notification-dispatcher (conn method params)
  (let ((hook (intern-soft (format "qutebrowser-on-%s-functions" method))))
    (qutebrowser-exwm-update-window-info params)
    (run-hook-with-args hook params)))

;; TODO: Implement methods
(defun qutebrowser-rpc--request-dispatcher (conn method params)
  (message "Receive request from QB: %s, %s" method params)
  "Responding from Emacs!")

;;;; Command sending functions

(defvar qutebrowser-ipc-protocol-version 1
  "The protocol version for Qutebrowser IPC.")

(defun qutebrowser-ipc-socket-path ()
  "Return the path to Qutebrowser's IPC socket."
  (expand-file-name
   (format "qutebrowser/ipc-%s" (md5 (user-login-name)))
   (or (getenv "XDG_RUNTIME_DIR")
       (format "/run/user/%d" (user-real-uid)))))

(defun qutebrowser-ipc-send (&rest commands)
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
    (file-error
     (progn
       (message "Error connecting to Qutebrowser IPC socket: %s" (error-message-string err))
       (message "Starting new Qutebrowser instance.")
       (apply #'qutebrowser-commandline-send commands)))
    (error
     (message "Unexpected error in qutebrowser-ipc-send: %s" (error-message-string err)))))

(defun qutebrowser-commandline-send (&rest commands)
  "Send COMMANDS to Qutebrowser via commandline."
  (apply #'start-process "qutebrowser" nil "qutebrowser" commands))

(defvar qutebrowser-fifo nil
  "Holds the path of the Qutebrowser FIFO when called as a userscript.")

(defun qutebrowser-fifo-send (&rest commands)
  "Send COMMANDS to Qutebrowser via FIFO.
Expects to be called from Qutebrowser through a userscript that
let-binds the path to the Qutebrowser FIFO to the variable
`qutebrowser-fifo'."
  (dolist (cmd commands)
    (write-region (concat cmd "\n") nil qutebrowser-fifo t 'novisit)))

(defun qutebrowser-send-commands (&rest commands)
  "Send COMMANDS to Qutebrowser via the selected backend."
  (apply qutebrowser-command-backend commands))


;;;; Qutebrowser command wrappers

(defun qutebrowser-open-url (url &optional target)
  "Open URL in Qutebrowser.
TARGET specifies where to open it, or `qutebrowser-default-open-target'
if nil."
  (let* ((target (or target qutebrowser-default-open-target))
         (flag (qutebrowser--target-to-flag target)))
    (qutebrowser-send-commands (format ":open %s %s" flag url))))

(defun qutebrowser-config-source (&optional config-file start-if-not-running)
  "Source CONFIG-FILE in running Qutebrowser instance.
If START-IF-NOT-RUNNING is non-nil, start Qutebrowser if it is not running."
  (interactive)
  (when (or (qutebrowser-is-running-p)
            start-if-not-running)
    (qutebrowser-send-commands (concat ":config-source " config-file))))

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
        (setq-local bookmark-make-record-function
                    #'qutebrowser-bookmark-make-record))
    (kill-local-variable 'bookmark-make-record-function)))

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

(defun qutebrowser-theme-export ()
  "Export selected Emacs faces to Qutebrowser theme format."
  (interactive)
  (with-temp-file (expand-file-name "emacs_theme.py"
                                    qutebrowser-config-directory)
    (insert "# Qutebrowser theme exported from Emacs\n\n")
    (dolist (mapping qutebrowser-theme-export-face-mappings)
      (let* ((qute-face (symbol-name (car mapping)))
             (emacs-face (cdr mapping))
             (is-fg (string-match-p "\\.fg$" qute-face))
             (attribute (if is-fg :foreground :background))
             (color (face-attribute emacs-face attribute nil 'default))
             (hex-color (apply #'color-rgb-to-hex
                               (append (color-name-to-rgb color) '(2)))))
        (insert (format "c.colors.%s = '%s'\n" qute-face hex-color))))))

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
            (qutebrowser--get-process-pid))
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
