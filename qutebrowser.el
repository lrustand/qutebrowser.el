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
(require 'color)
(require 'password-store)
(require 'cl-lib)
(require 'dash)

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
           (tabs.selected.even.bg . tab-line)
           (webpage.bg . default))
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

(defface qutebrowser-title-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight Qutebrowser titles."
  :group 'qutebrowser-faces)

(defvar qutebrowser-history-matching-pattern
  "(url || title) LIKE '%%%s%%'")

(defvar qutebrowser-bookmark--tofu (consult--tofu-encode 1))
(defvar qutebrowser-buffer--tofu (consult--tofu-encode 2))
(defvar qutebrowser-history--tofu (consult--tofu-encode 3))

(defvar qutebrowser-mode-enter-hook nil
  "Hook run after Qutebrowser enters a mode.")

(defvar qutebrowser-mode-leave-hook nil
  "Hook run after Qutebrowser leaves a mode.")

(defvar qutebrowser-new-window-hook nil
  "Hook run after a new window Qutebrowser window is created.")

(defvar qutebrowser-config-changed-hook nil
  "Hook run after Qutebrowser config is changed.")

(defvar qutebrowser-got-search-hook nil
  "Hook run after a search term is entered.")

(defvar qutebrowser-url-changed-hook nil
  "Hook run after Qutebrowser changes URL.")

(defvar qutebrowser--db-object nil
  "Contains a reference to the database connection.")

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
         (inclusion (mapcar (apply-partially 'format qutebrowser-history-matching-pattern)
                            words))
         (exclusion (mapcar (apply-partially 'format " AND url NOT LIKE '%s'")
                            qutebrowser-history-exclusion-patterns))
         (where (concat "WHERE " (string-join inclusion " AND ")
                        (string-join exclusion)))
         (query (format "SELECT url,substr(title,0,%d)
                         FROM CompletionHistory
                         %s
                         ORDER BY %s
                         %s"
                        (1- qutebrowser-title-display-length)
                        where
                        qutebrowser-history-order-by
                        (if limit (format "LIMIT %s" limit) ""))))
    ;; Return list of URLs propertized with input and title
    (mapcar (lambda (row)
              (let* ((url (car row))
                     (title (cadr row)))
                (propertize (concat qutebrowser-history--tofu url)
                            'input input
                            'title title)))
            (sqlite-select db query))))

(defun qutebrowser--target-to-flag (target)
  "Return the :open flag corresponding to TARGET."
  (pcase target
    ('window "-w")
    ('tab "-t")
    ('private-window "-p")
    ('auto "")))

(defun qutebrowser-find-buffer (url)
  "Find the buffer showing URL."
  (seq-find (lambda (buffer)
              (string= url (get-text-property 0 'url (buffer-name buffer))))
            (qutebrowser-buffer-list)))

;;;###autoload
(defun qutebrowser-launcher (&optional initial target)
  "Select a URL to open in Qutebrowser.
Set initial completion input to INITIAL.  Open the URL in TARGET or the
default target if nil."
  (interactive)
  (let* ((qutebrowser-default-open-target
          (or target qutebrowser-default-open-target))
         (selected (qutebrowser-select-url initial)))
    (when selected
      (cond
       ((string-prefix-p qutebrowser-buffer--tofu selected)
        (let* ((url (substring selected 1))
               (buffer (qutebrowser-find-buffer url)))
          (switch-to-buffer buffer)))
       ((string-prefix-p qutebrowser-bookmark--tofu selected)
        (let ((url (substring selected 1)))
          (qutebrowser-open-url url)))
       ((string-prefix-p qutebrowser-history--tofu selected)
        (let ((url (substring selected 1)))
          (qutebrowser-open-url url)))
       (t (qutebrowser-open-url selected))))))

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

(defun qutebrowser-exwm-p (&optional buffer)
  "Return t if BUFFER is a Qutebrowser EXWM buffer."
  (with-current-buffer (or buffer (current-buffer))
    (string-equal "qutebrowser"
                  exwm-class-name)))

(defun qutebrowser-propertize-buffer-name (window-title)
  "Propertize the buffer name of Qutebrowser buffer.
WINDOW-TITLE is the title of the Qutebrowser window, as reported by
`exwm-title'.  Expects the window title to be formatted in the following
way:

c.window.title_format = '{audio}{private}{current_title}{title_sep}{current_url}'

This function should be added to `exwm-update-title-hook'.  If you
already have set up a hook to update buffer names, the hook should be
modified so that it runs this function for Qutebrowser buffers.

The following is what I have in my own init.el:

  (defun exwm-update-title ()
    (if (string-equal \"qutebrowser\" exwm-class-name)
        (exwm-workspace-rename-buffer
         (qutebrowser-propertize-buffer-name exwm-title))
      (exwm-workspace-rename-buffer exwm-title)))
  (add-hook 'exwm-update-title-hook #'exwm-update-title)"

  (let ((mid (string-match " - https?://.*$" window-title)))
    (if mid
        (let ((title (substring window-title 0 mid))
              (url (substring window-title (+ 3 mid))))
          (propertize title 'url url))
      window-title)))

(defun qutebrowser--shorten-display-url (url)
  "Shorten URL by making the end invisible."
  (let ((url-length (length url))
        (max-length qutebrowser-url-display-length))
    (when (> url-length max-length)
      (put-text-property max-length url-length 'invisible t url))
    url))

(defun qutebrowser-buffer-url (&optional buffer)
  "Return the URL of BUFFER or the current buffer."
  (get-text-property 0 'url (buffer-name buffer)))

(defun qutebrowser-buffer-list ()
  "Return a list of all Qutebrowser buffers."
  (seq-filter #'qutebrowser-exwm-p (buffer-list)))

(defun qutebrowser-buffer-filter (words buffers)
  "Filter BUFFERS to find those matching WORDS.
Both buffer names and URLs are used for matching."
  (seq-filter
   (lambda (buffer)
     ;; All search words matching
     (-all-p (lambda (word)
               (let ((title (or (buffer-name buffer) ""))
                     (url (or (qutebrowser-buffer-url buffer) "")))
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
                (propertize (concat qutebrowser-bookmark--tofu url)
                            'input input
                            'title bookmark
                            'bookmark t)))
            matching-bookmarks)))

(defun qutebrowser-buffer-search (&optional input)
  "Return a propertized list of Qutebrowser buffers matching INPUT."
  (let* ((words (string-split (or input "")))
         (buffers (qutebrowser-buffer-list))
         (matching-buffers (qutebrowser-buffer-filter words buffers)))
    (mapcar (lambda (buffer)
              (let* ((title (substring-no-properties (buffer-name buffer)))
                     (url (qutebrowser-buffer-url buffer)))
                (propertize (concat qutebrowser-buffer--tofu url)
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

(defun qutebrowser-annotate (entry)
  "Return annotation for ENTRY.
ENTRY can be a bookmark, a buffer, or a history item.  ENTRY should be a
string containing a URL, and it should be propertized with at least some
of 'input, 'url, 'title, 'buffer, 'visited, and/or 'bookmark.

ENTRY will be modified to highlight any words contained in the 'input
property, and the end of the string will be hidden by setting the
'invisible property."
  (let ((input (get-text-property 0 'input entry))
        (url (substring-no-properties entry))
        (title (get-text-property 0 'title entry))
        (buffer (get-text-property 0 'buffer entry))
        (visited (get-text-property 0 'visited entry))
        (bookmark (get-text-property 0 'bookmark entry)))
    ;; Set main face of annotation (title)
    (put-text-property 0 (length title) 'face 'completions-annotations title)
    ;; Highlight all matching words (both in url and title)
    (when input
      (qutebrowser-highlight-matches input entry)
      (qutebrowser-highlight-matches input title))
    (qutebrowser--shorten-display-url entry)
    (let* ((pad-length (max 0 (- qutebrowser-url-display-length
                                 (length url))))
           (padding (make-string pad-length ?\ )))
      (concat padding " "  (truncate-string-to-width title qutebrowser-title-display-length)))))

(defun qutebrowser-select-url (&optional initial)
  "Dynamically select a URL from Qutebrowser history.
INITIAL sets the initial input in the minibuffer."
  (let ((consult-async-min-input 0))
    (consult--read
     (consult--dynamic-collection
      (lambda (input)
        (append
         (qutebrowser-buffer-search input)
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
     :annotate #'qutebrowser-annotate
     ;; Ugly hack to show results immediately. Consult requires at
     ;; least one character to start displaying results from dynamic
     ;; sources, so we replace the empty initial input with a space
     ;; character. The space character is simply ignored by our
     ;; word-by-word searches.
     :initial initial
     :require-match nil)))

(defvar qutebrowser--exwm-buffer-source
  (list :name "Qutebrowser buffers"
        :hidden nil
        :narrow ?q
        :history nil
        :category 'other
        :action (lambda (entry)
                  (switch-to-buffer (get-text-property 0 'buffer entry)))
        :annotate #'qutebrowser-annotate
        :items #'qutebrowser-buffer-search)
  "`consult-buffer' source for open Qutebrowser windows.")

(defun qutebrowser-bookmark-p (bookmark)
  "Return t if BOOKMARK is a Qutebrowser bookmark."
  (eq 'qutebrowser-bookmark-jump
      (bookmark-get-handler bookmark)))

(defun qutebrowser-bookmarks-list ()
  "Return a list of Qutebrowser bookmarks."
  (seq-filter #'qutebrowser-bookmark-p
              (bookmark-all-names)))

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

;; Prevent Prescient history from being clogged up by web pages.
(with-eval-after-load 'vertico-prescient
  (defun qutebrowser-advice-vertico-prescient (orig-fun &rest args)
    "Exclude Qutebrowser buffer names and URLs from prescient history."
    (let* ((selected-candidate
            (substring (minibuffer-contents-no-properties) 0 -1))
           (selected-buffer (get-buffer selected-candidate)))
      (unless (or (qutebrowser-exwm-p selected-buffer)
                  (string-match-p "^https?://" selected-candidate))
        (funcall orig-fun))))
  (advice-add 'vertico-prescient--remember-minibuffer-contents :around
              #'qutebrowser-advice-vertico-prescient))

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

(defun qutebrowser-open-url (url &optional target)
  "Open URL in Qutebrowser.
TARGET specifies where to open it, or `qutebrowser-default-open-target'
if nil."
  (let* ((target (or target qutebrowser-default-open-target))
         (flag (qutebrowser--target-to-flag target)))
    (qutebrowser-send-commands (format ":open %s %s" flag url))))

(defun qutebrowser-config-source (&optional config-file)
  "Source CONFIG-FILE in running Qutebrowser instance."
  (interactive)
  (qutebrowser-send-commands (concat ":config-source " config-file)))

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
(define-minor-mode qutebrowser-exwm-mode
  "Minor mode for Qutebrowser buffers in EXWM."
  :lighter nil
  :global nil
  (if qutebrowser-exwm-mode
      (setq-local bookmark-make-record-function
                  #'qutebrowser-bookmark-make-record)
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


(defun qutebrowser-bookmark-make-record ()
  "Make a bookmark record for Qutebrowser buffers."
  `(,(buffer-name)
    (handler . qutebrowser-bookmark-jump)
    (url . ,(get-text-property 0 'url (buffer-name)))))

(defun qutebrowser-bookmark-url (bookmark)
  "Return the URL that BOOKMARK is pointing to."
  (bookmark-prop-get bookmark 'url))

(defun qutebrowser-bookmark-jump (bookmark)
  "Jump to a Qutebrowser BOOKMARK."
  (let ((url (qutebrowser-bookmark-url bookmark)))
    (qutebrowser-open-url url)))

(defun qutebrowser-theme-export ()
  "Export selected Emacs faces to Qutebrowser theme format."
  (interactive)
  (with-temp-file "~/.config/qutebrowser/emacs_theme.py"
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
  (qutebrowser-theme-export)
  ;; TODO only if qutebrowser is running
  (qutebrowser-config-source "~/.config/qutebrowser/emacs_theme.py"))

;;;###autoload
(define-minor-mode qutebrowser-theme-export-mode
  "Minor mode to automatically export Emacs theme to Qutebrowser."
  :lighter nil
  :global t
  (if qutebrowser-theme-export-mode
      (advice-add 'enable-theme :after #'qutebrowser-theme-export-and-apply)
    (advice-remove 'enable-theme #'qutebrowser-theme-export-and-apply)))

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

(defun qutebrowser-pass--find-matching (pattern)
  "Return list of password-store entries matching PATTERN."
  (cl-remove-if-not
   (lambda (entry)
     (string-match-p pattern entry))
   (password-store-list)))

;;;###autoload
(defun qutebrowser-pass (url &optional limit)
  "Autofill username and password matching URL.
LIMIT can be :password-only, :username-only, or nil."
  (let* ((domain (url-domain (url-generic-parse-url url)))
         (pass-entries (qutebrowser-pass--find-matching domain))
         (selected (completing-read "Select: " pass-entries))
         (username (car (last (string-split selected "/"))))
         (password (password-store-get selected)))
    (unless (eq :password-only limit)
      (qutebrowser-fake-keys username))
    ;; Only tab when inputting both username and password
    (unless limit
      (qutebrowser-fake-keys--raw "<Tab>"))
    (unless (eq :username-only limit)
      (qutebrowser-fake-keys password))
    (qutebrowser-fake-keys--raw "<Return>")))

;;;###autoload
(defun qutebrowser-pass-username-only (url)
  "Autofill username matching URL."
  (qutebrowser-pass url :username-only))

;;;###autoload
(defun qutebrowser-pass-password-only (url)
  "Autofill password matching URL."
  (qutebrowser-pass url :password-only))

(defun qutebrowser-pass-otp (url)
  ;; TODO: Add interactive
  "Autofill OTP code matching URL."
  (let* ((domain (url-domain (url-generic-parse-url url)))
         (pass-entries (qutebrowser-pass--find-matching domain))
         (selected (completing-read "Select: " pass-entries))
         (token (password-store-otp-token selected)))
    (qutebrowser-fake-keys token)
    (qutebrowser-fake-keys--raw "<Return>")))

(defun qutebrowser--signal-mode-enter (mode)
  "Called by Qutebrowser when entering a MODE."
  (run-hooks 'qutebrowser-mode-enter-hook))

(defun qutebrowser--signal-mode-leave (mode)
  "Called by Qutebrowser when leaving a MODE."
  (run-hooks 'qutebrowser-mode-leave-hook))

(defun qutebrowser--signal-new-window (win-id)
  "Called by Qutebrowser when creating a new window WIN-ID."
  (run-hooks 'qutebrowser-new-window-hook))

;; This triggers ~300 times (maybe once per line?)
(defun qutebrowser--signal-config-changed ()
  "Called by Qutebrowser when any configuration option changes."
  (run-hooks 'qutebrowser-config-changed-hook))

(defun qutebrowser--signal-url-changed (win-id url)
  "Called by Qutebrowser when the URL changes in WIN-ID."
  (run-hooks 'qutebrowser-url-search-hook))

(defvar qutebrowser-current-search nil
  "Contains the current search terms of Qutebrowser.")

(defun qutebrowser--signal-got-search (search)
  "Called by Qutebrowser when a SEARCH is performed."
  (setq qutebrowser-current-search search)
  (run-hooks 'qutebrowser-got-search-hook))

(defun qutebrowser--get-process-pid ()
  "Return a list of PIDs for Qutebrowser processes."
  (cl-remove-if-not
   (lambda (pid)
     (let* ((attrs (process-attributes pid))
            (cmd (alist-get 'comm attrs)))
       (string= ".qutebrowser-re" cmd)))
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

(define-minor-mode qutebrowser-config-mode
  "Minor mode for editing Qutebrowser config files."
  :lighter nil
  :global nil
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'qutebrowser-config-source-file)
    (define-key map (kbd "C-c C-b") #'qutebrowser-config-source-buffer)
    (define-key map (kbd "C-c C-r") #'qutebrowser-config-source-region)
    map))

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


(provide 'qutebrowser)

;;; qutebrowser.el ends here
