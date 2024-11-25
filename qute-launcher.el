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
(require 'marginalia)
(require 'consult)
(require 'exwm)
(require 'json)

(defgroup qutebrowser nil
  "Customizing EXWM enhancements for Qutebrowser.")

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
           (completion.match.fg . iedit-occurrence)
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
           (keyhint.bg . default)
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
           (prompts.bg . default)
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
  "The default open target for Qutebrowser.
Valid values are 'auto, 'tab, 'window, 'private-window."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Tab" tab)
                 (const :tag "Window" window)
                 (const :tag "Private Window" private-window))
  :group 'qutebrowser)

(defcustom qutebrowser-history-database
  "~/.local/share/qutebrowser/history.sqlite"
  "Path to the Qutebrowser history database."
  :type 'file
  :group 'qutebrowser)

(defun qutebrowser--history ()
  "Get the Qutebrowser history from the sqlite database."
  (let ((db (sqlite-open qutebrowser-history-database)))
    (sqlite-select db "SELECT url,substr(title,0,99)
                       FROM History
                       WHERE url NOT LIKE 'https://www.google.%/search?%'
                         AND url NOT LIKE 'https://www.google.com/sorry/%'
                         AND url NOT LIKE 'https://%youtube.com/results?%'
                         AND url NOT LIKE 'https://%perplexity.ai/search/%'
                         AND url NOT LIKE 'https://%/search?%'
                         AND url NOT LIKE 'https://%?search=%'
                         AND url NOT LIKE 'https://%/search/?%'
                         AND url NOT LIKE 'https://%/search_result?%'
                         AND url NOT LIKE 'https://www.finn.no/%/search.html?%'
                         AND url NOT LIKE 'https://www.finn.no/globalsearchlander?%'
                         AND url NOT LIKE 'https://%ebay.%/sch/%'
                         AND url NOT LIKE 'https://%amazon.%/s?%'
                         AND url NOT LIKE 'https://%duckduckgo.com/?%q=%'
                       GROUP BY url
                       ORDER BY
                       COUNT(url) DESC")))

(defun qutebrowser--pseudo-annotate (row &optional buffer)
  "Create pseudo-annotated entries from each ROW.
This simulates marginalia annotations, but allows the text in the
annotation to be searchable. Optionally embeds BUFFER as a text
property."
  (let* ((url (nth 0 row))
         (title (nth 1 row))
         (display-url (truncate-string-to-width url 50 0 ?\ )))
    (format "%s %s"
            (propertize display-url
                        'url url
                        'title title
                        'buffer buffer)
            (propertize title
                        'face 'marginalia-value))))

(defun qutebrowser--history-candidates ()
  "Lists completion candidates from Qutebrowser history.
Candidates contain the url, and a pseudo-annotation with the
website title, to allow searching based on either one."
  (let* ((history (qutebrowser-history)))
    (mapcar #'qutebrowser--pseudo-annotate
            history)))

(defun qutebrowser--target-to-flag (target)
  "Return the flag for TARGET."
  (pcase target
    ('window "-w")
    ('tab "-t")
    ('private-window "-p")
    ('auto "")))

(defun qutebrowser-launcher--internal (&optional url initial)
  "Internal dispatcher for the user-facing commands.
URL is the url to open, and INITIAL is the initial input for completion."
  (if url
      (qutebrowser-ipc-open-url url)
    (let* ((res (consult--multi '(qutebrowser--exwm-buffer-source
                                  qutebrowser--history-source)
                                :initial initial
                                :sort nil))
           (plist (cdr res))
           (selected (car res)))
      ;; If none of the buffer sources handled it
      (unless (plist-get plist :match)
        (qutebrowser-ipc-open-url selected)))))

(defun qutebrowser-launcher (&optional url _ prefilled)
  "Open URL in Qutebrowser in the default target.
Set initial completion input to PREFILLED."
  (interactive)
  (qutebrowser-launcher--internal url prefilled))

(defun qutebrowser-launcher-tab (&optional url _ prefilled)
  "Open URL in Qutebrowser in a new tab.
Set initial completion input to PREFILLED."
  (interactive)
  (let ((qutebrowser-default-open-target 'tab))
    (qutebrowser-launcher--internal url prefilled)))

(defun qutebrowser-launcher-window (&optional url _ prefilled)
  "Open URL in Qutebrowser in a new window.
Set initial completion input to PREFILLED."
  (interactive)
  (let ((qutebrowser-default-open-target 'window))
    (qutebrowser-launcher--internal url prefilled)))

(defun qutebrowser-launcher-private (&optional url _ prefilled)
  "Open URL in Qutebrowser in a private window.
Set initial completion input to PREFILLED."
  (interactive)
  (let ((qutebrowser-default-open-target 'private-window))
    (qutebrowser-launcher--internal url prefilled)))


(defun qutebrowser--format-window-entry (buffer)
  "Format a `consult--multi' entry for BUFFER.
Expects the `buffer-name' of BUFFER to be propertized with a url field."
  (let* ((bufname (buffer-name buffer))
         (title (substring-no-properties bufname))
         (url (get-text-property 0 'url bufname)))
    (cons
     (qutebrowser--pseudo-annotate
      (list
       (truncate-string-to-width (or url "") 50)
       (truncate-string-to-width title 99)))
     buffer)))

(defun qutebrowser-exwm-p (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (string-equal "qutebrowser"
                  exwm-class-name)))

(defvar qutebrowser--exwm-buffer-source
  `(;; consult-buffer source for open Qutebrowser windows
    :name "Qutebrowser buffers"
    :hidden nil
    :narrow ?q
    :category buffer
    :action ,#'switch-to-buffer
    :items
    ,(lambda () (consult--buffer-query
                 :sort 'visibility
                 :as #'qutebrowser--format-window-entry
                 :predicate #'qutebrowser-exwm-p
                 :mode 'exwm-mode))))


(defvar qutebrowser--history-source
  `(;; consult-buffer source for Qutebrowser history
    :name "Qutebrowser history"
    :hidden nil
    :narrow ?h
    :history nil
    :category buffer
    :action ,(lambda (entry)
               (let ((url (or (get-text-property 0 'url entry)
                              entry)))
                 (qutebrowser-ipc-open-url url)))
    :items ,#'qutebrowser--history-candidates))


(defvar qutebrowser-ipc-protocol-version 1
  "The protocol version for Qutebrowser IPC.")

(defun qutebrowser-ipc-socket-path ()
  "Return the path to Qutebrowser's IPC socket."
  (expand-file-name
   (format "qutebrowser/ipc-%s" (md5 (user-login-name)))
   (or (getenv "XDG_RUNTIME_DIR")
       (format "/run/user/%d" (user-real-uid)))))

(defun qutebrowser-ipc-send (&rest commands)
  "Send COMMANDS to Qutebrowser via IPC."
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
       (apply #'start-process "qutebrowser" nil "qutebrowser" commands)))
    (error
     (message "Unexpected error in qutebrowser-ipc-send: %s" (error-message-string err)))))

(defun qutebrowser-ipc-open-url (url &optional target)
  "Open URL in Qutebrowser through IPC."
  (let* ((target (or target qutebrowser-default-open-target))
         (flag (qutebrowser--target-to-flag target)))
    (qutebrowser-ipc-send (format ":open %s %s" flag url))))

(define-minor-mode qutebrowser-exwm-mode
  "Minor mode for Qutebrowser buffers in EXWM."
  :lighter nil
  :global nil
  (when qutebrowser-exwm-mode
    (setq-local bookmark-make-record-function
                #'qutebrowser-bookmark-make-record)))

(defun qutebrowser-bookmark-make-record ()
  "Make a bookmark record for Qutebrowser buffers."
  `(,(buffer-name)
    (handler . qutebrowser-bookmark-jump)
    (url . ,(get-text-property 0 'url (buffer-name)))))

(defun qutebrowser-bookmark-jump (bookmark)
  "Jump to a Qutebrowser bookmark in a new tab."
  (let ((url (bookmark-prop-get bookmark 'url)))
    (qutebrowser-launcher-tab url)))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (qutebrowser-exwm-p)
              (qutebrowser-exwm-mode 1))))


(defun qutebrowser-theme-export ()
  "Export selected Emacs faces to Qutebrowser theme format."
  (interactive)
  (with-temp-buffer
    (insert "# Qutebrowser theme exported from Emacs\n\n")
    (dolist (mapping qutebrowser-theme-export-face-mappings)
      (let* ((qute-face (symbol-name (car mapping)))
             (emacs-face (cdr mapping))
             (is-fg (string-match-p "\\.fg$" qute-face))
             (attribute (if is-fg :foreground :background))
             (color (face-attribute emacs-face attribute nil 'default)))
        (insert (format "c.colors.%s = '%s'\n" qute-face color))))
    (write-file "~/.config/qutebrowser/emacs_theme.py")))

(defun qutebrowser-theme-export-and-apply (&rest _)
  (qutebrowser-theme-export)
  (qutebrowser-ipc-send ":config-source ~/.config/qutebrowser/emacs_theme.py"))

;;;###autoload
(define-minor-mode qutebrowser-theme-export-mode
  "Minor mode to automatically export Emacs theme to Qutebrowser."
  :lighter nil
  :global t
  (if qutebrowser-theme-export-mode
      (advice-add 'enable-theme :after #'qutebrowser-theme-export-and-apply)
    (advice-remove 'enable-theme #'qutebrowser-theme-export-and-apply)))

(provide 'qutebrowser)

;;; qutebrowser.el ends here
