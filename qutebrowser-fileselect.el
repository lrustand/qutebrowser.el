;;; qutebrowser-fileselect.el --- File selector for Qutebrowser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Sergey Trofimov.

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
;; 
;;; Commentary:
;; Setting fileselect.handler to "external" makes qutebrowser launch the
;; specified command with a single argument - a temporary file where the paths
;; of selected files should go. The file name is prefixed with
;; "qutebrowser-fileselect-". `qutebrowser-fileselect-mode' installs a
;; `server-visit-hook' that looks for that prefix and calls
;; `qutebrowser-fileselect-handler' when found. The handler receives a buffer as
;; an argument where selected paths has to be written. In all cases the buffer
;; must be saved and killed before returning, even when the user aborts file
;; selection with C-g.
;;
;; Relevant configuration options for config.py:
;;   c.fileselect.handler = "external"
;;   c.fileselect.single_file.command = ["emacsclient", "{}"]
;;   c.fileselect.folder.command = ["emacsclient", "{}"]
;;   c.fileselect.multiple_files.command = ["emacsclient", "{}"]
;;
;;; Code:
(require 'dired)

(defcustom qutebrowser-fileselect-handler
  #'qutebrowser-fileselect-completion
  "Fileselect handler."
  :group 'qutebrowser
  :type '(radio
          (function-item qutebrowser-fileselect-completion)
          (function-item qutebrowser-fileselect-dired)
          function))

(defun qutebrowser-fileselect-dired (buffer)
  "Select files using Dired."
  (bind-key "C-c C-c"
            (lambda ()
              (interactive)
              (let ((files (dired-get-marked-files)))
                (with-current-buffer buffer
                  (dolist (f files) (insert f "\n"))
                  (let ((save-silently t)) (basic-save-buffer))
                  (kill-buffer))
                (unbind-key "C-C C-c" 'dired-mode-map))
              (kill-buffer))
            'dired-mode-map)

  (dired "~/"))

(defun qutebrowser-fileselect-completion (buffer)
  "Select a file using `read-file-name'."
  (with-current-buffer buffer
    (condition-case nil
        (insert
         (expand-file-name
          (read-file-name "Select file: " "~/" default-directory 'mustmatch)))
      (quit nil))
    (let ((save-silently t)) (basic-save-buffer))
    (kill-buffer)))

(defun qutebrowser--fileselect (&rest _)
  "Call `qutebrowser-fileselect-handler'.
The handler receives as an argument a buffer where the selected files
should be written. files should be written."
  (when (string-prefix-p "qutebrowser-fileselect-"
                         (file-name-nondirectory buffer-file-name))
    ;; run after `server-visit-hook' exits to not mess up its logic
    (run-at-time 0 nil qutebrowser-fileselect-handler (current-buffer))))

;;;###autoload
(define-minor-mode qutebrowser-fileselect-mode
  "Minor mode indicating active file selection."
  :lighter nil
  :group 'qutebrowser
  :global t
  (if qutebrowser-fileselect-mode
      (add-hook 'server-visit-hook #'qutebrowser--fileselect)
    (remove-hook 'server-visit-hook #'qutebrowser--fileselect)))

(provide 'qutebrowser-fileselect)
;;; qutebrowser-fileselect.el ends here.
