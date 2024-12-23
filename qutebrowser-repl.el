;;; qutebrowser-repl.el --- Qutebrowser remote REPL     -*- lexical-binding: t; -*-

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

;;; Commentary:

;;; Change Log:

;;; Code:

(require 'qutebrowser)


(defvar qutebrowser-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'qutebrowser-repl-send-input)
    (define-key map (kbd "<up>") 'qutebrowser-repl-previous-input)
    (define-key map (kbd "<down>") 'qutebrowser-repl-next-input)
    map)
  "Keymap used in `qutebrowser-repl-mode' buffers.")

(defvar qutebrowser-repl-prompt ">>> ")

(defvar qutebrowser-repl-history '()
  "List to store command history for Qutebrowser REPL.")

(defvar qutebrowser-repl-history-position 0
  "Current position in the command history.")

(defun qutebrowser-create-repl-buffer ()
  "Get existing Qutebrowser REPL buffer or create a new one."
  (if-let ((repl-buffer (get-buffer "*Qutebrowser REPL*")))
      repl-buffer
    (with-current-buffer (get-buffer-create "*Qutebrowser REPL*")
      (qutebrowser-repl-mode)
      (insert
       (propertize qutebrowser-repl-prompt 'read-only t 'rear-nonsticky t))
      (current-buffer))))

(defun qutebrowser-repl-send-input ()
  "Send the current input to Qutebrowser."
  (interactive)
  (let ((input (buffer-substring-no-properties
                (+ (line-beginning-position)
                   (length qutebrowser-repl-prompt))
                (point-max))))
    (push input qutebrowser-repl-history)
    (setq qutebrowser-repl-history-position 0)
    (goto-char (point-max))
    (insert "\n")
    (insert (qutebrowser-rpc-request "repl" `(:input ,input)) "\n")
    (insert qutebrowser-repl-prompt)
    (let ((inhibit-read-only t))
      (add-text-properties (point-min) (point-max)
                           '(read-only t rear-nonsticky t)))))

(defun qutebrowser-repl-previous-input ()
  "Cycle backwards through input history."
  (interactive)
  (when (< qutebrowser-repl-history-position
           (length qutebrowser-repl-history))
    (setq qutebrowser-repl-history-position
          (1+ qutebrowser-repl-history-position))
    (qutebrowser-repl-replace-input
     (nth (1- qutebrowser-repl-history-position)
          qutebrowser-repl-history))))

(defun qutebrowser-repl-next-input ()
  "Cycle forwards through input history."
  (interactive)
  (when (> qutebrowser-repl-history-position 0)
    (setq qutebrowser-repl-history-position
          (1- qutebrowser-repl-history-position))
    (if (zerop qutebrowser-repl-history-position)
        (qutebrowser-repl-replace-input "")
      (qutebrowser-repl-replace-input
       (nth (1- qutebrowser-repl-history-position)
            qutebrowser-repl-history)))))

(defun qutebrowser-repl-replace-input (new-input)
  "Replace the current input with NEW-INPUT."
  (delete-region (+ (line-beginning-position)
                    (length qutebrowser-repl-prompt))
                 (point-max))
  (insert new-input))

(define-derived-mode qutebrowser-repl-mode fundamental-mode "Qutebrowser REPL"
  "Major mode for Qutebrowser REPL."
  (use-local-map qutebrowser-repl-mode-map))

;;;###autoload
(defun qutebrowser-start-repl ()
  "Start Qutebrowser REPL and switch to the buffer."
  (interactive)
  (switch-to-buffer (qutebrowser-create-repl-buffer)))


(provide 'qutebrowser-repl)

;;; qutebrowser-repl.el ends here
