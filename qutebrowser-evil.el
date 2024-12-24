;;; qutebrowser-evil.el --- Evil integration for Qutebrowser     -*- lexical-binding: t; -*-

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
(require 'evil)
(require 'exwm)


(defun qutebrowser-evil-update-state (window-info)
  "Set evil state to match Qutebrowser keymode from WINDOW-INFO."
  (when-let* ((win-id (plist-get window-info :win-id))
              (buffer (exwm--id->buffer win-id)))
    (with-current-buffer buffer
      (qutebrowser--with-plist-key mode window-info
        (pcase mode
          ("KeyMode.insert" (evil-insert-state))
          ("KeyMode.caret" (evil-visual-state))
          ("KeyMode.hint" (evil-motion-state))
          ("KeyMode.command" (evil-emacs-state))
          ("KeyMode.normal" (evil-normal-state)))))))

(add-hook 'qutebrowser-update-window-info-functions
          #'qutebrowser-evil-update-state)


(provide 'qutebrowser-evil)
