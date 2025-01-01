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


(defvar qutebrowser-evil-state-mappings
  '(("KeyMode.insert" . evil-insert-state)
    ("KeyMode.caret" . evil-visual-char)
    ("KeyMode.hint" . evil-motion-state)
    ("KeyMode.command" . evil-emacs-state)
    ("KeyMode.normal" . evil-normal-state)
    ("KeyMode.passthrough" . evil-emacs-state)))

(defun qutebrowser-evil--update-state (window-info)
  "Set evil state to match Qutebrowser keymode from WINDOW-INFO."
  (when-let* ((x11-win-id (plist-get window-info :x11-win-id))
              (buffer (exwm--id->buffer x11-win-id)))
    (with-current-buffer buffer
      (qutebrowser--with-plist-key mode window-info
        (let ((func (alist-get mode qutebrowser-evil-state-mappings
                               'evil-operator-state nil 'string-equal)))
          (funcall func))))))

;;;###autoload
(define-minor-mode qutebrowser-evil-state-mode
  "Minor mode to synchronize the evil state with the Qutebrowser KeyMode."
  :lighter nil
  :global t
  (if qutebrowser-evil-state-mode
      (add-hook 'qutebrowser-update-window-info-functions
                #'qutebrowser-evil--update-state)
    (remove-hook 'qutebrowser-update-window-info-functions
                 #'qutebrowser-evil--update-state)))


(provide 'qutebrowser-evil)
