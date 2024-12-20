;;; qutebrowser-doom-modeline.el --- Doom modeline for Qutebrowser     -*- lexical-binding: t; -*-

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
(require 'doom-modeline)

(defsubst qutebrowser-doom--favicon ()
  "Show favicon in doom modeline."
  (propertize ""
              'display (or qutebrowser-exwm-favicon "")
              'face '(:inherit doom-modeline)))

(defsubst qutebrowser-doom--title ()
  "Qutebrowser title."
  (propertize "%b"
              'face (doom-modeline-face 'doom-modeline-buffer-file)
              'mouse-face 'doom-modeline-highlight
              'help-echo "Qutebrowser title
mouse-1: Previous buffer\nmouse-3: Next buffer"
              'local-map mode-line-buffer-identification-keymap))

(defun qutebrowser-doom-modeline--update (&rest _)
  (force-mode-line-update))

(doom-modeline-def-segment qutebrowser-url
  "Display the currently visited or hovered URL."
  (let ((url (or qutebrowser-exwm-hovered-url qutebrowser-exwm-current-url ""))
        (face (if qutebrowser-exwm-hovered-url 'link-visited 'success)))
    (doom-modeline-display-text
     (replace-regexp-in-string "%" "%%" ;; Avoid formatting nonsense
                               (concat (doom-modeline-spc)
                                       (propertize url 'face face))))))

(doom-modeline-def-segment qutebrowser-title
  (concat
   (qutebrowser-doom--favicon)
   (doom-modeline-spc)
   (qutebrowser-doom--title)))

(doom-modeline-def-modeline 'qutebrowser-doom-modeline
  '(bar workspace-name window-number modals qutebrowser-title)
  '(misc-info qutebrowser-url))

;;;###autoload
(define-minor-mode qutebrowser-doom-modeline-mode
  "Minor mode combining Qutebrowser statusbar and Doom modeline."
  :lighter nil
  :global nil
  (if qutebrowser-doom-modeline-mode
      (progn
        (qutebrowser-rpc-get-connection)
        (add-hook 'qutebrowser-on-url-changed-functions
                  #'qutebrowser-doom-modeline--update nil t)
        (add-hook 'qutebrowser-on-link-hovered-functions
                  #'qutebrowser-doom-modeline--update nil t)
        (doom-modeline-set-modeline 'qutebrowser-doom-modeline))
    (progn
      (doom-modeline-set-modeline 'main)
      (remove-hook 'qutebrowser-on-url-changed-functions
                   #'qutebrowser-doom-modeline--update t)
      (remove-hook 'qutebrowser-on-link-hovered-functions
                   #'qutebrowser-doom-modeline--update t))))

(defun qutebrowser-doom-modeline-mode-maybe-enable ()
  "Enable `qutebrowser-doom-modeline-mode' if the buffer is a Qutebrowser buffer."
  (when (qutebrowser-exwm-p)
    (qutebrowser-doom-modeline-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-qutebrowser-doom-modeline-mode
  qutebrowser-doom-modeline-mode
  qutebrowser-doom-modeline-mode-maybe-enable
  :group 'qutebrowser
  (if global-qutebrowser-doom-modeline-mode
      (add-hook 'exwm-manage-finish-hook #'qutebrowser-doom-modeline-mode-maybe-enable)
    (remove-hook 'exwm-manage-finish-hook #'qutebrowser-doom-modeline-mode-maybe-enable)))


(provide 'qutebrowser-doom-modeline)

;;; qutebrowser-doom-modeline.el ends here
