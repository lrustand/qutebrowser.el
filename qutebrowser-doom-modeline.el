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

(defun qutebrowser-doom-set-favicon (&rest _)
  "Show favicon in doom modeline."
  (with-current-buffer (current-buffer)
    (when-let* ((image qutebrowser-exwm-favicon))
      (setq-local doom-modeline--buffer-file-icon
                  (propertize ""
                              'display image
                              'face '(:inherit doom-modeline))))))

(doom-modeline-def-segment qutebrowser-url
  "Display the currently visited or hovered URL."
  (replace-regexp-in-string "%" "%%" ;; Avoid formatting nonsense
                            (doom-modeline-display-text
                             (concat " " (if qutebrowser-exwm-hovered-url
                                             (propertize qutebrowser-exwm-hovered-url 'face 'link-visited)
                                           (propertize (or qutebrowser-exwm-current-url "") 'face 'success))))))

(doom-modeline-def-modeline 'qutebrowser-doom-modeline
  '(bar workspace-name window-number modals buffer-info-simple)
  '(misc-info qutebrowser-url))

;;;###autoload
(define-minor-mode qutebrowser-doom-modeline-mode
  "Minor mode combining Qutebrowser statusbar and Doom modeline."
  :lighter nil
  :global nil
  (if qutebrowser-doom-modeline-mode
      (progn
        (qutebrowser-rpc-get-connection)
        (add-hook 'qutebrowser-on-icon-changed-functions
                  #'qutebrowser-doom-set-favicon nil t)
        (doom-modeline-set-modeline 'qutebrowser-doom-modeline))
    (progn
      (doom-modeline-set-modeline 'main)
      (remove-hook 'qutebrowser-on-icon-changed-functions
                   #'qutebrowser-doom-set-favicon t))))

(defun qutebrowser-doom-modeline-mode-maybe-enable ()
  "Enable `qutebrowser-doom-modeline-mode' if the buffer is a Qutebrowser buffer."
  (when (qutebrowser-exwm-p)
    (qutebrowser-doom-modeline-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-qutebrowser-doom-modeline-mode
  qutebrowser-doom-modeline-mode
  qutebrowser-doom-modeline-mode-maybe-enable
  (if global-qutebrowser-doom-modeline-mode
      (add-hook 'exwm-manage-finish-hook #'qutebrowser-doom-modeline-mode-maybe-enable)
    (remove-hook 'exwm-manage-finish-hook #'qutebrowser-doom-modeline-mode-maybe-enable)))


(provide 'qutebrowser-doom-modeline)

;;; qutebrowser-doom-modeline.el ends here
