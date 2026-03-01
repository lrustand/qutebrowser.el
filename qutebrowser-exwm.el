;;; qutebrowser-exwm.el --- EXWM integration for Qutebrowser     -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "29.1") (exwm "0.28"))

;;; Commentary:

;; This package provides EXWM-specific integration for Qutebrowser,
;; including buffer-local variables tracking window state, favicon
;; management, and a minor mode for Qutebrowser EXWM buffers.
;;
;;; Code:

(require 'exwm)

;;;; Buffer-local variables

(defvar-local qutebrowser-exwm-win-id nil
  "Contains the internal Qutebrowser window ID.")

(defvar-local qutebrowser-exwm-keymode nil
  "Contains the current keymode of the Qutebrowser window.")

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

;;;; Favicon management

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

;;;; Window information update

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

;;;; Bookmark functions

(defun qutebrowser-exwm-bookmark-make-record ()
  "Make a bookmark record for Qutebrowser buffers."
  `(,(buffer-name)
    (handler . qutebrowser-bookmark-jump)
    (url . ,(qutebrowser-exwm-buffer-url))))

;;;; Utility functions

(defun qutebrowser-exwm-revert-buffer-function (&rest _)
  "Revert buffer function for Qutebrowser EXWM buffers.
This function is used as the `revert-buffer-function' in
`qutebrowser-exwm-mode', so that `revert-buffer' will reload the
webpage."
  (qutebrowser-send-commands ":reload"))

(defun qutebrowser-exwm-find-buffer (url)
  "Find the buffer showing URL."
  (seq-find (lambda (buffer)
              (string= url (qutebrowser-exwm-buffer-url buffer)))
            (qutebrowser-exwm-buffer-list)))

(defun qutebrowser-exwm--win-id->buffer (win-id)
  "Find the buffer with the given internal WIN-ID."
  (seq-find (lambda (buffer)
              (eq win-id (buffer-local-value 'qutebrowser-exwm-win-id buffer)))
            (qutebrowser-exwm-buffer-list)))

(defun qutebrowser-exwm-p (&optional buffer)
  "Return t if BUFFER is a Qutebrowser EXWM buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (bound-and-true-p exwm-class-name)
         (string-equal "qutebrowser" exwm-class-name))))

(defun qutebrowser-exwm-audible-p (&optional buffer)
  "Return t if BUFFER is an audible Qutebrowser EXWM buffer."
  (eq t
      (buffer-local-value 'qutebrowser-exwm-recently-audible (or (get-buffer buffer) (current-buffer)))))

(defun qutebrowser-exwm-private-p (&optional buffer)
  "Return t if BUFFER is a private Qutebrowser EXWM buffer."
  (eq t
      (buffer-local-value 'qutebrowser-exwm-private (or (get-buffer buffer) (current-buffer)))))

(defun qutebrowser-exwm-buffer-url (&optional buffer)
  "Return the URL of BUFFER or the current buffer."
  (buffer-local-value 'qutebrowser-exwm-current-url (or buffer (current-buffer))))

(defun qutebrowser-exwm-buffer-list ()
  "Return a list of all Qutebrowser buffers."
  (seq-filter #'qutebrowser-exwm-p (buffer-list)))

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

                     (icon-string (propertize "" 'display icon)))
                (propertize (qutebrowser--shorten-display-url url)
			    :qutebrowser-candidate-type 'buffer
			    :qutebrowser-buffer buffer
                            :qutebrowser-title (if qutebrowser-launcher-show-icons
						   (concat icon-string " " title)
						 title))))
            matching-buffers)))

;;;; Minor mode

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
                    #'qutebrowser-exwm-revert-buffer-function)
        (setq-local bookmark-make-record-function
                    #'qutebrowser-exwm-bookmark-make-record))
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

;; Add window info update hook
(add-hook 'qutebrowser-update-window-info-functions #'qutebrowser-exwm--update-window-info)

(provide 'qutebrowser-exwm)

;;; qutebrowser-exwm.el ends here
