;;; qutebrowser-consult.el --- Consult completion for Qutebrowser -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Isaac Haller & Lars Rustand.

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

;; Consult-based completion for Qutebrowser buffers, history,
;; commands, and bookmarks.

;;; Change Log:

;;; Code:
(require 'qutebrowser)
(require 'consult)

;;;; Helper functions
(defun qutebrowser-consult--tofu-strip-lines (lines)
  "Strip tofus from LINES."
  (mapcar #'qutebrowser--tofu-strip lines))

;;;; Buffer source
(defvar qutebrowser-consult--exwm-buffer-source
  (list :name "Qutebrowser buffers"
        :hidden nil
        :narrow ?q
        :history nil
        :category 'other
        :action (lambda (entry)
		  (switch-to-buffer (qutebrowser--tofu-get-buffer entry)))
        :annotate #'qutebrowser-annotate
        :items #'qutebrowser-exwm-buffer-search)
  "Consult source for open Qutebrowser windows.")

;;;; Bookmark source
(defvar qutebrowser-consult--bookmark-source
  (list :name "Qutebrowser bookmarks"
        :hidden nil
        :narrow ?m
        :history nil
        :category 'bookmark
        :face 'consult-bookmark
        :action #'qutebrowser-bookmark-jump
        :items #'qutebrowser-bookmarks-list)
  "Consult source for Qutebrowser bookmarks.")

;;;; Command source
(defvar qutebrowser-consult--command-history nil)

(defvar qutebrowser-consult--command-source
  (list :name "Qutebrowser commands"
	:hidden nil
	:narrow ?:
	:history nil
	:category 'other
	:action #'qutebrowser-send-commands
	:async
	(consult--async-pipeline
	 (consult--async-min-input 0)
	 (consult--async-throttle)
	 (consult--async-dynamic (lambda (input)
				   (let ((words (string-split (or input ""))))
				     (qutebrowser-command-search words))))
	 (consult--async-transform #'qutebrowser-consult--tofu-strip-lines)))
  "Consult source for Qutebrowser commands.")

(defun qutebrowser-consult-command ()
  "Command entry for Qutebrowser based on Consult."
  (interactive)
  (let* ((consult-async-min-input 0)
	 (consult-async-split-style nil)
	 (selected (consult--multi '(qutebrowser-consult--command-source)
				   :prompt "Command: "
				   :initial ":"
				   :history 'qutebrowser-consult--command-history)))
    (unless (plist-get (cdr selected) :match)
      (qutebrowser-send-commands (car selected)))))

(provide 'qutebrowser-consult)

;;; qutebrowser-consult.el ends here
