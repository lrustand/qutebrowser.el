;;; qutebrowser-consult.el --- Consult completion for Qutebrowser -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isaac Haller & Lars Rustand.

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
;; commands, and bookmarks. The sources provided in this file can be
;; added as additional sources to 'consult-buffer' or similar. See
;; 'consult-buffer-sources' and 'consult--multi'.

;;; Change Log:

;;; Code:
(require 'qutebrowser)
(require 'consult)

(defgroup qutebrowser-consult nil
  "Consult completion for Qutebrowser."
  :group 'qutebrowser
  :prefix "qutebrowser-consult")

;;;; Helper functions
(defun qutebrowser-consult--format-entry (entry)
  "Modify ENTRY for presentation."
  (qutebrowser--shorten-display-url (qutebrowser--tofu-strip entry)))

(defun qutebrowser-consult--annotate (entry)
  "Return annotation for ENTRY."
  (qutebrowser--shorten-display-url entry)
  (propertize (get-text-property 0 'title entry)
	      'face 'completions-annotations))

;;;; buffer source
(defvar qutebrowser-consult--exwm-buffer-source
  (list :name "Qutebrowser buffers"
        :hidden nil
        :narrow ?q
        :history nil
        :category 'url
        :action (lambda (entry)
		  (switch-to-buffer (qutebrowser--tofu-get-buffer entry)))
        :annotate #'qutebrowser-consult--annotate
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
        :annotate #'qutebrowser-consult--annotate
	:async
	(consult--dynamic-collection
	    (lambda (input)
	      (qutebrowser-command-search (string-split (or input ""))))
	  :min-input 0
	  :throttle 0
	  :debounce 0
	  :transform (consult--async-map #'qutebrowser-consult--format-entry)))
  "Consult source for Qutebrowser commands.")

;;;###autoload
(defun qutebrowser-consult-command (&optional initial)
  "Command entry for Qutebrowser based on Consult.
Set initial completion input to INITIAL."
  (interactive)
  (let* ((consult-async-min-input 0)
	 (consult-async-split-style nil)
	 (selected
	  (consult--multi `(,(plist-put (seq-copy qutebrowser-consult--command-source)
					:name nil))
			  :prompt "Command: "
			  :initial (or initial ":")
			  :history 'qutebrowser-consult--command-history)))
    (qutebrowser-send-commands selected)))

;;;; History source
(defvar qutebrowser-consult--history-source
  (list :name "Qutebrowser history"
	:hidden nil
	:narrow ?h
	:history nil
	:category 'url
	:action #'qutebrowser-open-url
	:new #'qutebrowser-open-url
	:annotate #'qutebrowser-consult--annotate
	:async
	(consult--dynamic-collection
	    (lambda (input)
	      (qutebrowser--history-search (string-split (or input ""))
					   qutebrowser-dynamic-results))
	  :min-input 0
	  :throttle 0
	  :debounce 0
	  :highlight t
	  :transform (consult--async-map #'qutebrowser-consult--format-entry)))
  "Consult source for Qutebrowser history.")

;;;; `qutebrowser-launcher' replacement
(defvar qutebrowser-consult-launcher-sources
  '(qutebrowser-consult--command-source
    qutebrowser-consult--exwm-buffer-source
    qutebrowser-consult--bookmark-source
    qutebrowser-consult--history-source)
  "Sources used by `qutebrowser-launcher' and family.")

(defun qutebrowser-consult--suppress-action (source)
  "Return SOURCE with no action."
  (let ((new-source (seq-copy source)))
    (plist-put new-source :action nil)))

;; NOTE does this still need to exist?
(defun qutebrowser-consult-select-url (&optional initial default)
  "Dynamically select a URL, buffer, or command using consult.
INITIAL sets the initial input in the minibuffer."
  (let ((consult-async-min-input 0)
        (consult-async-split-style nil)
	(sources (list qutebrowser-consult--command-source
		       qutebrowser-consult--exwm-buffer-source
		       qutebrowser-consult--bookmark-source
		       qutebrowser-consult--history-source)))
    (consult--multi (mapcar #'qutebrowser-consult--suppress-action sources))
    :prompt (if default
                (format "Select (default %s): " default)
              "Select: ")
    :default default
    :sort nil
    :initial initial
    :require-match nil)))

;;;###autoload
(defun qutebrowser-consult-launcher (&optional initial target)
  "Select a URL to open in Qutebrowser using Consult.

Set initial completion input to INITIAL. Open the URL in TARGET or the
default target if nil.

Modify `qutebrowser-consult-launcher-sources' to change which sources
are included."
  (interactive)
  (let* ((consult-async-split-style nil)
	 (qutebrowser-default-open-target (or target qutebrowser-default-open-target))
	 (selected (consult--multi qutebrowser-consult-launcher-sources
				   :initial initial
				   :sort nil)))
    (unless (plist-get (cdr selected) :match)
      (qutebrowser-open-url (car selected)))))

;;;###autoload
(defun qutebrowser-consult-launcher-tab (&optional initial)
  "Select a URL to open in a new tab using Consult.

Set initial completion input to INITIAL.
Modify `qutebrowser-consult-launcher-sources' to change which sources
are included."
  (interactive)
  (qutebrowser-consult-launcher initial 'tab))

;;;###autoload
(defun qutebrowser-consult-launcher-window (&optional initial)
  "Select a URL to open in a new window using Consult.

Set initial completion input to INITIAL.
Modify `qutebrowser-consult-launcher-sources' to change which sources
are included."
  (interactive)
  (qutebrowser-consult-launcher initial 'window))

;;;###autoload
(defun qutebrowser-consult-launcher-private (&optional initial)
  "Select a URL to open in a private window using Consult.

Set initial completion input to INITIAL.
Modify `qutebrowser-consult-launcher-sources' to change which sources
are included."
  (interactive)
  (qutebrowser-consult-launcher initial 'private-window))

(define-minor-mode qutebrowser-consult-mode
  "Use Consult for Qutebrowser completion."
  :lighter nil
  :global t
  (if qutebrowser-consult-mode
      (progn
	(advice-add 'qutebrowser-launcher :override #'qutebrowser-consult-launcher)
	(advice-add 'qutebrowser-launcher-tab :override #'qutebrowser-consult-launcher-tab)
	(advice-add 'qutebrowser-launcher-window :override #'qutebrowser-consult-launcher-window)
	(advice-add 'qutebrowser-launcher-private :override #'qutebrowser-consult-launcher-private))
    (advice-remove 'qutebrowser-launcher #'qutebrowser-consult-launcher)
    (advice-remove 'qutebrowser-launcher-tab #'qutebrowser-consult-launcher-tab)
    (advice-remove 'qutebrowser-launcher-window #'qutebrowser-consult-launcher-window)
    (advice-remove 'qutebrowser-launcher-private #'qutebrowser-consult-launcher-private)))

(provide 'qutebrowser-consult)

;;; qutebrowser-consult.el ends here
