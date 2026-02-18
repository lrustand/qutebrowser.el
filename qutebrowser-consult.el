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

;; To use Consult for all Qutebrowser-related selection, set
;; `qutebrowser-selection-function' to `qutebrowser-consult-select-url'.

;;; Change Log:

;;; Code:
(require 'qutebrowser)
(require 'consult)

(defgroup qutebrowser-consult nil
  "Consult completion for Qutebrowser."
  :group 'qutebrowser
  :prefix "qutebrowser-consult")

(defcustom qutebrowser-consult-launcher-sources
  '(qutebrowser-consult--exwm-buffer-source
    qutebrowser-consult--bookmark-url-source
    qutebrowser-consult--history-source
    qutebrowser-consult--command-source)
  "Sources used by `qutebrowser-consult-launcher'."
  :type '(repeat symbol)
  :group 'qutebrowser-consult)

;;;; Helper functions
(defun qutebrowser-consult--annotate (entry)
  "Return annotation for ENTRY."
  (let* ((title (get-text-property 0 :qutebrowser-title entry))
         (timestamp (get-text-property 0 :qutebrowser-timestamp entry))
         (date-str (when timestamp (format-time-string "[%F] " timestamp))))
    (propertize (concat date-str title)
                'face 'completions-annotations)))

(defun qutebrowser-consult--format (entry)
  "Format ENTRY for completion."
  (let ((title (get-text-property 0 :qutebrowser-title entry)))
    (concat entry (propertize title 'invisible t))))

;;;; Buffer source
(defvar qutebrowser-consult--exwm-buffer-source
  (list :name "Qutebrowser buffers"
        :hidden nil
        :narrow ?q
        :history nil
        :category 'url
	:state #'qutebrowser-consult--exwm-buffer-state
        :annotate #'qutebrowser-consult--annotate
        :items (lambda ()
                 (mapcar #'qutebrowser-consult--format
                         (qutebrowser-exwm-buffer-search))))
  "Consult source for open Qutebrowser windows.")

(defun qutebrowser-consult--exwm-buffer-state ()
  "State function for Qutebrowser buffers with preview."
  (let ((state (consult--buffer-state)))
    (lambda (action cand)
      (funcall state action (qutebrowser--candidate-buffer cand)))))

;;;; Audio playing buffer source

(defvar qutebrowser-consult--audible-buffer-source
  (list :name "Qutebrowser audible buffers"
        :hidden nil
        :narrow ?a
        :history nil
        :category 'url
	:state #'qutebrowser-consult--exwm-buffer-state
        :annotate #'qutebrowser-consult--annotate
        :items (lambda ()
                 (qutebrowser-exwm-buffer-search nil
                                                 #'qutebrowser-exwm-audible-p)))
  "Consult source for audible Qutebrowser windows.")

;;;; Private buffer source

(defvar qutebrowser-consult--private-buffer-source
  (list :name "Qutebrowser private buffers"
        :hidden nil
        :narrow ?p
        :history nil
        :category 'url
	:state #'qutebrowser-consult--exwm-buffer-state
        :annotate #'qutebrowser-consult--annotate
        :items (lambda ()
                 (qutebrowser-exwm-buffer-search nil
                                                 #'qutebrowser-exwm-private-p)))
  "Consult source for private Qutebrowser windows.")

;;;; Bookmark source
(defvar qutebrowser-consult--bookmark-source
  (list :name "Qutebrowser bookmarks"
        :hidden nil
        :narrow ?m
        :history nil
        :category 'bookmark
        :face 'consult-bookmark
        :action #'qutebrowser-bookmark-jump
	:annotate #'qutebrowser-consult--annotate
        :items #'qutebrowser-bookmarks-list)
  "Consult source for Qutebrowser bookmarks.")

(defvar qutebrowser-consult--bookmark-url-source
  (list :name "Qutebrowser bookmarks"
        :hidden nil
        :narrow ?m
        :history nil
        :category 'url
        :action (lambda (url)
		  (qutebrowser-open-url
		   (string-remove-suffix
		    (get-text-property 0 :qutebrowser-title url)
		    url)))
	:annotate #'qutebrowser-consult--annotate
        :items (lambda ()
                 (mapcar #'qutebrowser-consult--format
                         (qutebrowser-bookmark-search))))
  "Consult source for Qutebrowser bookmark URLs.")

;;;; Command source
(defvar qutebrowser-consult--command-history nil)

(defvar qutebrowser-consult--command-source
  (list :name "Qutebrowser commands"
	:hidden nil
	:narrow ?:
	:history nil
	:category 'other
	:action #'qutebrowser-send-commands
	:new #'qutebrowser-send-commands
        :annotate #'qutebrowser-consult--annotate
	:items (apply-partially #'qutebrowser-command-search ":"))
  "Consult source for Qutebrowser commands.")

(defvar qutebrowser-consult--command-source-hidden
  `(:hidden t
     ,@qutebrowser-consult--command-source)
  "Like `qutebrowser-consult--command-source' but hidden by default.")

;;;###autoload
(defun qutebrowser-consult-command (&optional initial)
  "Command entry for Qutebrowser based on Consult.
Set initial completion input to INITIAL."
  (interactive)
  (let* ((consult-async-min-input 0)
	 (consult-async-split-style nil))
    (consult--multi '(qutebrowser-consult--command-source)
                    :group nil
		    :prompt "Command: "
		    :initial initial
		    :history 'qutebrowser-consult--command-history)))

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
	     (qutebrowser--history-search input))
	  :min-input 0
	  :throttle 0
	  :debounce 0
	  :highlight t))
  "Consult source for Qutebrowser history.")

;;;; Google autosuggest source
(defvar qutebrowser-consult--google-autosuggest-source
  (list :name "Google suggestions"
	:hidden nil
	:narrow ?g
	:history nil
	:category 'url
	:action #'qutebrowser-open-url
	:new #'qutebrowser-open-url
	:async
	(consult--dynamic-collection
            (lambda (input)
              (let* ((buffer (url-retrieve-synchronously (concat "https://suggestqueries.google.com/complete/search?client=firefox&q=" (url-hexify-string input))))
                     (json-array-type 'list)
                     json-data)
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (re-search-forward "^$")
                  (setq json-data (json-read)))
                (kill-buffer buffer)
                (cadr json-data)))
	  :min-input 0
	  :throttle 0
	  :debounce 0
	  :highlight t))
  "Consult source for Google search suggestions.")


;;;; `qutebrowser-launcher' backend
;;;###autoload
(defun qutebrowser-consult-launcher (&optional initial default target)
  "Backend for `qutebrowser-launcher' based on Consult."
  (let* ((consult-async-min-input 0)
         (consult-async-split-style nil)
	 (qutebrowser-default-open-target
	  (or target qutebrowser-default-open-target))
	 (selection (consult--multi
		     qutebrowser-consult-launcher-sources
		     :prompt (if default
				 (format "Select (default %s): " default)
			       "Select: ")
		     :default default
		     :sort nil
		     :initial initial
		     :require-match nil)))
    (unless (plist-get (cdr selection) :match)
      (qutebrowser-open-url (car selection)))))

(provide 'qutebrowser-consult)

;;; qutebrowser-consult.el ends here
