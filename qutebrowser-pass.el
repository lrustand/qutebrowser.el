;;; qutebrowser-pass.el --- Pass autofill for Qutebrowser     -*- lexical-binding: t; -*-

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

;;; Commentary:

;;; Change Log:

;;; Code:

(require 'qutebrowser)
(require 'password-store)
(require 'password-store-otp)
(require 'url-parse)

(defgroup qutebrowser-pass nil
  "Password store integration for Qutebrowser."
  :group 'qutebrowser
  :prefix "qutebrowser-pass")

(defcustom qutebrowser-pass-username-function
  #'qutebrowser-pass-username-from-path
  "Function to retrieve the username for a pass entry."
  :type 'symbol
  :group 'qutebrowser-pass)

(defalias 'qutebrowser-pass-username-from-path #'file-name-nondirectory
  "Extract username as the last path segment of FILENAME.")

(defsubst qutebrowser-pass-username-from-field (field)
  "Extract username from FIELD of a pass entry."
  (lambda (e) (password-store-get-field e field)))

(defun qutebrowser-pass--select-entry (search)
  "Select an entry from password store matching SEARCH."
  (if-let* ((url (url-generic-parse-url search))
            (search (if (url-host url)
                        (url-domain url)
                      (or search "")))
            (pass-entries (cl-remove-if-not
                           (lambda (entry)
                             (string-match-p search entry))
                           (password-store-list))))
      (if (= (length pass-entries) 1)
          (car pass-entries)
        (completing-read "Select: " pass-entries))
    (message "No pass entry found for %s" search)
    nil))

;;;###autoload
(defun qutebrowser-pass (&optional search limit)
  "Autofill username and password from password store.
SEARCH can be either a URL or a string to search for in password store.
LIMIT can be :password-only, :username-only, or nil.

If SEARCH is a URL, the domain name is extracted and used to search for
matching entries.

If multiple entries match SEARCH, `completing-read' is used to select
one.  If there is only one matching entry it is selected automatically."
  (interactive)
  (when-let ((selected (qutebrowser-pass--select-entry search)))
    (unless (eq :password-only limit)
      (let ((username (funcall qutebrowser-pass-username-function selected)))
        (qutebrowser-fake-keys username)))
    ;; Only tab when inputting both username and password
    (unless limit (qutebrowser-fake-keys--raw "<Tab>"))
    (unless (eq :username-only limit)
      (let ((password (password-store-get selected)))
        (qutebrowser-fake-keys password)))))

;;;###autoload
(defun qutebrowser-pass-username-only (&optional search)
  "Autofill username matching SEARCH."
  (interactive)
  (qutebrowser-pass search :username-only))

;;;###autoload
(defun qutebrowser-pass-password-only (&optional search)
  "Autofill password matching SEARCH."
  (interactive)
  (qutebrowser-pass search :password-only))

;;;###autoload
(defun qutebrowser-pass-otp (&optional search)
  "Autofill OTP code matching SEARCH."
  (interactive)
  (if-let* ((selected (qutebrowser-pass--select-entry search))
            (token (password-store-otp-token selected)))
      (qutebrowser-fake-keys token)
    (message "Failed to get OTP token for %s." search)))

(provide 'qutebrowser-pass)

;;; qutebrowser-pass.el ends here
