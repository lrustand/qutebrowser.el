;;; qute-launcher.el --- launcher for qutebrowser     -*- lexical-binding: t; -*-

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

;; Author: Lars Rustand
;; URL: http://githhub.com/lrustand/qute-launcher
;; Version: 

;;; Commentary:

;;; Change Log:

;;; Code:

;; TODO Separate out search history from regular history
;; i.e. remove Google, Ebay, Amazon, Youtube searches etc
(require 'sqlite)
(require 'marginalia)
(defun qute-launcher--select-from-history ()
  (let* ((db (sqlite-open "~/.local/share/qutebrowser/history.sqlite"))
         (history (sqlite-select db "SELECT url,substr(title,0,99) FROM History GROUP BY url ORDER BY COUNT(url) DESC"))
         (candidates (mapcar (lambda (row)
                               (let* ((url (nth 0 row))
                                      (title (nth 1 row))
                                      (display-url (truncate-string-to-width url 50 0 ?\ )))
                                 (cons
                                  (format "%s %s"
                                          display-url
                                          (propertize title
                                                      'face 'marginalia-value))
                                  url)))
                             history))
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata
                  (display-sort-function . identity)
                  (cycle-sort-function . identity)
                  (category . qutebrowser-history)
                  (annotation-function . nil))
              (complete-with-action action candidates string pred))))
         (selected (completing-read "Choose URL from history: " completion-table nil nil)))
    ;; Return the URL of the selected candidate
    (cdr (or (assoc selected candidates)
             (cons nil selected)))))

(defun qute-launcher--internal (target)
  (let ((url (qute-launcher--select-from-history))
        (pipe (getenv "QUTE_FIFO"))
        (flag (pcase target
                    ('window "-w")
                    ('tab "-t")
                    ('private-window "-p")
                    ('auto "auto"))))
    (if pipe
        (write-region (format "open %s %s" flag url) nil pipe t)
      (start-process "qutebrowser" nil "qutebrowser" "--target" (symbol-name target) url))))

(defun qute-launcher ()
  (interactive)
  (qute-launcher--internal 'auto))

(defun qute-launcher-tab ()
  (interactive)
  (qute-launcher--internal 'tab))

(defun qute-launcher-window ()
  (interactive)
  (qute-launcher--internal 'window))

(defun qute-launcher-private ()
  (interactive)
  (qute-launcher--internal 'private-window))


(provide 'qute-launcher)

;;; qute-launcher.el ends here
