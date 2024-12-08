(defvar qutebrowser-repl-prompt ">>> ")

(defvar qutebrowser-repl-history '()
  "List to store command history for Qutebrowser REPL.")

(defvar qutebrowser-repl-history-position 0
  "Current position in the command history.")

(defun qutebrowser-create-repl-buffer ()
  (get-buffer-create "*Qutebrowser REPL*"))

(defun qutebrowser-repl-send-input ()
  (interactive)
  (let ((input (buffer-substring-no-properties
                (+ (line-beginning-position)
                   (length qutebrowser-repl-prompt))
                (point-max))))
    (push input qutebrowser-repl-history)
    (setq qutebrowser-repl-history-position 0)
    (qutebrowser-rpc-call `((eval . ,input)))
    (insert "\n")))

(defun qutebrowser-repl-previous-input ()
  (interactive)
  (when (< qutebrowser-repl-history-position
           (length qutebrowser-repl-history))
    (setq qutebrowser-repl-history-position
          (1+ qutebrowser-repl-history-position))
    (qutebrowser-repl-replace-input
     (nth (1- qutebrowser-repl-history-position)
          qutebrowser-repl-history))))

(defun qutebrowser-repl-next-input ()
  (interactive)
  (when (> qutebrowser-repl-history-position 0)
    (setq qutebrowser-repl-history-position
          (1- qutebrowser-repl-history-position))
    (if (zerop qutebrowser-repl-history-position)
        (qutebrowser-repl-replace-input "")
      (qutebrowser-repl-replace-input
       (nth (1- qutebrowser-repl-history-position)
            qutebrowser-repl-history)))))

(defun qutebrowser-repl-replace-input (new-input)
  (delete-region (+ (line-beginning-position)
                    (length qutebrowser-repl-prompt))
                 (point-max))
  (insert new-input))

(defun qutebrowser-repl-receive-response (response)
  (with-current-buffer (qutebrowser-create-repl-buffer)
    (goto-char (point-max))
    (insert response "\n")
    (insert qutebrowser-repl-prompt)))

(define-derived-mode qutebrowser-repl-mode fundamental-mode "Qutebrowser REPL"
  "Major mode for Qutebrowser REPL."
  (use-local-map qutebrowser-repl-mode-map))

(defvar qutebrowser-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'qutebrowser-repl-send-input)
    (define-key map (kbd "<up>") 'qutebrowser-repl-previous-input)
    (define-key map (kbd "<down>") 'qutebrowser-repl-next-input)
    map))

(defun qutebrowser-start-repl ()
  (interactive)
  (let ((repl-buffer (qutebrowser-create-repl-buffer)))
    (with-current-buffer repl-buffer
      (qutebrowser-repl-mode)
      (insert qutebrowser-repl-prompt))
    (switch-to-buffer-other-window repl-buffer)))
