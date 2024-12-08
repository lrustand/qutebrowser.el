(defvar qutebrowser-repl-prompt ">>> ")

(defun qutebrowser-create-repl-buffer ()
  (get-buffer-create "*Qutebrowser REPL*"))

(defun qutebrowser-repl-send-input ()
  (interactive)
  (let ((input (buffer-substring-no-properties
                (+ (line-beginning-position)
                   (1- (length qutebrowser-repl-prompt)))
                (point-max))))
    (qutebrowser-rpc-call `((eval . ,input)))
    (insert "\n")))

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
    map))

(defun qutebrowser-start-repl ()
  (interactive)
  (let ((repl-buffer (qutebrowser-create-repl-buffer)))
    (with-current-buffer repl-buffer
      (qutebrowser-repl-mode)
      (insert qutebrowser-repl-prompt))
    (switch-to-buffer-other-window repl-buffer)))
