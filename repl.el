(defvar qutebrowser-repl-prompt ">>> ")

(defvar qutebrowser-repl-history '()
  "List to store command history for Qutebrowser REPL.")

(defvar qutebrowser-repl-history-position 0
  "Current position in the command history.")

(defun qutebrowser-create-repl-buffer ()
  "Get existing Qutebrowser REPL buffer or create a new one."
  (if-let ((repl-buffer (get-buffer "*Qutebrowser REPL*")))
      repl-buffer
    (with-current-buffer (get-buffer-create "*Qutebrowser REPL*")
      (qutebrowser-repl-mode)
      (insert
       (propertize qutebrowser-repl-prompt 'read-only t 'rear-nonsticky t))
      (current-buffer))))

(defun qutebrowser-repl-send-input ()
  "Send the current input to Qutebrowser."
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
  "Cycle backwards through input history."
  (interactive)
  (when (< qutebrowser-repl-history-position
           (length qutebrowser-repl-history))
    (setq qutebrowser-repl-history-position
          (1+ qutebrowser-repl-history-position))
    (qutebrowser-repl-replace-input
     (nth (1- qutebrowser-repl-history-position)
          qutebrowser-repl-history))))

(defun qutebrowser-repl-next-input ()
  "Cycle forwards through input history."
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
  "Replace the current input with NEW-INPUT."
  (delete-region (+ (line-beginning-position)
                    (length qutebrowser-repl-prompt))
                 (point-max))
  (insert new-input))

(defun qutebrowser-repl-receive-response (response)
  "Receive a response from Qutebrowser and output it in the REPL."
  (with-current-buffer (qutebrowser-create-repl-buffer)
    (goto-char (point-max))
    (insert response "\n")
    (insert qutebrowser-repl-prompt)
    (let ((inhibit-read-only t))
      (add-text-properties (point-min) (point-max)
                           '(read-only t rear-nonsticky t)))))

(defvar qutebrowser-repl-mode-map
  "Keymap used in Qutebrowser REPL buffers."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'qutebrowser-repl-send-input)
    (define-key map (kbd "<up>") 'qutebrowser-repl-previous-input)
    (define-key map (kbd "<down>") 'qutebrowser-repl-next-input)
    map))

(define-derived-mode qutebrowser-repl-mode fundamental-mode "Qutebrowser REPL"
  "Major mode for Qutebrowser REPL."
  (use-local-map qutebrowser-repl-mode-map))

(defun qutebrowser-start-repl ()
  "Start Qutebrowser REPL and switch to the buffer."
  (interactive)
  (switch-to-buffer (qutebrowser-create-repl-buffer)))
