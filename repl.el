
(require 'comint)

(defun make-python (buffer)
  (make-network-process
   :name "python-repl"
   :family 'local
   :service  "/tmp/emacs-ipc"
   :buffer buffer
   :filter 'comint-output-filter
   :sentinel 'python-repl-sentinel))

(defun python-repl-start ()
  "Start the Python REPL using comint."
  (interactive)
  (let ((buffer (get-buffer-create "*Python REPL*")))
    (with-current-buffer buffer
      (setq-local comint-input-sender
                  (lambda (proc string)
                    (comint-simple-send proc (json-encode `((cmd . "eval")
                                                            (args . ,string))))))
      (setq keepalive-timer
            (run-at-time 0 1
                         (lambda ()
                           (let ((proc (get-process "python-repl")))
                             (comint-simple-send proc (json-encode `((cmd . "keepalive")
                                                                     (args . "nothing"))))))))
      (unless (comint-check-proc buffer)
        (let ((process (make-python buffer)))
          (comint-mode))))))

(defun python-repl-sentinel (process event)
  (message "EVENT!!! %s" event)
  (if (or (string-match "closed" event)
          (string-match "broken" event))
      (progn
        (message "Connection lost, attempting to reconnect...")
                       (let ((buffer (process-buffer process)))
                         (when buffer
                           ;; Restart the process and associate it with the buffer
                           (with-current-buffer buffer
                             (python-repl-start)))))))
