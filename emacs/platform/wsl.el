;; WSL-specific configuration with AI integration
;; Save as: /mnt/c/Users/chip/Dropbox/dotfiles/emacs/platform/wsl.el

;; WSL clipboard integration
(defun wsl-copy-region (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun wsl-paste-from-clipboard ()
  "Paste from Windows clipboard."
  (let ((clipboard (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2>/dev/null")))
    (replace-regexp-in-string "\r" "" clipboard)))

;; AI interaction helpers
(defun copy-for-ai ()
  "Copy current region or buffer for AI interaction."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-string))))
    (wsl-copy-region (point-min) (point-max))
    (message "Copied %d characters for AI" (length text))))

(defun paste-ai-response ()
  "Paste AI response from clipboard."
  (interactive)
  (insert (wsl-paste-from-clipboard)))

;; Key bindings
(global-set-key (kbd "C-c w c") 'wsl-copy-region)
(global-set-key (kbd "C-c w v") 'paste-ai-response)
;;(global-set-key (kbd "C-c c p") 'copy-for-ai)

(message "WSL AI integration loaded")
