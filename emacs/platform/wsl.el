;; WSL-specific configuration with enhanced AI integration
;; This file: ~/dotfiles/emacs/platform/wsl.el

;; WSL clipboard integration
(defun wsl-copy-region (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (call-process-region start end "clip.exe" nil 0))

(defun wsl-paste-from-clipboard ()
  "Paste from Windows clipboard."
  (let ((clipboard (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2>/dev/null")))
    (replace-regexp-in-string "\r" "" clipboard)))

;; Override clipboard functions for WSL integration
(setq interprogram-cut-function
      (lambda (text)
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) "clip.exe"))))

(setq interprogram-paste-function
      (lambda ()
        (let ((clipboard (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2>/dev/null")))
          (unless (string= clipboard "")
            (replace-regexp-in-string "\r" "" clipboard)))))

;; Enhanced AI interaction functions for WSL
(defun wsl-ai-copy-for-interaction ()
  "WSL-specific version of copying for AI interaction."
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-string)))
         (filename (if (buffer-file-name)
                       (file-name-nondirectory (buffer-file-name))
                     (buffer-name)))
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         (context (format "=== Emacs Context ===\nFile: %s\nTime: %s\nSystem: WSL\n\n=== Content ===\n%s\n\n=== End Context ===" 
                         filename timestamp text)))
    (with-temp-buffer
      (insert context)
      (call-process-region (point-min) (point-max) "clip.exe"))
    (message "Copied %d characters with WSL context for AI" (length context))))

(defun wsl-ai-paste-and-process ()
  "Paste from WSL clipboard and process AI response."
  (interactive)
  (let ((response (wsl-paste-from-clipboard)))
    (when response
      ;; Clean up any Windows line endings
      (setq response (replace-regexp-in-string "\r\n" "\n" response))
      ;; Insert the response
      (insert response)
      ;; Offer to save to session
      (when (y-or-n-p "Save this AI response to session? ")
        (ai-save-interaction response)))))

;; WSL-specific file path helpers
(defun wsl-to-windows-path (path)
  "Convert WSL path to Windows path."
  (if (string-prefix-p "/mnt/c/" path)
      (concat "C:" (substring path 6))
    path))

(defun windows-to-wsl-path (path)
  "Convert Windows path to WSL path."
  (if (string-match "^\\([A-Za-z]\\):" path)
      (concat "/mnt/" (downcase (match-string 1 path)) (substring path 2))
    path))

;; AI session management for WSL
(defvar wsl-ai-temp-dir "/tmp/ai-sessions/"
  "Temporary directory for AI sessions on WSL.")

(defun wsl-ai-quick-session ()
  "Create a quick AI session file in temp directory."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (filename (format "%squick-ai_%s.org" wsl-ai-temp-dir timestamp)))
    (make-directory wsl-ai-temp-dir t)
    (find-file filename)
    (insert (format "#+TITLE: Quick AI Session\n#+DATE: %s\n\n* Context\n\n* Query\n\n* Response\n\n" 
                   (format-time-string "%Y-%m-%d %H:%M:%S")))
    (goto-char (point-max))
    (previous-line 4)
    (end-of-line)
    (message "Quick AI session created: %s" filename)))

;; WSL system integration
(defun wsl-open-in-windows (file)
  "Open file in Windows application."
  (interactive "fFile: ")
  (let ((windows-path (wsl-to-windows-path (expand-file-name file))))
    (call-process "cmd.exe" nil 0 nil "/c" "start" windows-path)))

;; Denote integration for WSL
(defun wsl-denote-open-in-explorer ()
  "Open denote directory in Windows Explorer."
  (interactive)
  (let ((windows-path (wsl-to-windows-path denote-directory)))
    (call-process "explorer.exe" nil 0 nil windows-path)))

;; Enhanced key bindings for WSL (integrate with xah-fly-keys)
(when (and (featurep 'xah-fly-keys) xah-fly-keys)
  ;; WSL-specific AI bindings
  (define-key xah-fly-command-map (kbd "SPC w c") 'wsl-ai-copy-for-interaction)
  (define-key xah-fly-command-map (kbd "SPC w p") 'wsl-ai-paste-and-process)
  (define-key xah-fly-command-map (kbd "SPC w q") 'wsl-ai-quick-session)
  (define-key xah-fly-command-map (kbd "SPC w e") 'wsl-denote-open-in-explorer)
  (define-key xah-fly-command-map (kbd "SPC w o") 'wsl-open-in-windows))

;; WSL-specific org-mode settings
(when (featurep 'org)
  ;; Use Windows paths for org-agenda-files if needed
  (setq org-agenda-files (list org-directory))
  
  ;; WSL-specific org capture templates
  (add-to-list 'org-capture-templates
               '("w" "WSL Note" entry (file+headline org-default-notes-file "WSL Notes")
                 "* %?\n  %U\n  System: WSL\n  Path: %F"))
  
  ;; Quick access to AI session directory
  (global-set-key (kbd "C-c C-w a") 
                  (lambda () (interactive) (dired ai-sessions-directory))))

;; Performance optimizations for WSL
(when (and (eq system-type 'gnu/linux)
           (getenv "WSL_DISTRO_NAME"))
  ;; Reduce file watching overhead
  (setq auto-revert-use-notify nil)
  ;; Faster file operations
  (setq vc-follow-symlinks t)
  ;; Reduce network timeouts
  (setq url-gateway-method 'native))

;; WSL clipboard test function
(defun test-wsl-clipboard ()
  "Test WSL clipboard functionality."
  (interactive)
  (let ((test-text "WSL clipboard test"))
    (with-temp-buffer
      (insert test-text)
      (call-process-region (point-min) (point-max) "clip.exe"))
    (let ((result (wsl-paste-from-clipboard)))
      (if (string= (string-trim result) test-text)
          (message "WSL clipboard working correctly")
        (message "WSL clipboard test failed: got '%s', expected '%s'" result test-text)))))

;; Initialize WSL-specific settings
(defun init-wsl-ai-integration ()
  "Initialize WSL AI integration."
  (message "WSL AI integration initialized")
  ;; Create necessary directories
  (make-directory ai-sessions-directory t)
  (make-directory wsl-ai-temp-dir t)
  ;; Test clipboard
  (run-with-timer 2 nil 'test-wsl-clipboard))

;; Run initialization
(init-wsl-ai-integration)

;; Display WSL-specific help
(defun show-wsl-ai-help ()
  "Show WSL-specific AI keybindings and help."
  (interactive)
  (with-output-to-temp-buffer "*WSL AI Help*"
    (princ "WSL-Specific AI Integration Help\n")
    (princ "================================\n\n")
    (princ "Keybindings (in xah-fly command mode):\n")
    (princ "SPC w c - Copy with WSL context for AI\n")
    (princ "SPC w p - Paste and process AI response\n")
    (princ "SPC w q - Create quick AI session\n")
    (princ "SPC w e - Open denote directory in Explorer\n")
    (princ "SPC w o - Open file in Windows app\n\n")
    (princ "File Locations:\n")
    (princ (format "AI Sessions: %s\n" ai-sessions-directory))
    (princ (format "Quick Sessions: %s\n" wsl-ai-temp-dir))
    (princ (format "Denote Directory: %s\n" denote-directory))
    (princ (format "Org Directory: %s\n" org-directory))
    (princ "\nWorkflow:\n")
    (princ "1. Copy code/text with SPC w c\n")
    (princ "2. Paste in external AI tool\n")
    (princ "3. Copy AI response\n")
    (princ "4. Paste back with SPC w p\n")
    (princ "5. Save to session when prompted\n")))

(define-key xah-fly-command-map (kbd "SPC w h") 'show-wsl-ai-help)

(message "WSL platform configuration loaded successfully")
