
---

# emacs/platform/clipboard-config.el
;; Universal Emacs clipboard configuration for multi-platform environments

;; Detect environment for clipboard integration
(defun my/detect-clipboard-environment ()
  "Detect the current environment for clipboard operations."
  (cond
   ;; WSL detection
   ((and (eq system-type 'gnu/linux)
         (string-match "microsoft\\|WSL" (shell-command-to-string "uname -r")))
    'wsl)
   ;; MSYS2 detection
   ((or (eq system-type 'windows-nt)
        (getenv "MSYSTEM"))
    'msys2)
   ;; VirtualBox detection
   ((and (eq system-type 'gnu/linux)
         (or (getenv "VBOX_VERSION")
             (string-match "vbox" (shell-command-to-string "lsmod 2>/dev/null || true"))))
    'virtualbox)
   ;; Native Linux
   ((eq system-type 'gnu/linux)
    'linux)
   ;; Windows
   ((eq system-type 'windows-nt)
    'windows)
   ;; Default
   (t 'unknown)))

(defconst my/clipboard-env (my/detect-clipboard-environment))

;; WSL clipboard integration
(when (eq my/clipboard-env 'wsl)
  (defun my/wsl-copy-to-clipboard (text)
    "Copy TEXT to Windows clipboard via WSL."
    (let ((process-connection-type nil))
      (let ((proc (start-process "clip" nil "clip.exe")))
        (process-send-string proc text)
        (process-send-eof proc))))
  
  (defun my/wsl-paste-from-clipboard ()
    "Paste from Windows clipboard via WSL."
    (let ((clipboard (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2>/dev/null")))
      (replace-regexp-in-string "\r" "" clipboard)))
  
  ;; Set Emacs clipboard functions
  (setq interprogram-cut-function 'my/wsl-copy-to-clipboard)
  (setq interprogram-paste-function 'my/wsl-paste-from-clipboard)
  
  ;; Key bindings
  (global-set-key (kbd "C-c w c") (lambda () (interactive) (my/wsl-copy-to-clipboard (buffer-substring-no-properties (region-beginning) (region-end)))))
  (global-set-key (kbd "C-c w v") (lambda () (interactive) (insert (my/wsl-paste-from-clipboard)))))

;; MSYS2 clipboard integration
(when (eq my/clipboard-env 'msys2)
  (defun my/msys2-copy-to-clipboard (text)
    "Copy TEXT to Windows clipboard via MSYS2."
    (with-temp-buffer
      (insert text)
      (shell-command-on-region (point-min) (point-max) "clip")))
  
  (defun my/msys2-paste-from-clipboard ()
    "Paste from Windows clipboard via MSYS2."
    (condition-case nil
        (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2>/dev/null | sed 's/\\r$//'")
      (error
       ;; Fallback method
       (shell-command-to-string "cat /dev/clipboard 2>/dev/null || echo ''"))))
  
  (setq interprogram-cut-function 'my/msys2-copy-to-clipboard)
  (setq interprogram-paste-function 'my/msys2-paste-from-clipboard))

;; VirtualBox clipboard integration
(when (eq my/clipboard-env 'virtualbox)
  (defun my/vbox-ensure-clipboard-service ()
    "Ensure VirtualBox clipboard service is running."
    (unless (string-match "VBoxClient" (shell-command-to-string "ps aux | grep VBoxClient || true"))
      (start-process "vbox-clipboard" nil "VBoxClient-all")))
  
  (defun my/vbox-copy-to-clipboard (text)
    "Copy TEXT to clipboard via X11 with VirtualBox integration."
    (my/vbox-ensure-clipboard-service)
    (cond
     ((executable-find "xclip")
      (with-temp-buffer
        (insert text)
        (shell-command-on-region (point-min) (point-max) "xclip -selection clipboard")))
     ((executable-find "xsel")
      (with-temp-buffer
        (insert text)
        (shell-command-on-region (point-min) (point-max) "xsel --clipboard --input")))
     (t (message "No clipboard tool found. Install xclip or xsel."))))
  
  (defun my/vbox-paste-from-clipboard ()
    "Paste from clipboard via X11 with VirtualBox integration."
    (my/vbox-ensure-clipboard-service)
    (cond
     ((executable-find "xclip")
      (shell-command-to-string "xclip -selection clipboard -o"))
     ((executable-find "xsel")
      (shell-command-to-string "xsel --clipboard --output"))
     (t "")))
  
  (when (getenv "DISPLAY")
    (setq interprogram-cut-function 'my/vbox-copy-to-clipboard)
    (setq interprogram-paste-function 'my/vbox-paste-from-clipboard)))

;; Native Linux clipboard integration
(when (eq my/clipboard-env 'linux)
  (defun my/linux-copy-to-clipboard (text)
    "Copy TEXT to clipboard on Linux."
    (cond
     ((executable-find "xclip")
      (with-temp-buffer
        (insert text)
        (shell-command-on-region (point-min) (point-max) "xclip -selection clipboard")))
     ((executable-find "xsel")
      (with-temp-buffer
        (insert text)
        (shell-command-on-region (point-min) (point-max) "xsel --clipboard --input")))
     ((executable-find "wl-copy")
      (with-temp-buffer
        (insert text)
        (shell-command-on-region (point-min) (point-max) "wl-copy")))
     (t (message "No clipboard tool found. Install xclip, xsel, or wl-clipboard."))))
  
  (defun my/linux-paste-from-clipboard ()
    "Paste from clipboard on Linux."
    (cond
     ((executable-find "xclip")
      (shell-command-to-string "xclip -selection clipboard -o"))
     ((executable-find "xsel")
      (shell-command-to-string "xsel --clipboard --output"))
     ((executable-find "wl-paste")
      (shell-command-to-string "wl-paste"))
     (t "")))
  
  (when (or (getenv "DISPLAY") (getenv "WAYLAND_DISPLAY"))
    (setq interprogram-cut-function 'my/linux-copy-to-clipboard)
    (setq interprogram-paste-function 'my/linux-paste-from-clipboard)))

;; Windows native clipboard
(when (eq my/clipboard-env 'windows)
  (defun my/windows-copy-to-clipboard (text)
    "Copy TEXT to Windows clipboard."
    (with-temp-buffer
      (insert text)
      (shell-command-on-region (point-min) (point-max) "clip")))
  
  (defun my/windows-paste-from-clipboard ()
    "Paste from Windows clipboard."
    (shell-command-to-string "powershell.exe -command 'Get-Clipboard'"))
  
  (setq interprogram-cut-function 'my/windows-copy-to-clipboard)
  (setq interprogram-paste-function 'my/windows-paste-from-clipboard))

;; Universal clipboard functions for interactive use
(defun my/copy-to-external-clipboard ()
  "Copy current region to external clipboard."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (cond
         ((eq my/clipboard-env 'wsl) (my/wsl-copy-to-clipboard text))
         ((eq my/clipboard-env 'msys2) (my/msys2-copy-to-clipboard text))
         ((eq my/clipboard-env 'virtualbox) (my/vbox-copy-to-clipboard text))
         ((eq my/clipboard-env 'linux) (my/linux-copy-to-clipboard text))
         ((eq my/clipboard-env 'windows) (my/windows-copy-to-clipboard text))
         (t (message "Unknown clipboard environment")))
        (message "Copied to external clipboard (%s)" my/clipboard-env))
    (message "No region selected")))

(defun my/paste-from-external-clipboard ()
  "Paste from external clipboard."
  (interactive)
  (let ((text (cond
               ((eq my/clipboard-env 'wsl) (my/wsl-paste-from-clipboard))
               ((eq my/clipboard-env 'msys2) (my/msys2-paste-from-clipboard))
               ((eq my/clipboard-env 'virtualbox) (my/vbox-paste-from-clipboard))
               ((eq my/clipboard-env 'linux) (my/linux-paste-from-clipboard))
               ((eq my/clipboard-env 'windows) (my/windows-paste-from-clipboard))
               (t ""))))
    (when (and text (> (length text) 0))
      (insert text)
      (message "Pasted from external clipboard (%s)" my/clipboard-env))))

(defun my/show-clipboard-info ()
  "Show clipboard environment information."
  (interactive)
  (message "Clipboard environment: %s" my/clipboard-env))

;; Universal key bindings
(global-set-key (kbd "C-c c c") 'my/copy-to-external-clipboard)
(global-set-key (kbd "C-c c v") 'my/paste-from-external-clipboard)
(global-set-key (kbd "C-c c i") 'my/show-clipboard-info)

;; Make standard copy/paste work with external clipboard (optional)
;; Uncomment these lines if you want M-w and C-y to use external clipboard
;; (global-set-key (kbd "M-w") 'my/copy-to-external-clipboard)
;; (global-set-key (kbd "C-y") 'my/paste-from-external-clipboard)

(message "Universal clipboard configuration loaded for %s" my/clipboard-env)
