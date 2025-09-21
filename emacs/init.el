;; Enhanced Emacs Configuration
;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install and configure no-littering first
(unless (package-installed-p 'no-littering)
  (package-refresh-contents)
  (package-install 'no-littering))
(require 'no-littering)

;; Required packages list
(defvar required-packages
  '(xah-fly-keys
    denote
    markdown-mode
    yaml-mode
    which-key
    undo-fu
    vertico
    orderless
    marginalia
    consult))

;; Install required packages
(dolist (package required-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Basic UI improvements
(setq inhibit-startup-message t)
(when (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Line numbers and basic editing
(global-display-line-numbers-mode t)
(show-paren-mode t)
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Enhanced editing with undo-fu
(require 'undo-fu)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

;; Configure xah-fly-keys
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty") ; or "dvorak", "colemak", etc.
(xah-fly-keys 1)

;; Custom xah-fly-keys bindings for AI workflow
(define-key xah-fly-command-map (kbd "SPC a c") 'ai-copy-for-interaction)
(define-key xah-fly-command-map (kbd "SPC a p") 'ai-paste-response)
(define-key xah-fly-command-map (kbd "SPC a n") 'ai-new-session)
(define-key xah-fly-command-map (kbd "SPC a l") 'ai-list-sessions)

;; Which-key for key discovery
(require 'which-key)
(which-key-mode 1)
(setq which-key-idle-delay 0.5)

;; Vertico for better completion
(require 'vertico)
(vertico-mode 1)

;; Orderless for flexible matching
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; Marginalia for richer annotations
(require 'marginalia)
(marginalia-mode 1)

;; Consult for enhanced search/navigation
(require 'consult)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-x b") 'consult-buffer)

;; Org-mode configuration
(require 'org)
(setq org-directory "/mnt/c/Users/chip/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Org-mode keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;; Basic org-mode settings
(setq org-log-done t
      org-startup-indented t)

;; Denote configuration
(require 'denote)
(setq denote-directory "/mnt/c/Users/chip/Dropbox/org/denote/")
(setq denote-known-keywords '("ai" "prompt" "project" "meeting" "idea" "reference"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type 'org) ; or 'markdown-yaml, 'markdown-toml, 'text

;; Denote keybindings
(define-key xah-fly-command-map (kbd "SPC n n") 'denote)
(define-key xah-fly-command-map (kbd "SPC n f") 'denote-find-file)
(define-key xah-fly-command-map (kbd "SPC n l") 'denote-find-link)
(define-key xah-fly-command-map (kbd "SPC n b") 'denote-find-backlink)
(define-key xah-fly-command-map (kbd "SPC n i") 'denote-link)
(define-key xah-fly-command-map (kbd "SPC n r") 'denote-rename-file)

;; AI Interaction Functions
(defvar ai-sessions-directory "/mnt/c/Users/chip/Dropbox/org/ai-sessions/"
  "Directory for AI interaction sessions.")

(defun ensure-directory-exists (dir)
  "Create directory if it doesn't exist."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun ai-copy-for-interaction ()
  "Copy current region or buffer for AI interaction with context."
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-string)))
         (filename (if (buffer-file-name)
                       (file-name-nondirectory (buffer-file-name))
                     (buffer-name)))
         (context (format "File: %s\n\n%s" filename text)))
    (with-temp-buffer
      (insert context)
      (clipboard-kill-ring-save (point-min) (point-max)))
    (message "Copied %d characters with context for AI" (length context))))

(defun ai-paste-response ()
  "Paste AI response from clipboard and optionally save to session."
  (interactive)
  (let ((response (current-kill 0)))
    (when response
      (insert response)
      (when (y-or-n-p "Save this interaction to AI session? ")
        (ai-save-interaction response)))))

(defun ai-new-session ()
  "Create a new AI interaction session file."
  (interactive)
  (ensure-directory-exists ai-sessions-directory)
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (topic (read-string "Session topic: "))
         (filename (format "%sai-session_%s_%s.org" 
                          ai-sessions-directory
                          timestamp
                          (replace-regexp-in-string "[^a-zA-Z0-9]" "_" topic)))
         (buffer (find-file filename)))
    (with-current-buffer buffer
      (insert (format "#+TITLE: AI Session - %s\n" topic))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert "#+FILETAGS: ai session\n\n")
      (insert "* Session Overview\n\n")
      (insert "* Interactions\n\n")
      (insert "** Query 1\n\n")
      (insert "*** My Input\n\n")
      (insert "*** AI Response\n\n"))
    (message "Created new AI session: %s" filename)))

(defun ai-save-interaction (response)
  "Save the current interaction to the most recent AI session file."
  (let* ((session-files (directory-files ai-sessions-directory t "ai-session.*\\.org$"))
         (latest-session (car (sort session-files (lambda (a b) (string> a b))))))
    (if latest-session
        (with-current-buffer (find-file-noselect latest-session)
          (goto-char (point-max))
          (insert (format "\n** Query %s\n\n" 
                         (format-time-string "%H:%M:%S")))
          (insert "*** My Input\n")
          (insert (format "%s\n\n" (read-string "What was your query? ")))
          (insert "*** AI Response\n")
          (insert (format "%s\n\n" response))
          (save-buffer)
          (message "Saved interaction to %s" (file-name-nondirectory latest-session)))
      (message "No AI session file found. Create one with ai-new-session."))))

(defun ai-list-sessions ()
  "List and open AI session files."
  (interactive)
  (ensure-directory-exists ai-sessions-directory)
  (let ((session-files (directory-files ai-sessions-directory t "ai-session.*\\.org$")))
    (if session-files
        (find-file (completing-read "Open AI session: " session-files))
      (message "No AI sessions found. Create one with ai-new-session."))))

;; Markdown mode for AI interaction files
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; YAML mode for configuration files
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

;; Custom org capture templates for AI interactions
(setq org-capture-templates
      '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %U\n  %a")
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?\n  %U")
        ("a" "AI Prompt Idea" entry (file+headline org-default-notes-file "AI Prompts")
         "* %?\n  %U\n  Context: \n  Expected Output: \n  Tags: ")
        ("p" "Project" entry (file+headline org-default-notes-file "Projects")
         "* PROJECT %?\n  %U\n  Description: \n  Goals: \n  Next Steps: ")))

;; Load platform-specific config if it exists
(let ((platform-config "~/dotfiles/emacs/platform/wsl.el"))
  (when (file-exists-p (expand-file-name platform-config))
    (load (expand-file-name platform-config))))

;; Custom functions for AI workflow optimization
(defun ai-format-code-for-sharing ()
  "Format current buffer or region as a code block for sharing with AI."
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-string)))
         (mode-name (symbol-name major-mode))
         (lang (replace-regexp-in-string "-mode$" "" mode-name))
         (formatted (format "```%s\n%s\n```" lang text)))
    (with-temp-buffer
      (insert formatted)
      (clipboard-kill-ring-save (point-min) (point-max)))
    (message "Code formatted and copied for AI sharing")))

;; Add to xah-fly-keys command map
(define-key xah-fly-command-map (kbd "SPC a f") 'ai-format-code-for-sharing)

;; Display startup message
(message "Enhanced Emacs config loaded with xah-fly-keys, denote, and AI workflow!")

;; Show key bindings reminder
(defun show-ai-keybindings ()
  "Display AI interaction keybindings."
  (interactive)
  (with-output-to-temp-buffer "*AI Keybindings*"
    (princ "AI Interaction Keybindings (in command mode):\n\n")
    (princ "SPC a c - Copy for AI interaction (with context)\n")
    (princ "SPC a p - Paste AI response\n")
    (princ "SPC a n - Create new AI session\n")
    (princ "SPC a l - List/open AI sessions\n")
    (princ "SPC a f - Format code for sharing\n\n")
    (princ "Denote Keybindings:\n\n")
    (princ "SPC n n - Create new note\n")
    (princ "SPC n f - Find note file\n")
    (princ "SPC n l - Find note link\n")
    (princ "SPC n b - Find backlinks\n")
    (princ "SPC n i - Insert link to note\n")
    (princ "SPC n r - Rename note file\n\n")
    (princ "Remember: Press 'i' to enter insert mode, ESC for command mode")))

(define-key xah-fly-command-map (kbd "SPC h a") 'show-ai-keybindings)
