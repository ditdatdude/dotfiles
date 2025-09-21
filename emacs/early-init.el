;;; early-init.el --- Enhanced early Emacs configuration for optimal startup -*- lexical-binding: t -*-

;; This file is loaded before the package system and GUI is initialized.
;; It's perfect for startup optimizations and early configuration.

;;; UTF-8 Configuration
;; Set UTF-8 as default encoding early to avoid any encoding issues
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(set-selection-coding-system 'utf-8)

;;; Startup Time Tracking
;; Record startup time for benchmarking
(defvar emacs-startup-time (current-time)
  "Time when Emacs startup began.")

;; Store initial load times for detailed analysis
(defvar startup-times-alist nil
  "List of (description . time) pairs for startup profiling.")

(defun record-startup-time (description)
  "Record startup time with DESCRIPTION."
  (push (cons description (current-time)) startup-times-alist))

(record-startup-time "early-init-start")

;;; Garbage Collection Optimization
;; Significantly increase GC threshold during startup to prevent pauses
(defvar default-gc-cons-threshold gc-cons-threshold
  "Default garbage collection threshold.")

(defvar default-gc-cons-percentage gc-cons-percentage
  "Default garbage collection percentage.")

;; Max out GC settings during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8)

;; Track GC during startup (optional, for debugging)
(defvar startup-gc-count 0
  "Number of garbage collections during startup.")

(add-hook 'post-gc-hook
          (lambda ()
            (setq startup-gc-count (1+ startup-gc-count))))

;;; File Handler Optimization
;; Temporarily disable file name handlers for faster file operations
(defvar default-file-name-handler-alist file-name-handler-alist
  "Saved file name handler alist.")

(setq file-name-handler-alist nil)

;;; Package System Configuration
;; Disable package loading at startup (we handle this explicitly in init.el)
(setq package-enable-at-startup nil)

;; Prevent package.el from automatically adding packages to load-path
(setq package--init-file-ensured t)

;;; Frame and UI Optimization
;; Prevent frame resizing flicker during init
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Set initial frame size to prevent resizing
(add-to-list 'initial-frame-alist '(width . 120))
(add-to-list 'initial-frame-alist '(height . 40))

;; Disable visual bell early
(setq ring-bell-function 'ignore
      visible-bell nil)

;;; UI Element Management
;; Disable unnecessary UI elements as early as possible
;; Check if we're in a GUI environment before disabling UI elements
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable startup screen and messages early
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)

;;; Font and Display Optimization
;; Set a good default font early to prevent font fallback delays
;; Adjust these based on your system and preferences
(when (display-graphic-p)
  (cond
   ;; Windows/WSL with Windows fonts
   ((or (eq system-type 'windows-nt)
        (and (eq system-type 'gnu/linux) (getenv "WSL_DISTRO_NAME")))
    (set-face-attribute 'default nil :family "Consolas" :height 110))
   ;; macOS
   ((eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Monaco" :height 130))
   ;; Linux
   ((eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 110))))

;;; Compilation and Loading Optimizations
;; Prefer newer files and byte-compile when possible
(setq load-prefer-newer t)

;; Increase read process output max for faster subprocess communication
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Reduce regexp backtracking issues
(setq redisplay-dont-pause t)

;;; Network and Security Optimizations
;; Reduce security handshaking delays
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Faster network operations
(setq network-security-level 'low) ; Only for startup speed, can be increased later

;;; Platform-Specific Optimizations
;; WSL-specific optimizations
(when (and (eq system-type 'gnu/linux) (getenv "WSL_DISTRO_NAME"))
  ;; Reduce file system overhead in WSL
  (setq auto-revert-use-notify nil
        auto-revert-avoid-polling t)
  
  ;; Optimize for WSL file system performance
  (setq create-lockfiles nil
        make-backup-files nil))

;; Windows-specific optimizations
(when (eq system-type 'windows-nt)
  ;; Faster file operations on Windows
  (setq w32-get-true-file-attributes nil))

;; macOS-specific optimizations
(when (eq system-type 'darwin)
  ;; Better macOS integration
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

;;; Window Management Preparation
;; Prepare for our window management system
(setq window-combination-resize t
      even-window-sizes 'height-only
      window-sides-vertical nil)

;; Set up initial display buffer configuration that works well with our system
(setq display-buffer-base-action '(display-buffer-reuse-window))

;;; Startup Completion Hook
;; Comprehensive startup completion and cleanup
(add-hook 'emacs-startup-hook
          (lambda ()
            (record-startup-time "emacs-startup-hook")
            
            ;; Reset GC to reasonable production values
            (setq gc-cons-threshold (* 20 1000 1000)  ; 20MB
                  gc-cons-percentage 0.1)
            
            ;; Restore file name handlers
            (setq file-name-handler-alist default-file-name-handler-alist)
            
            ;; Reset network security if we lowered it
            (setq network-security-level 'medium)
            
            ;; Calculate and display detailed startup information
            (let* ((startup-time (time-subtract (current-time) emacs-startup-time))
                   (startup-seconds (time-to-seconds startup-time)))
              
              ;; Display startup summary
              (message "🚀 Emacs ready in %.3f seconds [GC: %d times] [Packages: %d loaded]"
                       startup-seconds
                       startup-gc-count
                       (length package-activated-list))
              
              ;; Optionally show detailed timing breakdown
              (when (> startup-seconds 2.0) ; Only show if startup was slow
                (let ((times (reverse startup-times-alist)))
                  (message "Startup timing breakdown:")
                  (let ((prev-time emacs-startup-time))
                    (dolist (time-entry times)
                      (let* ((desc (car time-entry))
                             (time (cdr time-entry))
                             (delta (time-to-seconds (time-subtract time prev-time))))
                        (message "  %s: +%.3fs" desc delta)
                        (setq prev-time time))))))))

          ;; Run this hook late in the startup process
          90)

;; Additional hook for after everything is loaded
(add-hook 'window-setup-hook
          (lambda ()
            (record-startup-time "window-setup-complete")
            
            ;; Force a garbage collection now that we're ready
            (garbage-collect)
            
            ;; Optional: Display memory usage
            (when (> startup-gc-count 5)
              (message "Memory: %s (GC freed: %s during startup)"
                       (format-human-readable-bytes (memory-use-counts))
                       startup-gc-count))))

;;; Development and Debugging Support
;; Add a function to benchmark startup performance
(defun benchmark-init-time ()
  "Benchmark Emacs initialization time."
  (interactive)
  (let ((start-time (current-time)))
    (eval-buffer)
    (message "Init benchmark: %.3f seconds"
             (time-to-seconds (time-subtract (current-time) start-time)))))

;; Function to show detailed startup information
(defun show-startup-info ()
  "Show detailed information about Emacs startup."
  (interactive)
  (with-output-to-temp-buffer "*Startup Info*"
    (princ (format "Emacs Startup Information\n"))
    (princ (format "========================\n\n"))
    (princ (format "Startup time: %.3f seconds\n"
                   (time-to-seconds (time-subtract (current-time) emacs-startup-time))))
    (princ (format "Garbage collections during startup: %d\n" startup-gc-count))
    (princ (format "Packages loaded: %d\n" (length package-activated-list)))
    (princ (format "Features loaded: %d\n" (length features)))
    (princ (format "Emacs version: %s\n" emacs-version))
    (princ (format "System: %s\n" system-configuration))
    (princ (format "Memory usage: %s\n" (format-human-readable-bytes (memory-use-counts))))
    
    (when startup-times-alist
      (princ "\nTiming Breakdown:\n")
      (princ "-----------------\n")
      (let ((times (reverse startup-times-alist))
            (prev-time emacs-startup-time))
        (dolist (time-entry times)
          (let* ((desc (car time-entry))
                 (time (cdr time-entry))
                 (delta (time-to-seconds (time-subtract time prev-time))))
            (princ (format "%s: +%.3fs\n" desc delta))
            (setq prev-time time)))))))

(record-startup-time "early-init-complete")

;;; early-init.el ends here
