;;; init.el --- Optimized Minimal Emacs Configuration -*- lexical-binding: t -*-

;; --- HANDLE CUSTOM FILE ---
;; Prevent Emacs from adding 'custom-set-variables' to the bottom of init.el
;; Instead, save them to a separate file that we load if it exists.
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Load the custom file (if it exists) silently
(load custom-file 'noerror 'nomessage)

;; --- 1. PERFORMANCE & CORE SETTINGS ---

;; Increase garbage collection threshold during startup to speed things up
(setq gc-cons-threshold 100000000) 
;; Increase output limit for subprocesses (crucial for LSP/Jupyter)
(setq read-process-output-max (* 2 1024 1024))

;; Disable annoying backup and autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Change "yes or no" prompts to just "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; --- 2. PACKAGE MANAGER SETUP ---

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(require 'use-package)
;; Ensure packages are installed automatically by default
(setq use-package-always-ensure t)

;; --- 3. MACOS PATH INJECTION ---

;; Solves "command not found" errors for pip, jupyter, etc.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (exec-path-from-shell-initialize))

;; --- 4. UI & VISUALS ---

;; Minimalist UI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)

;; Line Numbers (Relative)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Parentheses
(show-paren-mode 1)
(electric-pair-mode 1)

;; --- 5. EDITING TOOLS ---

;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C-." . mc/mark-next-like-this-word)
         ("C-," . mc/mark-previous-like-this-word)
         ("C-M-." . mc/mark-all-words-like-this)))


;; --- MAGIT (Git Client) ---
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; --- COMPLETION ENGINE (Corfu) ---
;; Minimal, lightning-fast UI for completion-at-point
(use-package corfu
  ;; Optional: Download from ELPA if not built-in (Corfu is external)
  :ensure t  
  
  :custom
  (corfu-auto t)                 ;; Enable auto-completion
  (corfu-auto-delay 0.2)         ;; Small delay to prevent stuttering
  (corfu-auto-prefix 2)          ;; Start showing after 2 chars
  (corfu-cycle t)                ;; Enable cycling with TAB/arrows
  (corfu-preselect 'prompt)      ;; Always preselect the first candidate
  
  ;; Recommended: Use these settings to make Tab work naturally
  :init
  (global-corfu-mode)
  
  ;; This connects corfu to the built-in completion system
  (setq tab-always-indent 'complete))

;; Allows you to type space-separated terms to match (e.g. "py file" matches "python_file_name")
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; --- LSP CLIENT (Eglot) ---
;; Built-in since Emacs 29. Lightweight and uses native Emacs APIs.
(use-package eglot
  :ensure nil ;; It is built-in
  :hook 
  ;; Hook Eglot into the programming modes you use
  ((python-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (c-mode . eglot-ensure))
   
  :config
  ;; Optimization: specific performance tweaks for Eglot
  (setq eglot-events-buffer-size 0) ;; Disable logging to improve speed
  (fset #'jsonrpc--log-event #'ignore)) ;; Further silence jsonrpc logging

;;; Jupyter (Run kernels inside Emacs)
;(use-package jupyter
;  :defer t
;  :config
;  (message "Jupyter loaded."))

;; --- 6. ORG MODE & BEORG SYNC ---

(use-package org
  :ensure nil ;; Org is built-in, don't try to install it
  :bind 
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c o d" . my/open-org-directory)) ;; Bind custom function here
  
  :config
  ;; -- Variables --
  (setq org-directory "~/org/beorg")
  
  ;; Recursively find .org files for agenda
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  
  ;; Capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/beorg/inbox.org" "Tasks")
           "* TODO %?\n  %i\n  %a")))

  ;; -- Custom Functions --
  
  (defun my/open-org-directory ()
    "Open the user's main Org directory in Dired."
    (interactive)
    (let ((org-dir (file-truename org-directory)))
      (if (file-directory-p org-dir)
          (dired org-dir)
        (message "Org directory not found at: %s" org-dir)))))

