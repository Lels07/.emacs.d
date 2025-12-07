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
;; Increase output limit for subprocesses
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


;; use https://github.com/Echinoidea/Aporetic-Nerd-Font nerd font version of https://github.com/protesilaos/aporetic
(set-face-attribute 'default nil :family "Roboto Mono" :height 180)


;; Theming using prot's modus operandi

(use-package modus-themes
  :ensure t
  :demand t
  :init
  ;; Starting with version 5.0.0 of the `modus-themes', other packages
  ;; can be built on top to provide their own "Modus" derivatives.
  ;; For example, this is what I do with my `ef-themes' and
  ;; `standard-themes' (starting with versions 2.0.0 and 3.0.0,
  ;; respectively).
  ;;
  ;; The `modus-themes-include-derivatives-mode' makes all Modus
  ;; commands that act on a theme consider all such derivatives, if
  ;; their respective packages are available and have been loaded.
  ;;
  ;; Note that those packages can even completely take over from the
  ;; Modus themes such that, for example, `modus-themes-rotate' only
  ;; goes through the Ef themes (to this end, the Ef themes provide
  ;; the `ef-themes-take-over-modus-themes-mode' and the Standard
  ;; themes have the `standard-themes-take-over-modus-themes-mode'
  ;; equivalent).
  ;;
  ;; If you only care about the Modus themes, then (i) you do not need
  ;; to enable the `modus-themes-include-derivatives-mode' and (ii) do
  ;; not install and activate those other theme packages.
  ;; (modus-themes-include-derivatives-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  ;; Your customizations here:
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-to-rotate modus-themes-items
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides nil)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'modus-vivendi))

;; --- 5. EDITING TOOLS ---

;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C-." . mc/mark-next-like-this-word)
         ("C-," . mc/mark-previous-like-this-word)
         ("C-M-." . mc/mark-all-words-like-this)))


;; Expand region package
(use-package expand-region
  :bind ("C-=" . er/expand-region))


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

