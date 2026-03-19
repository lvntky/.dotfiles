;;; init.el --- Emacs Configuration
;;; Commentary:
;; Emacs 28.2 configuration for C/C++/Python/ASM development
;; Focus: Performance, minimal UI, great buffer/window management

;;; Code:

;; ============================================================================
;; PERFORMANCE & STARTUP
;; ============================================================================

(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; ============================================================================
;; PACKAGE MANAGEMENT
;; ============================================================================

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(setq package-install-upgrade-built-in t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ============================================================================
;; BASIC SETTINGS
;; ============================================================================

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 ring-bell-function 'ignore
 scroll-conservatively 101
 scroll-margin 3)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(recentf-mode 1)
(setq recentf-max-saved-items 50)

(save-place-mode 1)

;; ============================================================================
;; THEME & FONT
;; ============================================================================

(use-package emacs
  :config
  (load-theme 'modus-vivendi t))

(set-face-attribute 'default nil
                    :family "Martian Mono"
                    :height 160)

(set-face-attribute 'fixed-pitch nil
                    :family "Martian Mono"
                    :height 160)

;; ============================================================================
;; WHICH-KEY
;; ============================================================================

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; ============================================================================
;; WINDOW & BUFFER MANAGEMENT
;; ============================================================================

(use-package winum
  :config
  (winum-mode)
  (setq winum-auto-setup-mode-line nil))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always t))

(use-package windmove
  :config
  (windmove-default-keybindings 'shift))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ============================================================================
;; COMPLETION FRAMEWORK - COMPANY (SADELEŞTİRİLMİŞ)
;; ============================================================================

(use-package company
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2

        ;; GÜRÜLTÜYÜ KES
        company-show-quick-access nil
        company-tooltip-align-annotations nil
        company-tooltip-limit 6
        company-tooltip-margin 0
        company-tooltip-offset-display 'lines

        ;; SADE FRONTEND
        company-format-margin-function nil
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))

  :bind (:map company-active-map
              ("TAB" . company-complete-selection)
              ("<tab>" . company-complete-selection)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :hook (after-init . global-company-mode))

;; ============================================================================
;; SNIPPETS
;; ============================================================================

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; ============================================================================
;; VISUAL HELPERS
;; ============================================================================

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================================
;; GIT
;; ============================================================================

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; ============================================================================
;; NAVIGATION
;; ============================================================================

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'completing-read))

;; ============================================================================
;; PROJECTS
;; ============================================================================

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Dev/"))
  (setq projectile-switch-project-action #'projectile-dired))

;; ============================================================================
;; C / C++
;; ============================================================================

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "linux"
        c-basic-offset 4)

  (add-hook 'c++-mode-hook
            (lambda ()
              (c-set-offset 'innamespace 0))))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;; ============================================================================
;; PYTHON
;; ============================================================================

(use-package python
  :ensure nil
  :config
  (setq python-shell-interpreter "python3"
        python-indent-offset 4))

;; ============================================================================
;; ASSEMBLY
;; ============================================================================

(use-package nasm-mode
  :mode "\\.\\(asm\\|s\\|nasm\\)$"
  :config
  (add-hook 'nasm-mode-hook
            (lambda ()
              (setq tab-width 8
                    indent-tabs-mode t))))

(add-to-list 'auto-mode-alist '("\\.fasm\\'" . asm-mode))

;; ============================================================================
;; HELP & SEARCH
;; ============================================================================

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

(use-package rg
  :config
  (rg-enable-default-bindings))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(defun open-init-file ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'open-init-file)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (message "init.el reloaded!"))
(global-set-key (kbd "C-c r") 'reload-init-file)

(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)

;; ============================================================================
;; MODELINE
;; ============================================================================

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                " "
                mode-line-buffer-identification
                "  "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))


;; ============================================================================
;; CTAGS
;; ============================================================================
(use-package citre
  :ensure t
  :init (require 'citre-config)
  :bind (:map citre-mode-map
         ("M-."     . citre-jump)
         ("M-,"     . citre-jump-back)
         ("C-x c p" . citre-peek)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
