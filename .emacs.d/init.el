;;; init.el --- Emacs Configuration -*- lexical-binding: t; -*-

;;; Code:

;; ============================================================================
;; PERFORMANCE & STARTUP
;; ============================================================================

(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1000 1000))))

(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil
      native-comp-async-report-warnings-errors 'silent)

;; ============================================================================
;; CUSTOM FILE
;; ============================================================================

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;; ============================================================================
;; PRE-LOAD DECLARATIONS
;; ============================================================================

(setq org-replace-disputed-keys t)

;; ============================================================================
;; PACKAGE MANAGEMENT
;; ============================================================================

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(setq package-install-upgrade-built-in t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
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
                org-agenda-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                compilation-mode-hook
                helpful-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4
 fill-column 80)

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      ring-bell-function 'ignore
      scroll-conservatively 101
      scroll-margin 3
      require-final-newline t
      sentence-end-double-space nil
      vc-follow-symlinks t
      use-short-answers t
      tramp-default-method "ssh")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

(show-paren-mode 1)
(setq show-paren-delay 0)

(delete-selection-mode 1)
(global-so-long-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(recentf-mode 1)
(setq recentf-max-saved-items 50
      recentf-exclude '("/tmp/" "/ssh:" "/sudo:"))

(save-place-mode 1)
(savehist-mode 1)
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring compile-history))

(setq dired-listing-switches "-alh --group-directories-first"
      dired-dwim-target t
      dired-recursive-copies 'always
      dired-recursive-deletes 'top)

;; ============================================================================
;; THEME & FONT
;; ============================================================================

(setq modus-themes-mixed-fonts t
      modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-org-blocks 'gray-background
      modus-themes-headings
      '((0 . (variable-pitch light 1.5))
        (1 . (variable-pitch semibold 1.35))
        (2 . (variable-pitch semibold 1.2))
        (3 . (variable-pitch 1.1))
        (agenda-date . (semibold 1.2))
        (agenda-structure . (variable-pitch light 1.5))
        (t . (1.0))))

(load-theme 'modus-vivendi t)

(add-to-list 'default-frame-alist '(font . "Martian Mono-16"))

(defun lk/pick-font (candidates fallback)
  (or (seq-find (lambda (f) (member f (font-family-list))) candidates)
      fallback))

(defun lk/set-fonts (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (let ((mono (lk/pick-font '("Martian Mono" "Iosevka" "JetBrains Mono")
                              "Monospace"))
          (vari (lk/pick-font '("Iosevka Aile" "IBM Plex Sans" "Source Sans 3"
                                "Cantarell" "DejaVu Sans")
                              "Sans Serif")))
      (set-face-attribute 'default nil :family mono :height 160)
      (set-face-attribute 'fixed-pitch nil :family mono :height 160)
      (set-face-attribute 'variable-pitch nil :family vari :height 170))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'lk/set-fonts)
  (lk/set-fonts))

;; ============================================================================
;; WHICH-KEY
;; ============================================================================

(use-package which-key
  :ensure nil
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; ============================================================================
;; WINDOW & BUFFER MANAGEMENT
;; ============================================================================

(use-package winum
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6))
  :config
  (winum-mode)
  (setq winum-auto-setup-mode-line nil))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-dispatch-always t))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ============================================================================
;; TREE-SITTER
;; ============================================================================

(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (python "https://github.com/tree-sitter/tree-sitter-python")))

(defun lk/treesit-install-grammars ()
  (interactive)
  (if (not (and (fboundp 'treesit-available-p) (treesit-available-p)))
      (message "This Emacs was built without tree-sitter support")
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)))))

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (dolist (pair '((c-mode . c-ts-mode)
                  (c++-mode . c++-ts-mode)
                  (python-mode . python-ts-mode)))
    (add-to-list 'major-mode-remap-alist pair)))

(setq c-ts-mode-indent-offset 4
      c-ts-mode-indent-style 'linux)

;; ============================================================================
;; EGLOT / LSP
;; ============================================================================

(use-package eglot
  :ensure nil
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode python-mode python-ts-mode)
         . eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-report-progress nil)
  (if (boundp 'eglot-events-buffer-config)
      (setq eglot-events-buffer-config '(:size 0 :format full))
    (setq eglot-events-buffer-size 0))
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode)
                 . ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--header-insertion=never"
                    "--completion-style=detailed")))
  :bind (:map eglot-mode-map
              ("C-c e r" . eglot-rename)
              ("C-c e a" . eglot-code-actions)
              ("C-c e f" . eglot-format-buffer)))

(setq eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-prefer-doc-buffer t)

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

;; ============================================================================
;; COMPLETION FRAMEWORK - COMPANY
;; ============================================================================

(use-package company
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2
        company-show-quick-access nil
        company-tooltip-align-annotations nil
        company-tooltip-limit 6
        company-tooltip-margin 0
        company-tooltip-offset-display 'lines
        company-format-margin-function nil
        company-backends '(company-capf company-files)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  :bind (("C-<tab>" . company-complete)
         :map company-active-map
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

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode)
  :config
  (setq dtrt-indent-verbosity 0))

;; ============================================================================
;; GIT
;; ============================================================================

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq git-commit-summary-max-length 72))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

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

(use-package clang-format)

(use-package disaster
  :commands disaster)

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c f") 'clang-format-buffer)
  (define-key c-mode-base-map (kbd "C-c d") 'disaster))

(with-eval-after-load 'c-ts-mode
  (define-key c-ts-base-mode-map (kbd "C-c f") 'clang-format-buffer)
  (define-key c-ts-base-mode-map (kbd "C-c d") 'disaster))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; ============================================================================
;; PYTHON
;; ============================================================================

(use-package python
  :ensure nil
  :config
  (setq python-shell-interpreter "python3"
        python-indent-offset 4))

;; ============================================================================
;; ASSEMBLY & BINARY
;; ============================================================================

(use-package nasm-mode
  :mode "\\.\\(asm\\|nasm\\)\\'"
  :config
  (add-hook 'nasm-mode-hook
            (lambda ()
              (setq tab-width 8
                    indent-tabs-mode t))))

(add-to-list 'auto-mode-alist '("\\.\\(s\\|S\\|fasm\\)\\'" . asm-mode))
(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 8
                  indent-tabs-mode t)))

(use-package x86-lookup
  :bind ("C-h x" . x86-lookup)
  :config
  (setq x86-lookup-pdf (expand-file-name "~/doc/intel-sdm.pdf")))

(use-package nhexl-mode
  :commands nhexl-mode)

;; ============================================================================
;; ZIG
;; ============================================================================

(use-package zig-mode
  :mode "\\.\\(zig\\|zon\\)\\'"
  :hook (zig-mode . eglot-ensure)
  :config
  (setq zig-format-on-save nil)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(zig-mode . ("zls")))))

;; ============================================================================
;; MARKUP & CONFIG FORMATS
;; ============================================================================

(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; ============================================================================
;; ORG - CORE
;; ============================================================================

(defvar lk/org-dir (expand-file-name "~/org"))
(defvar lk/org-notes-dir (expand-file-name "notes" lk/org-dir))

(dolist (dir (list lk/org-dir lk/org-notes-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun lk/org-mode-setup ()
  (setq-local company-idle-delay nil
              fill-column 92
              electric-pair-inhibit-predicate
              (lambda (c)
                (or (memq c '(?< ?>))
                    (electric-pair-default-inhibit c)))))

(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c C-t" . org-todo)
         ("M-g h" . consult-org-heading))
  :hook ((org-mode . lk/org-mode-setup)
         (org-mode . visual-line-mode))
  :init
  (setq org-directory lk/org-dir
        org-agenda-files (list lk/org-dir)
        org-default-notes-file (expand-file-name "inbox.org" lk/org-dir))
  :config
  (require 'org-tempo)

  (setq org-startup-indented t
        org-startup-folded 'content
        org-startup-with-inline-images t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis " ..."
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-insert-heading-respect-content t
        org-M-RET-may-split-line nil
        org-return-follows-link t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-image-actual-width '(640)
        org-tags-column 0
        org-auto-align-tags nil
        org-log-done 'time
        org-log-into-drawer t
        org-cycle-separator-lines 1)

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)"
                    "|" "DONE(d!)" "KILL(k@)")))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline ,(expand-file-name "inbox.org" lk/org-dir) "Inbox")
           "* TODO %?\n%U\n%a")
          ("n" "Note" entry
           (file+headline ,(expand-file-name "inbox.org" lk/org-dir) "Notes")
           "* %?\n%U")
          ("c" "Code reference" entry
           (file+headline ,(expand-file-name "inbox.org" lk/org-dir) "Code")
           "* TODO %?\n%U\n%a\n#+begin_src %^{lang}\n%i\n#+end_src")
          ("j" "Journal" entry
           (file+olp+datetree ,(expand-file-name "journal.org" lk/org-dir))
           "* %<%H:%M> %?\n%i")))

  (setq org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-tags-column 0
        org-agenda-block-separator ?-)

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT" ((org-agenda-overriding-header "Next")))
            (todo "WAIT" ((org-agenda-overriding-header "Blocked")))))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (python . t)
     (shell . t)))

  (setq org-babel-C-compiler "gcc"
        org-babel-C++-compiler "g++"))

(with-eval-after-load 'org-src
  (dolist (pair '(("C" . c-ts)
                  ("c" . c-ts)
                  ("C++" . c++-ts)
                  ("cpp" . c++-ts)
                  ("python" . python-ts)
                  ("zig" . zig)
                  ("asm" . asm)
                  ("nasm" . nasm)
                  ("cmake" . cmake)
                  ("yaml" . yaml)))
    (add-to-list 'org-src-lang-modes pair)))

;; ============================================================================
;; ORG - PRESENTATION
;; ============================================================================

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-star 'replace
        org-modern-hide-stars nil
        org-modern-table nil
        org-modern-list '((?- . "–") (?* . "•") (?+ . "‣"))
        org-modern-checkbox nil
        org-modern-block-name '("" . "")
        org-modern-keyword nil))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-delay 0.1))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height nil)
  (dolist (face '(org-table org-code org-block org-block-begin-line
                  org-block-end-line org-verbatim org-special-keyword
                  org-property-value org-drawer org-date org-tag
                  org-formula org-meta-line))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

;; ============================================================================
;; ORG - NOTES (DENOTE)
;; ============================================================================

(use-package denote
  :bind (("C-c n n" . denote)
         ("C-c n c" . denote-region)
         ("C-c n t" . denote-type)
         ("C-c n i" . denote-link)
         ("C-c n I" . denote-add-links)
         ("C-c n b" . denote-backlinks)
         ("C-c n f" . denote-open-or-create)
         ("C-c n r" . denote-rename-file)
         ("C-c n k" . denote-rename-file-using-front-matter))
  :hook (dired-mode . denote-dired-mode)
  :config
  (setq denote-directory lk/org-notes-dir
        denote-file-type 'org
        denote-known-keywords '("c" "os" "kernel" "graphics" "thesis"
                                "zig" "asm" "paper" "work")
        denote-sort-keywords t
        denote-date-prompt-use-org-read-date t
        denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (denote-rename-buffer-mode 1))

;; ============================================================================
;; DEBUGGING
;; ============================================================================

(setq gdb-many-windows t
      gdb-show-main t
      gdb-restore-window-configuration-after-quit t)

;; ============================================================================
;; COMPILATION
;; ============================================================================

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-scroll-output 'first-error
      compilation-ask-about-save nil
      compile-command "make -k ")

(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "<f7>") 'gdb)

;; ============================================================================
;; HELP & SEARCH
;; ============================================================================

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-symbol] . helpful-symbol))

(use-package rg
  :config
  (rg-enable-default-bindings))

;; ============================================================================
;; MINIBUFFER - VERTICO / ORDERLESS / CONSULT / MARGINALIA / EMBARK
;; ============================================================================

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-count 12))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind
  ("C-x b"   . consult-buffer)
  ("C-x r b" . consult-bookmark)
  ("M-y"     . consult-yank-pop)
  ("M-s r"   . consult-ripgrep)
  ("M-s l"   . consult-line)
  ("M-s f"   . consult-find)
  ("M-g g"   . consult-goto-line)
  ("M-g i"   . consult-imenu)
  ("M-g f"   . consult-flymake)
  ("M-g a"   . consult-org-agenda)
  :config
  (setq consult-preview-key "M-."))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-x k") 'kill-current-buffer)

(defun lk/open-init-file ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'lk/open-init-file)

(defun lk/reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (message "init.el reloaded"))
(global-set-key (kbd "C-c r") 'lk/reload-init-file)

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
;; PAIR MODE
;; ============================================================================

(electric-pair-mode 1)

(provide 'init)
;;; init.el ends here
