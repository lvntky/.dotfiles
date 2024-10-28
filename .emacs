;;(package-initialize)
(setq custom-file "~/.emacs.custom.el")

(load-file "~/.emacs.rc/rc.el")



;; package management stuff
(require 'package)

;; Add MELPA repository
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)

(add-to-list 'default-frame-alist `(font . "Iosevka-20"))
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(delete-selection-mode 1)
;;(buffer-number-mode 1)
;; Ensure guru-mode is available
(require 'guru-mode)

;; Enable guru-mode globally
(guru-global-mode +1)


;; custom el loading starts

(add-to-list 'load-path "~/.emacs.local/")
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; custom el loading ends

(rc/require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; multiple cursors
(rc/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; yasnippet and company stuff starts
;; Enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Enable company-mode for auto-completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Optional: Improve company completion delay and settings
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)

;; Use tab to cycle through company completions
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(define-key company-active-map [tab] 'company-complete-common-or-cycle)

;; Integrating yasnippet with company-mode
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Add yasnippet support to company BACKEND."
  (if (or (not company-mode/enable-yas)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; Optionally use `yasnippet` with `company-mode` in specific modes
(add-hook 'prog-mode-hook
          (lambda ()
            (yas-minor-mode)
            (company-mode)))

;; yasnippet and company stuff ends

(load-file custom-file)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)


(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Ensure undo-tree is installed
(require 'undo-tree)

;; Enable undo-tree globally
(global-undo-tree-mode 1)

;; Optionally save the undo history between sessions
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history")))

;; Optionally disable the auto-save file if you don't want to keep history
;; (setq undo-tree-auto-save-history nil)


;; Use ibuffer instead of the default buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
