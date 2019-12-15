;; Not my shit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(git-gutter:modified-sign "~")
 '(helm-completion-style (quote emacs))
 '(lsp-haskell-process-path-hie "~/.local/bin/hie-8.6.5")
 '(org-agenda-files (quote ("~/.emacs.d/org/tasks.org")))
 '(package-selected-packages
   (quote
    (bnf-mode powerline eglot scala-mode rjsx-mode evil company auto-virtualenv pipenv evil-magit magit evil-numbers helm-projectile hl-fill-column all-the-icons-dired all-the-icons haskell-mode projectile yasnippet highlight-indent-guides git-gutter diminish rainbow-delimiters rainvow-delimiters hlinum linum-highlight-current-line-number smooth-scrolling use-package org-evil org-bullets nord-theme helm evil-visual-mark-mode evil-surround evil-leader evil-commentary))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; My shit
;; Disable startup message and gtk pop-ups
(setq inhibit-startup-message t
      x-gtk-use-system-tooltips nil
      use-dialog-box nil)
;; Disable menu-bar
(menu-bar-mode -1)
;; Disable scroll-bar
(scroll-bar-mode -1)
;; Disable tool-bar
(tool-bar-mode -1)
;; Disable tooltip
(tooltip-mode -1)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Save and restore session
(desktop-save-mode t)
(save-place-mode t)

;; Font and font size
(set-face-attribute 'default nil :height 100)
(set-frame-font "Hack Nerd Font Mono-10")

;; Don't create backup files
(setq make-backup-files nil)

;; Disable auto-saves
(setq auto-save-default nil)

;; Non-blinking cursor
(blink-cursor-mode 0)

;; Highlight current line
(global-hl-line-mode t)

;; Auto-follow symlinks
(setq vc-follow-symlinks t)

;; Move flymake garbage
(setq flymake-run-in-place nil)
(setq temporary-file-directory "~/.emacs.d/tmp/")

;; Wind Mode
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;; Buffer
(global-set-key (kbd "M-<tab>") 'next-buffer)
(global-set-key (kbd "<backtab>") 'previous-buffer)

;; Relative line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(package-initialize)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmoderog/elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))

(use-package bnf-mode
  :ensure t)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c m") 'magit-status))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

;; Helm
(use-package helm
  :ensure t
  :config
  (require 'helm)
  (require 'helm-config)
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-autoresize-min-height 20
        helm-autoresize-max-height 20
        helm-autoresize-mode t)
  (helm-mode t)
  (helm-autoresize-mode t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;; Org-mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; Evil
;; (use-package treemacs-evil
;;   :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode t))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))


(use-package evil-leader
  :ensure t
  :config
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "q" 'kill-this-buffer
    "b" 'helm-buffers-list
    "v" 'split-window-right
    "h" 'split-window-below
    "o" 'helm-find-files
    "," 'helm-projectile-find-file
    "e" 'eglot
    "d" 'eglot-find-declaration
    "f" 'eglot-format
    "a" 'eglot-code-actions
    "m" 'eglot-help-at-point))
    ;; "d" 'lsp-find-definition
    ;; "r" 'lsp-find-references
    ;; "i" 'lsp-find-implementation))

(use-package org-evil
  :ensure t)

(use-package evil
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-u") (lambda()
                                                  (interactive)
                                                  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-d") (lambda()
                                                  (interactive)
                                                  (evil-scroll-down nil)))
  (add-hook 'evil-mode #'(lambda () (modify-syntax-entry ?_ "w")))
  (evil-mode t))

(use-package evil-numbers
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-magit
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (global-company-mode t))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))

(use-package auto-virtualenv
  :ensure t
  :config
  (setq python-shell-interpreter "python")
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv))

(use-package flymake-cursor
  :load-path "~/.emacs.d/lisp/emacs-flymake-cursor"
  :config
  (flymake-cursor-mode))

;; LSP

(use-package eglot
  :ensure t)
  ;; (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp"))))

;; ;; That shit lags as fuck
;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :config
;;   ;; (setq lsp-prefer-flymake nil)
;;   ;; (setq lsp-log-io t)
;;   (setq lsp-response-timeout 25
;;         lsp-document-sync-method 'full)
;;   (add-hook 'prog-mode-hook #'lsp-deferred)
;;   (global-set-key (kbd "C-c d") 'lsp-find-definition)
;;   (global-set-key (kbd "C-c r") 'lsp-find-references)
;;   (global-set-key (kbd "C-c i") 'lsp-find-implementation))

;; (use-package helm-lsp
;;   :ensure t)

;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode))

;; (use-package lsp-haskell
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode-hook #'lsp))

;; Doesn't work
;; (use-package ccls
;;   :ensure t
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list
;;   :config
;;   (lsp-treemacs-sync-mode t))

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   :config
;;   (push 'company-lsp company-backends))

(use-package haskell-mode
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

;; (use-package hl-fill-column
;;   :ensure t
;;   :config
;;   (global-hl-fill-column-mode))

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Clean up powerline
(use-package diminish
  :ensure t
  :config
  (diminish 'visual-line-mode)
  (diminish 'undo-tree-mode)
  (diminish 'projectile-mode)
  (diminish 'company-mode)
  (diminish 'git-gutter-mode)
  (diminish 'helm-mode)
  (diminish 'yas-minor-mode)
  (diminish 'highlight-indent-guides-mode)
  (diminish 'evil-commentary-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

;; Open i3-sensible-terminal
(global-set-key (kbd "M-RET") (kbd "M-! i3-sensible-terminal& RET"))

;; Remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Auto pairs
(electric-pair-mode t)

;; Highlight matching brace
(show-paren-mode t)

;; <esc> quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; use spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-default-style "k&r"
      c-basic-offset 4)

;; reload config
(defun reload-config ()
  "Reload config file (~/.emacs.d/init.el)."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(provide 'emacs)
;;; emacs ends here
