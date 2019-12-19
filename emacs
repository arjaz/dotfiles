;; Not my shit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    ((engine-omega "zathura")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "zathura")
     (output-pdf "zathura")
     (output-html "zathura"))))
 '(custom-safe-themes
   (quote
    ("d6c5b8dc6049f2e9dabdfcafa9ef2079352640e80dffe3e6cc07c0f89cbf9748" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(git-gutter:modified-sign "~")
 '(helm-completion-style (quote emacs))
 '(lsp-haskell-process-path-hie "~/.local/bin/hie-8.6.5")
 '(org-agenda-files (quote ("~/.emacs.d/org/tasks.org")))
 '(package-selected-packages
   (quote
    (git-gutter-fringe doom-modeline hl-todo auctex terminal-here bnf-mode eglot scala-mode rjsx-mode evil company auto-virtualenv pipenv evil-magit magit evil-numbers helm-projectile hl-fill-column all-the-icons-dired all-the-icons haskell-mode projectile yasnippet highlight-indent-guides git-gutter diminish rainbow-delimiters rainvow-delimiters hlinum linum-highlight-current-line-number smooth-scrolling use-package org-evil org-bullets helm evil-visual-mark-mode evil-surround evil-leader evil-commentary))))
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
;; (global-set-key (kbd "C-h") 'windmove-left)
;; (global-set-key (kbd "C-j") 'windmove-down)
;; (global-set-key (kbd "C-k") 'windmove-up)
;; (global-set-key (kbd "C-l") 'windmove-right)

;; Buffers
;; (global-set-key (kbd "M-<tab>") 'next-buffer)
;; (global-set-key (kbd "<backtab>") 'previous-buffer)

;; Clean buffers
(setq clean-buffer-list-delay-general 1)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; Folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Zathura for pdf
(setq TeX-view-program-list '(("zathura" "zathura --page=%(outpage) %o")))

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

;; (use-package nord-theme
;;   :ensure t
;;   :config
;;   (load-theme 'nord t))

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

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

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

(use-package eglot
  :ensure t)

;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode))

;; (use-package lsp-haskell
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode-hook #'lsp))

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

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-lsp t))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:window-width 2
        git-gutter:update-interval 1
        git-gutter:ask-p nil))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))

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

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode t))

(use-package terminal-here
  :ensure t
  :config
  (setq terminal-here-terminal-command (list "st" "--"))
  (global-set-key (kbd "M-RET") #'terminal-here-launch))

;; Go away mouse I don't like you
(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode)
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map)))

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
