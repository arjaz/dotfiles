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
 '(ansi-color-names-vector
   ["#282828" "#fb4934" "#8ec07c" "#fabd2f" "#268bd2" "#fb2874" "#83a598" "#ebdbb2"])
 '(custom-safe-themes
   (quote
    ("e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "d6c5b8dc6049f2e9dabdfcafa9ef2079352640e80dffe3e6cc07c0f89cbf9748" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(fci-rule-color "#555556")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fabd2f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#8ec07c"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#555556"))
 '(lsp-haskell-process-path-hie "~/.local/bin/hie-8.6.5")
 '(objed-cursor-color "#fb4934")
 '(org-agenda-files (quote ("~/.emacs.d/org/tasks.org")))
 '(package-selected-packages
   (quote
    (dired-sidebar all-the-icons-ivy auto-package-update package-safe-delete ledger-mode telega yasnippet-snippets use-package typescript-mode terminal-here scala-mode rjsx-mode rainbow-delimiters purescript-mode powerline pipenv org-evil org-bullets nord-theme markdown-mode key-chord hl-todo highlight-indent-guides haskell-mode git-gutter-fringe evil-surround evil-numbers evil-magit evil-leader evil-escape evil-commentary eglot doom-themes doom-modeline disable-mouse diminish company bnf-mode auto-virtualenv auctex all-the-icons-dired)))
 '(pdf-view-midnight-colors (cons "#ebdbb2" "#282828"))
 '(rustic-ansi-faces
   ["#282828" "#fb4934" "#8ec07c" "#fabd2f" "#268bd2" "#fb2874" "#83a598" "#ebdbb2"])
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(vc-annotate-background "#282828")
 '(vc-annotate-color-map
   (list
    (cons 20 "#8ec07c")
    (cons 40 "#b2bf62")
    (cons 60 "#d5be48")
    (cons 80 "#fabd2f")
    (cons 100 "#fba827")
    (cons 120 "#fc9420")
    (cons 140 "#fe8019")
    (cons 160 "#fd6237")
    (cons 180 "#fb4555")
    (cons 200 "#fb2874")
    (cons 220 "#fb335e")
    (cons 240 "#fa3e49")
    (cons 260 "#fb4934")
    (cons 280 "#d14c3c")
    (cons 300 "#a84f45")
    (cons 320 "#7e514d")
    (cons 340 "#555556")
    (cons 360 "#555556")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; My shit

;; Startup optimization
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 134217728 ; 128mb
          gc-cons-percentage 0.1)))

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
(set-frame-font "Fira Code-10")
(setq default-frame-alist '((font . "Fira Code-10")))
;; (set-frame-font "Inconsolata-12")
;; (setq default-frame-alist '((font . "Inconsolata-12")))

;; Don't create backup files
(setq make-backup-files nil)

;; Disable auto-saves
(setq auto-save-default nil)

;; Non-blinking cursor
(blink-cursor-mode 0)

;; Remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Auto pairs
(electric-pair-mode t)

;; Highlight matching brace
(show-paren-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Symlinks
(setq vc-follow-symlinks t)
(setq find-file-visit-truename t)

;; Move flymake garbage
(setq flymake-run-in-place nil)
(setq temporary-file-directory "~/.emacs.d/tmp/")

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

;; use spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq js-indent-level 2)

;; Smooth scrolling
(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering

;; open *.h as c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(package-initialize)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmoderog/elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package magit
  :config
  (global-set-key (kbd "C-c m") 'magit-status))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

(use-package buffer-flip
  :defer t
  :bind (("<backtab>" . buffer-flip-backward)
         ("M-<tab>" . buffer-flip-forward))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))

(use-package ivy
  :config
  (ivy-mode t)
  (counsel-mode t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line))

(use-package ivy-hydra)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode t))

;; Org-mode
(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))

;; Evil
(setq evil-want-keybinding nil)
(setq evil-want-integration t)

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-leader
  :config
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "q" 'evil-quit
    "t" 'dired-sidebar-toggle-with-current-directory
    "b" 'ivy-switch-buffer
    "s" 'swiper-isearch
    "a" 'counsel-ag
    "v" 'evil-window-vsplit
    "h" 'evil-window-split
    "o" 'counsel-find-file
    "," 'counsel-projectile-find-file
    "j" 'counsel-file-jump
    "g" 'counsel-bookmark
    "e" 'eglot
    "d" 'eglot-find-declaration
    "f" 'eglot-format
    "r" 'eglot-rename
    "m" 'eglot-help-at-point))

(use-package org-evil
  :defer t)

(use-package evil
  :hook (after-change-major-mode . (lambda () (modify-syntax-entry ?_ "w")))
  :config
  (define-key evil-normal-state-map (kbd "C-u") (lambda()
                                                  (interactive)
                                                  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-d") (lambda()
                                                  (interactive)
                                                  (evil-scroll-down nil)))
  (evil-mode t)
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c c") 'evil-numbers/dec-at-pt))

(use-package key-chord
  :config
  (key-chord-mode t)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package evil-magit)

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package company
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "<return>") 'company-complete)
  (global-company-mode t))

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets)

(use-package yapfify
  :hook (python-mode . yapf-mode))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode)
  :hook (rjsx-mode . prettier-js-mode))

(use-package flymake-cursor
  :load-path "~/.emacs.d/lisp/emacs-flymake-cursor"
  :defer t
  :config
  (flymake-cursor-mode))

(use-package eglot)
  ;; :hook (prog-mode . eglot-ensure))

(use-package cmake-mode)

(use-package haskell-mode
  :defer t
  :config
  (flymake-mode 0))

(use-package csharp-mode)

(use-package rjsx-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package typescript-mode
  :defer t)

(use-package purescript-mode
  :defer t)

(use-package web-mode
  :defer t)

(use-package scala-mode
  :defer t
  :interpreter
  ("scala" . scala-mode))

(use-package markdown-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package bnf-mode
  :defer t)

(use-package tex
  :ensure auctex)

(use-package vimrc-mode
  :defer t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon t)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-modal-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-lsp t))

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:window-width 2
        git-gutter:update-interval 1
        git-gutter:ask-p nil))

(use-package git-gutter-fringe
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

(use-package highlight-indent-guides
 :hook (prog-mode . highlight-indent-guides-mode)
 :config
 (setq highlight-indent-guides-method 'character)
 (setq highlight-indent-guides-responsive 'stack))

(use-package all-the-icons)

(use-package all-the-icons-dired
 :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-sidebar)

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package terminal-here
  :config
  (setq terminal-here-terminal-command (list "st" "--"))
  (global-set-key (kbd "M-RET") #'terminal-here-launch))

(use-package ledger-mode
  :defer t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Go away mouse I don't like you
(use-package disable-mouse
  :config
  (global-disable-mouse-mode)
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map)))

;; <esc> quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;"
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

(provide 'emacs)
;;; emacs ends here
