;;; init.el --- My configuration
;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; My Emacs configuration

;;; Code:

(defvar comp-deferred-compilation-deny-list ())
(defvar bootstrap-version)
(defvar straight-repository-branch "develop")
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package use-package-core
  :straight (:type built-in)
  :custom (use-package-hook-name-suffix nil))

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-check-for-modifications '(watch-files find-when-checking)))

(use-package system-packages
  :custom
  (system-packages-package-manager 'pacman))

(use-package use-package-ensure-system-package)

(use-package emacs
  :straight (:type built-in)
  :custom
  (tab-always-indent 'complete)
  (inhibit-compacting-font-caches t)
  (gc-cons-threshold most-positive-fixnum "2^61 bytes")
  (gc-cons-percentage 0.6)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)
  (x-gtk-use-system-tooltips nil)
  (use-dialog-box nil)
  (inhibit-startup-message t "disable startup message and gtk pop-ups")
  (vc-follow-symlinks t)
  (temporary-file-directory (concat user-emacs-directory "tmp"))
  (auto-save-timeout 20 "number of seconds idle time before auto-save")
  (auto-save-interval 200 "number of keystrokes between auto-saves")
  (vc-make-backup-files t "make backups for version-controlled files as well")
  (create-lockfiles nil)
  (savehist-file (concat user-emacs-directory "savehist"))
  (history-length t)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (enable-recursive-minibuffers t)
  :hook
  (emacs-startup-hook . (lambda ()
                          (setq gc-cons-threshold 536870912 ; 512mb
                                gc-cons-percentage 0.1)))
  :config
  (let ((tmp-dir (concat user-emacs-directory "tmp")))
    (unless (file-exists-p tmp-dir)
      (make-directory tmp-dir t)))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (savehist-mode t)
  (setq-default
   indent-tabs-mode nil
   tab-width 4))

(use-package display-fill-column-indicator
  :straight (:type built-in)
  :config
  (setq-default fill-column 100))

(use-package frame
  :straight (:type built-in)
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)
  (cursor-type 'hbar)
  :config
  (unbind-key (kbd "C-x C-z") 'global-map)
  (window-divider-mode t)
  (blink-cursor-mode 0))

(use-package faces
  :straight (:type built-in)
  :preface
  (defun init-fonts ()
    (interactive)
    (defvar used-font "Iosevka")
    (add-to-list 'default-frame-alist `(font . ,used-font))
    (set-face-attribute 'default nil :family used-font :height 105)
    (set-face-attribute 'variable-pitch nil :family "Roboto" :height 100)
    (set-frame-font used-font))
  :hook
  (server-after-make-frame-hook . init-fonts)
  (window-setup-hook . init-fonts))

(use-package cus-edit
  :straight (:type built-in)
  :custom
  (custom-file (concat user-emacs-directory "garbage.el"))
  :config
  (load custom-file))

(use-package autorevert
  :straight (:type built-in)
  :custom
  (auto-revert-interval 2)
  :config
  (global-auto-revert-mode t))

(use-package window
  :straight (:type built-in)
  :demand
  :preface
  (defun split-window-right+switch ()
    (interactive)
    (split-window-right)
    (other-window 1))
  (defun split-window-below+switch ()
    (interactive)
    (split-window-below)
    (other-window 1))
  :bind
  (("C-x 2" . split-window-below+switch)
   ("C-x 3" . split-window-right+switch)
   :map ctl-x-map
   ([remap split-window-below] . split-window-below+switch)
   ([remap split-window-right] . split-window-right+switch)))

(use-package lisp-mode
  :straight (:type built-in)
  :demand t
  :config
  (let ((path "~/.dotfiles/emacs/elisp-fix-indent.el"))
    (when (file-exists-p path)
      (load path))))

(use-package bookmark
  :straight (:type built-in)
  :custom
  (bookmark-fontify nil))

(use-package sql
  :demand
  :bind
  ("C-l" . comint-clear-buffer)
  :config
  (setq sql-postgres-login-params
        (append sql-postgres-login-params '(port))))

(use-package simple
  :straight (:type built-in)
  :bind
  (:map ctl-x-map
   ("k" . kill-current-buffer)))

(use-package cc-vars
  :straight (:type built-in)
  :custom
  (c-default-style "k&r")
  (c-basic-offset 4)
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode)))

(use-package files
  :straight (:type built-in)
  :custom
  (enable-local-eval t)
  (confirm-kill-processes nil)
  (find-file-visit-truename t)
  (make-backup-files t "backup of a file the first time it is saved")
  (backup-by-copying t "don't clobber symlinks")
  (delete-old-versions t "delete excess backup files silently")
  (version-control t "version numbers for backup files")
  (kept-new-versions 6 "oldest versions to keep when a new numbered backup is made")
  (kept-old-versions 2 "newest versions to keep when a new numbered backup is made")
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
  (auto-save-default t "auto-save every buffer that visits a file")
  :config
  (unless (file-exists-p (concat user-emacs-directory "backups"))
    (make-directory (concat user-emacs-directory "backups") t)))

(use-package hideshow
  :straight (:type built-in)
  :hook (prog-mode-hook . hs-minor-mode))

(use-package gcmh
  :demand
  :custom
  (gcmh-high-cons-threshold (/ 1073741824 2))
  :config
  (gcmh-mode))

(use-package so-long
  :config
  (global-so-long-mode))

(use-package prog-mode
  :disabled
  :straight (:type built-in)
  :config
  (global-prettify-symbols-mode))

(use-package dash)

(use-package helpful
  :demand
  :bind
  (([remap describe-key] . helpful-key)
   ([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)))

(use-package goto-chg
  :bind
  ("C-c b l l" . goto-last-change)
  ("C-c b l r" . goto-last-change-reverse))

(use-package visual-regexp
  :bind
  (([remap query-replace] . vr/replace)))

(use-package org
  :hook (org-babel-after-execute-hook . org-redisplay-inline-images)
  :preface
  (defun xmonad-org-capture ()
    (interactive)
    (org-capture nil "t"))
  :bind
  ("C-c b o o" . org-agenda)
  ("C-c b o c" . org-capture)
  :demand
  :custom
  (org-confirm-babel-evaluate nil)
  (org-directory "~/.org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-hide-leading-stars t)
  (org-startup-folded t)
  (org-startup-indented nil)
  (org-agenda-files (list org-default-notes-file))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t)
     (python     . t))))

(use-package dbus)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-gruvbox-light-variant "hard")

  :preface

  (defun load-dark-theme ()
    "Load the saved dark theme."
    (interactive)
    (message "Loading dark theme")
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme dark-theme t))

  (defun load-light-theme ()
    "Load the saved light theme."
    (interactive)
    (message "Loading light theme")
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme light-theme t))

  (defun load-theme-on-startup ()
    (interactive)
    (unless custom-enabled-themes
      (dbus-call-method-asynchronously
       :session "org.freedesktop.portal.Desktop"
       "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings"
       "Read"
       (lambda (value) (set-theme-from-dbus-value (caar value)))
       "org.freedesktop.appearance"
       "color-scheme")))

  (defun choose-theme (light-theme-p theme)
    (interactive (list
                  (y-or-n-p "Choose the light theme?")
                  (intern (completing-read "Load custom theme: " (mapcar #'symbol-name (custom-available-themes))))))
    (mapcar #'disable-theme custom-enabled-themes)
    (if light-theme-p
        (progn
          (setq light-theme theme)
          (load-light-theme))
      (setq dark-theme theme)
      (load-dark-theme)))

  (defun set-theme-from-dbus-value (value)
    (message "value is %s" value)
    (if (equal value '1)
        (load-dark-theme)
      (load-light-theme)))

  (defun dbus-on-theme-changed (path var value)
    (when (and (string-equal path "org.freedesktop.appearance")
               (string-equal var "color-scheme"))
      (set-theme-from-dbus-value (car value))))

  :init
  (defvar light-theme 'doom-flatwhite)
  ;; (defvar dark-theme 'doom-tomorrow-night)
  (defvar dark-theme 'doom-nord)

  :hook
  (server-after-make-frame-hook . (lambda () (interactive) (load-theme-on-startup)))
  :config
  (doom-themes-org-config)
  (dbus-register-signal
   :session "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings"
   "SettingChanged"
   #'dbus-on-theme-changed)
  (load-theme-on-startup))

(use-package feebleline
  :config
  ;; TODO: replace feebleline-related functions with custom ones
  (setq-default
   mode-line-format
   '("%5l:"
     (:propertize "_ " face magit-dimmed)
     (:propertize (:eval (feebleline-file-directory)) face font-lock-variable-name-face)
     (:propertize (:eval (feebleline-file-or-buffer-name)) face font-lock-keyword-face)
     (:propertize (:eval (feebleline-file-modified-star)) face font-lock-warning-face)
     " · "
     (:propertize (:eval (feebleline-git-branch)) face magit-dimmed))))

(use-package page-break-lines
  :config
  (page-break-lines-mode))


(use-package good-scroll
  :custom
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (good-scroll-avoid-vscroll-reset t)
  :demand t
  :preface
  (defun good-scroll-move-recenter (step)
    (interactive)
    (good-scroll-move step)
    (sit-for good-scroll-duration)
    (move-to-window-line nil))
  (defun good-scroll-up-half-screen ()
    (interactive)
    (good-scroll-move-recenter (/ (good-scroll--window-usable-height) 2)))
  (defun good-scroll-down-half-screen ()
    (interactive)
    (good-scroll-move-recenter (/ (good-scroll--window-usable-height) -2)))
  :config
  (good-scroll-mode)
  :bind
  ("C-z" . recenter))

(use-package prism
  :preface
  (defun nice-prism-colors (&rest _)
    (interactive)
    (prism-set-colors
     :lightens '(0)
     :desaturations (list (if (member light-theme custom-enabled-themes) 0 7.5))
     :colors (mapcar #'doom-color '(red blue magenta green cyan))))
  :custom
  (prism-parens t)
  :config
  (advice-add #'load-light-theme :after #'nice-prism-colors)
  (advice-add #'load-dark-theme  :after #'nice-prism-colors)
  :hook
  ((lisp-mode-hook clojure-mode-hook scheme-mode) . prism-mode)
  (prism-mode-hook . nice-prism-colors))

(use-package indent-guide
  :hook (python-mode-hook . indent-guide-mode)
  :custom (indent-guide-char ":"))

(use-package highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode-hook . hes-mode))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
   `(("TODO" warning bold)
     ("FIXME" error bold)
     ("HACK" font-lock-constant-face bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("NOTE" success bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("BUG" error bold)
     ("XXX" font-lock-constant-face bold)))
  :config
  (global-hl-todo-mode))

(use-package diff-hl
  :custom
  (left-fringe-width 3)
  :config
  (global-diff-hl-mode))

(use-package dired
  :straight (:type built-in)
  :hook (dired-mode-hook . auto-revert-mode)
  :custom
  (dired-listing-switches "-alhg")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top))

(use-package diredfl
  :hook (dired-mode-hook . diredfl-mode))

(use-package dired-hacks
  :disabled
  :bind
  (:map dired-mode-map
   ("<tab>" . dired-subtree-toggle))
  :demand
  :config
  (dired-async-mode))

(use-package multiple-cursors
  :bind
  (("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m c" . mc/edit-lines)))

(use-package ace-mc
  :bind
  ("C-c m o" . ace-mc-add-multiple-cursors))

(use-package expand-region
  :bind
  ("C-M-SPC" . er/expand-region)
  :custom
  (expand-region-smart-cursor t)
  (expand-region-contract-fast-key "C-M-SPC"))

(use-package embrace
  :bind
  ("M-c" . embrace-commander))

(use-package smart-comment
  :bind
  ("M-;" . smart-comment))

(use-package avy
  :preface
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  :bind
  (("M-t" . avy-goto-char-timer)
   ("C-t" . avy-goto-word-1)
   ("C-c a l" . avy-goto-line))
  :custom
  (avy-background t)
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (avy-dispatch-alist '((?\q . avy-action-embark)
                        (?\M-q . avy-action-embark))))

(use-package ace-window
  :custom
  (aw-scope 'frame)
  :bind
  ([remap other-window] . ace-window))

(use-package rotate
  :bind
  (("C-x C-o" . rotate-window)
   ("C-x M-o" . rotate-layout)))

(use-package undo-fu
  :bind
  (([remap undo] . undo-fu-only-undo)
   ([remap undo-redo] . undo-fu-only-redo)))

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

(use-package ws-butler
  :config
  (ws-butler-global-mode t))

(use-package dashboard
  :demand
  :custom
  (show-week-agenda-p t)
  (dashboard-set-heading-icons t)
  (dashboard-startup-banner 3)
  (dashboard-set-navigator t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-startup-banner "~/.config/emacs/emacs-dash.png")
  (dashboard-items '((agenda . 15)))
  (dashboard-banner-logo-title "Eendracht Maakt Macht")
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  :bind
  (:map dashboard-mode-map
   ("n" . dashboard-next-line)
   ("p" . dashboard-previous-line)))

(use-package olivetti
  :custom
  (olivetti-body-width 120)
  :bind
  (:map ctl-x-map
   ("n o" . olivetti-mode)))

(use-package elec-pair
  :straight (:type built-in)
  :hook (prog-mode-hook . electric-pair-mode))

(use-package puni
  :preface
  (defun puni-rewrap-with (ldelim rdelim)
    (interactive (list "(" ")"))
    (let ((point-pos (point)))
      (puni-squeeze)
      (insert ldelim)
      (yank)
      (insert rdelim)
      (goto-char point-pos)))

  (defun puni-rewrap-with-parens ()
    (interactive)
    (puni-rewrap-with "(" ")"))

  (defun puni-rewrap-with-braces ()
    (interactive)
    (puni-rewrap-with "[" "]"))

  (defun puni-rewrap-with-brackets ()
    (interactive)
    (puni-rewrap-with "{" "}"))

  (defun puni-rewrap-with-single-quotes ()
    (interactive)
    (puni-rewrap-with "'" "'"))

  (defun puni-rewrap-with-double-quotes ()
    (interactive)
    (puni-rewrap-with "\"" "\""))

  :bind
  (("M-r"   . puni-raise)
   ("M-s"   . puni-splice)
   ("M->"   . puni-slurp-forward)
   ("M-<"   . puni-barf-forward)
   ("C-M-s" . puni-squeeze)
   ("M-("   . puni-rewrap-with-parens)
   ("M-["   . puni-rewrap-with-braces)
   ("M-{"   . puni-rewrap-with-brackets)
   ("M-\""  . puni-rewrap-with-double-quotes)
   ("M-'"   . puni-rewrap-with-single-quotes)))

(use-package ansi-color
  :straight (:type built-in)
  :preface
  (defun colorize-compilation-buffer ()
    (read-only-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode))
  :hook (compilation-filter-hook . colorize-compilation-buffer))

(use-package xterm-color)

(use-package shell
  :straight (:type built-in)
  :demand
  :bind
  (:map shell-mode-map
   ("C-l" . comint-clear-buffer)))

(use-package esh-mode
  :straight (:type built-in)
  :demand
  :hook (eshell-before-prompt-hook
         .
         (lambda ()
           (setq-local xterm-color-preserve-properties t)))
  :custom
  (eshell-history-size 1024)
  (eshell-scroll-to-bottom-on-input nil)
  (eshell-hist-ignoredups t)
  :preface
  (defun eshell-new ()
    "Open a new eshell session."
    (interactive)
    (eshell 'N))
  (defun eshell/clear-buffer ()
    "Clear terminal."
    (interactive)
    (when (equal major-mode 'eshell-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input))))
  :bind
  (:map eshell-mode-map
   ("C-l" . eshell/clear-buffer)
   :map ctl-x-map
   ("e" . eshell)
   ("n e" . eshell-new))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))

(use-package eshell-up)

(use-package pcmpl-args)

(use-package bash-completion
  :ensure-system-package (bash))

(use-package fish-completion
  :ensure-system-package (fish)
  :after bash-completion
  :demand
  :custom
  (fish-completion-fallback-on-bash-p t)
  :config
  (global-fish-completion-mode))

(use-package eshell-prompt-extras
  :demand
  :custom
  (eshell-prompt-function 'epe-theme-dakrone)
  (eshell-highlight-prompt t))

(use-package eshell-syntax-highlighting
  :demand
  :config
  (eshell-syntax-highlighting-global-mode t))

(use-package vterm
  :bind
  (:map ctl-x-map
   ("n v" . vterm)))

(use-package org-mime)

(use-package apheleia
  :hook ((clojure-mode-hook haskell-mode-hook python-mode-hook) . apheleia-mode)
  :demand
  :config
  (setf (alist-get 'cljstyle     apheleia-formatters) '("cljstyle" "pipe"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'cljstyle)  )

(use-package elfeed
  :config
  (let ((path "~/.dotfiles/emacs/elfeed-local-feed.el"))
    (when (file-exists-p path)
      (load path)))
  :bind
  (:map ctl-x-map
   ("n f" . elfeed)))

(use-package elfeed-summary
  :straight (:host github
             :repo "SqrtMinusOne/elfeed-summary"))

(use-package magit
  :bind
  ("C-c m m" . magit-status))

(use-package magit-delta
  :ensure-system-package (delta . git-delta)
  :hook (magit-mode-hook . magit-delta-mode))

(use-package blamer
  :straight (:host github
             :repo "Artawower/blamer.el"))

(use-package browse-at-remote)

(use-package projectile
  :custom
  (projectile-project-search-path '("~/Code/"))
  (compilation-scroll-output t)
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package wgrep)

(use-package vertico
  :demand
  :straight (vertico :host github
                     :repo "minad/vertico"
                     :files ("*.el" "extensions/*.el"))
  :bind
  (("C-c b c r" . vertico-repeat)
   :map vertico-map
   ("<escape>" . abort-minibuffers))
  :config
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package prescient
  :config
  (prescient-persist-mode)
  :disabled)

(use-package selectrum-prescient
  :disabled)

(use-package selectrum
  :disabled
  :bind
  (("C-c b b" . selectrum-repeat)
   :map selectrum-minibuffer-map
   ("<escape>" . keyboard-quit))
  :custom
  (selectrum-files-select-input-dirs t)
  (selectrum-quick-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (magit-completing-read-function #'selectrum-completing-read)
  :config
  (selectrum-prescient-mode t)
  (selectrum-mode t))

(use-package isearch
  :straight (:type built-in)
  :bind
  (:map isearch-mode-map
   ("C-e" . isearch-occur)))

(use-package ctrlf
  :disabled
  :custom
  (ctrlf-default-search-style 'fuzzy)
  (ctrlf-alternate-search-style 'regexp)
  :demand
  :config
  (ctrlf-mode)
  :bind
  (:map ctrlf-minibuffer-mode-map
   ("C-e" . ctrlf-occur)
   ("<escape>" . ctrlf-cancel)))

(use-package consult
  :ensure-system-package (rg . ripgrep)
  :custom
  (consult-locate-args "plocate --ignore-case --existing --regexp")
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ("C-c C-s" . consult-line)
   ("C-c b a" . consult-ripgrep)
   ("C-c b r" . consult-recent-file))
  :config
  (recentf-mode))

(use-package consult-flycheck
  :after flycheck
  :straight (:host github
             :repo "minad/consult-flycheck"))

(use-package consult-projectile
  :straight (:type git
             :host gitlab
             :repo "OlMon/consult-projectile")
  :after (consult projectile)
  :custom
  (consult-project-root-function 'projectile-project-root)
  :bind
  ([remap projectile-find-file] . consult-projectile))

(use-package marginalia
  :config
  (marginalia-mode t))

;; TODO: embark-lsp
(use-package embark
  :after marginalia
  :demand
  :preface
  (defun cider-refine-expression-type (type target)
    (cons
     (if (bound-and-true-p cider-mode) 'clojure-sexp type)
     target))
  :config
  (add-to-list 'embark-transformer-alist '(expression . cider-refine-expression-type))
  (add-to-list 'embark-transformer-alist '(symbol     . cider-refine-expression-type))
  (add-to-list 'embark-transformer-alist '(defun      . cider-refine-expression-type))
  (add-to-list 'embark-transformer-alist '(identifier . cider-refine-expression-type))
  (embark-define-keymap clojure-expression-map
    "Keymap for actions on clojure expressions"
    :parent embark-expression-map
    ("i" cider-inspect-expr)
    ("e" cider-read-and-eval))
  (add-to-list 'embark-keymap-alist
               '(clojure-sexp . clojure-expression-map))
  :bind
  (("M-q" . embark-act)
   ("M-." . embark-dwim)
   :map vertico-map
   ("C-e" . embark-export)
   :map embark-general-map
   ([remap describe-symbol] . helpful-symbol)))

(use-package embark-consult
  :demand t
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package direnv
  :config
  (direnv-mode))

(use-package paren-face)

(use-package smart-tabs-mode
  :hook
  (c-mode-common-hook . (lambda () (setq-local indent-tabs-mode t)))
  :config
  (smart-tabs-insinuate 'c 'c++))

(use-package company
  :disabled
  :demand t
  :custom
  (company-idle-delay 0.1)
  (company-echo-delay 0.1)
  (company-show-numbers t)
  (company-eclim-auto-save nil)
  (company-dabbrev-downcase nil)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-global-modes '(not erc-mode message-mode help-mode gud-mode))
  (company-require-match 'never)
  ;; Buffer-local backends will be computed when loading a major mode, so
  ;; only specify a global default here.
  (company-backends '((company-capf company-dabbrev-code)))
  (company-auto-commit nil)
  (company-auto-commit-chars nil)
  :config
  (global-company-mode)
  :bind
  (:map company-active-map
   ("RET" . company-complete-selection)
   ("<ret>" . company-complete-selection)))

(use-package corfu
  :custom
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preselect-first nil)
  :bind
  (:map corfu-map
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous))
  :demand
  :config
  (put 'completion-at-point-functions 'safe-local-variable #'listp)
  (global-corfu-mode))

(use-package corfu-doc
  :straight (:host github
             :repo "galeo/corfu-doc")
  :hook (corfu-mode-hook . corfu-doc-mode)
  :custom
  (corfu-doc-delay "1.5"))

(use-package cape
  :straight (:host github
             :repo "minad/cape"
             :files ("*.el" "extensions/*.el"))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :bind
  ("M-/" . cape-dabbrev))

(use-package kind-icon
  :disabled
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package company-tabnine
  :preface
  (defun turn-off-tabnine ()
    (interactive)
    ;; (setq company-backends (remove 'company-tabnine company-backends))
    (remove-hook 'completion-at-point-functions (cape-company-to-capf #'company-tabnine) nil)
    )
  (defun turn-on-tabnine ()
    (interactive)
    ;; (add-to-list 'company-backends #'company-tabnine)
    (add-hook 'completion-at-point-functions (cape-company-to-capf #'company-tabnine) nil t))
  :config
  (turn-on-tabnine))

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (dumb-jump-default-project "~/Code")
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package gumshoe
  :disabled
  :bind
  (("M-g f" . gumshoe-backtrack-forward)
   ("M-g b" . gumshoe-backtrack-back)
   ("M-g d" . gumshoe-peruse-globally))
  :config
  (global-gumshoe-mode))

(use-package dogears
  :disabled
  :demand
  :bind
  (("M-g d"   . dogears-go)
   ("M-g M-f" . dogears-forward)
   ("M-g M-b" . dogears-back)
   ("C-."     . dogears-forward)
   ("C-,"     . dogears-backward))
  :config
  (add-to-list 'savehist-additional-variables 'dogears-list)
  (dogears-mode))

(use-package consult-better-jumper
  :straight (:host github
             :repo "NicholasBHubbard/consult-better-jumper")
  :bind
  ("M-g d" . consult-better-jumper))

(use-package flycheck
  :custom
  (flycheck-indication-mode 'right-fringe)
  :demand t
  :config
  (global-flycheck-mode)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(use-package flycheck-inline
  :disabled
  :hook (flycheck-mode-hook . flycheck-inline-mode))

(use-package flycheck-pos-tip
  :disabled
  :custom
  (flycheck-pos-tip-timeout 0)
  :config
  (flycheck-pos-tip-mode t))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p 3)
  (eldoc-echo-area-prefer-doc-buffer 'maybe)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eglot
  :disabled
  :custom
  (read-process-output-max (* 1024 1024 10))
  :bind
  (:map eglot-mode-map
   ("C-c l a" . eglot-code-actions)
   ("C-c l r" . eglot-rename)))

;; TODO: Check out lsp-bridge
(use-package lsp-mode
  :preface
  (defun lsp-mode-setup-completion-for-corfu ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex)))
  :hook (lsp-completion-mode-hook . lsp-mode-setup-completion-for-corfu)
  :demand t
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-symbol-highlighting t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-lens-place-position 'above-line)
  (lsp-prefer-capf t)
  (lsp-completion-provider :none) ; use corfu instead
  ;; (lsp-completion-provider :capf)
  (lsp-idle-delay 0.75)
  (lsp-enable-snippet nil)
  (lsp-headerline-breadcrumb-enable nil)
  (read-process-output-max (* 1024 1024 10))
  (lsp-file-watch-threshold 512)
  (lsp-diagnostics-flycheck-default-level 'warning))

(use-package consult-lsp
  :demand
  :bind
  (:map lsp-mode-map
   ("C-c l c d" . consult-lsp-diagnostics)
   ("C-c l c s" . consult-lsp-symbols)
   ("C-c l c f" . consult-lsp-file-symbols))
  :config
  (consult-lsp-marginalia-mode))

(use-package dap-mode
  :disabled
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-python-debugger 'debugpy)
  :hook
  (dap-stopped-hook . (lambda (arg) (call-interactively #'dap-hydra)))
  :preface
  (defun dap-variables-python-module ()
    (s-concat
     (s-replace "/" "." (dap-variables-project-relative-dirname))
     (dap-variables-buffer-basename-sans-extension)))
  :config
  (require 'dap-python)
  (require 'dap-variables)
  (require 'dap-gdb-lldb)
  (push
   '("\\`pythonModule\\'" . dap-variables-python-module)
   dap-variables-standard-variables))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 1)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil))

(use-package haskell-mode
  :custom
  (haskell-completing-read-function #'completing-read)
  (haskell-process-show-overlays nil)
  (haskell-process-suggest-restart nil)
  (haskell-font-lock-symbols t)
  :hook
  (haskell-mode-hook . haskell-indentation-mode))

(use-package lsp-haskell
  :after lsp-mode
  :preface
  (defun turn-on-haskell-lsp ()
    (interactive)
    (add-hook 'haskell-mode-hook #'lsp-deferred)
    (lsp))
  (defun turn-off-haskell-lsp ()
    (interactive)
    (remove-hook 'haskell-mode-hook #'lsp-deferred)
    (call-interactively
     (lsp-workspace-shutdown))))

(use-package python-x
  :preface
  (defun run-django-shell ()
    (let ((python-shell-interpreter
           (expand-file-name (completing-read
                              "Locate manage.py: "
                              (projectile-project-files (projectile-acquire-root))
                              nil
                              t
                              "manage.py")
                             (projectile-project-root)))
          (python-shell-interpreter-args "shell"))
      (run-python (python-shell-calculate-command) nil t)))
  (defun django-shell ()
    (interactive)
    (if (poetry-venv-activated-p)
        (run-django-shell)
      (poetry-venv-workon)
      (run-django-shell)
      (poetry-venv-deactivate)))
  :bind
  (:map inferior-python-mode-map
   ("C-l" . comint-clear-buffer))
  :demand t
  :config
  (python-x-setup))

(use-package python-mls
  :straight (:host github
             :repo "jdtsmith/python-mls")
  :config
  (python-mls-setup))

(use-package cython-mode)

(use-package python-isort
  :hook (python-mode-hook . python-isort-on-save-mode))

(use-package pyvenv)

(use-package auto-virtualenv
  :hook (python-mode-hook . auto-virtualenv-set-virtualenv))

(use-package poetry)

(use-package highlight-defined
  :hook (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package eros
  :hook (emacs-lisp-mode-hook . eros-mode))

(use-package sly
  :demand
  :bind
  (:map sly-mode-map
   ("C-c M-i" . sly-inspect))
  :custom
  (sly-complete-symbol-fuction 'sly-simple-completions)
  (inferior-lisp-program "sbcl"))

(use-package sly-quicklisp)

(use-package clojure-mode)

(use-package flycheck-clj-kondo)

(use-package cider
  :bind
  (:map cider-repl-mode-map
   ("C-l" . cider-repl-clear-buffer)
   :map cider-mode-map
   ("C-c M-c" . cider-debug-defun-at-point)
   ("C-c C-p" . cider-inspect-last-sexp))
  :custom
  (cider-repl-display-help-banner nil))

(use-package sayid
  :disabled
  :hook (clojure-mode-hook . sayid-setup-package))

(use-package cider-eval-sexp-fu)

(use-package clj-refactor
  :custom
  (cljr-warn-on-eval nil)
  (cljr-clojure-test-declaration "[clojure.test :as test :refer [deftest testing is]]")
  :hook
  (clojure-mode-hook . (lambda ()
                         (interactive)
                         (clj-refactor-mode t)
                         (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package rust-mode
  :bind
  (:map rust-mode-map
   ("C-c C-p" . rust-run-clippy)
   ("C-c C-c" . rust-run))
  :custom
  (rust-format-on-save t))

(use-package typescript-mode)

(use-package web-mode
  :mode "\\.tsx\\'"
  :custom
  (web-mode-code-indent-offset 2))

(use-package markdown-mode)

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package visible-mark
  :config
  (visible-mark-mode))

(use-package tree-sitter
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (push '(clojure-mode . clojure) tree-sitter-major-mode-language-alist)
  (push '(haskell-mode . haskell) tree-sitter-major-mode-language-alist)
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(use-package ligature
  :straight (:host github
             :repo "mickeynp/ligature.el")
  :config
  ;; Iosevka ligatures
  (defvar iosevka-ligatures
    '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
      "<==" "<===" "=>" "=>>" "==>" "===>" "<=>" "<==>" "<===>" "<====>" "<!---"
      "(*" "*)" "[|" "|]" "{|" "|}" "<." "<.>" ".>"
      "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!==" ">>=" "=<<" "<>" ":>"
      ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (ligature-set-ligatures 'prog-mode iosevka-ligatures)
  (global-ligature-mode))

(use-package pdf-tools
  :custom
  (pdf-view-display-page 'fit-page))

(use-package tex-site
  :straight auctex
  :hook
  ((LaTeX-mode-hook . LaTeX-math-mode)
   (LaTeX-mode-hook . turn-on-reftex)
   (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))
  :custom
  (reftex-plug-into-AUCTeX t))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package which-key
  :config
  (which-key-mode t))

(use-package rainbow-mode)

(use-package screenshot
  :straight (:host github
             :repo "tecosaur/screenshot"
             :build (:not compile)))

(use-package nix-mode)

(use-package selected
  :bind
  (:map selected-keymap
   ("<tab>" . indent-region)
   ("w" . kill-ring-save))
  :demand t
  :config
  (selected-global-mode))

(use-package sudo-edit)

(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618))
  :bind
  (:map ctl-x-map
   ("n z" . zoom-mode)))

(use-package telega
  :straight (:host github
             :repo "zevlg/telega.el"
             :branch "release-0.8.0")
  :hook (telega-chat-mode-hook . olivetti-mode)
  :custom
  (telega-server-libs-prefix "/usr")
  (telega-use-docker t)
  (telega-use-images t)
  (telega-root-show-avatars t)
  (telega-chat-show-avatars t)
  (telega-completing-read-function #'completing-read)
  :bind
  (:map telega-msg-button-map
   ("z" . recenter)))

(use-package explain-pause-mode)

(use-package svg-tag-mode
  :hook (org-mode-hook . svg-tag-mode)
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  (setq svg-tag-tags
        `(
          ;; Org tags
          (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))

          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


          ;; Citation of the form [cite:@Knuth:1984]
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                     (svg-tag-make tag
                                                                   :end -1
                                                                   :crop-left t))))


          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))

(provide 'init)
;;; init.el ends here
