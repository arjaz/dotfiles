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

(defvar use-package-enable-imenu-support t)
(straight-use-package 'use-package)

(use-package use-package-core
  :straight (:type built-in)
  :custom
  (use-package-hook-name-suffix nil))

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-check-for-modifications '(watch-files find-when-checking)))

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
  (inhibit-startup-message t "disable startup message and gtk pop-ups")
  (vc-follow-symlinks t)
  (auto-save-timeout 20 "number of seconds idle time before auto-save")
  (auto-save-interval 200 "number of keystrokes between auto-saves")
  (vc-make-backup-files t "make backups for version-controlled files as well")
  (create-lockfiles nil)
  (use-dialog-box nil)
  (use-short-answers t)
  (history-length t)
  (history-delete-duplicates t)
  (enable-recursive-minibuffers t)
  :preface
  (defun allow-garbage ()
    (setq gc-cons-threshold 536870912 ; 512mb
          gc-cons-percentage 0.1))
  :hook
  (after-init-hook . allow-garbage)
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (setq-default tab-width 4))

(use-package savehist
  :straight (:type built-in)
  :custom
  (savehist-file (concat user-emacs-directory "savehist"))
  (savehist-save-minibuffer-history t)
  :config
  (savehist-mode))

(use-package loaddefs
  :straight (:type built-in)
  :custom
  (disabled-command-function nil))

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
  (unbind-key (kbd "C-z") 'global-map)
  (window-divider-mode)
  (blink-cursor-mode 0))

(use-package faces
  :straight (:type built-in)
  :preface
  (defun setup-fonts ()
    (interactive)
    (let ((font "Iosevka"))
      (add-to-list 'default-frame-alist `(font . ,font))
      (set-face-attribute 'default nil :family font :height 120)
      (set-face-attribute 'variable-pitch nil :family "Roboto" :height 150)
      (set-frame-font font)))
  :hook
  (after-init-hook . setup-fonts))

(use-package pixel-scroll
  :straight (:type built-in)
  :custom
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  :hook
  (after-init-hook . pixel-scroll-precision-mode))

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

(use-package mode-local
  :straight (:type built-in))

(use-package lisp-mode
  :straight (:type built-in)
  :demand t
  :config
  (let ((path "~/.dotfiles/emacs/elisp-fix-indent.el"))
    (when (file-exists-p path)
      (load path))))

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  (auto-save-default nil)
  (super-save-exclude '(".gpg"))
  :config
  (super-save-mode)
  (add-to-list 'super-save-triggers 'ace-window))

(use-package bookmark
  :straight (:type built-in)
  :custom
  (bookmark-fontify nil))

(use-package simple
  :straight (:type built-in)
  :bind
  (:map ctl-x-map
   ("k" . kill-current-buffer))
  :config
  (setq-default indent-tabs-mode nil))

(use-package cc-vars
  :straight (:type built-in)
  :custom
  (c-basic-offset 4)
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode)))

(use-package files
  :straight (:type built-in)
  :preface
  (defun auto-create-missing-dirs ()
    (let ((target-dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p target-dir)
        (make-directory target-dir t))))
  :hook
  (find-file-not-found-functions . auto-create-missing-dirs)
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

(use-package dash)

(use-package helpful
  :demand
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable))

(use-package goto-chg
  :bind
  ("C-c g l" . goto-last-change)
  ("C-c g r" . goto-last-change-reverse))

(use-package visual-regexp
  :bind
  ([remap query-replace] . vr/replace))

;; TODO: check out org-super-agenda
(use-package org
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images)
  (org-mode-hook . variable-pitch-mode)
  :bind
  ("C-c o a" . org-agenda)
  ("C-c o c" . org-capture)
  :demand
  :custom
  (org-confirm-babel-evaluate nil)
  (org-directory "~/Documents/org/")
  (org-default-notes-file (concat org-directory "tasks.org"))
  (org-hide-leading-stars t)
  (org-startup-indented nil)
  (org-agenda-files (list org-default-notes-file))
  (org-agenda-custom-commands
   '(("d" "Today's Tasks"
      ((agenda "" ((org-agenda-span 1)
                   (org-agenda-overriding-header "Today's Tasks")))))))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t)
     (python     . t))))

(use-package org-modern
  :hook (org-mode-hook . org-modern-mode))

(use-package denote
  :straight (:host github
             :repo "protesilaos/denote"))

(use-package consult-notes
  :straight (:host github
             :repo "mclear-tools/consult-notes")
  :custom
  (consult-notes-sources '(("Notes" ?n "~/Documents/notes"))))

(use-package dbus)

(use-package modus-themes
  :custom
  (modus-themes-mode-line '(moody accented borderless))
  (modus-themes-syntax '(green-strings))
  (modus-themes-paren-match '(intense))
  (modus-themes-region '(bg-only))
  :hook
  (after-init-hook . load-theme-on-startup)
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
       "/org/freedesktop/portal/desktop"
       "org.freedesktop.portal.Settings"
       "Read"
       (-compose #'set-theme-from-dbus-value #'caar)
       "org.freedesktop.appearance"
       "color-scheme")))
  (defun choose-theme (light-theme-p theme)
    (interactive (list
                  (y-or-n-p "Choose the light theme?")
                  (intern
                   (completing-read "Load custom theme: "
                                    (mapcar #'symbol-name (custom-available-themes))))))
    (mapcar #'disable-theme custom-enabled-themes)
    (if light-theme-p
        (progn
          (setq light-theme theme)
          (load-light-theme))
      (setq dark-theme theme)
      (load-dark-theme)))
  (defun set-theme-from-dbus-value (value)
    (if (equal value '1)
        (load-dark-theme)
      (load-light-theme)))
  (defun dbus-on-theme-changed (path var value)
    (when (and (string-equal path "org.freedesktop.appearance")
               (string-equal var "color-scheme"))
      (set-theme-from-dbus-value (car value))))
  :config
  (dbus-register-signal
   :session "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings"
   "SettingChanged"
   #'dbus-on-theme-changed)
  :init
  (defvar light-theme 'modus-operandi)
  (defvar dark-theme 'modus-vivendi))

(use-package ef-themes
  :straight (:host github
             :repo "protesilaos/ef-themes"))

(use-package lambda-themes
  :straight (:host github
             :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords nil)
  (lambda-themes-set-variable-pitch t))

(use-package page-break-lines
  :config
  (page-break-lines-mode))

(use-package prism
  :preface
  (defun prism-set-doom-colors (&rest _)
    (interactive)
    (prism-set-colors
      :lightens '(0)
      :desaturations (0)
      :colors (mapcar #'doom-color '(red blue magenta green cyan))))
  (defun prism-set-modus-colors (&rest _)
    (interactive)
    (when (or (member 'modus-operandi custom-enabled-themes)
              (member 'modus-vivendi custom-enabled-themes))
      (prism-set-colors
        :desaturations '(0)
        :lightens '(0)
        :colors (modus-themes-with-colors
                  (list fg-main
                        magenta
                        cyan-alt-other
                        magenta-alt-other
                        blue
                        magenta-alt
                        cyan-alt
                        red-alt-other
                        green
                        fg-main
                        cyan
                        yellow
                        blue-alt
                        red-alt
                        green-alt-other
                        fg-special-warm)))))
  :custom
  (prism-parens t)
  :hook
  ((lisp-mode-hook clojure-mode-hook scheme-mode-hook) . prism-mode)
  (prism-mode-hook . prism-set-modus-colors))
  ;; :config
  ;; (setq prism-num-faces 16)

(use-package gdscript-mode)

(use-package indent-guide
  :hook (python-mode-hook . indent-guide-mode)
  :custom (indent-guide-char ":"))

(use-package highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package readable-numbers
  :straight (:host github
             :repo "Titan-C/cardano.el"
             :files ("readable-numbers.el")))

(use-package highlight-escape-sequences
  :hook (prog-mode-hook . hes-mode))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :hook
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))

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
  :disabled
  :custom
  (left-fringe-width 3)
  :config
  (global-diff-hl-mode))

(use-package dired
  :straight (:type built-in)
  :hook
  (dired-mode-hook . auto-revert-mode)
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
   ("C-c m c" . mc/edit-lines)
   ("C-c m d" . mc/mark-all-dwim)
   ("C-c m n" . mc/mark-next-word-like-this)
   ("C-c m p" . mc/mark-previous-word-like-this)))

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
  :disabled
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
   ("C-t" . avy-goto-word-1))
  :custom
  (avy-background t)
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (avy-dispatch-alist '((?\q . avy-action-embark)
                        (?\M-q . avy-action-embark))))

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-keys avy-keys)
  (aw-ignore-current t)
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
  (ws-butler-global-mode))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package dashboard
  :preface
  (defun dashboard-buffer ()
    (get-buffer "*dashboard*"))
  :custom
  (show-week-agenda-p t)
  (dashboard-set-heading-icons t)
  (dashboard-startup-banner 3)
  (dashboard-set-navigator t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-startup-banner (concat user-emacs-directory "emacs-dash.png"))
  (dashboard-items '((agenda . 15)))
  (dashboard-banner-logo-title "Eendracht Maakt Macht")
  (initial-buffer-choice #'dashboard-buffer)
  :init
  (require 'linum)
  (dashboard-setup-startup-hook)
  :bind
  (:map dashboard-mode-map
   ("n" . dashboard-next-line)
   ("p" . dashboard-previous-line)))

(use-package olivetti
  :custom
  (olivetti-body-width 150)
  :hook
  (nov-mode-hook . olivetti-mode)
  :bind
  ("C-c o o" . olivetti-mode))

(use-package elec-pair
  :straight (:type built-in)
  :hook (prog-mode-hook . electric-pair-mode))

;; TODO: fix C-M-S-f and C-M-S-b
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
   ("M-S"   . puni-split)
   ("C-M-s" . puni-squeeze)
   ("C-M-t" . puni-transpose)
   ("M-?"   . puni-convolute)
   ("C-("   . puni-slurp-backward)
   ("C-)"   . puni-slurp-forward)
   ("C-{"   . puni-barf-backward)
   ("C-}"   . puni-barf-forward)
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

(use-package coterm
  :config
  (coterm-mode))

(use-package shell
  :straight (:type built-in)
  :demand
  :bind
  (:map shell-mode-map
   ("C-l" . comint-clear-buffer)))

(use-package esh-mode
  :straight (:type built-in)
  :demand
  :hook
  (eshell-before-prompt-hook
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
  (("C-c o e" . eshell)
   ("C-c o n e" . eshell-new)
   :map eshell-mode-map
   ("C-l" . eshell/clear-buffer))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))

(use-package eshell-up)

(use-package pcmpl-args)

(use-package bash-completion)

(use-package fish-completion
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
  ("C-c o v" . vterm))

(use-package org-mime)

(use-package apheleia
  :hook ((clojure-mode-hook haskell-mode-hook python-mode-hook) . apheleia-mode)
  :demand
  :config
  (setf (alist-get 'cljstyle     apheleia-formatters) '("cljstyle" "pipe"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'cljstyle))

(use-package elfeed
  :config
  (let ((path "~/.dotfiles/emacs/elfeed-local-feed.el"))
    (when (file-exists-p path)
      (load path)))
  :custom
  (elfeed-search-filter "@6-months-ago")
  :bind
  ("C-c o f" . elfeed))

(use-package magit
  :bind
  ("C-c o m" . magit-status)
  :preface
  (defun file-directory ()
    (let ((home (expand-file-name "~")))
      (when (buffer-file-name)
        (replace-regexp-in-string (concat "^" home) "~" default-directory))))
  (defun file-or-buffer-name ()
    (if (buffer-file-name)
        (file-name-nondirectory (buffer-file-name))
      (buffer-name)))
  (defun file-read-write-indicator ()
    (cond
     (buffer-read-only "RO")
     ((and (buffer-file-name) (buffer-modified-p)) "⬤")
     (t "◯")))
  (defun git-branch ()
    (if (and (buffer-file-name)
             (file-remote-p (buffer-file-name)))
        ""
      (when-let ((branch (magit-get-current-branch))
                 (fancy (when (char-displayable-p ?) " ")))
        (concat fancy branch))))
  ;; :config
  ;; (setq-default
  ;;  ;; TODO: maybe display the "%5l:%c" bit at the right
  ;;  mode-line-format
  ;;  '("%5l:%c"
  ;;    (:eval (s-repeat (- 4 (length (number-to-string (current-column)))) " "))
  ;;    (:propertize (:eval (file-read-write-indicator)) face font-lock-warning-face)
  ;;    "  "
  ;;    (:propertize (:eval (file-directory)) face font-lock-variable-name-face)
  ;;    (:propertize (:eval (file-or-buffer-name)) face font-lock-keyword-face)
  ;;    " · "
  ;;    (:propertize (:eval (git-branch)) face magit-dimmed)))
  )

(use-package magit-todos
  :config
  (magit-todos-mode))

(use-package code-review)

;; TODO: there's a problem with lsp-ui -- mini-modeline hides the doc frame
(use-package mini-modeline
  :config
  ;; TODO: maybe rethink eldoc
  (setq
   mini-modeline-enhance-visual nil
   mini-modeline-display-gui-line nil
   mini-modeline-l-format
   '((:propertize (:eval (file-directory)) face font-lock-variable-name-face)
     (:propertize (:eval (file-or-buffer-name)) face font-lock-keyword-face))
   mini-modeline-r-format
   '("%5l:%c"
     (:eval (s-repeat (- 4 (length (number-to-string (current-column)))) " "))
     (:propertize (:eval (file-read-write-indicator)) face font-lock-warning-face)
     "  "
     (:propertize (:eval (git-branch)) face magit-dimmed)))
  (mini-modeline-mode))

(use-package magit-delta
  :hook (magit-mode-hook . magit-delta-mode))

(use-package browse-at-remote)

(use-package project
  :straight (:type built-in))

(use-package consult-project-extra
  :bind
  ([remap project-find-file] . consult-project-extra-find))

(use-package projectile
  :disabled
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

(use-package consult
  :custom
  (consult-locate-args "plocate --ignore-case --existing --regexp")
  :bind
  (("C-c s"           . consult-line)
   ("C-c M-s"         . consult-line-multi)
   ("C-c b a"         . consult-ripgrep)
   ("C-x b"           . consult-buffer)
   ([remap imenu]     . consult-imenu)
   ("M-g M-i"         . consult-imenu-multi)
   ([remap goto-line] . consult-goto-line))
  :config
  (recentf-mode))

(use-package consult-flycheck
  :after flycheck
  :straight (:host github
             :repo "minad/consult-flycheck"))

(use-package marginalia
  :config
  (marginalia-mode))

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
   ("M-s o" . embark-export)
   :map embark-general-map
   ([remap describe-symbol] . helpful-symbol)))

(use-package embark-consult
  :demand t
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package envrc
  :config
  (envrc-global-mode))

(use-package smart-tabs-mode
  :config
  (setq-mode-local c-mode indent-tabs-mode t)
  (setq-mode-local c++-mode indent-tabs-mode t)
  (smart-tabs-insinuate 'c 'c++))

(use-package smart-tab
  :config
  (global-smart-tab-mode))

(use-package corfu
  :custom
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-quit-at-boundary t)
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
  :preface
  (defun cape-setup ()
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))
  :hook (prog-mode-hook . cape-setup)
  :bind
  ("M-/" . cape-dabbrev))

(use-package kind-icon
  :disabled
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package tabnine-capf
  :after cape
  :straight (:host github
             :repo "50ways2sayhard/tabnine-capf"
             :files ("*.el" "*.sh"))
  :hook
  (kill-emacs . tabnine-capf-kill-process)
  :preface
  ;; TODO: handle lsp-mode; I think its lsp-after-initialize-hook
  (defun enable-tabnine ()
    (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))
  (defun disable-tabnine ()
    (delete #'tabnine-completion-at-point 'completion-at-point-functions))
  (defun turn-on-tabnine ()
    (interactive)
    (enable-tabnine)
    (add-hook 'prog-mode-hook #'enable-tabnine)
    (message "TabNine enabled"))
  (defun turn-off-tabnine ()
    (interactive)
    (disable-tabnine)
    (remove-hook 'prog-mode-hook #'enable-tabnine)
    (message "TabNine disabled")))

(use-package tempel
  :bind
  (:map tempel-map
   ("C-M-n" . tempel-next)
   ("C-M-p" . tempel-previous))
  :preface
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  (prog-mode-hook . tempel-setup-capf)
  (text-mode-hook . tempel-setup-capf))

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

(use-package flycheck
  :custom
  (flycheck-indication-mode 'right-fringe)
  :demand t
  :config
  ;; (global-flycheck-mode)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(use-package flycheck-posframe
  :hook
  (flycheck-mode-hook . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'frame-top-right-corner)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(use-package flycheck-inline
  :disabled
  :hook (flycheck-mode-hook . flycheck-inline-mode))

(use-package flycheck-pos-tip
  :disabled
  :custom
  (flycheck-pos-tip-timeout 0)
  :config
  (flycheck-pos-tip-mode t))

;; TODO: maybe flymake cursos
(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (eldoc-echo-area-prefer-doc-buffer 'maybe)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  :hook (eglot-managed-mode-hook . eldoc-box-hover-mode))

(use-package eglot
  ;; :disabled
  :custom
  (read-process-output-max (* 1024 1024 10))
  (eglot-confirm-server-initiated-edits nil)
  :bind
  (("C-c l l" . eglot)
   :map eglot-mode-map
   ("C-c l w r" . eglot-reconnect)
   ("C-c l w q" . eglot-shutdown)
   ("C-c l a" . eglot-code-actions)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format)
   ("C-c l e" . consult-flymake)
   ("C-c l h" . eldoc-print-current-symbol-info)
   ("C-c l o" . open-markdown-link)
   :map haskell-mode-map
   ("C-c l l" . eglot-turn-on-hls))
  :preface
  (defun eglot-turn-on-hls ()
    (interactive)
    (add-hook 'haskell-mode-hook #'eglot-ensure)
    (call-interactively #'eglot))
  (defun open-markdown-link (&rest _)
    (interactive "P")
    (let ((buffer-list-update-hook nil))
      (-let [(buffer point) (if-let* ((valid (and (listp last-input-event)
                                                  (eq (car last-input-event) 'mouse-2)))
                                      (event (cadr last-input-event))
                                      (win (posn-window event))
                                      (buffer (window-buffer win)))
                                `(,buffer ,(posn-point event))
                              `(,(current-buffer) ,(point)))]
        (with-current-buffer buffer
          ;; Markdown-mode puts the url in 'help-echo
          (-some--> (get-text-property point 'help-echo)
            (and (string-match-p goto-address-url-regexp it)
                 (browse-url it)))))))
  :config
  (require 'goto-addr)
  ;; corfu setup
  (push '(haskell-mode . ("haskell-language-server" "--lsp")) eglot-server-programs)
  (push '(eglot (styles orderless)) completion-category-overrides))

;; TODO: Check out lsp-bridge
(use-package lsp-mode
  :straight (lsp-mode
             :type git
             :flavor melpa
             :files (:defaults "clients/*.el" "lsp-mode-pkg.el")
             :host github
             :repo "emacs-lsp/lsp-mode"
             :build (:not compile))
  :disabled
  :after corfu
  :preface
  (defun lsp-mode-setup-completion-for-corfu ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (lsp-completion-mode-hook . lsp-mode-setup-completion-for-corfu)
  :demand t
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-symbol-highlighting t)
  (lsp-modeline-code-actions-enable nil)
  ;; (lsp-lens-place-position 'above-line)
  (lsp-prefer-capf t)
  (lsp-completion-provider :none) ; use corfu instead
  ;; (lsp-completion-provider :capf)
  (lsp-idle-delay 0.75)
  (lsp-enable-snippet nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (read-process-output-max (* 1024 1024 10))
  (lsp-file-watch-threshold 512)
  (lsp-diagnostics-flycheck-default-level 'warning))

(use-package consult-lsp
  :disabled
  :bind
  (:map lsp-mode-map
   ("C-c l c d" . consult-lsp-diagnostics)
   ("C-c l c s" . consult-lsp-symbols)
   ("C-c l c f" . consult-lsp-file-symbols)))

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
  :disabled
  :straight (lsp-ui
             :type git
             :flavor melpa
             :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el")
             :host github
             :repo "emacs-lsp/lsp-ui"
             :build (:not compile))
  :bind
  (:map lsp-ui-mode-map
   ("C-c l t s" . lsp-ui-sideline-toggle-symbols-info)
   ("C-c l h o" . lsp-ui-open-docs-link-hack))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 1)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-diagnostic-max-lines 10)
  (lsp-ui-sideline-show-hover nil)
  :preface
  (defun lsp-ui-open-docs-link-hack ()
    (interactive)
    (let ((contents (-some->> (lsp--text-document-position-params)
                      (lsp--make-request "textDocument/hover")
                      (lsp--send-request)
                      (lsp:hover-contents))))
      (if (and contents (not (equal contents "")))
          (let ((lsp-help-buf-name "*lsp-help*"))
            (with-current-buffer (get-buffer-create lsp-help-buf-name)
              (with-help-window lsp-help-buf-name
                (insert (string-trim-right (lsp--render-on-hover-content contents t))))
              (beginning-of-buffer)
              (search-forward "Documentation")
              (backward-char)
              (lsp-ui-doc--open-markdown-link)
              (quit-windows-on lsp-help-buf-name)))
        (lsp--info "No content at point.")))))

(use-package treemacs
  :bind
  ("C-c o t" . treemacs)
  :custom
  (treemacs-is-never-other-window t)
  (treemacs-width 32)
  :config
  (treemacs-project-follow-mode))

;; TODO: no icons for directories
(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-magit)

(use-package lsp-treemacs
  :disabled
  :config
  (lsp-treemacs-sync-mode))

(use-package haskell-mode
  :custom
  (haskell-completing-read-function #'completing-read)
  (haskell-process-show-overlays nil)
  (haskell-process-suggest-restart nil)
  (haskell-font-lock-symbols nil))
  ;; :hook
  ;; (haskell-mode-hook . haskell-indentation-mode)
  
(use-package lsp-haskell
  :after lsp-mode
  :disabled
  :custom
  (lsp-haskell-formatting-provider "brittany")
  :bind
  (:map haskell-mode-map
   ("C-c l l" . turn-on-hls))
  :preface
  (defun turn-on-hls ()
    (interactive)
    (add-hook 'haskell-mode-hook #'lsp-deferred)
    (lsp))
  (defun turn-off-hls ()
    (interactive)
    (remove-hook 'haskell-mode-hook #'lsp-deferred)
    (call-interactively #'lsp-workspace-shutdown))
  :config
  (setf (alist-get 'lsp-haskell-server-path safe-local-variable-values)
        "haskell-language-server"))

(use-package python-x
  :bind
  (:map inferior-python-mode-map
   ("C-l" . comint-clear-buffer))
  :config
  (python-x-setup))

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
  (sly-complete-symbol-function 'sly-simple-completions)
  (inferior-lisp-program "sbcl"))

(use-package stumpwm-mode)

(use-package geiser)

(use-package geiser-guile)

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
                         (clj-refactor-mode)
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

(use-package nginx-mode)

(setq load-path (cons "/usr/lib/erlang/lib/tools-3.5.3/emacs" load-path))
(use-package erlang-start
  :straight (:type built-in)
  :config
  (setq erlang-root-dir "/usr/lib/erlang/")
  (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  (setq erlang-man-root-dir "/usr/lib/erlang/man"))

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
      ">=>" "<=<" "<==" "<===" "=>" "=>>" "==>" "===>" "<=>" "<==>" "<===>" "<====>" "<!---"
      "(*" "*)" "[|" "|]" "{|" "|}" "<." "<.>" ".>"
      ":-->"
      "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!==" ">>=" "=<<" "<>" ":>"
      ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (ligature-set-ligatures 'prog-mode iosevka-ligatures)
  (global-ligature-mode))

(use-package pdf-tools
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
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

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 120))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package rainbow-mode)

(use-package screenshot
  :straight (:host github
             :repo "tecosaur/screenshot"
             :build (:not compile))
  :custom
  (screenshot-max-width 130))

(use-package nix-mode)

(use-package pretty-sha-path
  :config
  (global-pretty-sha-path-mode))

(use-package detached
  :init
  (detached-init)
  :demand
  :bind
  ([remap async-shell-command] . detached-shell-command)
  ([remap compile] . detached-compile)
  ([remap recompile] . detached-compile-recompile)
  ([remap detached-open-session] . detached-consult-session)
  :custom
  (detached-show-output-on-attach t)
  (detached-terminal-data-command system-type))

(use-package selected
  :bind
  (:map selected-keymap
   ("<tab>" . indent-region))
  :config
  (selected-global-mode))

(use-package sudo-edit)

(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618))
  :bind
  ("C-c o z" . zoom-mode))

(use-package ement)

(use-package telega
  :straight (:host github
             :repo "zevlg/telega.el"
             :branch "release-0.8.0"
             :build (:not compile))
  :demand
  :hook (telega-chat-mode-hook . olivetti-mode)
  :custom
  (telega-server-command "~/.telega/telega-server")
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

(use-package keyfreq
  :config
  (keyfreq-mode))

(provide 'init)
;;; init.el ends here
