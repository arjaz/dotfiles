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

(use-package paren
  :custom
  (show-paren-context-when-offscreen 'overlay))

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
  :config
  (let ((font "Iosevka ss18 Medium-12"))
    (add-to-list 'initial-frame-alist `(font . ,font))
    (add-to-list 'default-frame-alist `(font . ,font))
    (set-face-attribute 'variable-pitch nil :family "Iosevka Aile Regular" :height 120)
    ;; (set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 120)
    (set-frame-font font)))

(use-package pixel-scroll
  :straight (:type built-in)
  :custom
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-interpolate-page t)
  :hook
  (after-init-hook . pixel-scroll-precision-mode)
  :bind
  ("C-v" . (lambda ()
             (interactive)
             (pixel-scroll-interpolate-down)
             (move-to-window-line nil)))
  ("M-v" . (lambda ()
             (interactive)
             (pixel-scroll-interpolate-up)
             (move-to-window-line nil))))

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
  :bind
  ("C-c t t" . hs-toggle-hiding)
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

(use-package mermaid-mode)

(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path "/usr/bin/mmdc"))

(use-package org
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images)
  :bind
  ("C-c a a" . org-agenda)
  ("C-c a c" . org-capture)
  :demand
  :custom
  (org-confirm-babel-evaluate nil)
  (org-directory "~/Documents/org/")
  (org-default-notes-file (concat org-directory "todo.org"))
  (org-hide-leading-stars t)
  (org-startup-indented nil)
  (org-agenda-files (list org-default-notes-file))
  (org-columns-default-format "%50ITEM(Task) %TODO %10CLOCKSUM %16TIMESTAMP_IA")
  (org-capture-templates
   `(("t" "Todo" entry (file+headline ,org-default-notes-file "Tasks")
      "* TODO %?\n%i\n%a\n  %u")
     ("n" "Active task" entry (file+headline ,org-default-notes-file "Tasks")
      "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (current-time))\n%i\n%a\n  %u")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t)
     (python     . t))))

(use-package org-super-agenda
  :custom
  (org-super-agenda-groups
   '((:name "Work"
      :tag "work")
     (:name "Today"
      :time-grid t
      :scheduled today)))
  :config
  (org-super-agenda-mode))

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
  (defun run-after-load-theme-hooks (&rest r)
    (run-hooks 'after-load-theme-hook))
  ;; TODO: replace all of that with a plugin
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
  (defvar after-load-theme-hook ())
  (defvar light-theme 'modus-operandi)
  (defvar dark-theme 'modus-vivendi-tinted)
  (advice-add #'load-theme :after #'run-after-load-theme-hooks))

;; TODO: make doom-monokai-pro use its green face for the comments
(use-package doom-themes)

(use-package ef-themes
  :straight (:host github
             :repo "protesilaos/ef-themes"))

(use-package page-break-lines
  :config
  (page-break-lines-mode))

(use-package highlight-sexp
  ;; :hook
  ;; ((lisp-mode-hook clojure-mode-hook scheme-mode-hook) . highlight-sexp-mode)
  :config
  (defun my-hl-sexp-create-overlay ()
    (when (overlayp hl-sexp-overlay)
      (if (or (member 'modus-operandi custom-enabled-themes)
              (member 'modus-vivendi custom-enabled-themes))
          (overlay-put
           hl-sexp-overlay
           'face
           `(:background
             ,(car
               (assoc-default
                'bg-inactive
                (modus-themes--current-theme-palette)))))
        (overlay-put
         hl-sexp-overlay
         'face
         `(:background
           ,(car
             (assoc-default
              'bg-alt
              doom-themes--colors)))))))
  (add-hook 'after-load-theme-hook 'my-hl-sexp-create-overlay)
  (advice-add 'hl-sexp-create-overlay :after 'my-hl-sexp-create-overlay))

(use-package prism
  :preface
  (defun prism-doom-colors ()
    (interactive)
    (prism-set-colors
      :lightens '(0)
      :desaturations '(0)
      :colors (mapcar 'doom-color '(red blue magenta green cyan))))
  (defun prism-or-modus-randomize (&rest _)
    (interactive)
    (if (--any (member it custom-enabled-themes) modus-themes-items)
        (prism-set-modus-colors)
      (prism-randomize-colors)))
  (defun prism-set-modus-colors (&rest _)
    (interactive)
    (prism-set-colors
      :desaturations '(0)
      :lightens '(0)
      :colors (prism-set-colors
                :desaturations '(0)
                :lightens '(0)
                :colors (modus-themes-with-colors
                          (list fg-main
                                magenta
                                cyan-cooler
                                magenta-cooler
                                blue
                                magenta-warmer
                                cyan-warmer
                                red-cooler
                                green
                                fg-main
                                cyan
                                yellow
                                blue-warmer
                                red-warmer
                                green-cooler
                                yellow-faint)))))
  :custom
  (prism-parens t)
  :hook
  ((lisp-mode-hook clojure-mode-hook scheme-mode-hook) . prism-mode))

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

(use-package vscode-icon)

(use-package all-the-icons-dired
  ;; :hook (dired-mode-hook . all-the-icons-dired-mode)
  )

(use-package dirvish
  :demand
  :custom
  (dirvish-attributes '(all-the-icons file-time file-size))
  :config
  (dirvish-override-dired-mode)
  (require 'dirvish-icons)
  (require 'dirvish-side)
  (require 'dirvish-subtree)
  :bind
  (("C-c o t" . dirvish-side)
   :map dirvish-mode-map
   ("<tab>" . dirvish-subtree-toggle)))

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

(use-package macrursors
  :disabled
  :straight (:host github
             :repo "corytertel/macrursors")
  :custom
  (macrursors-preapply-command
   (lambda () (corfu-mode -1)))
  (macrursors-postapply-command
   (lambda () (corfu-mode)))
  :init
  (define-prefix-command 'macrursors-mark-map)
  :bind
  ("C->" . macrursors-mark-next-instance-of)
  ("C-<" . macrursors-mark-previous-instance-of)
  ;; (global-set-key (kbd "C-;") 'macrursors-mark-map)
  ;; (define-key macrursors-mark-map (kbd "C-;") #'macrursors-mark-all-lines-or-instances)
  ;; (define-key macrursors-mark-map (kbd ";") #'macrursors-mark-all-lines-or-instances)
  ;; (define-key macrursors-mark-map (kbd "l") #'macrursors-mark-all-lists)
  ;; (define-key macrursors-mark-map (kbd "s") #'macrursors-mark-all-symbols)
  ;; (define-key macrursors-mark-map (kbd "e") #'macrursors-mark-all-sexps)
  ;; (define-key macrursors-mark-map (kbd "f") #'macrursors-mark-all-defuns)
  ;; (define-key macrursors-mark-map (kbd "n") #'macrursors-mark-all-numbers)
  ;; (define-key macrursors-mark-map (kbd ".") #'macrursors-mark-all-sentences)
  ;; (define-key macrursors-mark-map (kbd "r") #'macrursors-mark-all-lines)
  )

(use-package multiple-cursors
  :bind
  (("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m c" . mc/edit-lines)
   ("C-c m d" . mc/mark-all-dwim)
   ("C-c m m a" . mc/mark-all-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)))

(use-package ace-mc
  :disabled
  :bind
  ("C-c m o" . ace-mc-add-multiple-cursors))

(use-package expand-region
  :bind
  ("C-M-SPC" . er/expand-region)
  :custom
  (expand-region-smart-cursor t)
  (expand-region-contract-fast-key "C-M-SPC"))

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
  (("M-t" . avy-goto-char-2)
   ("C-t" . avy-goto-word-1))
  :custom
  (avy-background t)
  (avy-keys '(?r ?s ?n ?t ?a ?e ?e ?i ?h))
  (avy-dispatch-alist '((?\M-o . avy-action-embark))))

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
  ;; (global-hungry-delete-mode)
  )

(use-package which-key
  :config
  (which-key-mode))

(use-package dashboard
  :preface
  (defun dashboard-buffer ()
    (get-buffer-create "*dashboard*"))
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
  ;; (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
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
  :bind
  ("C-c o o" . olivetti-mode))

(use-package elec-pair
  :straight (:type built-in)
  :hook (prog-mode-hook . electric-pair-mode))

(use-package parinfer-rust-mode)

(use-package puni
  :config
  (setq
   mc/cmds-to-run-for-all
   (cons 'puni-backward-delete-char mc/cmds-to-run-for-all))
  (setq
   mc/cmds-to-run-once
   (remove 'puni-backward-delete-char mc/cmds-to-run-once))
  :hook (prog-mode-hook . puni-mode)
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
   ("C-M-s" . puni-splice)
   ("M-S"   . puni-split)
   ("C-M-t" . puni-transpose)
   ("C-M-?" . puni-convolute)
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
   ("C-c C-l" . eshell/clear-buffer))
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

(use-package hledger-mode
  :mode "\\.journal\\'"
  :custom
  (hledger-jfile "~/Documents/org/hledger/hledger.journal"))

(use-package apheleia
  :hook
  ((clojure-mode-hook haskell-mode-hook python-mode-hook) . apheleia-mode)
  :config
  (setf (alist-get 'cljstyle     apheleia-formatters) '("cljstyle" "pipe"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'cljstyle)
  (setf (alist-get 'fourmolu     apheleia-formatters) '("fourmolu" file))
  (setf (alist-get 'haskell-mode apheleia-mode-alist) 'fourmolu))

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
  :config
  (setq magit-git-environment
         (append magit-git-environment
            (list "OVERCOMMIT_COLOR=0"))))

(use-package magit-todos)

(use-package forge)

(use-package code-review
  :straight
  (:host github
   :repo "wandersoncferreira/code-review"
   :build (:not compile))
  :demand t
  :config
  (transient-append-suffix 'forge-dispatch '(0 2 -1)
    '("b c" "code-review pr at point" code-review-forge-pr-at-point)))

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

(use-package wgrep
  :custom
  (wgrep-enable-key "e"))

(use-package vertico
  :config
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; TODO: I think the previews can be better
(use-package consult
  :custom
  (consult-locate-args "plocate --ignore-case --existing --regexp")
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ("M-s l"   . consult-line)
   ("M-s M-l" . consult-line-multi)
   ("M-s r"   . consult-ripgrep)
   ("M-s M-o" . consult-multi-occur)
   ("M-s d"   . consult-find)
   ("M-s M-d" . consult-locate)
   ("M-s e"   . consult-isearch-history)
   ("M-g e"           . consult-compile-error)
   ("M-g m"           . consult-mark)
   ("M-g f"           . consult-flycheck)
   ("M-g o"           . consult-outline)
   ([remap imenu]     . consult-imenu)
   ("M-g M-i"         . consult-imenu-multi)
   ([remap goto-line] . consult-goto-line)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap yank-pop] . consult-yank-pop))
  :config
  (recentf-mode))

(use-package consult-flycheck
  :after flycheck
  :straight (:host github
             :repo "minad/consult-flycheck"))

(use-package marginalia
  :config
  (marginalia-mode)
  :custom
  (marginalia-align 'center))

(use-package embark
  :config
  :bind
  (("M-o" . embark-act)
   ("M-." . embark-dwim)
   :map vertico-map
   ("M-s o" . embark-export)
   :map embark-general-map
   ([remap describe-symbol] . helpful-symbol)))

(use-package embark-consult
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; (use-package envrc
;;   :config
;;   (envrc-global-mode))

(use-package smart-tabs-mode
  :config
  (setq-mode-local c-mode indent-tabs-mode t)
  (setq-mode-local c++-mode indent-tabs-mode t)
  (smart-tabs-insinuate 'c 'c++))

(use-package smart-tab
  :config
  (global-smart-tab-mode))

;; TODO: does it work the way it should with all the capf ordering?
(use-package tempel
  :bind
  (:map tempel-map
   ("C-M-n" . tempel-next)
   ("C-M-p" . tempel-previous))
  :preface
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete completion-at-point-functions)))
  :hook
  (prog-mode-hook . tempel-setup-capf)
  (text-mode-hook . tempel-setup-capf))

(use-package corfu
  :straight
  (:host github
   :repo "minad/corfu"
   :files ("*" "extensions/*" (:exclude ".git")))
  :preface
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :bind
  (:map corfu-map
   ("M-s o" . corfu-move-to-minibuffer))
  :custom
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'directory)
  :demand
  :config
  (put 'completion-at-point-functions 'safe-local-variable #'listp)
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (set-face-attribute 'corfu-popupinfo nil :height 0.9))

(use-package cape
  :straight (:host github
             :repo "minad/cape"
             :files ("*.el" "extensions/*.el"))
  :preface
  (defun cape-setup ()
    (setq-local completion-at-point-functions
                (append (list #'cape-file #'cape-keyword #'cape-dabbrev)
                        completion-at-point-functions)))
  :hook (prog-mode-hook . cape-setup)
  :bind
  ("M-/" . cape-dabbrev))

(use-package kind-icon
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
  (kill-emacs-hook . tabnine-capf-kill-process)
  ;; (prog-mode-hook . enable-tabnine)
  :preface
  (defun enable-tabnine ()
    (setq-local completion-at-point-functions (cons #'tabnine-completion-at-point completion-at-point-functions)))
  (defun disable-tabnine ()
    (setq-local completion-at-point-functions (delete #'tabnine-completion-at-point completion-at-point-functions)))
  (defun turn-on-tabnine ()
    (interactive)
    (enable-tabnine)
    (add-hook 'lsp-completion-mode-hook #'enable-tabnine)
    (add-hook 'prog-mode-hook #'enable-tabnine)
    (message "TabNine enabled"))
  (defun turn-off-tabnine ()
    (interactive)
    (disable-tabnine)
    (remove-hook 'lsp-completion-mode-hook #'enable-tabnine)
    (remove-hook 'prog-mode-hook #'enable-tabnine)
    (message "TabNine disabled")))

(use-package codeium
  :disabled
  :straight (:host github
             :repo "Exafunction/codeium.el")
  :config
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (dumb-jump-default-project "~/Code")
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package consult-flyspell
  :hook
  (text-mode-hook . flyspell-mode)
  (prog-mode-hook . flyspell-prog-mode)
  :custom
  (ispell-program-name "hunspell")
  (ispell-local-dictionary "en_US"))

(use-package wucuo
  :hook
  (text-mode-hook . wucuo-start)
  (prog-mode-hook . wucuo-start))

(use-package flycheck
  :custom
  (flycheck-indication-mode 'right-fringe)
  :demand t
  :config
  (global-flycheck-mode)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(use-package flycheck-posframe
  ;; :hook
  ;; (flycheck-mode-hook . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'window-bottom-right-corner)
  (flycheck-posframe-border-width 10)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(use-package flycheck-inline
  ;; :disabled
  ;; :hook (flycheck-mode-hook . flycheck-inline-mode)
  )

(use-package flycheck-pos-tip
  :disabled
  :custom
  (flycheck-pos-tip-timeout 0)
  :config
  (flycheck-pos-tip-mode t))

(use-package topsy
  :disabled
  :hook
  (prog-mode-hook . topsy-mode)
  :preface
  (defun haskell-beginning-of-function ()
    "Return the line moved to by `haskell-ds-backward-decl'."
    (when (> (window-start) 1)
      (save-excursion
        (goto-char (window-start))
        (haskell-ds-backward-decl)
        (font-lock-ensure (point) (point-at-eol))
        (buffer-substring (point) (point-at-eol)))))
  :config
  (push `(haskell-mode . haskell-beginning-of-function) topsy-mode-functions))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (eldoc-echo-area-prefer-doc-buffer 'maybe)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  :hook (eldoc-mode-hook . eldoc-box-hover-mode))

;; TODO: eglot-ignored-server-capabilites to ignore haskell on-save
(use-package eglot
  :disabled
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

(use-package lsp-mode
  :straight (lsp-mode
             :type git
             :flavor melpa
             :files (:defaults "clients/*.el" "lsp-mode-pkg.el")
             :host github
             :repo "emacs-lsp/lsp-mode"
             :build (:not compile))
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
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (read-process-output-max (* 1024 1024 10))
  (lsp-file-watch-threshold 512)
  (lsp-diagnostics-flycheck-default-level 'warning))

(use-package consult-lsp
  :bind
  (:map lsp-mode-map
   ("C-c l c d" . consult-lsp-diagnostics)
   ("C-c l c s" . consult-lsp-symbols)
   ("C-c l c f" . consult-lsp-file-symbols)))

(use-package dap-mode
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
  (lsp-ui-peek-enable t)
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
  ;; :bind
  ;; ("C-c o t" . treemacs)
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
  :config
  (lsp-treemacs-sync-mode))

(use-package haskell-mode
  :custom
  (haskell-completing-read-function #'completing-read)
  (haskell-process-show-overlays nil)
  (haskell-process-suggest-restart nil)
  (haskell-font-lock-symbols nil)
  :config
  (require 'haskell-decl-scan))

(use-package lsp-haskell
  :after lsp-mode
  :custom
  (lsp-haskell-formatting-provider "fourmolu")
  :bind
  (:map haskell-mode-map
   ("C-c l l" . lsp))
  :config
  (setf (alist-get 'lsp-haskell-server-path safe-local-variable-values)
        "haskell-language-server"))

;; TODO: check out https://github.com/astoff/comint-mime
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
  (inferior-lisp-program "sbcl")
  :config
  (setq-default sly-symbol-completion-mode nil))

(use-package stumpwm-mode)

(use-package geiser)

(use-package geiser-guile)

(use-package flycheck-clj-kondo)

(use-package clojure-ts-mode
  :straight (:host github
             :repo "clojure-emacs/clojure-ts-mode"))

(use-package cider
  :bind
  (:map cider-repl-mode-map
   ("C-l" . cider-repl-clear-buffer)
   :map cider-mode-map
   ("C-c M-c" . cider-debug-defun-at-point)
   ("C-c C-p" . cider-inspect-last-sexp))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-enrich-classpath t)
  :preface
  (defun clerk/show ()
    (interactive)
    (when-let
        ((filename
          (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")"))))
  ;; Leverage an existing cider nrepl connection to evaluate portal.api functions
  ;; and map them to convenient key bindings.
  ;; def portal to the dev namespace to allow dereferencing via @dev/portal
  (defun portal.api/open ()
    (interactive)
    (cider-nrepl-sync-request:eval
     "
(do (ns dev)
  (def portal ((requiring-resolve 'portal.api/open)))
  (add-tap (requiring-resolve 'portal.api/submit)))"))
  (defun portal.api/clear ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/clear)"))
  (defun portal.api/close ()
    (interactive)
    (cider-nrepl-sync-request:eval "(portal.api/close)"))
  ;; NOTE: You do need to have portal on the class path and the easiest way I know
  ;; how is via a clj user or project alias.
  (setq cider-clojure-cli-global-options "-A:portal"))

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

(use-package aggressive-indent-mode
  :hook (lisp-mode-hook . aggressive-indent-mode))

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

(use-package treesit
  :straight (:type built-in)
  :config
  (setq
   treesit-extra-load-path
   '("~/.tree-sitter/bin/"
     "~/.config/emacs/straight/build/tree-sitter-langs/bin/")))

;; ;; NOTE: doesn't modify the hooks
;; (use-package treesit-auto
;;   :straight
;;   (:host github
;;    :repo "renzmann/treesit-auto")
;;   :custom
;;   (treesit-auto-install t)
;;   :config
;;   (add-to-list
;;    'treesit-auto-fallback-alist
;;    '(bash-ts-mode . sh-mode))
;;   (global-treesit-auto-mode))

;; (use-package combobulate
;;   :straight
;;   (:host github
;;    :repo "michkeynp/combobulate"))

(use-package tree-sitter
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (push '(clojure-mode . clojure) tree-sitter-major-mode-language-alist)
  (push '(haskell-mode . haskell) tree-sitter-major-mode-language-alist)
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(use-package ts-fold
  :straight (:host github
             :repo "emacs-tree-sitter/ts-fold")
  :hook (tree-sitter-after-on-hook . ts-fold-mode))

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

(use-package xah-math-input
  :bind
  ("C-c m i" . xah-math-input-change-to-symbol))

(use-package pdf-tools
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-page 'fit-page))

(use-package tex-site
  :straight auctex
  :hook
  (LaTeX-mode-hook . LaTeX-math-mode)
  (LaTeX-mode-hook . turn-on-reftex)
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  (LaTeX-mode-hook . prettify-symbols-mode)
  :custom
  (reftex-plug-into-AUCTeX t))

;; (use-package latex-preview-pane)

(use-package nov
  :straight (nov :type git
                 :flavor melpa
                 :repo "https://depp.brause.cc/nov.el.git"
                 :build (:not compile))
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 120))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package rainbow-mode)

;; TODO: something's wrong with the font sizes
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

(use-package moldable-emacs
  :straight nil
  ;; must be downloaded separately
  :load-path "~/.config/emacs/moldable-emacs/"
  :config
  (require 'moldable-emacs)
  (me-setup-molds))

;; (use-package code-compass
;;   :straight nil
;;   ;; must be downloaded separately
;;   :load-path "~/.config/emacs/code-compass/"
;;   :init
;;   (use-package async)
;;   (use-package dash)
;;   (use-package f)
;;   (use-package s)
;;   (use-package simple-httpd))

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

(use-package mpdel
  :config
  (mpdel-mode)
  :custom
  (mpdel-prefix-key (kbd "C-c o n m")))

(use-package mpdel-embark
  :straight
  (:host github
   :repo "mpdel/mpdel-embark")
  :after (embark mpdel)
  :config
  (mpdel-embark-setup))

(use-package ement)

(use-package alert
  :demand t
  :init
  (setq alert-default-style 'notifications))

(use-package slack
  :custom
  (slack-render-image-p t)
  (slack-buffer-emojify t)
  :bind-keymap
  ("C-c o n s" . slack-prefix-map)
  :config
  (defvar slack-prefix-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "s") #'slack-start)
      (define-key map (kbd "u") #'slack-select-unread-rooms)
      (define-key map (kbd "m") #'slack-select-rooms)
      map))
  (slack-register-team
   :name "blocklabs"
   :token (auth-source-pick-first-password
           :host "blocklabs.slack.com"
           :user "arjaz")
   :cookie (auth-source-pick-first-password
           :host "blocklabs.slack.com"
           :user "arjaz^cookie")))

(use-package telega
  :straight (:host github
             :repo "zevlg/telega.el"
             :branch "release-0.8.0"
             :build (:not compile))
  :hook
  ;; Make the text variable-pitch
  (telega-root-mode-hook . buffer-face-mode)
  (telega-chat-mode-hook . buffer-face-mode)
  :bind-keymap
  ("C-c o n t" . telega-prefix-map)
  :custom
  (telega-use-images t)
  (telega-root-show-avatars t)
  (telega-chat-show-avatars t)
  (telega-completing-read-function #'completing-read)
  :config
  ;; make the avatars appear on separate lines
  ;; to avoid stripes
  ;; (setf (alist-get 2 telega-avatar-factors-alist) '(0.4 . 0.1))
  ;; TODO: figure this one out
  ;; (require 'telega-adblock)
  ;; (require 'telega-transient)
  ;; (telega-adblock-mode)
  ;; (telega-transient-mode)
  (telega-notifications-mode))

(use-package explain-pause-mode)

(use-package keyfreq
  :config
  (keyfreq-mode))

(provide 'init)
;;; init.el ends here
