;;; init.el --- My configuration
;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; My Emacs configuration

;;; Code:

(defvar comp-deferred-compilation-deny-list ())
(defvar native-comp-deferred-compilation-deny-list ())
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

(straight-use-package 'org)
(defvar use-package-enable-imenu-support t)
(straight-use-package 'use-package)

(use-package use-package-core
  :straight (:type built-in)
  :custom
  (use-package-hook-name-suffix nil))

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-check-for-modifications '(watch-files vfind-when-checking)))

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
  (sentence-end-double-space nil)
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
  (frame-resize-pixelwise t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)
  (cursor-type 'hbar)
  :config
  (unbind-key (kbd "C-x C-z") 'global-map)
  (unbind-key (kbd "C-z") 'global-map)
  (window-divider-mode)
  (blink-cursor-mode 0))

(use-package sublimity
  ;; :custom
  ;; (sublimity-scroll-weight 20.0)
  ;; (sublimity-scroll-drift-length 0)
  :config
  (require 'sublimity-scroll)
  (sublimity-mode))

(use-package pixel-scroll
  :straight (:type built-in)
  :custom
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (pixel-scroll-precision-momentum-seconds 1.75)
  (pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-interpolate-page t)
  ;; :hook
  ;; (after-init-hook . pixel-scroll-precision-mode)
  ;; :bind
  ;; ("C-v" . (lambda ()
  ;;            (interactive)
  ;;            (pixel-scroll-interpolate-down)
  ;;            (move-to-window-line nil)))
  ;; ("M-v" . (lambda ()
  ;;            (interactive)
  ;;            (pixel-scroll-interpolate-up)
  ;;            (move-to-window-line nil)))
  )

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
  (let ((path "~/dotfiles/emacs/elisp-fix-indent.el"))
    (when (file-exists-p path)
      (load path))))

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
  (auto-save-default nil "stop creating those ugly #name# files")
  :config
  (unless (file-exists-p (concat user-emacs-directory "backups"))
    (make-directory (concat user-emacs-directory "backups") t)))

(use-package hideshow
  :straight (:type built-in)
  :init
  (defvar-keymap hideshow-map
    :doc "Keymap for hs-minor-mode"
    "t"   'hs-toggle-hiding
    "h"   'hs-hide-block
    "s"   'hs-show-block
    "C-h" 'hs-hide-all
    "C-s" 'hs-show-all
    "l"   'hs-hide-level)
  :bind-keymap
  ("C-c t" . hideshow-map)
  :hook
  (prog-mode-hook . hs-minor-mode))

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

(use-package visual-regexp
  :bind
  ([remap query-replace] . vr/replace))

(use-package visual-regexp-steroids
  :custom
  (vr/default-regexp-modifiers '(:I t :M t :S nil :U nil))
  :bind
  ([remap isearch-forward] . vr/isearch-forward)
  ([remap isearch-backward] . vr/isearch-backward)
  :config
  (defadvice vr--isearch (around add-case-insensitive (forward string &optional bound noerror count) activate)
    (when (and (eq vr/engine 'python) case-fold-search)
      (setq string (concat "(?i)" string)))
    ad-do-it))

(use-package mermaid-mode)

(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path "/usr/bin/mmdc"))

(use-package org
  :hook
  (org-mode-hook . variable-pitch-mode)
  (org-mode-hook . auto-fill-mode)
  (org-babel-after-execute-hook . org-redisplay-inline-images)
  :bind
  ("C-c a a" . org-agenda)
  ("C-c a c" . org-capture)
  (:map org-mode-map
   ("C-c a d" . org-archive-all-done))
  :demand
  :preface
  (defun open-org-agenda ()
    (org-agenda nil "n")
    (delete-other-windows)
    (get-buffer "*Org Agenda*"))
  :custom
  (initial-buffer-choice #'open-org-agenda)
  (org-confirm-babel-evaluate nil)
  (org-directory "~/documents/org/")
  (org-default-notes-file (concat org-directory "todo.org"))
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-agenda-files (list org-default-notes-file "~/android-sync/Notes/Notes.org"))
  (org-columns-default-format "%50ITEM(Task) %TODO %10CLOCKSUM %16TIMESTAMP_IA")
  (org-capture-templates
   `(("t" "Todo" entry (file+headline ,org-default-notes-file "Tasks")
      "* TODO %?\n%i\n%a\n  %u")
     ("n" "Active task" entry (file+headline ,org-default-notes-file "Tasks")
      "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (current-time))\n%i\n%a\n  %u")))
  :config
  (require 'org-archive)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t)
     (mermaid    . t)
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
  :preface
  (defun org-set-line-spacing ()
    (setq-local line-spacing 0.05))
  :config
  (set-face-attribute 'org-modern-label nil :height 1.0)
  :hook
  (org-mode-hook . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda)
  (org-mode-hook . org-set-line-spacing)
  (org-agenda-finalize-hook . org-set-line-spacing)
  :custom
  (org-modern-hide-stars 'leading)
  (org-modern-table nil)
  (org-indent-indentation-per-level 1))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package dbus
  :hook
  (after-init-hook . load-theme-on-startup)
  :preface
  (defun set-theme-from-dbus-value (value)
    (if (equal value '1)
        (run-hooks 'dbus-dark-theme-hook)
      (run-hooks 'dbus-light-theme-hook)))
  (defun dbus-on-theme-changed (path var value)
    (when (and (string-equal path "org.freedesktop.appearance")
               (string-equal var "color-scheme"))
      (set-theme-from-dbus-value (car value))))
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
  :init
  (defvar dbus-light-theme-hook ())
  (defvar dbus-dark-theme-hook ())
  :config
  (dbus-register-signal
   :session "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings"
   "SettingChanged"
   #'dbus-on-theme-changed))

(use-package spacious-padding
  :demand
  :bind
  ("C-c o p" . spacious-padding-mode))

(use-package modus-themes
  :custom
  (modus-operandi-palette-overrides
   '((comment green-faint)
     (docstring green-faint)
     (string green-faint)))
  (modus-vivendi-palette-overrides
   '((comment olive)
     (docstring olive)
     (string olive)))
  (modus-themes-common-palette-overrides
   '((bg-region bg-green-nuanced)
     (fg-region unspecified)
     (comment olive)
     (docstring olive)
     (string olive)
     (preprocessor fg-main)
     (constant fg-main)
     (variable fg-main)
     (type fg-main)
     (fnname fg-main)
     (keyword fg-main)
     (builtin fg-main)))
  (modus-themes-mixed-fonts t)
  :hook
  (dbus-light-theme-hook . load-light-theme)
  (dbus-dark-theme-hook . load-dark-theme)
  :preface
  (defun load-dark-theme ()
    "Load the saved dark theme."
    (interactive)
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme dark-theme t)
    (set-face-attribute font-lock-keyword-face nil
                        :weight 'semibold))
  (defun load-light-theme ()
    "Load the saved light theme."
    (interactive)
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme light-theme t)
    (set-face-attribute font-lock-keyword-face nil
                        :weight 'semibold))
  :init
  (defvar light-theme 'modus-operandi)
  (defvar dark-theme 'modus-vivendi))

(use-package fontaine
  :preface
  (defun fontaine-load-light ()
    (fontaine-set-preset '110-normal))
  (defun fontaine-load-dark ()
    (fontaine-set-preset '110-normal))
  :hook
  (dbus-light-theme-hook . fontaine-load-light)
  (dbus-dark-theme-hook . fontaine-load-dark)
  :config
  (setq fontaine-presets
        '((90-light
           :default-family "Iosevka Comfy Motion"
           :default-height 90
           :default-weight light
           :variable-pitch-family "Iosevka Comfy Motion Duo")
          (100-semilight
           :inherit 90-light
           :default-weight semilight
           :default-height 100)
          (110-normal
           :inherit 90-light
           :default-height 110
           :default-weight normal)
          (120-normal
           :inherit 90-light
           :default-height 120
           :default-weight normal)
          (100-normal
           :inherit 90-light
           :default-height 100
           :default-weight normal)
          (90-normal
           :inherit 90-light
           :default-height 90
           :default-weight normal))))

(use-package indent-bars
  :disabled
  :straight
  (:host github
   :repo "jdtsmith/indent-bars")
  :hook
  (prog-mode-hook . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.3))
  (indent-bars-highlight-current-depth '(:face default :blend 0.3))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth nil)
  (indent-bars-display-on-blank-lines t))

(use-package gdscript-mode)

(use-package all-the-icons)

(use-package vscode-icon)

(use-package all-the-icons-dired
  ;; :hook (dired-mode-hook . all-the-icons-dired-mode)
  )

(use-package dirvish
  :demand
  :custom
  (dirvish-attributes '(all-the-icons collapse file-time file-size))
  (dirvish-use-mode-line nil)
  (dirvish-subtree-prefix "  ")
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

(use-package dired
  :straight (:type built-in)
  :hook
  (dired-mode-hook . auto-revert-mode)
  :custom
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)
  (dired-listing-switches "-alhg")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top))

(use-package diredfl
  :hook (dired-mode-hook . diredfl-mode))

;; TODO: integrate with avy
(use-package macrursors
  :straight (:host github
             :repo "corytertel/macrursors")
  :hook
  (macrursors-pre-finish-hook . corfu-mode)
  (macrursors-post-finish-hook . corfu-mode)
  :init
  (define-prefix-command 'macrursors-mark-map)
  :bind
  (("C->" . macrursors-mark-next-line)
   ("C-<" . macrursors-mark-previous-line)
   ("C-c m n" . macrursors-mark-next-instance-of)
   ("C-c m p" . macrursors-mark-previous-instance-of)
   :map isearch-mode-map
   ("M-s m" . macrursors-mark-from-isearch)
   ("M-s n" . macrursors-mark-next-from-isearch)
   ("M-s p" . macrursors-mark-previous-from-isearch)))

(use-package smart-comment
  :bind
  ("M-;" . smart-comment))

(use-package isearch
  :straight
  (:type built-in)
  :preface
  (defun isearch-forward-other-window (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))
  (defun isearch-backward-other-window (prefix)
    "Function to isearch-backward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix 1 -1)))
          (other-window next)
          (isearch-backward)
          (other-window (- next))))))
  :bind
  ("C-M-s" . isearch-forward-other-window)
  ("C-M-r" . isearch-backward-other-window))

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
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  :bind
  (("M-t" . avy-goto-char-2)
   ("C-t" . avy-goto-word-1)
   :map isearch-mode-map
   ("M-t" . avy-isearch))
  :custom
  (avy-style 'post)
  (avy-background t)
  (avy-keys '(?r ?s ?n ?t ?a ?e ?i ?h))
  (avy-dispatch-alist
   '((?\o . avy-action-embark)
     (?\M-t . avy-action-teleport)
     (?\M-T . avy-action-teleport-whole-line)
     (?m . avy-action-mark)
     (?  . avy-action-mark-to-char)
     (?z . avy-action-zap-to-char)
     (?w . avy-action-copy)
     (?W . avy-action-copy-whole-line)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-whole-line)
     (?k . avy-action-kill-stay)
     (?K . avy-action-kill-whole-line))))

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-keys avy-keys)
  (aw-ignore-current t)
  :bind
  ([remap other-window] . ace-window))

(use-package rotate
  :custom
  (rotate-functions
   '(rotate:even-horizontal
     rotate:even-vertical))
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
  :custom
  (hungry-delete-join-reluctantly t)
  :config
  (global-hungry-delete-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package dashboard
  :disabled
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
  ;; (dashboard-agenda-release-buffers t)
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
  (olivetti-body-width 180)
  ;; :hook
  ;; (compilation-mode-hook . olivetti-mode)
  :bind
  ("C-c o o" . olivetti-mode))

(use-package auto-olivetti
  :straight
  (auto-olivetti
   :host sourcehut
   :repo "ashton314/auto-olivetti")
  :config
  (auto-olivetti-mode)
  :custom
  (auto-olivetti-enabled-modes '(text-mode prog-mode)))

(use-package elec-pair
  :straight (:type built-in)
  :hook (prog-mode-hook . electric-pair-mode))

(use-package parinfer-rust-mode)

(use-package puni
  :bind
  (("M-r"   . puni-raise)
   ("C-M-s" . puni-splice)
   ("M-S"   . puni-split)
   ("C-M-t" . puni-transpose)
   ("C-M-?" . puni-convolute)
   ("C-("   . puni-slurp-backward)
   ("C-)"   . puni-slurp-forward)
   ("C-{"   . puni-barf-backward)
   ("C-}"   . puni-barf-forward)))

(use-package ansi-color
  :straight (:type built-in)
  :preface
  (defun colorize-compilation-buffer ()
    (read-only-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode))
  :hook
  (compilation-filter-hook . colorize-compilation-buffer))

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
  (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
  ;; (remove-hook 'eshell-preoutput-filter-functions 'eshell-handle-ansi-color)
  (setenv "TERM" "xterm-256color"))

(use-package eshell-up)

(use-package pcmpl-args
  :config
  (require 'pcmpl-gnu))

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

(use-package mistty
  :straight (:host github
             :repo "szermatt/mistty")
  :custom
  (explicit-shell-file-name "nu")
  :bind
  ("C-c o s" . mistty))

(use-package vterm
  :custom
  (vterm-shell "nu")
  :bind
  ("C-c o v" . vterm))

(use-package eshell-vterm
  :config
  (eshell-vterm-mode)
  (push "btm" eshell-visual-commands))

(use-package org-mime)

(use-package hledger-mode
  :mode "\\.journal\\'"
  :custom
  (hledger-jfile "~/documents/org/hledger/hledger.journal"))

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
  (magit-wip-mode)
  (setq magit-git-environment
         (append magit-git-environment
            (list "OVERCOMMIT_COLOR=0"))))

(use-package magit-delta
  :hook (magit-mode-hook . magit-delta-mode))

(use-package magit-todos
  :config
  (magit-todos-mode))

(use-package forge)

(use-package code-review
  :disabled
  :straight
  (:host github
   :repo "wandersoncferreira/code-review"
   :build (:not compile))
  :demand t
  :config
  (transient-append-suffix 'forge-dispatch '(0 2 -1)
    '("b c" "code-review pr at point" code-review-forge-pr-at-point)))

(use-package breadcrumb
  :straight
  (:host github
   :repo "joaotavora/breadcrumb"))

(use-package mini-modeline
  :config
  (setq
   mini-modeline-enhance-visual nil
   mini-modeline-display-gui-line nil
   mini-modeline-l-format
   '((:propertize (:eval (file-directory)) face font-lock-variable-name-face)
     (:propertize (:eval (file-or-buffer-name)) face font-lock-keyword-face))
   mini-modeline-r-format
   '(;; (:eval (breadcrumb-imenu-crumbs))
     "%5l:%c"
     (:eval (s-repeat (- 4 (length (number-to-string (current-column)))) " "))
     (:propertize (:eval (file-read-write-indicator)) face font-lock-warning-face)
     "  "
     (:propertize
      (:eval (unless (file-remote-p default-directory)
               (git-branch)))
      face magit-dimmed)))
  (mini-modeline-mode))

(use-package browse-at-remote)

(use-package project
  :straight (:type built-in))

(use-package projection
  :disabled
  :bind-keymap
  ("C-x P" . projection-map))

(use-package consult-project-extra
  :bind
  ([remap project-find-file] . consult-project-extra-find))

(use-package wgrep
  :custom
  (wgrep-enable-key "e"))

(use-package vertico
  :straight (vertico :includes vertico-multiform
                     :files (:defaults "extensions/vertico-multiform.el"))
  :config
  (vertico-mode))

(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode))

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

(use-package consult-ls-git
  :bind
  ("M-s g" . consult-ls-git))

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
  :custom
  (smart-tab-user-provided-completion-function  'corfu-candidate-overlay-complete-at-point)
  (smart-tab-completion-functions-alist nil)
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

;; TODO: I want rounded borders and the same background color
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
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match 'insert)
  (corfu-preselect 'first)
  :demand
  :config
  (set-face-attribute
   'corfu-default nil
   :background 'unspecified
   :inherit 'default)
  (put 'completion-at-point-functions 'safe-local-variable #'listp)
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (set-face-attribute 'corfu-popupinfo nil :height 0.9))

(use-package corfu-candidate-overlay
  :after corfu
  :bind
  ("C-<tab>" . corfu-candidate-overlay-complete-at-point)
  :config
  (corfu-candidate-overlay-mode))

(use-package cape
  :straight (:host github
             :repo "minad/cape"
             :files ("*.el" "extensions/*.el"))
  :preface
  (defun cape-setup ()
    (setq-local completion-at-point-functions
                (delete-dups
                 (append (list #'cape-file #'cape-keyword #'cape-dabbrev)
                         completion-at-point-functions))))
  ;; TODO: use lisp-complete-symbol for elisp
  :hook
  (prog-mode-hook . cape-setup)
  (lsp-completion-mode-hook . cape-setup)
  :bind
  ("M-/" . cape-dabbrev))

(use-package kind-icon
  :custom
  (kind-icon-extra-space t)
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

(use-package copilot
  :disabled
  :straight (:host github
             :repo "zerolfx/copilot.el"
             :files ("dist" "*.el"))
  :bind
  ("M-\\" . copilot-accept-completion)
  ;; :hook
  ;; (prog-mode-hook . copilot-mode)
  )

(use-package codeium
  :straight (:host github
             :repo "Exafunction/codeium.el")
  :preface
  (defun enable-codeium ()
    (setq-local completion-at-point-functions (cons #'codeium-completion-at-point completion-at-point-functions)))
  (defun disable-codeium ()
    (setq-local completion-at-point-functions (delete #'codeium-completion-at-point completion-at-point-functions)))
  (defun turn-on-codeium ()
    (interactive)
    (enable-codeium)
    (add-hook 'lsp-completion-mode-hook #'enable-codeium)
    (add-hook 'prog-mode-hook #'enable-codeium)
    (message "Codeium enabled"))
  (defun turn-off-codeium ()
    (interactive)
    (disable-codeium)
    (remove-hook 'lsp-completion-mode-hook #'enable-codeium)
    (remove-hook 'prog-mode-hook #'enable-codeium)
    (message "Codeium disabled")))

(use-package dumb-jump
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package jinx
  :straight
  (:host github
   :repo "minad/jinx"
   :files ("*"))
  :custom
  (ispell-program-name "hunspell")
  (ispell-local-dictionary "en_US")
  :config
  (global-jinx-mode))

(use-package haskell-mode
  :config
  (remove-hook 'haskell-mode-hook #'interactive-haskell-mode)
  :custom
  (haskell-completing-read-function #'completing-read)
  (haskell-process-show-overlays nil)
  (haskell-process-suggest-restart nil)
  (haskell-font-lock-symbols nil))

(use-package flycheck
  :custom
  (flycheck-indication-mode 'right-fringe)
  :demand t
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

(use-package flycheck-posframe
  :hook
  (flycheck-mode-hook . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'window-bottom-right-corner)
  (flycheck-posframe-border-width 1)
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

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  ;; (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-prefer-doc-buffer 'maybe)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  :hook (eldoc-mode-hook . eldoc-box-hover-mode)
  :config
  ;; (set-face-attribute
  ;;  'eldoc-box-body nil
  ;;  :background 'unspecified
  ;;  :foreground 'unspecified
  ;;  :inherit 'default)
  (set-face-attribute
   'eldoc-box-border nil
   :background 'unspecified
   :foreground 'unspecified
   :inherit 'default))

(use-package eglot
  :disabled
  :hook
  (eglot-managed-mode-hook . eglot-inlay-hints-mode)
  :custom
  (read-process-output-max (* 1024 1024 10))
  (eglot-confirm-server-initiated-edits nil)
  (eglot-stay-out-of '(yasnippet))
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
  (fset #'eglot--snippet-expansion-fn #'ignore)
  (require 'goto-addr)
  (push '(haskell-mode . ("haskell-language-server" "--lsp")) eglot-server-programs)
  ;; corfu setup
  (push '(eglot (styles orderless)) completion-category-overrides))

(use-package consult-eglot
  :disabled
  :bind
  (:map eglot-mode-map
   ("C-c l c" . consult-eglot-symbols)))

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
  (typescript-mode-hook . lsp-deferred)
  (typescript-ts-mode-hook . lsp-deferred)
  (tsx-ts-mode-hook . lsp-deferred)
  (zig-mode-hook . lsp-deferred)
  (haskell-mode-hook . lsp-deferred)
  (rust-mode-hook . lsp-deferred)
  (rust-ts-mode-hook . lsp-deferred)
  :bind
  ("C-c l l" . lsp)
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
  (lsp-inlay-hint-enable t)
  (lsp-enable-snippet nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-semantic-tokens-enable nil)
  (read-process-output-max (* 1024 1024 10))
  (lsp-file-watch-threshold 512)
  (lsp-diagnostics-flycheck-default-level 'warning)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t))

(use-package consult-lsp
  :bind
  (:map lsp-mode-map
   ("C-c l c d" . consult-lsp-diagnostics)
   ("C-c l c s" . consult-lsp-symbols)
   ("C-c l c f" . consult-lsp-file-symbols)))

(use-package dap-mode
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip))
  :hook
  (dap-stopped-hook . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (require 'dap-python)
  (require 'dap-variables)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (require 'dap-cpptools)
  (dap-cpptools-setup)
  (require 'dap-chrome)
  (dap-chrome-setup))

(use-package lsp-ui
  :straight (lsp-ui
             :type git
             :flavor melpa
             :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el")
             :host github
             :repo "emacs-lsp/lsp-ui"
             :build (:not compile))
  :disabled
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
  (lsp-ui-sideline-show-diagnostics nil)
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

(use-package lsp-haskell
  :after lsp-mode
  :preface
  :custom
  (lsp-haskell-formatting-provider "fourmolu")
  (lsp-haskell-plugin-pragmas-completion-on nil)
  (lsp-haskell-plugin-ghcide-completions-config-auto-extend-on nil)
  (lsp-haskell-plugin-ghcide-completions-config-snippets-on nil)
  :config
  (setf (alist-get 'lsp-haskell-server-path safe-local-variable-values)
        "haskell-language-server"))

(use-package tuareg)

(use-package merlin)

(use-package dune)

(use-package eros
  :hook (emacs-lisp-mode-hook . eros-mode))

(use-package sly
  :demand
  :bind
  (:map sly-mode-map
   ("C-c M-i" . sly-inspect))
  :custom
  (sly-complete-symbol-function 'sly-simple-completions)
  (inferior-lisp-program "ros -Q run")
  :config
  (setq-default sly-symbol-completion-mode nil))

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
  :preface
  (defun setup-clj-refactor ()
    (interactive)
    (clj-refactor-mode)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  :hook
  (clojure-mode-hook . setup-clj-refactor))

(use-package aggressive-indent-mode
  :hook (lisp-mode-hook . aggressive-indent-mode))

(use-package rust-mode)

(use-package typescript-mode)

(use-package markdown-mode)

(use-package yaml-mode)

(use-package yuck-mode)

(use-package dockerfile-mode)

(use-package nginx-mode)

(use-package zig-mode)

(use-package lua-mode)

(use-package graphql-mode)

(use-package solidity-mode)

(use-package elixir-mode)

;; (setq load-path (cons "/usr/lib/erlang/lib/tools-3.5.3/emacs" load-path))
;; (use-package erlang-start
;;   :straight (:type built-in)
;;   :config
;;   (setq erlang-root-dir "/usr/lib/erlang/")
;;   (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;;   (setq erlang-man-root-dir "/usr/lib/erlang/man"))

(use-package visible-mark
  :config
  (visible-mark-mode))

(use-package treesit
  :straight (:type built-in)
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq
   treesit-extra-load-path
   '("~/.tree-sitter/bin/"
     "~/.config/emacs/straight/build/tree-sitter-langs/bin/")))

(use-package treesit-auto
  :straight
  (:host github
   :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode))

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
  ;; (global-ligature-mode)
  )

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

(use-package apheleia
  :hook
  (clojure-mode-hook . apheleia-mode)
  (haskell-mode-hook . apheleia-mode)
  (python-mode-hook . apheleia-mode)
  (rust-mode-hook . apheleia-mode)
  (rust-ts-mode-hook . apheleia-mode)
  (typescript-mode-hook . apheleia-mode)
  (typescript-ts-mode-hook . apheleia-mode)
  (tsx-ts-mode-hook . apheleia-mode)
  :demand
  :config
  (setf (alist-get 'cljstyle     apheleia-formatters) '("cljstyle" "pipe"))
  (setf (alist-get 'clojure-mode apheleia-mode-alist) 'cljstyle)
  (setf (alist-get 'fourmolu     apheleia-formatters) '("fourmolu" file))
  (setf (alist-get 'haskell-mode apheleia-mode-alist) 'fourmolu))

(use-package editorconfig
  :config
  (editorconfig-mode))

(use-package rainbow-mode)

;; TODO: something's wrong with the font sizes, only 9 works
(use-package screenshot
  :straight (:host github
             :repo "tecosaur/screenshot"
             :build (:not compile))
  :custom
  (screenshot-max-width 140))

(use-package nix-mode)

(use-package pretty-sha-path
  :config
  (global-pretty-sha-path-mode))

(use-package detached
  :disabled
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

(use-package sudo-edit)

(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618)))

(use-package alert
  :demand t
  :init
  (setq alert-default-style 'notifications))

(provide 'init)
;;; init.el ends here
