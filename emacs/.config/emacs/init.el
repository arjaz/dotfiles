;;; init.el --- My configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;; My Emacs configuration

;;; Code:
;; (setq esup-depth 0)
;; (use-package esup)

(use-package benchmark-init
  :disabled
  :hook (after-init-hook . benchmark-init/deactivate)
  :demand)

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

(defun shut-up--advice (fn &rest args)
  (let ((inhibit-message t)
        (message-log-max))
    (apply fn args)))
(advice-add 'repeat-mode :around #'shut-up--advice)
(advice-add 'recentf-load-list :around #'shut-up--advice)
(advice-add 'recentf-cleanup :around #'shut-up--advice)
(advice-add 'undo-fu-session--recover-impl :around #'shut-up--advice)

(use-package ef-themes :disabled)
(use-package srcery-theme :disabled)

(use-package alabaster-theme
  :straight
  (:host github :repo "uzhne/alabaster-emacs"))

(use-package modus-themes
  :defer t
  :custom
  (modus-operandi-palette-overrides
   '(;; (bg-region bg-cyan-subtle)
     ;; (fg-region fg-main)
     (bg-region fg-main)
     (fg-region bg-main)
     (keyword fg-main)
     (docstring fg-main)
     (docmarkup fg-main)
     (fg-line-number-inactive fg-dim)
     (fg-line-number-active fg-dim)
     (bg-line-number-inactive bg-main)
     (bg-line-number-active bg-main)))
  (modus-vivendi-palette-overrides
   '((keyword fg-main)
     (bg-paren-match fg-dim)
     (docstring fg-main)
     (docmarkup fg-main)
     (fg-line-number-inactive fg-dim)
     (fg-line-number-active fg-dim)
     (bg-line-number-inactive bg-main)
     (bg-line-number-active bg-main)
     ;; (bg-region bg-cyan-subtle)
     ;; (fg-region fg-main)
     (bg-region fg-main)
     (fg-region bg-main)
     (bg-hover bg-magenta-intense)
     (bg-search-current bg-yellow-intense)
     (bg-search-lazy bg-cyan-intense)
     (bg-search-replace bg-red-intense)
     (bg-search-rx-group-0 bg-blue-intense)
     (bg-search-rx-group-1 bg-green-intense)
     (bg-search-rx-group-2 bg-red-subtle)
     (bg-search-rx-group-3 bg-magenta-subtle)))
  (modus-themes-common-palette-overrides
   '((string fg-main)
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     (fringe unspecified)
     (keybind cyan-faint)
     (accent-0 cyan-faint)
     (accent-1 yellow-faint)
     (accent-2 blue-faint)
     (accent-3 red-faint)
     ;; TODO: diffs
     ;; (bg-term-black black)
     ;; (fg-term-black black)
     ;; (bg-term-black-bright black)
     ;; (fg-term-black-bright black)
     ;; (bg-term-red red-faint)
     ;; (fg-term-red red-faint)
     ;; (bg-term-red-bright red-faint)
     ;; (fg-term-red-bright red-faint)
     ;; (bg-term-green green-faint)
     ;; (fg-term-green green-faint)
     ;; (bg-term-green-bright green-faint)
     ;; (fg-term-green-bright green-faint)
     ;; (bg-term-yellow yellow-faint)
     ;; (fg-term-yellow yellow-faint)
     ;; (bg-term-yellow-bright yellow-faint)
     ;; (fg-term-yellow-bright yellow-faint)
     ;; (bg-term-blue blue-faint)
     ;; (fg-term-blue blue-faint)
     ;; (bg-term-blue-bright blue-faint)
     ;; (fg-term-blue-bright blue-faint)
     ;; (bg-term-magenta magenta-faint)
     ;; (fg-term-magenta magenta-faint)
     ;; (bg-term-magenta-bright magenta-faint)
     ;; (fg-term-magenta-bright magenta-faint)
     ;; (bg-term-cyan cyan-faint)
     ;; (fg-term-cyan cyan-faint)
     ;; (bg-term-cyan-bright cyan-faint)
     ;; (fg-term-cyan-bright cyan-faint)
     ;; (bg-term-white white)
     ;; (fg-term-white white)
     ;; (bg-term-white-bright white)
     ;; (fg-term-white-bright white)
     (fg-heading-0 fg-main)
     (fg-heading-1 fg-main)
     (fg-heading-2 fg-main)
     (fg-heading-3 fg-main)
     (fg-heading-4 fg-main)
     (fg-heading-5 fg-main)
     (fg-heading-6 fg-main)
     (fg-heading-7 fg-main)
     (fg-heading-8 fg-main)
     (fg-prompt cyan-faint)
     (rx-construct cyan-faint)
     (rx-backslash fg-main)
     (bg-mode-line-active bg-dim)
     (bg-mode-line-inactive bg)
     (date-common fg-main)
     (date-deadline fg-main)
     (date-event fg-main)
     (date-holiday fg-main)
     (date-now fg-main)
     (date-scheduled fg-main)
     (date-weekday fg-main)
     (date-weekend fg-main)
     (docstring fg-main)
     (docmarkup fg-main)
     (comment fg-main)
     (property fg-main)
     (preprocessor fg-main)
     (constant fg-main)
     (variable fg-main)
     (type fg-main)
     (fnname fg-main)
     (keyword fg-main)
     (builtin fg-main)))
  (modus-themes-mixed-fonts t))

(setq light-theme 'modus-operandi)
(setq dark-theme 'modus-vivendi)
(defun load-dark-theme ()
  "Load the saved dark theme."
  (interactive)
  (setq use-dark-theme-p t)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme dark-theme t)
  (custom-set-faces
   '(region
     ((t :extend nil)))
   `(font-lock-comment-face
     ((t :background ,(modus-themes-get-color-value 'bg-green-subtle))))
   `(font-lock-doc-face
     ((t :background ,(modus-themes-get-color-value 'bg-green-subtle))))))
(defun load-light-theme ()
  "Load the saved light theme."
  (interactive)
  (setq use-dark-theme-p nil)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme light-theme t)
  ;; TODO: maybe dim out the punctuation?
  (custom-set-faces
   ;; '(font-lock-keyword-face
   ;;   ((t :weight semibold)))
   '(region
     ((t :extend nil)))
   `(font-lock-comment-face
     ((t :background ,(modus-themes-get-color-value 'bg-green-nuanced))))
   `(font-lock-doc-face
     ((t :background ,(modus-themes-get-color-value 'bg-green-nuanced))))))
(defvar use-dark-theme-p nil)

(if use-dark-theme-p
    (load-dark-theme)
  (load-light-theme))

(use-package emacs
  :straight (:type built-in)
  :custom
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil)
  (frame-inhibit-implied-resize t)
  (auto-mode-case-fold nil)
  (read-process-output-max (* 1024 1024))
  (window-resize-pixelwise nil)
  (cursor-in-nonselected-windows nil)
  (fast-but-imprecise-scrolling t)
  (tab-always-indent t)
  (inhibit-compacting-font-caches t)
  (ad-redefinition-action 'accept)
  (ffap-machine-p-known 'reject)
  (idle-update-delay 1.0)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-display-reordering 'left-to-right)
  (bidi-inhibit-bpa t)
  (x-gtk-use-system-tooltips nil)
  (inhibit-splash-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-screen t)
  (inhibit-startup-echo-area-message user-login-name)
  (inhibit-startup-buffer-menu t)
  (inhibit-x-resources t)
  (vc-follow-symlinks 120)
  (vc-make-backup-files t "make backups for version-controlled files as well")
  (create-lockfiles nil)
  (use-file-dialog nil)
  (use-dialog-box nil)
  (use-short-answers t)
  (history-length 1000)
  (history-delete-duplicates t)
  (enable-recursive-minibuffers t)
  (sentence-end-double-space nil)
  ;; Temporarily disable GC during startup
  (gc-cons-threshold most-positive-fixnum "2^61 bytes")
  (gc-cons-percentage 1.0)
  (split-width-threshold 170)
  (split-height-threshold nil)
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'mode)
  (x-stretch-cursor nil)
  :preface
  (defun allow-garbage ()
    (setq ;; this does nothing with MPC-based GC
          gc-cons-threshold (* 32 1024 1024)
          ;; this does nothing with MPC-based GC
          gc-cons-percentage 0.1))
  :hook
  ;; Enable the GC back
  (after-init-hook . allow-garbage)
  :config
  (setq-default tab-width 4))

(setq truncate-string-ellipsis "…")

(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package delsel
  :straight (:type built-in)
  :hook
  (after-init-hook . delete-selection-mode))

(use-package ansi-color
  :straight (:type built-in))

(use-package compile
  :straight (:type built-in)
  :custom
  (compilation-scroll-output 'first-error)
  :hook
  (compilation-mode-hook . visual-line-mode)
  (compilation-filter-hook . ansi-color-compilation-filter)
  :bind
  ("C-c r" . recompile))

(use-package auth-source
  :straight (:type built-in)
  :defer 0.2)

(use-package repeat
  :straight (:type built-in)
  :hook
  (after-init-hook . repeat-mode))

(use-package which-key
  :straight (:type built-in)
  :hook
  (after-init-hook . which-key-mode))

(use-package misc
  :straight (:type built-in)
  :bind
  ;; ("C-M-y" . duplicate-line-next-line)
  ("C-M-y" . duplicate-dwim)
  ("C-o" . open-line-forward)
  ("C-S-o" . open-line-backward)
  ("C-M-o" . split-line-tab)
  ;; ("M-t" . jump-to-char-forward)
  ;; ("C-M-t" . jump-to-char-backward)
  :preface
  ;; (defun jump-to-char-forward (char)
  ;;   (interactive "cFind char: ")
  ;;   (search-forward (char-to-string char) (line-end-position) t))
  ;; (defun jump-to-char-backward (char)
  ;;   (interactive "cFind char: ")
  ;;   (search-backward (char-to-string char) (line-beginning-position) t))
  ;;   (search-backward (char-to-string char) (line-beginning-position) t))
  (defun split-line-tab ()
    (interactive)
    (newline)
    (indent-according-to-mode)
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode))
  (defun duplicate-line-next-line ()
    (interactive)
    (duplicate-line)
    (next-line))
  (defun open-line-forward ()
    (interactive)
    (end-of-line)    
    (newline)
    (indent-according-to-mode))
  (defun open-line-backward ()
    (interactive)
    (previous-line)
    (open-line-forward)))

(use-package paren
  :straight (:type built-in)
  :init
  (show-paren-mode -1)
  :custom
  ;; (show-paren-context-when-offscreen 'child-frame)
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t))

(use-package loaddefs
  :straight (:type built-in)
  :custom
  (disabled-command-function nil))

(use-package display-fill-column-indicator
  :straight (:type built-in)
  :config
  (setq-default fill-column 120))

(use-package frame
  :straight (:type built-in)
  :custom
  (frame-resize-pixelwise t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)
  (cursor-type 'box)
  (blink-cursor-delay 1.5)
  :config
  (unbind-key (kbd "C-x C-z") 'global-map)
  (unbind-key (kbd "C-z") 'global-map)
  ;; (window-divider-mode)
  (blink-cursor-mode 0))

(setq scroll-margin 0
      scroll-conservatively 101)
(use-package pixel-scroll
  :disabled
  :straight (:type built-in)
  :custom
  (hscroll-margin 2)
  (hscroll-step 1)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-interpolation-between-scroll 0.001)
  (pixel-scroll-precision-interpolation-total-time 0.15)
  (pixel-scroll-precision-interpolation-factor 1.5)
  (pixel-scroll-precision-interpolate-page t)
  (auto-window-vscroll nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 1)
  ;; :hook
  ;; (after-init-hook . pixel-scroll-precision-mode)
  )

(use-package ultra-scroll
  ;; :disabled
  :straight (:host github :repo "jdsmith/ultra-scroll")
  :custom
  (pixel-scroll-precision-interpolation-total-time 0.15)
  (pixel-scroll-precision-interpolate-page t)
  (scroll-conservatively 101)
  (scroll-margin 0)
  :config
  (ultra-scroll-mode))

(use-package cus-edit
  :straight (:type built-in)
  :defer 3
  :custom
  (custom-file (concat user-emacs-directory "garbage.el"))
  :config
  (when (file-exists-p custom-file)
    (load custom-file nil 'nomessage)))

(use-package autorevert
  :straight (:type built-in)
  :custom
  (auto-revert-interval 2)
  :config
  (global-auto-revert-mode t))

(use-package eww
  :straight (:type built-in)
  :defer t
  :custom
  (eww-default-download-directory "~/downloads/"))

(use-package window
  :straight (:type built-in)
  :custom
  (fit-window-to-buffer-horizontally t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  ;; (display-buffer-alist
  ;;  '(("\\*\\(compilation\\|Async\\)\\*"
  ;;     (display-buffer-in-side-window)
  ;;     (window-width . 0.4)
  ;;     (side . right)
  ;;     (slot . 1))
  ;;    ("\\magit:"
  ;;     (display-buffer-same-window))
  ;;    ("\\*Flycheck error messages"
  ;;     (display-buffer-in-side-window)
  ;;     (window-width . 0.4)
  ;;     (side . bottom)
  ;;     (slot . -1))
  ;;    ("\\*Flycheck errors\\*"
  ;;     (display-buffer-in-side-window)
  ;;     (window-width . 0.4)
  ;;     (side . right)
  ;;     (slot . -1))
  ;;    ("\\*eldoc"
  ;;     (display-buffer-in-side-window)
  ;;     (window-width . 0.4)
  ;;     (side . right)
  ;;     (slot . -1))
  ;;    ("\\*lsp-help\\*"
  ;;     (display-buffer-in-side-window)
  ;;     (window-width . 0.4)
  ;;     (side . right)
  ;;     (slot . -1))
  ;;    ;; ("\\*Occur\\*"
  ;;    ;;  (display-buffer-in-side-window)
  ;;    ;;  (window-width . 0.35)
  ;;    ;;  (side . left)
  ;;    ;;  (slot . -1))
  ;;    ))
  :preface
  (defun split-window-right+switch ()
    (interactive)
    (split-window-right)
    (other-window 1))
  (defun split-window-below+switch ()
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer nil))
  :bind
  (("C-x C-b" . switch-to-last-buffer)
   ("C-x 2" . split-window-below+switch)
   ("C-x 3" . split-window-right+switch)
   :map ctl-x-map
   ([remap split-window-below] . split-window-below+switch)
   ([remap split-window-right] . split-window-right+switch)))

(use-package spacious-padding
  :disabled
  :custom
  (spacious-padding-subtle-frame-lines t)
  :config
  (spacious-padding-mode))

;; (setq backward-delete-char-untabify-method 'hungry)
(use-package hungry-delete
  ;; :disabled
  :hook (prog-mode-hook . hungry-delete-mode))

(use-package winner
  :straight (:type built-in)
  :hook
  (after-init-hook . winner-mode))

(use-package mode-local
  :straight (:type built-in))

(use-package simple
  :straight (:type built-in)
  :bind
  (:map ctl-x-map
        ("k" . kill-current-buffer))
  :custom
  (blink-matching-paren nil)
  :config
  (setq-default indent-tabs-mode nil))

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
  (make-backup-files t)
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
  (auto-save-default nil)
  (auto-save-timeout 20 "number of seconds idle time before auto-save")
  (auto-save-interval 200 "number of keystrokes between auto-saves")
  :config
  (unless (file-exists-p (concat user-emacs-directory "backups"))
    (make-directory (concat user-emacs-directory "backups") t)))

(use-package hideshow
  ;; :disabled
  :straight (:type built-in)
  :preface
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;; TODO: Fix this case. `hs-show-block' needs to be
             ;; called twice to open all folds of the parent
             ;; block.
             (save-excursion (hs-show-block))
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))
  :bind
  ("C-M-<tab>" . hs-cycle)
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package treesit-fold
  :disabled
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (global-treesit-fold-mode)
  :preface
  (defun fold-toggle ()
    (interactive)
    (if (treesit-parser-list)
        (treesit-fold-toggle)
      (call-interactively 'hs-cycle)))
  :bind
  ("C-M-<tab>" . fold-toggle))

(use-package visual-regexp
  :bind
  ([remap query-replace] . vr/replace))

(use-package org
  :defer t
  :hook
  (org-mode-hook . variable-pitch-mode)
  (org-mode-hook . visual-line-mode)
  :custom
  (org-confirm-babel-evaluate nil)
  (org-directory "~/documents/org/")
  (org-default-notes-file (concat org-directory "todo.org"))
  (org-hide-leading-stars t)
  (org-startup-indented t))

(use-package dired
  :straight (:type built-in)
  :demand
  :hook
  (dired-mode-hook . auto-revert-mode)
  :bind
  (:map dired-mode-map
        ("e" . wdired-change-to-wdired-mode))
  :custom
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)
  (dired-listing-switches "-alhg")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top))

(use-package dired-hacks
  :demand
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-lines)
   ("C-<" . mc/mark-previous-lines)
   ;; TODO: include word boundaries please
   ("C-M->" . mc/mark-next-like-this-symbol)
   ("C-M-<" . mc/mark-previous-like-this-symbol)
   :map mc/keymap
   ("<return>" .  nil))
  ;; :preface
  ;; (defun toggle-completion-preview-mode ()
  ;;   (interactive)
  ;;   (if completion-preview-mode
  ;;       (completion-preview-mode -1)
  ;;     (completion-preview-mode t)))
  ;; :hook
  ;; (multiple-cursors-mode-hook . toggle-completion-preview-mode)
  )

(use-package macrursors
  :disabled
  :straight
  (:host github :repo "corytertel/macrursors")
  :hook
  (macrursors-mode-hook . deactivate-mark)
  ((macrursors-pre-finish-hook macrursors-post-finish-hook)
   . corfu-mode)
  :bind
  (("C->" . macrursors-mark-next-line)
   ("C-<" . macrursors-mark-previous-line)
   ("C-M->" . macrursors-mark-next-instance-of)
   ("C-M-<" . macrursors-mark-previous-instance-of)
   :map macrursors-mode-map
   ("C-," . macrursors-end)))

(use-package selection-highlight-mode
  :straight
  (:type git :host github :repo "balloneij/selection-highlight-mode")
  :config (selection-highlight-mode))

(use-package smart-comment
  :bind
  ("M-;" . smart-comment))

(use-package isearch
  :straight
  (:type built-in)
  :custom
  (lazy-highlight-initial-delay 0)
  (isearch-lazy-count t)
  (search-ring-max 100)
  (regexp-search-ring-max 100)
  :config
  (defvar search-recenter-context-lines 6)
  (defvar-local save-scroll-margin nil)
  (add-hook 'isearch-mode-hook
            (lambda ()
              (when (local-variable-if-set-p 'scroll-margin)
                (setq save-scroll-margin scroll-margin))
              (setq-local scroll-margin search-recenter-context-lines)))
  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (if save-scroll-margin
                  (prog1
                      (setq-local scroll-margin save-scroll-margin)
                    (kill-local-variable 'save-scroll-margin))
                (kill-local-variable 'scroll-margin)))))

(use-package isearch-mb
  :disabled
  :hook
  ;; TODO: that makes recentering harder
  (after-init-hook . isearch-mb-mode))

(use-package flash
  :straight
  (:host github :repo "Prgebish/flash")
  :demand
  :custom
  (flash-labels "scnitehafbpydoluv#w*g'm-")
  (flash-label-uppercase t)
  (flash-backdrop nil)
  :bind
  ("C-t" . flash-jump)
  :config
  (require 'flash-isearch)
  (flash-isearch-mode))

(use-package vundo)
(use-package undo-fu
  :bind
  ([remap undo] . undo-fu-only-undo)
  ([remap undo-redo] . undo-fu-only-redo))

(use-package undo-fu-session
  ;; :disabled
  :hook
  (after-init-hook . global-undo-fu-session-mode))

(use-package ws-butler
  ;; :disabled
  :hook
  (prog-mode-hook . ws-butler-mode))

(use-package elec-pair
  :straight (:type built-in)
  :disabled
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-pair-preserve-balance nil)
  :hook (prog-mode-hook . electric-pair-mode))

(use-package wrap-region
  :hook
  (after-init-hook . wrap-region-global-mode))

(use-package puni
  :disabled
  :bind
  ("M-r"   . puni-raise)
  ;; FIXME: conflicts with isearch
  ;; ("C-M-s" . puni-splice)
  ("C-("   . puni-slurp-backward)
  ("C-)"   . puni-slurp-forward)
  ("C-{"   . puni-barf-backward)
  ("C-}"   . puni-barf-forward))

(use-package xterm-color)

(use-package esh-mode
  :disabled
  :defer t
  :straight (:type built-in)
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
  (defun eshell/clear-buffer ()
    "Clear terminal."
    (interactive)
    (when (equal major-mode 'eshell-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input))))
  :bind
  (;; ("C-c o e" . eshell)
   :map eshell-mode-map
   ("C-c C-l" . eshell/clear-buffer)
   ;; :map eshell-hist-mode-map
   ;; ("<up>" . previous-line)
   ;; ("<down>" . next-line)
   )
  :config
  (require 'em-hist)
  (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setenv "TERM" "xterm-256color"))

(setq shell-file-name "zsh")
(use-package vterm
  :config
  ;; (advice-add
  ;;  'set-window-vscroll :after
  ;;  (defun me/vterm-toggle-scroll (&rest _)
  ;;    (when (eq major-mode 'vterm-mode)
  ;;      (if (> (window-end) (buffer-size))
  ;;          (when vterm-copy-mode (vterm-copy-mode-done nil))
  ;;        (vterm-copy-mode 1)))))
  (push '("find-file-other-window" find-file-other-window)
        vterm-eval-cmds)
  :custom
  (vterm-timer-delay 0.01)
  (vterm-max-scrollback 10000)
  (vterm-environment '("PAGER"))
  :bind
  ("C-c o v" . vterm))

(use-package magit
  :bind
  ("C-c o m" . magit-status)
  :config
  (add-to-list
   'display-buffer-alist
   '(("\\magit:"
      (display-buffer-same-window))))
  (add-to-list 'magit-git-environment "OVERCOMMIT_COLOR=0"))

(column-number-mode)
(size-indication-mode)
(setq-default
 mode-line-format
 '("%e" " "
   (:propertize
    (""  mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-window-dedicated)
    display (min-width (6.0)))
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   mode-line-position
   (project-mode-line project-mode-line-format)
   "  "))
;; (setq-default mode-line-format nil)

(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e"))

(use-package vertico
  :custom
  (vertico-resize nil)
  :hook
  (after-init-hook . vertico-mode)
  (after-init-hook . vertico-multiform-mode)
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :config
  ;; (setq
  ;;  vertico-multiform-commands
  ;;  '((consult-ripgrep
  ;;     buffer
  ;;     (vertico-buffer-display-action . (display-buffer-same-window)))))
  )

(use-package orderless
  ;; :config
  ;; (defun orderless-fast-dispatch (word index total)
  ;;   (and (= index 0) (= total 1) (length< word 4)
  ;;        (cons 'orderless-literal-prefix word)))
  ;; (orderless-define-completion-style orderless-fast
  ;;   (orderless-style-dispatchers '(orderless-fast-dispatch))
  ;;   (orderless-matching-styles '(orderless-literal orderless-regexp)))
  :custom
  (completion-styles '(orderless basic))
  ;; (completion-styles '(orderless-fast basic))
  ;; (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package goto-chg
  :bind
  ("C-," . goto-last-change)
  ("C-." . goto-last-change-reverse))

(use-package javelin
  :straight
  (:host github :repo "DamianB-BitFlipper/javelin.el")
  :config
  (global-javelin-minor-mode t))

(use-package consult
  :custom
  (consult-locate-args "plocate --ignore-case --existing --regexp")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :bind
  (;; ([remap switch-to-buffer] . consult-buffer)
   ("M-s l"   . consult-line)
   ("M-s M-l" . consult-line-multi)
   ("M-s r"   . consult-ripgrep)
   ("M-s M-o" . consult-multi-occur)
   ("M-s d"   . consult-find)
   ("M-s M-d" . consult-locate)
   ("M-s e"   . consult-isearch-history)
   ("M-g e"           . consult-compile-error)
   ("M-g m"           . consult-mark)
   ("M-g k"           . consult-global-mark)
   ("M-g f"           . consult-flycheck)
   ("M-g o"           . consult-outline)
   ("M-g r"           . consult-recent-file)
   ([remap imenu]     . consult-imenu)
   ("M-g M-i"         . consult-imenu-multi)
   ([remap goto-line] . consult-goto-line)
   ;; ([remap bookmark-jump] . consult-bookmark)
   ([remap yank-pop] . consult-yank-pop))
  :hook
  (after-init-hook . recentf-mode))

(use-package vertico-posframe
  :disabled)
(use-package nova
  :disabled
  :straight (:host github :repo "thisisran/nova"))

(use-package project
  :straight (:type built-in)
  :custom
  (project-vc-extra-root-markers
   '("Cargo.toml" ".jj"))
  :bind
  (([remap project-compile] . arjaz-project-compile)
   :map project-prefix-map
   ("t" . project-vterm))
  :preface
  (defun arjaz-project-compile ()
    "For some bizarre reason project-compile doesn't update the compile-command"
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (call-interactively #'compile)))
  (defun project-vterm ()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (call-interactively #'vterm))))

(use-package vc-jj
  :straight
  (:host codeberg :repo "emacs-jj-vc/vc-jj.el")
  :init
  (add-to-list 'auto-mode-alist '("\\.jjdescription\\'". diff-mode)))

(use-package jj-mode
  :straight (:host github :repo "bolivier/jj-mode.el")
  :bind
  ("C-c o j" . jj-log)
  :config
  (custom-set-faces
   `(jj-working-copy-heading
     ((t :background ,(modus-themes-get-color-value 'bg-cyan-nuanced) :inherit nil)))
   `(jj-trunk-heading
     ((t :background ,(modus-themes-get-color-value 'bg-green-nuanced) :inherit nil)))))

(use-package bookmark
  :straight (:type built-in)
  :custom
  (bookmark-fringe-mark nil))

(use-package consult-project-extra
  :disabled
  ;; :bind
  ;; ([remap project-find-file] . consult-project-extra-find)
  ;; :config
  ;; (consult-customize
  ;;  consult-project-extra-find
  ;;  :preview-key "C-'")
  )

(use-package consult-flycheck
  ;; :disabled
  :defer t
  :straight
  (:host github :repo "minad/consult-flycheck"))

(use-package marginalia
  :disabled
  :hook
  (after-init-hook . marginalia-mode)
  :custom
  (marginalia-align 'right))

(use-package embark
  :after vertico
  :custom
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  :hook
  (embark-collect-mode-hook . visual-line-mode)
  :config
  (custom-set-faces
   '(embark-target
     ((t :inherit region)))
   )
  :bind
  (("M-o" . embark-act)
   :map vertico-map
   ("M-s o" . embark-export)
   ("M-s l" . embark-live)
   ;; :map embark-general-map
   ;; ([remap describe-symbol] . helpful-symbol)
   ))

(use-package embark-consult
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package keychain-environment
  :defer 0.3
  :config
  (keychain-refresh-environment))

(use-package c-ts-mode
  ;; :disabled
  :straight (:type built-in)
  :custom
  (c-ts-mode-emacs-sources-support nil)
  (c-ts-mode-indent-style 'k&r)
  (c-basic-offset 4)
  (c-ts-mode-indent-offset 4))

(use-package completion-preview
  :straight (:type built-in)
  ;; :disabled
  :hook
  (prog-mode-hook . completion-preview-mode)
  :custom
  (completion-preview-minimum-symbol-length 3)
  :bind
  (:map completion-preview-active-mode-map
        ("TAB" . nil)
        ("M-i" . nil)
        ("C-'" . completion-preview-insert))
  :config
  ;; TODO: custom-set-faces
  ;; (set-face-attribute 'completion-preview-common nil
  ;;                     :underline nil)
  ;; (set-face-attribute 'completion-preview-exact nil
  ;;                     :underline 'unspecified)
  (custom-set-faces
   '(completion-preview
     ((t :inherit shadow)))
   '(completion-preview-common
     ((t :inherit completion-preview)))
   '(completion-preview-exact
     ((t :inherit completion-preview)))
   '(completion-preview-highlight
     ((t :inherit completion-preview))))
  ;; (setq completion-preview-active-mode-map
  ;;       (let ((m (make-sparse-keymap)))
  ;;         (bind-keys
  ;;          :map m
  ;;          ("C-'" . completion-preview-insert))
  ;;         m))
  )

(use-package corfu
  :straight
  (:host github
         :repo "minad/corfu"
         :files ("*" "extensions/*" (:exclude ".git")))
  :config
  (setq corfu-map
        (let ((m (make-sparse-keymap)))
          (bind-keys
           :map m
           ("C-g" . corfu-quit)
           ("C-'" . corfu-insert)
           ("C-*" . corfu-next)
           ("C--" . corfu-previous))
          m))
  :custom
  (corfu-auto nil)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match 'insert)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-bar-width 0.01)
  :hook
  (after-init-hook . global-corfu-mode)
  :config
  ;; Why do I have to do this?
  (setq company-minimum-prefix-length corfu-auto-prefix)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode))

(use-package tempel
  :bind
  ("M-'" . tempel-complete)
  :config
  (setq tempel-map
        (let ((m (make-sparse-keymap)))
          (bind-keys
           :map m
           ("C-g" . tempel-done)
           ([remap right-word] . tempel-next)
           ([remap left-word] . tempel-next))
          m)))

(use-package cape
  :straight
  (:host github
         :repo "minad/cape"
         :files ("*.el" "extensions/*.el"))
  :bind
  ("M-/" . cape-dabbrev))

(defun capf-setup ()
  (interactive)
  (setq-local
   completion-at-point-functions
   (delete-dups
    (remove
     'tags-completion-at-point-function
     (remove
      'ispell-completion-at-point
      (append '(tempel-expand cape-file cape-dabbrev)
              completion-at-point-functions))))))
(add-hook 'prog-mode-hook 'capf-setup)

(use-package dumb-jump
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :straight (:type built-in)
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package haskell-ts-mode
  :mode "\\.hs\\'"
  :custom
  (haskell-ts-use-indent t)
  :init
  (add-to-list
   'treesit-language-source-alist
   '(haskell "https://github.com/tree-sitter/tree-sitter-haskell")))

(use-package flycheck
  :hook
  ;; TODO: customize if possible
  (flycheck-error-list-mode-hook . visual-line-mode)
  :custom
  (flycheck-display-errors-function nil)
  (flycheck-indication-mode nil)
  ;; (flycheck-highlighting-mode 'symbols)
  (flycheck-highlighting-mode nil)
  (flycheck-check-syntax-automatically '(save idle-change mode-enable)))

(use-package flycheck-posframe
  :hook
  (flycheck-mode-hook . flycheck-posframe-mode)
  :disabled
  :preface
  (defun used-window-side ()
    ;; Calculate the left and right distances to the frame edge of the
    ;; active window.  If the left distance is less than or equal to the
    ;; right distance, it indicates that the active window is on the left.
    ;; Otherwise, it is on the right.
    (let* ((window-left (nth 0 (window-absolute-pixel-edges)))
           (window-right (nth 2 (window-absolute-pixel-edges)))
           (frame-left (nth 0 (frame-edges)))
           (frame-right (nth 2 (frame-edges)))
           (distance-left (- window-left frame-left))
           (distance-right (- frame-right window-right)))
      ;; When `distance-left' equals `distance-right', it means there is
      ;; only one window in current frame, or the current active window
      ;; occupies the entire frame horizontally, return left.
      (if (<= distance-left distance-right) 'left 'right)))
  (defun posframe-poshandler-frame-bottom-opposite-corner (info)
    (pcase (used-window-side)
      ('right (posframe-poshandler-frame-bottom-left-corner info))
      ('left (posframe-poshandler-frame-bottom-right-corner info))))
  :config
  (flycheck-posframe-configure-pretty-defaults)
  :custom
  ;; I want it to be shown on a bottom corner the most removed from the pos
  (flycheck-posframe-position 'window-bottom-right-corner)
  (flycheck-posframe-border-width 1))

;; eldoc-help-at-pt
(use-package eldoc
  :bind
  ("C-c h" . eldoc-show)
  :config
  (add-to-list
   'display-buffer-alist
   ;; '("\\*eldoc"
   ;;   (display-buffer-in-direction)
   ;;   (window-width . 0.3)
   ;;   (direction . right))
   '("\\*eldoc"
     (display-buffer-in-side-window)
     (window-width . 0.3)
     (side . right)
     (slot . -1))
   )
  (defun eldoc-visual-line-mode-advice (&rest _)
    (with-current-buffer eldoc--doc-buffer
      (visual-line-mode t)))
  (advice-add 'eldoc-doc-buffer :after #'eldoc-visual-line-mode-advice)
  (defvar eldoc-auto-hide--point nil)
  (defun eldoc-auto-hide ()
    (unless (= (point) eldoc-auto-hide--point)
      (setq eldoc-auto-hide--point nil)
      (remove-hook 'post-command-hook #'eldoc-auto-hide)
      (when-let* ((b (get-buffer-window (eldoc-doc-buffer))))
        (delete-window b))))
  ;; TODO: ideally don't resize other windows
  (defun eldoc-show ()
    (interactive)
    (setq eldoc-auto-hide--point (point))
    (call-interactively #'eldoc-doc-buffer)
    (add-hook 'post-command-hook #'eldoc-auto-hide))
  :custom
  (eldoc-display-functions '(eldoc-display-in-buffer))
  (eldoc-idle-delay 0)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  ;; (eldoc-echo-area-use-multiline-p 5)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  :disabled
  :custom
  ;; (eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer))
  (eldoc-message-function #'ignore) ;; don't show the message in the minibuffer
  (eldoc-box-clear-with-C-g t)
  :bind
  ("C-c h" . eldoc-box-help-at-point)
  :config
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t))

(use-package eldoc-mouse
  :disabled
  :straight
  (:host github :repo "huangfeiyu/eldoc-mouse")
  :config
  (eldoc-mouse-enable))

(use-package eglot
  ;; :disabled
  :demand t
  :straight t
  :hook
  ((js-ts-hook
    typescript-mode-hook
    typescript-ts-mode-hook
    tsx-ts-mode-hook
    zig-mode-hook
    haskell-ts-mode-hook
    elixir-ts-mode-hook
    elixir-mode-hook
    erlang-mode-hook
    rust-mode-hook
    rust-ts-mode-hook
    python-mode-hook
    python-ts-mode-hook
    go-ts-mode-hook
    odin-mode-hook
    c-mode-hook
    c-ts-mode-hook)
   . eglot-ensure)
  (eglot-managed-mode-hook . (lambda () (eglot-inlay-hints-mode -1)))
  (eglot-managed-mode-hook . disable-eglot-completion)
  :preface
  (defun disable-eglot-completion ()
    (setq-local completion-at-point-functions
                (remove #'eglot-completion-at-point completion-at-point-functions)))
  :custom
  (eglot-prefer-plaintext t)
  (eglot-confirm-server-edits '((t . nil)))
  (eglot-code-action-indications '())
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-stay-out-of '(company company-capf yasnippet company-backends))
  :bind
  (("C-c l l" . eglot)
   :map eglot-mode-map
   ;; TODO: enable that once flymake actually works like I want it to
   ;; ("M-g f" . consult-flymake)
   ("C-c l w r" . eglot-reconnect)
   ("C-c l w q" . eglot-shutdown)
   ("C-c l a" . eglot-code-actions)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format)
   ("C-c l t" . eglot-find-typeDefinition)
   ("C-c l h" . eldoc-print-current-symbol-info))
  :config
  (add-to-list 'eglot-server-programs
               '(erlang-mode
                 . ("elp" "server")))
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode)
                 . ("expert")))
  (add-to-list 'eglot-server-programs
               '(haskell-ts-mode
                 . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("odinls")))
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode)
                 "jdtls"))
  (add-to-list 'eglot-server-programs
               `((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
                 .
                 ("typescript-language-server" "--stdio"
                  :initializationOptions
                  (:preferences
                   (:includeInlayParameterNameHints
                    "all"
                    :includeInlayParameterNameHintsWhenArgumentMatchesName nil
                    :includeInlayFunctionParameterTypeHints nil
                    :includeInlayVariableTypeHints nil
                    :includeInlayVariableTypeHintsWhenTypeMatchesName nil
                    :includeInlayPropertyDeclarationTypeHints nil
                    :includeInlayFunctionLikeReturnTypeHints nil
                    :includeInlayEnumMemberValueHints nil)))))
  ;; (add-to-list 'eglot-server-programs
  ;;              '((typescript-ts-mode tsx-ts-mode typescript-mode js-mode js2-mode js3-mode)
  ;;                . ("vtsls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(aiken-mode . ("aiken" "lsp")))
  ;; (fset #'eglot--snippet-expansion-fn #'ignore)
  (eglot--code-action eglot-code-action-extract-function "refactor.extract.function")
  ;; corfu setup
  ;; (push '(eglot (styles orderless)) completion-category-overrides)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(defun flymake-bs-display--shut-up (_type _indicator-type)
  "")
(advice-add 'flymake--bs-display :override #'flymake-bs-display--shut-up)
(use-package flymake
  :disabled
  :straight (:type built-in)
  :custom
  ;; TODO: this still highlights the errors
  (flymake-fringe-indicator-position nil)
  (flymake-indicator-type nil)
  (flymake-error-bitmap nil)
  (flymake-warning-bitmap nil)
  (flymake-note-bitmap nil)
  (flymake-gui-warnings-enabled nil)
  (flymake-margin-indicator-position nil)
  (flymake-autoresize-margins nil)
  (flymake-margin-indicators-string
   '((error "" compilation-error)
     (warning "" compilation-error)
     (note "" compilation-error)))
  :config
  (put :note 'flymake-bitmap nil)
  (push '(face . nil) (get :note 'flymake-overlay-control))
  (put :warning 'flymake-bitmap nil)
  (push '(face . nil) (get :warning 'flymake-overlay-control))
  (put :error 'flymake-bitmap nil)
  (push '(face . nil) (get :error 'flymake-overlay-control))
  (custom-set-faces
   '(flymake-error ((t :inherit default)))
   '(flymake-warning ((t :inherit default)))
   '(flymake-note ((t :inherit default)))
   '(eglot-diagnostic-tag-unnecessary-face ((t :inherit default)))
   ))

;; TODO: maybe actually try flymake, also configure it so it shuts up
(use-package flycheck-eglot
  ;; :disabled
  ;; :after (flycheck eglot)
  :hook
  (eglot-managed-mode-hook . flycheck-eglot-mode))

(use-package yasnippet
  :disabled
  :defer 0.3
  :config
  (yas-global-mode)
  ;; TODO: custom-set-faces
  (set-face-attribute 'yas-field-highlight-face nil
                      :inherit 'bold))

(use-package xref
  :straight (:type built-in)
  :custom
  (xref-after-jump-hook '(recenter))
  (xref-after-return-hook '()))

(setq xref-prompt-for-identifier
      '(not xref-find-references xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame))
(use-package lsp-mode
  :disabled
  :straight (lsp-mode
             :type git
             :flavor melpa
             :files (:defaults "clients/*.el" "lsp-mode-pkg.el")
             :host github
             :repo "emacs-lsp/lsp-mode")
  :preface
  (defun lsp-mode-setup-completion-for-corfu ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :init
  (setq lsp-elixir-ls-version "v0.27.2")
  :hook
  (lsp-completion-mode-hook . lsp-mode-setup-completion-for-corfu)
  ((typescript-mode-hook
    typescript-ts-mode-hook
    js-mode-hook
    js-ts-mode-hook
    tsx-ts-mode-hook
    python-mode-hook
    python-ts-mode-hook
    zig-mode-hook
    haskell-ts-mode-hook
    rust-mode-hook
    rust-ts-mode-hook
    c-mode-hook
    c-ts-mode-hook
    erlang-hook
    elixir-mode-hook
    elixir-ts-mode-hook
    heex-ts-mode-hook
    go-ts-mode-hook
    odin-mode-hook
    aiken-mode-hook
    uiua-mode-hook)
   . lsp-deferred)
  :bind
  ("C-c l l" . lsp)
  :config
  ;; TODO: custom-set-faces
  (set-face-attribute 'lsp-face-highlight-textual nil
                      :inherit 'bold)
  (set-face-attribute 'lsp-face-highlight-read nil
                      :underline nil
                      :inherit 'bold)
  (set-face-attribute 'lsp-face-highlight-write nil
                      :inherit 'bold)
  (setq lsp-eslint-auto-fix-on-save nil)
  (defun lsp--eslint-before-save (orig-fun)
    "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."
    (when (and lsp-eslint-auto-fix-on-save
               (or (derived-mode-p 'typescript-ts-mode)
                   (derived-mode-p 'tsx-ts-mode)))
      (lsp-eslint-fix-all))
    (funcall orig-fun))
  (advice-add 'lsp--before-save :around #'lsp--eslint-before-save)
  (defgroup lsp-vtsls nil
    "LSP wrapper for typescript extension of vscode."
    :group 'lsp-mode
    :link '(url-link "https://github.com/yioneko/vtsls"))
  (defcustom lsp-clients-vtsls-server "vtsls"
    "The vtsls executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with variable `exec-path'."
    :group 'lsp-vtsls
    :risky t
    :type 'file
    :package-version '(lsp-mode . "8.0.0"))
  (defcustom lsp-clients-vtsls-server-args '("--stdio")
    "Extra arguments for starting the vtsls language server."
    :group 'lsp-vtsls
    :risky t
    :type '(repeat string)
    :package-version '(lsp-mode . "8.0.0"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                            (cons lsp-clients-vtsls-server
                                                                  lsp-clients-vtsls-server-args)))
                    :activation-fn #'lsp-typescript-javascript-tsx-jsx-activate-p
                    :priority -1
                    :completion-in-comments? t
                    :server-id 'vtsls))
  (push '(haskell-ts-mode . "haskell") lsp-language-id-configuration)
  (push '(odin-mode . "odin") lsp-language-id-configuration)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "ols")
    			    :major-modes '(odin-mode)
    			    :server-id 'ols
                    ;; This is just so lsp-mode sends the "workspaceFolders" param to the server.
     			    :multi-root t))
  (push '(aiken-mode . "aiken") lsp-language-id-configuration)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("aiken" "lsp"))
    			    :major-modes '(aiken-mode)
    			    :server-id 'aiken))
  (push '(uiua-mode . "uiua") lsp-language-id-configuration)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("uiua" "lsp"))
    			    :major-modes '(uiua-mode)
    			    :server-id 'uiua))
  :custom
  (lsp-completion-default-behaviour :insert)
  (lsp-completion-enable t)
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-symbol-highlighting nil)
  (lsp-symbol-highlighting-skip-current t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-inlay-hint-enable t)
  (lsp-lens-enable nil)
  (lsp-prefer-capf t)
  (lsp-completion-provider :none) ; use corfu instead
  (lsp-idle-delay 0.75)
  (lsp-enable-snippet nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-file-watch-threshold nil)
  (lsp-diagnostics-flycheck-default-level 'warning)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t)
  (lsp-typescript-surveys-enabled nil)
  (lsp-eslint-enable nil))

(use-package consult-lsp
  :disabled
  :bind
  ("M-g s" . consult-lsp-symbols)
  ("M-g d" . consult-lsp-diagnostics))

(use-package dape
  :straight
  (:host github :repo "svaante/dape")
  :commands (dape)
  :hook
  (kill-emacs-hook . dape-breakpoint-save)
  (after-init-hook . dape-breakpoint-load)
  :bind
  ("C-x C-a b" . dape-breakpoint-toggle)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints t)
  :config
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line))

;; TODO: defer
(use-package lsp-haskell
  :disabled
  :after lsp-mode
  :custom
  (lsp-haskell-server-path "haskell-language-server")
  (lsp-haskell-plugin-class-code-lens-on nil)
  (lsp-haskell-formatting-provider "fourmolu")
  (lsp-haskell-plugin-pragmas-completion-on nil)
  (lsp-haskell-plugin-ghcide-completions-config-auto-extend-on nil)
  (lsp-haskell-plugin-ghcide-completions-config-snippets-on nil))

(use-package sly
  :defer t
  :custom
  ;; (sly-complete-symbol-function 'completion-at-point)
  (inferior-lisp-program "sbcl")
  ;; (inferior-lisp-program "sbcl --dynamic-space-size 8Gb")
  :config
  (setq-default sly-symbol-completion-mode nil))

(use-package cider
  :defer t
  :custom
  (cider-repl-display-help-banner nil)
  (cider-enrich-classpath t)
  :bind
  (:map cider-mode-map
        ("C-c M-c" . cider-debug-defun-at-point)))

(use-package rust-mode
  :defer t)

(use-package nasm-mode
  :defer t)

(use-package prisma-ts-mode
  :after treesit
  :config
  (add-to-list
   'treesit-language-source-alist
   '(prisma "https://github.com/victorhqc/tree-sitter-prisma")))

(use-package graphql-ts-mode
  :after treesit
  :config
  (add-to-list
   'treesit-language-source-alist
   '(graphql "https://github.com/bkegley/tree-sitter-graphql"))
  :defer t)

(use-package typst-mode)

;; it's really slow, especially with eglot rendering hovers
(use-package markdown-mode
  :disabled
  )

(use-package yaml-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package nginx-mode
  :defer t)

(use-package rmsbolt)

;; TODO: https://codeberg.org/meow_king/zig-ts-mode
(use-package zig-mode
  :custom
  (zig-format-on-save nil)
  :defer t)

(use-package odin-mode
  :straight (:host github :repo "mattt-b/odin-mode")
  :hook
  (odin-mode-hook . indent-tabs-mode))

(use-package glsl-mode
  :defer t)

(use-package lua-mode
  :defer t)

(use-package elixir-mode
  :mode ("\\.heex\\'" . heex-ts-mode))

(if (file-exists-p "/usr/lib/erlang/lib/tools-4.1.2/emacs")
    (progn
      (add-to-list 'load-path "/usr/lib/erlang/lib/tools-4.1.2/emacs")
      (use-package erlang-start
        :straight (:type built-in)
        :defer t
        :mode ("\\.erl\\'" . erlang-mode)
        :custom
        (erlang-root-dir "/usr/lib/erlang/")
        (exec-path (cons "/usr/lib/erlang/bin" exec-path))
        (erlang-man-root-dir "/usr/lib/erlang/man"))))

(use-package treesit
  :defer t
  :straight (:type built-in)
  :custom
  (treesit-font-lock-level 2)
  (treesit-extra-load-path
   '("~/.tree-sitter/bin/"
     "~/.config/emacs/straight/build/tree-sitter-langs/bin/")))

(use-package treesit-auto
  :straight
  (:host github
   :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install t)
  :hook
  (after-init-hook . global-treesit-auto-mode)
  :config
  (delete 'markdown treesit-auto-langs)
  ;; (delete 'c treesit-auto-langs)
  (delete 'javascript treesit-auto-langs))

(use-package ligature
  :disabled
  :straight (:host github :repo "mickeynp/ligature.el")
  :hook
  (after-init-hook . global-ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '("</>"
     "</" "/>"
     "::" ":::"
     ";;" ";;;"
     "//"
     "<:" ":>"
     "=>"
     ;; "<=" ">="
     "==" "==="
     ;; "!="
     "->" "<-"
     ;; "~>" "<~"
     "<<" ">>"
     "+=" "-=" "/=" "*="
     ":=" ":-" ":+"
     "+:" "-:" "=:"
     "<*" "<*>" "*>"
     "<|" "<|>" "|>")))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install t))

(use-package apheleia
  :hook
  ((clojure-mode-hook
    haskell-ts-mode-hook
    python-mode-hook
    python-ts-mode-hook
    rust-mode-hook
    rust-ts-mode-hook
    typescript-mode-hook
    typescript-ts-mode-hook
    js-mode-hook
    js-ts-mode-hook
    tsx-ts-mode-hook
    erlang-mode-hook
    elixir-mode-hook
    elixir-ts-mode-hook
    go-mode-hook
    go-ts-mode-hook
    zig-mode-hook
    aiken-mode-hook
    tuareg-mode-hook)
   . apheleia-mode)
  :init
  (put 'apheleia-formatter 'safe-local-variable #'symbolp)
  :config
  (setf
   (alist-get 'prisma-ts-mode apheleia-mode-alist)
   'prisma
   (alist-get 'prisma apheleia-formatters)
   '("bunx" "--bun" "prisma" "format")
   ;; (alist-get 'prettier-typescript apheleia-formatters)
   ;; '("apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=typescript")
   (alist-get 'rebar3-format apheleia-formatters)
   '("apheleia-from-project-root" "rebar.config" "rebar3" "format" filepath)
   (alist-get 'erlang-mode apheleia-mode-alist)
   'rebar3-format
   (alist-get 'isort apheleia-formatters)
   '("isort" "--stdout" "-")
   (alist-get 'python-mode apheleia-mode-alist)
   '(isort black)
   (alist-get 'python-ts-mode apheleia-mode-alist)
   '(isort black)
   (alist-get 'aiken apheleia-formatters)
   '("aiken" "fmt" file)
   (alist-get 'aiken-mode apheleia-mode-alist)
   'aiken
   (alist-get 'typescript-ts-mode apheleia-mode-alist)
   'biome))

(add-to-list 'eglot-server-programs
             '((python-mode python-ts-mode)
               "pyrefly" "lsp"
               ;; "ty" "server"
               ))

;; (add-to-list 'eglot-server-programs
;;              '((python-mode python-ts-mode)
;;                "basedpyright-langserver" "--stdio"))
(use-package lsp-pyright
  :disabled
  :defer t
  :after python
  :custom
  (lsp-pyright-langserver-command "basedpyright"))

(use-package nix-mode
  :defer t)

(use-package ocaml-eglot
  :hook
  (tuareg-mode-hook . ocaml-eglot)
  (ocaml-eglot-hook . eglot-ensure))

(use-package tuareg
  ;; :custom
  ;; (exec-path (cons (expand-file-name "~/.opam/default/bin") exec-path))
  :defer t)
;; (add-to-list 'load-path "/home/arjaz/.opam/default/share/emacs/site-lisp")
;; (require 'ocp-indent)

(use-package go-mode
  :defer t
  :custom
  (go-ts-mode-indent-offset 4))

(use-package jinx
  ;; :hook
  ;; (emacs-startup-hook . global-jinx-mode)
  )

(use-package protobuf-mode
  :defer t)

(use-package aiken-mode
  :defer t
  :init
  (add-to-list
   'treesit-language-source-alist
   '(aiken "https://github.com/aiken-lang/tree-sitter-aiken")))

(use-package solidity-mode
  :defer t
  :hook
  (solidity-mode-hook . eglot-ensure)
  :config
  (add-to-list
   'eglot-server-programs
   '(solidity-mode
     . ("nomicfoundation-solidity-language-server" "--stdio"))))

(use-package julia-mode)
(use-package eglot-jl
  :config
  (eglot-jl-init))

(use-package uiua-mode
  :defer t)
  
(use-package bqn-mode
  :defer t
  :config
  ;; TODO: custom-set-faces
  (set-face-attribute 'bqn-default nil
                      :family 'unspecified
                      :inherit 'default)
  :bind
  (:map bqn-mode-map
        ("C-c C-c" . bqn-comint-send-dwim)
        ("C-c C-e" . bqn-comint-eval-dwim)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 120))

(use-package olivetti
  :disabled
  :custom
  (olivetti-body-width 140))

(use-package auto-olivetti
  :disabled
  :straight (:host sourcehut :repo "ashton314/auto-olivetti")
  :custom
  (auto-olivetti-enabled-modes '(text-mode prog-mode))
  :hook
  (after-init-hook . auto-olivetti-mode))

(use-package indent-bars
  :disabled
  :hook
  (prog-mode-hook . indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists t)
  (indent-bars-color '(highlight :blend 0.1))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.01)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth nil)
  (indent-bars-display-on-blank-lines t))

(use-package kkp
  :config
  (global-kkp-mode t))

(use-package xclip
  :config
  (xclip-mode t))

(use-package elastic-indent
  :disabled
  :straight
  (:host github :repo "jyp/elastic-modes")
  :custom
  (elastic-indent-fontify nil)
  :config
  (elastic-indent-mode))

(use-package elastic-table
  :disabled
  :straight
  (:host github :repo "jyp/elastic-modes"))

(use-package buffer-box
  :disabled
  :straight
  (:host github :repo "rougier/buffer-box")
  :hook
  (prog-mode-hook . buffer-box-on))

;; startup.el in `normal-top-level` messes up the PAGER for some dumb reason
;; and I can't figure out how to stop it
;; (setenv "PAGER" nil)

(provide 'init)
;;; init.el ends here
