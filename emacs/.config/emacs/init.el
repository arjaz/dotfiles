;;; init.el --- My configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;; My Emacs configuration

;;; Code:

(use-package esup
  :disabled
  :straight t
  :init
  (setq esup-depth 0))
(use-package benchmark-init
  :disabled
  :straight t
  :hook (after-init-hook . benchmark-init/deactivate)
  :demand t)

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
     ;; (fg-main "#FFBF00")
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
     (bg-hover bg-magenta-intense)))
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
     ;; (fg-search-current fg-main)
     ;; (fg-search-lazy fg-main)
     ;; (fg-search-static fg-main)
     ;; (fg-search-replace fg-main)
     ;; (bg-search-current bg-yellow-intense)
     ;; (bg-search-lazy bg-cyan-intense)
     ;; (bg-search-static bg-green-intense)
     ;; (bg-search-replace bg-red-intense)
     ;; (bg-search-rx-group-0 bg-blue-intense)
     ;; (bg-search-rx-group-1 bg-green-intense)
     ;; (bg-search-rx-group-2 bg-red-subtle)
     ;; (bg-search-rx-group-3 bg-magenta-subtle)
     ;; TODO: diffs
     ;; TODO: maybe refer the colors by name?
     (bg-term-black          "#000000")
     (fg-term-black          "#000000")
     (bg-term-black-bright   "#595959")
     (fg-term-black-bright   "#595959")
     (bg-term-red            "#a60000")
     (fg-term-red            "#a60000")
     (bg-term-red-bright     "#7f0000")
     (fg-term-red-bright     "#7f0000")
     (bg-term-green          "#006800")
     (fg-term-green          "#006800")
     (bg-term-green-bright   "#2a5045")
     (fg-term-green-bright   "#2a5045")
     (bg-term-yellow         "#6f5500")
     (fg-term-yellow         "#6f5500")
     (bg-term-yellow-bright  "#624416")
     (fg-term-yellow-bright  "#624416")
     (bg-term-blue           "#0031a9")
     (fg-term-blue           "#0031a9")
     (bg-term-blue-bright    "#003497")
     (fg-term-blue-bright    "#003497")
     (bg-term-magenta        "#721045")
     (fg-term-magenta        "#721045")
     (bg-term-magenta-bright "#7c318f")
     (fg-term-magenta-bright "#7c318f")
     (bg-term-cyan           "#005e8b")
     (fg-term-cyan           "#005e8b")
     (bg-term-cyan-bright    "#005077")
     (fg-term-cyan-bright    "#005077")
     (bg-term-white          "#ffffff")
     (fg-term-white          "#ffffff")
     (bg-term-white-bright   "#f2f2f2")
     (fg-term-white-bright   "#f2f2f2")
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
     (fg-mode-line-active fg-main)
     (fg-mode-line-inactive fg-main)
     (bg-mode-line-active bg)
     (bg-mode-line-inactive bg)
     (date-common fg-main)
     (date-deadline fg-main)
     (date-event fg-main)
     (date-holiday fg-main)
     (date-now fg-main)
     (date-scheduled fg-main)
     (date-weekday fg-main)
     (date-weekend fg-main)
     (bg-completion bg-dim) ;; TODO: probably also change up the match parts?
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
   ;; `(font-lock-comment-face
   ;;   ((t :background ,(modus-themes-get-color-value 'bg-green-subtle))))
   ;; `(font-lock-doc-face
   ;;   ((t :background ,(modus-themes-get-color-value 'bg-green-subtle))))
   ))

(defun load-light-theme ()
  "Load the saved light theme."
  (interactive)
  (setq use-dark-theme-p nil)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme light-theme t)
  (custom-set-faces
   ;; '(font-lock-keyword-face
   ;;   ((t :weight semibold)))
   '(region
     ((t :extend nil)))
   ;; '(font-lock-string-face
   ;;   ((t :underline t)))
   ;; '(font-lock-comment-face
   ;;   ((t :underline t)))
   ;; '(font-lock-doc-face
   ;;   ((t :underline t)))
   ;; `(font-lock-comment-face
   ;;   ((t :background ,(modus-themes-get-color-value 'bg-green-nuanced))))
   ;; `(font-lock-doc-face
   ;;   ((t :background ,(modus-themes-get-color-value 'bg-green-nuanced))))
   ))

(defvar use-dark-theme-p nil)
(if use-dark-theme-p
    (load-dark-theme)
  (load-light-theme))

(use-package emacs
  :custom
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil)
  (frame-inhibit-implied-resize t)
  (auto-mode-case-fold nil)
  (read-process-output-max (* 1024 1024))
  (window-resize-pixelwise nil)
  (cursor-in-nonselected-windows nil)
  (fast-but-imprecise-scrolling t)
  (redisplay-skip-fontification-on-input t)
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
  (history-delete-duplicates nil)
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

(use-package delsel
  :hook
  (after-init-hook . delete-selection-mode))

(use-package desktop
  :disabled
  :custom
  (desktop-path '("~/.config/emacs/.cache/desktop/"))
  (desktop-save 'if-exists)
  (desktop-load-locked-desktop 'check-pid)
  :config
  (desktop-save-mode 1))

(use-package ansi-color)

(use-package compile
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output 'first-error)
  :hook
  (compilation-mode-hook . visual-line-mode)
  (compilation-filter-hook . ansi-color-compilation-filter)
  :bind
  ("C-c r" . recompile))

(use-package auth-source
  :defer 0.2)

(use-package repeat
  :hook
  (after-init-hook . repeat-mode))

(use-package which-key
  :hook
  (after-init-hook . which-key-mode))

(use-package misc
  :custom
  (duplicate-region-final-position -1)
  (duplicate-line-final-position -1)
  :bind
  ;; ("C-M-y" . duplicate-line-next-line)
  ("C-M-y" . duplicate-dwim)
  ("C-o" . open-line-forward)
  ("C-S-o" . open-line-backward)
  ("C-M-o" . split-line-tab)
  ;; ("M-o" . mark-current-symbol)
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
  ;; TODO: I can remove like half of embark with that one
  (defun mark-current-symbol ()
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (goto-char (car bounds))
        (push-mark (cdr bounds) nil t))))
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
  :init
  (show-paren-mode -1)
  :custom
  ;; (show-paren-context-when-offscreen 'child-frame)
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t))

(use-package loaddefs
  :custom
  (disabled-command-function nil))

(use-package display-fill-column-indicator
  :config
  (setq-default fill-column 120))

(use-package frame
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

(use-package tab-bar
  :custom
  (tab-bar-show 1))

(setq scroll-margin 0
      scroll-conservatively 101)
(use-package pixel-scroll
  ;; :disabled
  :custom
  (hscroll-margin 2)
  (hscroll-step 1)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-interpolation-between-scroll 0.0001)
  (pixel-scroll-precision-interpolation-total-time 0.01)
  (pixel-scroll-precision-interpolation-factor 1.5)
  (pixel-scroll-precision-interpolate-page t)
  (auto-window-vscroll nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 1)
  :hook
  (after-init-hook . pixel-scroll-precision-mode))

(use-package ultra-scroll
  :disabled
  :straight (:host github :repo "jdtsmith/ultra-scroll")
  :custom
  (pixel-scroll-precision-interpolation-total-time 0.15)
  (pixel-scroll-precision-interpolate-page t)
  (scroll-conservatively 101)
  (scroll-margin 0)
  :config
  (ultra-scroll-mode))

(use-package cus-edit
  :defer 3
  :custom
  (custom-file (concat user-emacs-directory "garbage.el"))
  :config
  (when (file-exists-p custom-file)
    (load custom-file nil 'nomessage)))

(use-package autorevert
  :custom
  (auto-revert-interval 2)
  :config
  (global-auto-revert-mode t))

(use-package eww
  :defer t
  :custom
  (eww-default-download-directory "~/downloads/"))

(use-package window
  :custom
  (fit-window-to-buffer-horizontally t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
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

(setq backward-delete-char-untabify-method 'hungry)

(use-package winner
  :hook
  (after-init-hook . winner-mode))

(use-package mode-local)

(use-package simple
  :bind
  (:map ctl-x-map
        ("k" . kill-current-buffer))
  :custom
  (kill-do-not-save-duplicates t)
  (blink-matching-paren nil)
  (set-mark-command-repeat-pop t)
  :config
  (setq-default indent-tabs-mode nil))

(use-package files
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
  :bind
  ("C-M-<tab>" . hs-cycle)
  :hook
  (prog-mode-hook . hs-minor-mode))

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

(use-package proced
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descend t)
  (proced-format 'medium)
  (proced-filter 'user))

(use-package dired
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

(use-package multiple-cursors
  :straight t
  :bind
  (("C->" . mc/mark-next-lines)
   ("C-<" . mc/mark-previous-lines)
   ;; TODO: include word boundaries please
   ("C-M->" . mc/mark-next-like-this-symbol)
   ("C-M-<" . mc/mark-previous-like-this-symbol)
   :map mc/keymap
   ("<return>" .  nil))
  :preface
  (defun toggle-completion-preview-mode ()
    (interactive)
    (if completion-preview-mode
        (completion-preview-mode -1)
      (completion-preview-mode t)))
  :hook
  (multiple-cursors-mode-hook . toggle-completion-preview-mode)
  )

(use-package selection-highlight-mode
  :disabled
  :straight
  (:type git :host github :repo "balloneij/selection-highlight-mode")
  :config (selection-highlight-mode))

;; there is comment-dwim, but it is not dwim
(defun comment-really-dwim ()
  "Comment/uncomment region if selected, otherwise line"
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(bind-key (kbd "M-;") #'comment-really-dwim)

(use-package isearch
  :custom
  (lazy-highlight-initial-delay 0)
  (isearch-lazy-count t)
  (search-ring-max 100)
  (regexp-search-ring-max 100)
  :bind
  (("M-o" . isearch-forward-symbol-at-point)
   :map isearch-mode-map
   ("M-s r" . isearch-consult-ripgrep)
   ("M-w" . isearch-copy-match))
  :preface
  (defun isearch-copy-match ()
    (interactive)
    (kill-new
     (buffer-substring-no-properties
      (min (point) isearch-other-end)
      (max (point) isearch-other-end)))
    (isearch-exit))
  (defun isearch-consult-ripgrep ()
    (interactive)
    (let ((query isearch-string))
      (isearch-exit)
      (consult-ripgrep nil query)))
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

(use-package flash
  :disabled
  :straight
  (:host github :repo "Prgebish/flash")
  :demand
  :custom
  (flash-case-fold t)
  (flash-labels "scnitehafbpydoluw*g'm-")
  (flash-label-uppercase nil)
  (flash-backdrop nil)
  :bind
  ("C-t" . flash-jump)
  ;; :config
  ;; (require 'flash-isearch)
  ;; (flash-isearch-mode)
  )

(use-package undo-fu-session
  ;; :disabled
  :straight t
  :hook
  (after-init-hook . global-undo-fu-session-mode))

(use-package elec-pair
  :disabled
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-pair-preserve-balance nil)
  :hook (prog-mode-hook . electric-pair-mode))

(use-package xterm-color
  :disabled
  :straight t)

(setq shell-file-name "zsh")

(use-package ghostel
  :straight
  (ghostel
   :files (:defaults "*.so" "etc")
   :host github :repo "dakra/ghostel")
  :bind
  ("C-c o t" . ghostel)
  ;; :custom
  ;; (ghostel-shell "nu")
  :hook
  (after-init-hook . ghostel-comint-global-mode)
  (after-init-hook . ghostel-compile-global-mode))

(use-package magit
  :straight t
  ;; :disabled
  :bind
  ("C-c o m" . magit-status)
  :init
  (add-to-list
   'display-buffer-alist
   '(("\\magit:" (display-buffer-same-window)))))

;; (use-package git-link)

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

;; TODO: in-buffer completion uses completion-list-mode and it's somewhat shit with bindings
;;       I want search to work there, it does random stuff
;;       ^ I can fix that by (setq completion-in-region-function #'consult-completion-in-region)
;;         but that sort of feels like a hack <- does not even work, lol
;;       in-buffer completion also seems to only send the initial list to LSP?
;;       I kind of don't like it overloads tab?
;;       maybe I unbind tab, set completion-auto-select to nil and use C-n/C-p only
;;  I don't like sorting for files
;; For in-region completion it inserts too much text or something
;; Can I preselect the first candidate by default?
(use-package minibuffer
  :preface
  (defun minibuffer-truncate-lines ()
    "Keep minibuffer lines unwrapped."
    (setq truncate-lines t))
  (defun hide-minibuffer ()
    (setq-local mode-line-format nil)
    (force-mode-line-update))
  (defun minibuffer-choose-completion-no-exit ()
    (interactive)
    (unless (completion--selected-candidate)
      (minibuffer-next-completion))
    (minibuffer-choose-completion t))
  :bind
  (:map
   minibuffer-visible-completions-up-down-map
   ("C-n" . minibuffer-next-completion)
   ("C-p" . minibuffer-previous-completion)
   :map
   minibuffer-local-completion-map
   ;; ("TAB" . minibuffer-complete)
   ("TAB" . minibuffer-choose-completion-no-exit)
   ("SPC" . self-insert-command)
   ;; That kinda does not work?
   ;; I don't like that RET does shit for capf specifically <- region-completion or something?
   ;; :map completion-in-region-mode-map
   ;; ("TAB" . nil)
   ;; ("C-n" . minibuffer-next-completion)
   ;; ("C-p" . minibuffer-previous-completion)
   ;; ("C-<return>" . minibuffer-choose-completion)
   )
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode)
  (minibuffer-setup-hook . minibuffer-truncate-lines)
  ;; (completion-in-region-mode-hook . completion-in-region-no-auto-select)
  ;; (completion-list-mode-hook . hide-minibuffer)
  :custom
  ;; (completion-no-auto-exit t)
  (completion-auto-help t)
  (completion-auto-select nil)
  ;; (completion-auto-select t)
  (completion-eager-update t)
  (completion-eager-display t)
  (minibuffer-visible-completions 'up-down)
  (minibuffer-completion-auto-choose t)
  (completion-ignore-case t)
  (completion-show-help nil)
  (completion-show-inline-help nil)
  (completions-format 'one-column)
  (completions-max-height 10)
  (completions-sort 'historical)
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  :config
  (minibuffer-regexp-mode t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t))

(use-package fzf-native
  :straight
  (:repo "dangduc/fzf-native"
   :host github
   :files (:defaults "bin"))
  :config
  (fzf-native-load-dyn))

(use-package fussy
  :straight
  (fussy :type git :host github :repo "jojojames/fussy")
  :config
  (fussy-setup-fzf)
  ;; TODO: Do I need some fancy completion-category-overrides for eglot specifically?
  ;;       rahuljuliato introduces some flex-noinsert thingy <- seems to work with the setup commented out
  ;; (fussy-eglot-setup)
  ;; (fussy-corfu-setup)
  ;; (setf (alist-get 'file completion-category-overrides)
  ;;       '(basic))
  )

(use-package goto-chg
  :straight t
  :bind
  ("C-," . goto-last-change)
  ("C-." . goto-last-change-reverse))

(use-package consult
  :straight t
  :custom
  (consult-line-start-from-top t)
  (consult-locate-args "plocate --ignore-case --existing --regexp")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :preface
  (defun consult-flymake-project ()
    (interactive)
    (consult-flymake t))
  :bind
  (;; ([remap switch-to-buffer] . consult-buffer)
   ("M-s l"           . consult-line)
   ("M-s M-l"         . consult-line-multi)
   ("M-s r"           . consult-ripgrep)
   ("M-s d"           . consult-find)
   ("M-s M-d"         . consult-locate)
   ("M-s e"           . consult-isearch-history)
   ("M-g e"           . consult-compile-error)
   ("M-g m"           . consult-mark)
   ("M-g k"           . consult-global-mark)
   ("M-g f"           . consult-flymake-project)
   ("M-g o"           . consult-outline)
   ("M-g r"           . recentf)
   ;; ("M-g r"           . consult-recent-file)
   ([remap imenu]     . consult-imenu)
   ("M-g M-i"         . consult-imenu-multi)
   ([remap goto-line] . consult-goto-line)
   ;; ([remap bookmark-jump] . consult-bookmark)
   ([remap yank-pop] . consult-yank-pop))
  :hook
  (after-init-hook . recentf-mode))

(use-package project
  :custom
  (project-vc-extra-root-markers
   '("Cargo.toml" ".jj"))
  ;; :bind
  ;; ([remap project-compile] . arjaz-project-compile)
  ;; :preface
  ;; (defun arjaz-project-compile ()
  ;;   "For some bizarre reason project-compile doesn't update the compile-command"
  ;;   (interactive)
  ;;   (let ((default-directory (project-root (project-current t))))
  ;;     (call-interactively #'compile)))
  )

(use-package majutsu
  ;; :disabled
  :straight
  (:host github :repo "0WD0/majutsu" :files ("*.el"))
  :bind
  ("C-c o j" . majutsu)
  :config
  (setq majutsu-display-buffer-function
        #'majutsu-display-buffer-same-window-except-diff-v1)
  :custom
  ;; TODO: I probably don't like right-aligned fields
  (majutsu-log-commit-columns
   '((:field change-id :module heading
             :template majutsu-log-template-change-id :face t)
     (:field bookmarks :module heading
             :template majutsu-log-template-bookmarks :face t)
     (:field tags :module heading
             :template majutsu-log-template-tags :face t)
     (:field working-copies :module heading
             :template majutsu-log-template-working-copies :face t)
     (:field empty :module heading
             :template majutsu-log-template-empty :face t)
     (:field git-head :module heading
             :template majutsu-log-template-git-head :face t)
     (:field description :module heading
             :template majutsu-log-template-description :face t)
     (:field commit-id :module tail
             :template majutsu-log-template-commit-id :face t)
     (:field author :module tail
             :template majutsu-log-template-author :face t)
     (:field timestamp :module tail
             :template majutsu-log-template-timestamp :face t)
     (:field long-desc :module body
             :template majutsu-log-template-long-desc :face t)
     (:field id :module metadata
             :template majutsu-log-template-id :face t)
     (:field commit-id :module metadata
             :template majutsu-log--canonical-commit-id-template :face t)
     (:field parent-ids :module metadata
             :template majutsu-log-template-parent-ids :face t)
     (:field flags :module metadata
             :template majutsu-log-template-flags :face t)
     (:field description :module metadata
             :template majutsu-log--canonical-description-template :face t))))

(use-package bookmark
  :custom
  (bookmark-fringe-mark nil))

(use-package embark
  :straight t
  ;; :after vertico
  :custom
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  :hook
  (embark-collect-mode-hook . visual-line-mode)
  ;; :config
  ;; (custom-set-faces
  ;;  '(embark-target
  ;;    ((t :inherit region)))
  ;;  )
  :bind
  (;; ("M-o" . embark-act)
   :map minibuffer-visible-completions-up-down-map
   ("M-o" . embark-act)
   ("M-s" . nil)
   ("M-s o" . embark-export)
   ;; :map vertico-map
   ;; ("M-s o" . embark-export)
   ;; ("M-s l" . embark-live)
   ;; :map icomplete-vertical-mode-minibuffer-map
   ;; ("M-s o" . embark-export)
   ;; ("M-s l" . embark-live)
   ;; :map icomplete-vertical-mode-minibuffer-map
   ;; ("M-s o" . embark-export)
   ;; ("M-s l" . embark-live)
   ;; :map embark-general-map
   ;; ([remap describe-symbol] . helpful-symbol)
   ))

(use-package embark-consult
  :straight t)

(use-package keychain-environment
  :straight t
  :defer 0.3
  :config
  (keychain-refresh-environment))

(use-package c-ts-mode
  ;; :disabled
  :preface
  ;; TODO: put the eglot thing here
  (defun c-ts-mode-setup ()
    (setq-local c-ts-indent-offset 4)
    (setq-local indent-tabs-mode nil)
    (let ((lang (if (derived-mode-p 'c-ts-mode) 'c 'cpp)))
      (setf (alist-get lang treesit-simple-indent-rules)
            (append
             '(((node-is ")") parent-bol 0)
               ((parent-is "argument_list")
                parent-bol c-ts-indent-offset))
             (alist-get lang treesit-simple-indent-rules)))))
  :demand
  :hook
  (c-ts-mode-hook . c-ts-mode-setup)
  (c++-ts-mode-hook . c-ts-mode-setup)
  :custom
  (c-ts-mode-emacs-sources-support nil)
  (c-ts-mode-indent-style 'k&r)
  (c-basic-offset 4)
  (c-ts-mode-indent-offset 4))

(use-package completion-preview
  :disabled
  :hook
  (prog-mode-hook . completion-preview-mode)
  :custom
  (completion-preview-minimum-symbol-length 2)
  :bind
  (:map completion-preview-active-mode-map
        ("TAB" . nil)
        ("M-i" . nil)
        ("C-'" . completion-preview-insert))
  :config
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


(use-package cape
  :straight
  (:host github
         :repo "minad/cape"
         :files ("*.el" "extensions/*.el"))
  :bind
  ("C-<tab>" . cape-dabbrev))

(defun capf-setup ()
  (interactive)
  (setq-local
   completion-at-point-functions
   (delete-dups
    (remove
     'tags-completion-at-point-function
     (remove
      'ispell-completion-at-point
      (append '(cape-file cape-dabbrev)
              completion-at-point-functions))))))
(add-hook 'prog-mode-hook 'capf-setup)

(use-package dumb-jump
  :straight t
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package haskell-ts-mode
  :straight t
  :mode "\\.hs\\'"
  :custom
  (haskell-ts-use-indent t)
  :init
  (add-to-list
   'treesit-language-source-alist
   '(haskell "https://github.com/tree-sitter/tree-sitter-haskell")))

;; eldoc-help-at-pt?
(use-package eldoc
  :defer t
  :bind
  ("C-c h" . eldoc-show)
  :preface
  (defun eldoc-visual-line-mode-advice (&rest _)
    (with-current-buffer eldoc--doc-buffer
      (visual-line-mode t)))
  (defun eldoc-auto-hide ()
    (unless (= (point) eldoc-auto-hide--point)
      (setq eldoc-auto-hide--point nil)
      (remove-hook 'post-command-hook #'eldoc-auto-hide)
      (when-let* ((b (get-buffer-window (eldoc-doc-buffer))))
        (delete-window b))))
  (defun eldoc-show ()
    (interactive)
    (setq eldoc-auto-hide--point (point))
    (call-interactively #'eldoc-doc-buffer)
    (add-hook 'post-command-hook #'eldoc-auto-hide))
  :init
  (defvar eldoc-auto-hide--point nil)
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*eldoc"
     (display-buffer-in-side-window)
     (window-width . 0.3)
     (side . right)
     (slot . -1)))
  (advice-add 'eldoc-doc-buffer :after #'eldoc-visual-line-mode-advice)
  :custom
  (eldoc-display-functions '(eldoc-display-in-buffer))
  (eldoc-idle-delay 0)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-display-truncation-message nil))

;; TODO: eglot-momentary-inlay-hints
(use-package eglot
  ;; :disabled
  ;; :hook
  ;; ((js-ts-hook
  ;;   typescript-mode-hook
  ;;   typescript-ts-mode-hook
  ;;   tsx-ts-mode-hook
  ;;   zig-mode-hook
  ;;   zig-ts-mode-hook
  ;;   ;; haskell-ts-mode-hook
  ;;   elixir-ts-mode-hook
  ;;   elixir-mode-hook
  ;;   erlang-mode-hook
  ;;   rust-mode-hook
  ;;   rust-ts-mode-hook
  ;;   python-mode-hook
  ;;   python-ts-mode-hook
  ;;   go-ts-mode-hook
  ;;   odin-mode-hook
  ;;   c-mode-hook
  ;;   c-ts-mode-hook)
  ;;  . eglot-ensure)
  ;; (eglot-managed-mode-hook
  ;;  .
  ;;  (lambda () (eglot-inlay-hints-mode -1)))
  ;; (eglot-managed-mode-hook . disable-eglot-completion)
  ;; :preface
  ;; (defun disable-eglot-completion ()
  ;;   (setq-local completion-at-point-functions
  ;;               (remove #'eglot-completion-at-point completion-at-point-functions)))
  :custom
  (eglot-report-progress 'messages)
  (eglot-max-file-watches 5000)
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  (eglot-code-action-indications nil)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-documentation-rederer 'markdown-ts-view-mode)
  (eglot-confirm-server-edits '((t . nil)))
  (eglot-code-action-indications '())
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities
   '(:documentHighlightProvider :documentOnTypeFormattingProvider
     :documentFormattingProvider :documentRangeFormattingProvider
     :inlayHintProvider))
  (eglot-stay-out-of '(company company-capf yasnippet company-backends))
  :bind
  (("C-c l l" . eglot)
   :map eglot-mode-map
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
  (add-to-list 'eglot-server-programs
               '(zig-ts-mode . ("zls")))
  (eglot--code-action eglot-code-action-extract-function "refactor.extract.function")
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package flymake
  :config
  (setq
   flymake-fringe-indicator-position nil
   flymake-margin-indicator-position nil
   flymake-indicator-type nil)
  (custom-set-faces
   '(flymake-error ((t :underline nil)))
   '(flymake-warning ((t :underline nil)))
   '(flymake-note ((t :underline nil)))
   '(eglot-highlight-symbol-face ((t :underline nil)))
   '(eglot-diagnostic-tag-unnecessary-face ((t :underline nil))))
  (push '(face . nil) (get :note 'flymake-overlay-control))
  (push '(face . nil) (get :warning 'flymake-overlay-control))
  (push '(face . nil) (get :error 'flymake-overlay-control))
  ;; Why do I need that to suppress the "!" overlay?
  (defun flymake-no-before-string (ov &rest _)
    (overlay-put ov 'before-string nil))
  (advice-add 'flymake--highlight-line :filter-return #'flymake-no-before-string))

(use-package xref
  :custom
  (xref-after-jump-hook '(recenter))
  (xref-after-return-hook '()))

(setq xref-prompt-for-identifier
      '(not xref-find-references
            xref-find-definitions
            xref-find-definitions-other-window
            xref-find-definitions-other-frame))

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

(use-package sly
  :straight t
  :defer t
  :custom
  ;; (sly-complete-symbol-function 'completion-at-point)
  ;; (inferior-lisp-program "sbcl")
  (inferior-lisp-program "sbcl --dynamic-space-size 8Gb")
  (sly-default-lisp 'sbcl)
  ;; (sly-lisp-implementations '((sbcl  ("vend" "repl" "sbcl")  :coding-system utf-8-unix)
  ;;                             (ecl   ("vend" "repl" "ecl")   :coding-system utf-8-unix)
  ;;                             (abcl  ("vend" "repl" "abcl")  :coding-system utf-8-unix)
  ;;                             (clasp ("vend" "repl" "clasp") :coding-system utf-8-unix)))
  :config
  (setq-default sly-symbol-completion-mode nil))

(use-package cider
  :straight t
  :disabled
  :defer t
  :custom
  (cider-repl-display-help-banner nil)
  (cider-enrich-classpath t)
  :bind
  (:map cider-mode-map
        ("C-c M-c" . cider-debug-defun-at-point)))

(use-package nasm-mode
  :straight t
  :disabled
  :defer t)

(use-package prisma-ts-mode
  :straight t
  :disabled
  :after treesit
  :defer t
  :config
  (add-to-list
   'treesit-language-source-alist
   '(prisma "https://github.com/victorhqc/tree-sitter-prisma")))

(use-package graphql-ts-mode
  :straight t
  :disabled
  :after treesit
  :config
  (add-to-list
   'treesit-language-source-alist
   '(graphql "https://github.com/bkegley/tree-sitter-graphql"))
  :defer t)

(use-package yaml-mode
  :straight t
  :defer t)

(use-package dockerfile-mode
  :straight t
  :defer t)

(use-package nginx-mode
  :straight t
  :defer t)

(use-package glsl-mode
  :straight t)

(use-package zig-mode
  :straight t
  ;; (:host codeberg :repo "meow_king/zig-ts-mode")
  :defer t)

(use-package glsl-mode
  :straight t
  :disabled
  :defer t)

(use-package elixir-mode
  :straight t
  :defer t
  :mode ("\\.heex\\'" . heex-ts-mode))

(let ((p (concat
          "/usr/lib/erlang/lib/"
          (seq-find
           (lambda (file) (string-prefix-p "tools" file))
           (directory-files "/usr/lib/erlang/lib/"))
          "/emacs")))
  (when (file-exists-p p)
    (add-to-list 'load-path p)
    (use-package erlang-start
      :defer t
      :mode ("\\.erl\\'" . erlang-mode)
      :custom
      (erlang-root-dir "/usr/lib/erlang/")
      (exec-path (cons "/usr/lib/erlang/bin" exec-path))
      (erlang-man-root-dir "/usr/lib/erlang/man"))))

(use-package treesit
  :defer t
  :custom
  (treesit-font-lock-level 1)
  (treesit-extra-load-path
   '("~/.tree-sitter/bin/"
     "~/.config/emacs/straight/build/tree-sitter-langs/bin/")))

(use-package markdown-ts-mode
  :defer t)

(use-package ligature
  ;; :disabled
  :straight (:host github :repo "mickeynp/ligature.el")
  :hook
  (after-init-hook . global-ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '(;; "</>"
     ;; "</" "/>"
     "::" ":::"
     ";;" ";;;"
     "///"
     "//"
     ;; "<:" ":>"
     ;; "=>"
     ;; "<=" ">="
     ;; "==" "==="
     ;; "!="
     ;; "->" "<-"
     ;; "~>" "<~"
     ;; "<<" ">>"
     "+=" "-=" "/=" "*="
     ":=" ":-" ":+"
     "+:" "-:" "=:"
     ;; "<*" "<*>" "*>"
     ;; "<|" "<|>" "|>"
     )))

(use-package pdf-tools
  :straight t
  :disabled
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package apheleia
  :straight t
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
    zig-ts-mode-hook
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
   (alist-get 'typescript-ts-mode apheleia-mode-alist)
   'oxfmt
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

;; (add-to-list 'eglot-server-programs
;;              '((python-mode python-ts-mode)
;;                "pyrefly" "lsp"
;;                ;; "ty" "server"
;;                ))

;; (add-to-list 'eglot-server-programs
;;              '((python-mode python-ts-mode)
;;                "basedpyright-langserver" "--stdio"))

(use-package nix-mode
  :straight t
  :defer t)

(use-package ocaml-eglot
  :straight t
  :disabled
  :hook
  (tuareg-mode-hook . ocaml-eglot)
  (ocaml-eglot-hook . eglot-ensure))

(use-package tuareg
  :straight t
  :disabled
  ;; :custom
  ;; (exec-path (cons (expand-file-name "~/.opam/default/bin") exec-path))
  :defer t)
;; (add-to-list 'load-path "/home/arjaz/.opam/default/share/emacs/site-lisp")
;; (require 'ocp-indent)

(use-package jinx
  :straight t
  :disabled
  :defer t
  ;; :hook
  ;; (emacs-startup-hook . global-jinx-mode)
  )

(use-package protobuf-mode
  :straight t
  :defer t)

(use-package kdl-mode
  :straight t
  :defer t)

(use-package aiken-mode
  :straight t
  :disabled
  :defer t
  :init
  (add-to-list
   'treesit-language-source-alist
   '(aiken "https://github.com/aiken-lang/tree-sitter-aiken")))

(use-package solidity-mode
  :straight t
  :disabled
  :defer t
  :hook
  (solidity-mode-hook . eglot-ensure)
  :config
  (add-to-list
   'eglot-server-programs
   '(solidity-mode
     . ("nomicfoundation-solidity-language-server" "--stdio"))))

(use-package forth-mode
  :straight t
  :defer t)

(use-package uiua-mode
  :straight t
  :defer t)

(use-package april-mode
  :defer t
  :mode "\\.apl\\'"
  ;; TODO: push to github or something
  :load-path "/home/arjaz/code/april-mode.el/")

(use-package gnu-apl-mode
  :straight t
  :defer t
  :disabled)
(use-package anaphora
  :straight t
  :defer t)
(use-package jpt-apl-mode
  :straight
  (:host github :repo "jthing/apl-mode")
  :hook
  (gnu-apl-mode-hook . jpt-apl-mode)
  (april-mode-hook . jpt-apl-mode)
  (dyalog-mode-hook . jpt-apl-mode)
  :config
  (when (file-exists-p "/home/arjaz/code/ael/ocicl/april-20250620-37511c0/aprepl/aprepl.el")
    (load "/home/arjaz/code/ael/ocicl/april-20250620-37511c0/aprepl/aprepl.el")))

(use-package bqn-mode
  :straight t
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

(use-package kkp
  :straight t
  :config
  (global-kkp-mode t))

(use-package xclip
  :straight t
  :config
  (xclip-mode t))

(use-package uniline
  :straight t
  :defer t)

(provide 'init)
;;; init.el ends here
