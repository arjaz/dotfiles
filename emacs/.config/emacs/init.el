;;; init.el --- My configuration
;;; -*- lexical-binding: t; -*-

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

(use-package emacs
  :straight (:type built-in)
  :custom
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil)
  (frame-inhibit-implied-resize t)
  (auto-mode-case-fold nil)
  (read-process-output-max (* 1024 1024 10))
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
  (history-length 100)
  (history-delete-duplicates t)
  (enable-recursive-minibuffers t)
  (sentence-end-double-space nil)
  (gc-cons-threshold most-positive-fixnum "2^61 bytes")
  (gc-cons-percentage 0.6)
  (split-width-threshold 170)
  (split-height-threshold nil)
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'mode)
  (x-stretch-cursor nil)
  :preface
  (defun allow-garbage ()
    (setq gc-cons-threshold 536870912 ; 512mb
          gc-cons-percentage 0.1))
  :hook
  (after-init-hook . allow-garbage)
  :config
  (setq-default tab-width 4))

(setq truncate-string-ellipsis "…")

(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face
                  minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package compile
  :straight (:type built-in)
  :custom
  (compilation-scroll-output 'first-error)
  :bind
  ("C-c r" . recompile))

(use-package auth-source
  :defer 0.2
  :straight (:type built-in))

(use-package repeat
  :straight (:type built-in)
  :hook
  (after-init-hook . repeat-mode))

;; (use-package evil
;;   :init
;;   (setq evil-want-keybinding nil)
;;   :custom
;;   (evil-move-beyond-eol t)
;;   (evil-want-C-w-delete nil)
;;   :config
;;   (evil-mode)
;;   (evil-define-key
;;    'normal 'global
;;    (kbd "SPC f f") 'find-file
;;    (kbd "SPC b") 'consult-buffer
;;    (kbd "SPC p f") 'project-find-file
;;    (kbd "SPC o m") 'magit))
;; (use-package evil-collection
;;   :config
;;   (evil-collection-init))
;; (use-package evil-surround
;;   :config
;;   (global-evil-surround-mode))
;; (use-package evil-commentary
;;   :config
;;   (evil-commentary-mode))

(use-package which-key
  :config
  (which-key-mode))

(defun set-safe-composition-table ()
  (interactive)
  (set-char-table-range composition-function-table t `(["[,-.;A-Z_a-z]+" 0 font-shape-gstring])))

(defun unset-safe-composition-table ()
  (interactive)
  (set-char-table-range composition-function-table t `(["" 0 font-shape-gstring])))

(defun toggle-safe-composition-table--around (old-fn &rest args)
  "Disable the composition table around a function invocation. Useful to prevent weird avy artifacts."
  (let ((visible-buffers (mapcar #'window-buffer (window-list))))
    (dolist (b visible-buffers)
      (with-current-buffer b
        (unset-safe-composition-table)))
    (let ((res (apply old-fn args)))
      (dolist (b visible-buffers)
        (with-current-buffer b
          (set-safe-composition-table)))
      res)))

;; (add-hook 'minibufer-setup-hook 'set-safe-composition-table)
;; (add-hook 'special-mode-hook 'set-safe-composition-table)
;; (add-hook 'text-mode-hook 'set-safe-composition-table)
;; (add-hook 'fundamental-mode-hook 'set-safe-composition-table)
;; (add-hook 'prog-mode-hook 'set-safe-composition-table)

;; (advice-add 'avy-jump
;;             :around
;;             #'toggle-safe-composition-table--around)

(use-package misc
  :straight (:type built-in)
  :bind
  ("C-M-y" . duplicate-line-next-line)
  ("C-o" . open-line-forward)
  ("C-S-o" . open-line-backward)
  ("C-M-o" . split-line-tab)
  ("M-t" . jump-to-char-forward)
  ("C-M-t" . jump-to-char-backward)
  :preface
  (defun jump-to-char-forward (char)
    (interactive "cFind char: ")
    (search-forward (char-to-string char) (line-end-position) t))
  (defun jump-to-char-backward (char)
    (interactive "cFind char: ")
    (search-backward (char-to-string char) (line-beginning-position) t))
  (defun split-line-tab ()
    (interactive)
    (default-indent-new-line nil t)
    (default-indent-new-line nil t)
    (previous-line)
    (indent-according-to-mode))
  (defun duplicate-line-next-line ()
    (interactive)
    (duplicate-line)
    (next-line))
  (defun open-line-forward ()
    (interactive)
    (end-of-line)
    (default-indent-new-line nil t))
  (defun open-line-backward ()
    (interactive)
    (previous-line)
    (open-line-forward)))

(use-package paren
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery nil))

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
  (cursor-type t)
  (blink-cursor-delay 1.5)
  :config
  (unbind-key (kbd "C-x C-z") 'global-map)
  ;; (window-divider-mode)
  (blink-cursor-mode 0))

(use-package pixel-scroll
  :straight (:type built-in)
  :custom
  (hscroll-margin 2)
  (hscroll-step 1)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (pixel-scroll-precision-interpolation-total-time 0.05)
  (pixel-scroll-precision-interpolation-factor 1.25)
  (pixel-scroll-precision-interpolate-page t)
  (auto-window-vscroll nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 1)
  :hook
  (after-init-hook . pixel-scroll-precision-mode))

(use-package cus-edit
  :defer 3
  :straight (:type built-in)
  :custom
  (custom-file (concat user-emacs-directory "garbage.el"))
  :config
  (load custom-file nil 'nomessage))

(use-package autorevert
  :straight (:type built-in)
  :custom
  (auto-revert-interval 2)
  :config
  (global-auto-revert-mode t))

(use-package eww
  :defer t
  :straight (:type built-in)
  :custom
  (eww-default-download-directory "~/downloads/"))

(use-package window
  :straight (:type built-in)
  :custom
  (fit-window-to-buffer-horizontally t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  (display-buffer-alist
   '(("\\*\\(compilation\\|Async\\)\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 1))
     ("\\magit:"
      (display-buffer-same-window))
     ("\\*Flycheck error messages"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1))
     ("\\*Flycheck errors\\*"
      (display-buffer-in-side-window)
      (side . right)
      (slot . -1))
     ("\\*eldoc"
      (display-buffer-in-side-window)
      (side . right)
      (slot . -1))
     ("\\*lsp-help\\*"
      (display-buffer-in-side-window)
      (side . right)
      (slot . -1))))
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
  :custom
  (spacious-padding-subtle-mode-line t)
  :config
  (spacious-padding-mode))

(use-package winner
  :config
  (winner-mode))

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
  :config
  (gcmh-mode))

(use-package so-long
  :config
  (global-so-long-mode))

(use-package helpful
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable))

(use-package visual-regexp
  :bind
  ([remap query-replace] . vr/replace))

(use-package org
  :defer t
  :hook
  (org-mode-hook . variable-pitch-mode)
  :custom
  (org-confirm-babel-evaluate nil)
  (org-directory "~/documents/org/")
  (org-default-notes-file (concat org-directory "todo.org"))
  (org-hide-leading-stars t)
  (org-startup-indented t))

(use-package indent-bars
  :disabled
  ;; :hook
  ;; (c-mode-hook . indent-bars-mode)
  ;; (c-ts-mode-hook . indent-bars-mode)
  ;; (typescript-ts-mode-hook . indent-bars-mode)
  ;; (elixir-mode-hook . indent-bars-mode)
  ;; (elixir-ts-mode-hook . indent-bars-mode)
  ;; (zig-mode-hook . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg nil :blend 0.9))
  ;; (indent-bars-highlight-current-depth '(highlight :face-bg nil :blend 1.0))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-starting-column 0)
  (indent-bars-color-by-depth nil)
  (indent-bars-no-descend-lists t)
  (indent-bars-display-on-blank-lines t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-scope-min-lines 0)
  (indent-bars-ts-styling-scope 'out-of-scope)
  (indent-bars-ts-color '(highlight :face-bg nil :blend 0.1))
  (indent-bars-ts-highlight-current-depth '(highlight :face-bg nil :blend 0.1))
  (indent-bars-treesit-wrap
   '((c argument_list parameter_list init_declarator)))
  (indent-bars-treesit-scope
   '((c compound_statement))))

(use-package gdscript-mode
  :defer t)

(use-package dired
  :straight (:type built-in)
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
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

(use-package macrursors
  :straight (:host github
                   :repo "corytertel/macrursors")
  :hook
  (macrursors-mode-hook . deactivate-mark)
  (macrursors-pre-finish-hook . corfu-mode)
  (macrursors-post-finish-hook . corfu-mode)
  :bind
  (("C->" . macrursors-mark-next-line)
   ("C-<" . macrursors-mark-previous-line)
   ("C-M->" . macrursors-mark-next-instance-of)
   ("C-M-<" . macrursors-mark-previous-instance-of)
   :map macrursors-mode-map
   ("C-," . macrursors-end)))

(use-package selection-highlight-mode
  :straight (selection-highlight-mode :type git
                                      :host github
                                      :repo "balloneij/selection-highlight-mode")
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
  (regexp-search-ring-max 100))

(use-package avy
  :bind
  ("C-t" . avy-goto-char-2)
  :custom
  (avy-style 'de-bruijn)
  (avy-keys '(?c ?s ?n ?t ?a ?e ?i ?m))
  (avy-dispatch-alist '((?\w . avy-action-copy))))

(use-package rotate
  :config
  (setq
   rotate-functions
   '(rotate:even-horizontal
     rotate:even-vertical))
  :bind
  ("C-x C-o" . rotate-window)
  ("C-x M-o" . rotate-layout))

(use-package undo-fu
  :bind
  ([remap undo] . undo-fu-only-undo)
  ([remap undo-redo] . undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

(use-package ws-butler
  :config
  (ws-butler-global-mode))

(use-package elec-pair
  :straight (:type built-in)
  :disabled
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-pair-preserve-balance nil)
  :hook (prog-mode-hook . electric-pair-mode))

(use-package wrap-region
  :config
  (wrap-region-global-mode))

(use-package puni
  :bind
  ("M-r"   . puni-raise)
  ("C-M-s" . puni-splice)
  ("C-("   . puni-slurp-backward)
  ("C-)"   . puni-slurp-forward)
  ("C-{"   . puni-barf-backward)
  ("C-}"   . puni-barf-forward))

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

(use-package esh-mode
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
  (("C-c o e" . eshell)
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

(use-package fish-completion
  :straight (:host github
                   :repo "LemonBreezes/emacs-fish-completion")
  :hook
  (eshell-mode-hook . turn-on-fish-completion-mode)
  :custom
  (fish-completion-fallback-on-bash-p t)
  :config
  (global-fish-completion-mode))

(use-package vterm
  :custom
  (vterm-shell "fish")
  :bind
  ("C-c o v" . vterm))

(use-package magit
  :bind
  ("C-c o m" . magit-status)
  :config
  (add-to-list 'magit-git-environment "OVERCOMMIT_COLOR=0"))

(setq-default
 mode-line-format
 '("%e" mode-line-front-space
   (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-window-dedicated)
                display (min-width (6.0)))
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   mode-line-position
   (project-mode-line project-mode-line-format)
   (vc-mode vc-mode)
   "  "
   mode-line-end-spaces))

(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e"))

(use-package vertico
  :custom
  (vertico-resize nil)
  :config
  (vertico-mode))

(use-package vertico-directory
  :straight nil
  :after vertico
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :config
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         (cons 'orderless-literal-prefix word)))
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))
  :custom
  (completion-styles '(orderless-fast basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package goto-chg
  :bind
  ("C-," . goto-last-change)
  ("C-." . goto-last-change-reverse))

(use-package consult
  :custom
  (consult-locate-args "plocate --ignore-case --existing --regexp")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
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
   ("M-g k"           . consult-global-mark)
   ("M-g f"           . consult-flycheck)
   ("M-g o"           . consult-outline)
   ("M-g r"           . consult-recent-file)
   ([remap imenu]     . consult-imenu)
   ("M-g M-i"         . consult-imenu-multi)
   ([remap goto-line] . consult-goto-line)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap yank-pop] . consult-yank-pop))
  :config
  (recentf-mode)
  (consult-customize
   consult-bookmark consult-buffer consult-recent-file
   :preview-key "C-'"))

(use-package project
  :straight (:type built-in)
  :custom
  (project-vc-extra-root-markers
   '("Cargo.toml")))

(use-package consult-project-extra
  :after consult
  :bind
  ([remap project-find-file] . consult-project-extra-find)
  :config
  (consult-customize
   consult-project-extra-find
   :preview-key "C-'"))

(use-package consult-flycheck
  :defer t
  :straight (:host github :repo "minad/consult-flycheck"))

(use-package marginalia
  :config
  (marginalia-mode)
  :custom
  (marginalia-align 'right))

(use-package embark
  :after vertico
  :custom
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  :bind
  (("M-o" . embark-act)
   :map vertico-map
   ("M-s o" . embark-export)
   :map embark-general-map
   ([remap describe-symbol] . helpful-symbol)))

(use-package embark-consult
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package keychain-environment
  :defer 0.3
  :config
  (keychain-refresh-environment))

(use-package c-ts-mode
  :straight (:type built-in)
  :custom
  (c-ts-mode-indent-style 'k&r)
  (c-basic-offset 4)
  (c-ts-mode-indent-offset 4))

(use-package corfu
  :demand
  :straight
  (:host github
         :repo "minad/corfu"
         :files ("*" "extensions/*" (:exclude ".git")))
  :bind
  ("C-<tab>" . completion-at-point)
  :config
  (setq corfu-map
        (let ((m (make-sparse-keymap)))
          (bind-keys
           :map m
           ("C-g" . corfu-quit)
           ("C-'" . corfu-insert)
           ("C-q" . corfu-next)
           ("C--" . corfu-previous))
          m))
  :custom
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match 'insert)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  :config
  ;; Why do I have to do this?
  (setq company-minimum-prefix-length corfu-auto-prefix)
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode))

(use-package cape
  :straight
  (:host github
         :repo "minad/cape"
         :files ("*.el" "extensions/*.el"))
  :preface
  (defun cape-setup ()
    (setq-local
     completion-at-point-functions
     (delete-dups
      (remove
       #'ispell-completion-at-point
       (append (list #'cape-file #'cape-keyword #'cape-dabbrev)
               completion-at-point-functions)))))
  :hook
  (prog-mode-hook . cape-setup)
  (org-mode-hook . cape-setup)
  (markdown-mode-hook . cape-setup)
  :bind
  ("M-/" . cape-dabbrev))

(use-package dumb-jump
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :straight (:type built-in))

(use-package haskell-ts-mode
  :straight nil
  :load-path "/home/arjaz/.config/emacs/tmp/haskell-ts-mode"
  :mode "\\.hs\\'"
  :init
  (add-to-list
   'treesit-language-source-alist
   '(haskell "https://github.com/tree-sitter/tree-sitter-haskell")))

(use-package flycheck
  :defer t
  :custom
  (flycheck-indication-mode nil)
  ;; (flycheck-highlighting-mode 'symbols)
  (flycheck-highlighting-mode nil)
  (flycheck-check-syntax-automatically '(save idle-change mode-enable))
  ;; (flycheck-error-list-format
  ;;  [("Level" 8 flycheck-error-list-entry-level-<)
  ;;   (#("Message (Checker)" 0 7 (face flycheck-error-list-error-message) 9 16 (face flycheck-error-list-checker-name)) 0 t)])
  ;; :bind
  ;; ("C-c e e" . flycheck-list-errors)
  )

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
  :custom
  ;; I want it to be shown on a bottom corner the most removed from the pos
  (flycheck-posframe-position 'window-bottom-right-corner)
  (flycheck-posframe-border-width 1))

(use-package eldoc
  :bind
  ("C-c h e" . eldoc)
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  :custom
  (eldoc-display-functions '(eldoc-display-in-echo-area eldoc-display-in-buffer))
  (eldoc-box-clear-with-C-g t)
  :bind
  ("C-c h h" . eldoc-box-help-at-point))

(use-package eglot
  :disabled
  :demand
  :straight t
  :preface
  (defun eglot-remove-fluff ()
    (eglot-inlay-hints-mode -1)
    (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
    (remove-hook 'completion-in-region-mode-hook #'eglot--capf-session-flush t))
  :hook
  ;; a SPICY take
  (eglot-managed-mode-hook . eglot-remove-fluff)
  (js-ts-hook . eglot-ensure)
  (typescript-mode-hook . eglot-ensure)
  (typescript-ts-mode-hook . eglot-ensure)
  (tsx-ts-mode-hook . eglot-ensure)
  (zig-mode-hook . eglot-ensure)
  (haskell-ts-mode-hook . eglot-ensure)
  (elixir-ts-mode-hook . eglot-ensure)
  (elixir-mode-hook . eglot-ensure)
  (rust-mode-hook . eglot-ensure)
  (rust-ts-mode-hook . eglot-ensure)
  (odin-mode-hook . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities ())
  (eglot-stay-out-of '(company company-capf yasnippet company-backends))
  :bind
  (("C-c l l" . eglot)
   :map eglot-mode-map
   ("C-c l w r" . eglot-reconnect)
   ("C-c l w q" . eglot-shutdown)
   ("C-c l a" . eglot-code-actions)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format)
   ("C-c l h" . eldoc-print-current-symbol-info))
  :config
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode)
                 . ("/usr/lib/elixir-ls/language_server.sh")))
  (add-to-list 'eglot-server-programs
               '(haskell-ts-mode
                 . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("ols")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '((typescript-ts-mode tsx-ts-mode typescript-mode tsx-mode js-mode js2-mode js3-mode)
  ;;                . ("vtsls" "--stdio")))
  ;; (setq-default eglot-workspace-configuration
  ;;               '((vtsls (experimental (completion (enableServerSideFuzzyMatch . t)
  ;;                                                  (entriesLimit . 200)))
  ;;                        (autoUseWorkspaceTsdk . t))))
  ;; (fset #'eglot--snippet-expansion-fn #'ignore)
  ;; corfu setup
  (push '(eglot (styles orderless)) completion-category-overrides))

(use-package flycheck-eglot
  :disabled
  :config
  (global-flycheck-eglot-mode))

(use-package eglot-booster
  :disabled
  :straight (:host github
             :repo "jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode))

(use-package yasnippet
  :disabled
  :defer 0.3
  :config
  (yas-global-mode)
  (set-face-attribute 'yas-field-highlight-face nil
                      :inherit 'bold))

(use-package lsp-mode
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
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-elixir-ls-version "v0.22.0")
  :hook
  (lsp-completion-mode-hook . lsp-mode-setup-completion-for-corfu)
  (typescript-mode-hook . lsp-deferred)
  (typescript-ts-mode-hook . lsp-deferred)
  (tsx-ts-mode-hook . lsp-deferred)
  (zig-mode-hook . lsp-deferred)
  (haskell-ts-mode-hook . lsp-deferred)
  (rust-mode-hook . lsp-deferred)
  (rust-ts-mode-hook . lsp-deferred)
  (c-ts-mode-hook . lsp-deferred)
  (elixir-mode-hook . lsp-deferred)
  (erlang-hook . lsp-deferred)
  (elixir-ts-mode-hook - lsp-deferred)
  (heex-ts-mode-hook . lsp-deferred)
  (odin-mode-hook . lsp-deferred)
  :bind
  ("C-c l l" . lsp)
  :config
  (advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (set-face-attribute 'lsp-face-highlight-textual nil
                      :inherit 'bold)
  (set-face-attribute 'lsp-face-highlight-read nil
                      :underline nil
                      :inherit 'bold)
  (set-face-attribute 'lsp-face-highlight-write nil
                      :inherit 'bold)
  (setq lsp-eslint-auto-fix-on-save t)
  (defun lsp--eslint-before-save (orig-fun)
    "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."
    (when (and lsp-eslint-auto-fix-on-save
               (derived-mode-p '(typescript-ts-mode tsx-tx-mode)))
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
  :custom
  ;; a SPICY take
  (lsp-completion-enable nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-symbol-highlighting nil)
  (lsp-symbol-highlighting-skip-current t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-inlay-hint-enable nil)
  (lsp-lens-enable nil)
  (lsp-lens-place-position 'end-of-line)
  (lsp-prefer-capf t)
  (lsp-completion-provider :none) ; use corfu instead
  (lsp-idle-delay 0.75)
  (lsp-enable-snippet t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-file-watch-threshold 512)
  (lsp-diagnostics-flycheck-default-level 'warning)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t)
  (lsp-typescript-surveys-enabled nil))

(use-package consult-lsp
  :bind
  ("M-g d" . consult-lsp-diagnostics))

(use-package dape
  :straight
  (:host github :repo "svaante/dape")
  :commands (dape)
  :bind
  ("C-x C-a b" . dape-breakpoint-toggle)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints t)
  :config
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line))

(use-package lsp-haskell
  :after lsp-mode
  :preface
  :custom
  (lsp-haskell-plugin-class-code-lens-on nil)
  (lsp-haskell-formatting-provider "fourmolu")
  (lsp-haskell-plugin-pragmas-completion-on nil)
  (lsp-haskell-plugin-ghcide-completions-config-auto-extend-on nil)
  (lsp-haskell-plugin-ghcide-completions-config-snippets-on nil))

(use-package sly
  :defer t
  :custom
  ;; (sly-complete-symbol-function 'completion-at-point)
  (inferior-lisp-program "ros -Q run")
  ;; (inferior-lisp-program "sbcl --dynamic-space-size 8Gb")
  :config
  (setq-default sly-symbol-completion-mode nil))

(use-package cider
  :custom
  (cider-repl-display-help-banner nil)
  (cider-enrich-classpath t))

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

(use-package markdown-mode
  :hook
  (markdown-mode-hook . variable-pitch-mode)
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package nginx-mode
  :defer t)

(use-package zig-mode
  :custom
  (zig-format-on-save nil)
  :defer t)

(use-package odin-mode
  :straight (:host github :repo "mattt-b/odin-mode")
  :hook
  (odin-mode-hook . indent-tabs-mode))

(use-package lua-mode
  :defer t)

(use-package graphql-mode
  :defer t)

(use-package elixir-mode
  :mode ("\\.heex\\'" . heex-ts-mode))

(add-to-list 'load-path "/usr/lib/erlang/lib/tools-4.1/emacs")
(use-package erlang-start
  :straight (:type built-in)
  :custom
  (erlang-root-dir "/usr/lib/erlang/")
  (exec-path (cons "/usr/lib/erlang/bin" exec-path))
  (erlang-man-root-dir "/usr/lib/erlang/man"))

(use-package treesit
  :defer t
  :straight (:type built-in)
  :custom
  (treesit-font-lock-level 2)
  (treesit-extra-load-path
   '("~/.tree-sitter/bin/"
     "~/.config/emacs/straight/build/tree-sitter-langs/bin/")))

(use-package treesit-auto
  :defer 1
  :straight
  (:host github
   :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures
   'prog-mode
   '("</>" "</" "/>"
     "::" ":::"
     ";;"
     "//"
     "<:" ":>"
     "<<" ">>"
     "+=" "-=" "/=" "*="
     ":=" ":-" ":+"
     "+:" "-:" "=:"
     "<*" "<*>" "*>"
     "<|" "<|>" "|>"))
  (global-ligature-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install t))

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
  (haskell-ts-mode-hook . apheleia-mode)
  (python-mode-hook . apheleia-mode)
  (rust-mode-hook . apheleia-mode)
  (rust-ts-mode-hook . apheleia-mode)
  (typescript-mode-hook . apheleia-mode)
  (typescript-ts-mode-hook . apheleia-mode)
  (tsx-ts-mode-hook . apheleia-mode)
  (erlang-mode-hook . apheleia-mode)
  (elixir-mode-hook . apheleia-mode)
  (elixir-ts-mode-hook . apheleia-mode)
  :config
  (setf
   (alist-get 'prettier-typescript apheleia-formatters)
   '("apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=typescript")
   (alist-get 'cljstyle apheleia-formatters)
   '("cljstyle" "pipe")
   (alist-get 'clojure-mode apheleia-mode-alist)
   'cljstyle
   (alist-get 'fourmolu apheleia-formatters)
   '("fourmolu" file)
   (alist-get 'haskell-ts-mode apheleia-mode-alist)
   'fourmolu
   (alist-get 'rebar3-format apheleia-formatters)
   '("apheleia-from-project-root" "rebar.config" "rebar3" "format" filepath)
   (alist-get 'erlang-mode apheleia-mode-alist)
   'rebar3-format))

(use-package nix-mode
  :defer t)

(use-package sudo-edit
  :commands (sudo-edit-find-file))

(use-package tuareg)

(provide 'init)
;;; init.el ends here
