;;; init.el --- My configuration
;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; My Emacs configuration

;;; Code:
;; (setq esup-depth 0)
;; (use-package esup)

(defvar bootstrap-version)
(setq straight-check-for-modifications '(watch-files))
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 7))
  (load bootstrap-file nil 'nomessage))
(defvar use-package-enable-imenu-support t)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; (setq use-package-compute-statistics t)

(use-package use-package-core
  :straight (:type built-in)
  :custom
  (use-package-hook-name-suffix nil))

(use-package benchmark-init
  :disabled
  :hook (after-init-hook . benchmark-init/deactivate)
  :demand)

(use-package emacs
  :straight (:type built-in)
  :custom
  (tab-always-indent t)
  (inhibit-compacting-font-caches t)
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)
  (x-gtk-use-system-tooltips nil)
  (inhibit-startup-message t "disable startup message and gtk pop-ups")
  (vc-follow-symlinks 120)
  (auto-save-timeout 20 "number of seconds idle time before auto-save")
  (auto-save-interval 200 "number of keystrokes between auto-saves")
  (vc-make-backup-files t "make backups for version-controlled files as well")
  (create-lockfiles nil)
  (use-dialog-box nil)
  (use-short-answers t)
  (history-length 100)
  (history-delete-duplicates t)
  (enable-recursive-minibuffers t)
  (sentence-end-double-space nil)
  (gc-cons-threshold most-positive-fixnum "2^61 bytes")
  (gc-cons-percentage 0.6)
  :preface
  (defun allow-garbage ()
    (setq gc-cons-threshold 536870912 ; 512mb
          gc-cons-percentage 0.1))
  :hook
  (after-init-hook . allow-garbage)
  :config
  (setq-default tab-width 4))

(use-package savehist
  :straight (:type built-in)
  :custom
  (savehist-file (concat user-emacs-directory "savehist"))
  (savehist-save-minibuffer-history t)
  :config
  (savehist-mode))

(use-package solaire-mode
  :commands (solaire-global-mode))
(use-package doom-themes
  :defer t
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil))

(use-package modus-themes
  :defer t
  :custom
  (modus-operandi-palette-overrides
   '((bg-paren-match bg-green-nuanced)
     (preprocessor green-faint)
     (string green-faint)
     ;; (keyword cyan-faint)
     ;; (fnname cyan-faint)
     ))
  (modus-vivendi-tinted-palette-overrides
   '(;; (fg-main "#fffff8")
     ;; (bg-main "#1d1f21")
     (fg-main "#e5e8e6")
     (bg-main "#161719")
     (bg-paren-match bg-yellow-subtle)
     (preprocessor fg)
     (string yellow-faint)
     (keyword cyan-faint)
     (fnname cyan-faint)))
  (modus-themes-common-palette-overrides
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     (fringe unspecified)
     (bg-mode-line-active bg-dim)
     (bg-mode-line-inactive bg-dim)
     (bg-hover bg-magenta-nuanced)
     (bg-search-current bg-yellow-nuanced)
     (bg-search-lazy bg-cyan-nuanced)
     (bg-search-replace bg-red-nuanced)
     (bg-search-rx-group-0 bg-blue-nuanced)
     (bg-search-rx-group-1 bg-green-nuanced)
     (bg-search-rx-group-2 bg-red-subtle)
     (bg-search-rx-group-3 bg-magenta-subtle)
     (bg-region bg-green-nuanced)
     (fg-region unspecified)
     (docstring fg-dim)
     (comment fg-dim)
     (preprocessor fg-main)
     (constant fg-main)
     (variable fg-main)
     (type fg-main)
     (fnname fg-main)
     (keyword fg-main)
     (builtin fg-main)))
  (modus-themes-mixed-fonts t)
  ;; :preface
  ;; (defun my-modus-themes-custom-faces ()
  ;;   (modus-themes-with-colors
  ;;     (custom-set-faces
  ;;      `(solaire-default-face ((,c :inherit default :background ,bg-dim :foreground ,fg-main)))
  ;;      `(solaire-line-number-face ((,c :inherit solaire-default-face :foreground ,fg-main)))
  ;;      `(solaire-hl-line-face ((,c :background ,bg-active)))
  ;;      `(solaire-org-hide-face ((,c :background ,bg-dim :foreground ,bg-dim))))))
  )

(setq light-theme 'modus-operandi)
(setq dark-theme 'modus-vivendi)
(defun load-dark-theme ()
  "Load the saved dark theme."
  (interactive)
  (setq use-dark-theme-p t)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme dark-theme t)
  ;; (my-modus-themes-custom-faces)
  (solaire-global-mode t))
(defun load-light-theme ()
  "Load the saved light theme."
  (interactive)
  (setq use-dark-theme-p nil)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme light-theme t)
  ;; (set-face-attribute font-lock-keyword-face nil
  ;;                     :weight 'semibold)
  ;; (my-modus-themes-custom-faces)
  (solaire-global-mode -1))
(defvar use-dark-theme-p t)

(push 'use-dark-theme-p savehist-additional-variables)
(if use-dark-theme-p
    (load-dark-theme)
  (load-light-theme))

;; The following is a setup for smart kerneling font fonts that support those
;; BUG: this messes up how avy works unfortunately
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

(add-hook 'minibufer-setup-hook 'set-safe-composition-table)
(add-hook 'special-mode-hook 'set-safe-composition-table)
(add-hook 'text-mode-hook 'set-safe-composition-table)
(add-hook 'fundamental-mode-hook 'set-safe-composition-table)
(add-hook 'prog-mode-hook 'set-safe-composition-table)

(advice-add 'avy-jump
            :around
            #'toggle-safe-composition-table--around)

(use-package fontaine
  :init
  (defvar fontaine-my-preset '100-normal)
  (push 'fontaine-my-preset savehist-additional-variables)
  :config
  (advice-add 'fontaine-set-preset
              :around
              #'fontaine-set-preset--around)
  :preface
  (defun fontaine-load ()
    (fontaine-set-preset fontaine-my-preset))
  (defun fontaine-set-preset--around (old-fn preset &rest args)
    (setq fontaine-my-preset preset)
    (apply old-fn preset args))
  :hook
  (after-init-hook . fontaine-load)
  :custom
  (fontaine-presets
   '((90-light
      :default-family "Commit Mono"
      :fixed-pitch-family "Commit Mono"
      :line-number-family "Commit Mono"
      :variable-pitch-family "Commit Mono"
      ;; :default-family "Iosevka"
      ;; :fixed-pitch-family "Iosevka"
      ;; :line-number-family "Iosevka"
      ;; :variable-pitch-family "Iosevka Comfy Duo"
      :default-height 90
      :default-weight light)
     (90-normal
      :inherit 90-light
      :default-height 90
      :default-weight normal)
     (100-semilight
      :inherit 90-light
      :default-weight semilight
      :default-height 100)
     (100-light
      :inherit 90-light
      :default-height 100)
     (100-normal
      :inherit 90-light
      :default-height 100
      :default-weight normal)
     (110-normal
      :inherit 90-light
      :default-height 110
      :default-weight normal)
     (110-light
      :inherit 90-light
      :default-height 110)
     (115-normal
      :inherit 90-light
      :default-height 115
      :default-weight normal)
     (120-normal
      :inherit 90-light
      :default-height 120
      :default-weight normal)
     (120-light
      :inherit 90-light
      :default-height 120))))

(use-package misc
  :straight (:type built-in)
  :bind
  ("C-M-y" . duplicate-line-next-line)
  ("C-o" . open-line-from-end)
  ("C-M-o" . split-line-tab)
  :preface
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
  (defun open-line-from-end (arg)
    (interactive "P")
    (cond
     (arg
      (beginning-of-line)
      (open-line 1))
     (t
      (end-of-line)
      (default-indent-new-line nil t)))))

(use-package paren
  :custom
  (show-paren-when-point-in-periphery t))

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
  (window-divider-mode)
  (blink-cursor-mode 0))

(use-package pixel-scroll
  :straight (:type built-in)
  :custom
  (scroll-margin 0)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (pixel-scroll-precision-interpolation-total-time 0.1)
  (pixel-scroll-precision-interpolation-factor 0.75)
  (pixel-scroll-precision-interpolate-page t)
  (auto-window-vscroll nil)
  :hook
  (after-init-hook . pixel-scroll-precision-mode)
  :bind
  ([remap recenter-top-bottom] . smooth-recenter-top-bottom-pixel)
  ([remap scroll-up-command] . smooth-scroll-up)
  ([remap scroll-down-command] . smooth-scroll-down)
  :preface
  (defun smooth-recenter (&optional arg redisplay)
    "Like `recenter' but use smooth scroll."
    (pcase arg
      (`nil
       ;; Scroll smoothly, with line precision.
       (ignore-errors
         (pixel-scroll-precision-interpolate
          (* (line-pixel-height)
             (- (/ (count-screen-lines (window-start) (window-end)) 2)
                (count-screen-lines (window-start) (point))))
          nil 1))
       ;; Call original recenter for final adjustment.
       (recenter arg redisplay))
      ((pred (not numberp))
       (recenter arg redisplay))
      ((pred (<= 0))
       ;; Scroll smoothly, with line precision.
       (ignore-errors
         (pixel-scroll-precision-interpolate
          (* -1 (line-pixel-height)
             (max 0 (- (count-screen-lines (window-start) (point)) 2 arg)))
          nil 1))
       ;; Call original recenter for final adjustment.
       (recenter arg redisplay))
      ((pred (> 0))
       ;; Scroll smoothly, with line precision.
       (ignore-errors
         (pixel-scroll-precision-interpolate
          (* (line-pixel-height)
             (max 0 (- (count-screen-lines (point) (window-end)) 3 arg)))
          nil 1))
       ;; Call original recenter for final adjustment.
       (recenter arg redisplay))))
  (defun smooth-recenter-top-bottom-pixel (&optional arg)
    "Like `recenter-top-bottom' but use smooth scrolling."
    (interactive "P")
    (cond
     (arg (smooth-recenter arg t))                 ; Always respect ARG.
     (t
      (setq recenter-last-op
	        (if (eq this-command last-command)
	            (car (or (cdr (member recenter-last-op recenter-positions))
		                 recenter-positions))
	          (car recenter-positions)))
      (let ((this-scroll-margin
	         (min (max 0 scroll-margin)
		          (truncate (/ (window-body-height) 4.0)))))
        (cond ((eq recenter-last-op 'middle)
	           (smooth-recenter nil t))
	          ((eq recenter-last-op 'top)
	           (smooth-recenter this-scroll-margin t))
	          ((eq recenter-last-op 'bottom)
	           (smooth-recenter (- -1 this-scroll-margin) t))
	          ((integerp recenter-last-op)
	           (smooth-recenter recenter-last-op t))
	          ((floatp recenter-last-op)
	           (smooth-recenter (round (* recenter-last-op (window-height))) t)))))))
  (defun smooth-scroll-up (&optional arg)
    "Scroll smoothly up ARG lines. If ARG is nil, scroll a half page."
    (interactive)
    (let ((arg (or arg (/ (window-body-height) 2))))
      (next-line (- arg))
      (smooth-recenter)))
  (defun smooth-scroll-down (&optional arg)
    "Scroll smoothly down ARG lines. If ARG is nil, scroll a half page."
    (interactive)
    (let ((arg (or arg (/ (window-body-height) 2))))
      (previous-line (- arg))
      (smooth-recenter))))

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

(use-package mode-local
  :straight (:type built-in))

(use-package simple
  :straight (:type built-in)
  :bind
  (:map ctl-x-map
        ("k" . kill-current-buffer))
  :config
  (setq-default indent-tabs-mode nil))

(use-package cc-vars
  :straight (:type built-in)
  :defer 1
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

(use-package visual-regexp-steroids
  :disabled
  :custom
  (vr/default-regexp-modifiers '(:I t :M t :S nil :U nil))
  ;; :bind
  ;; ([remap isearch-forward] . vr/isearch-forward)
  ;; ([remap isearch-backward] . vr/isearch-backward)
  :config
  (defadvice vr--isearch (around add-case-insensitive (forward string &optional bound noerror count) activate)
    (when (and (eq vr/engine 'python) case-fold-search)
      (setq string (concat "(?i)" string)))
    ad-do-it))

(use-package mermaid-mode
  :defer t)

(use-package ob-mermaid
  :defer t
  :custom
  (ob-mermaid-cli-path "/usr/bin/mmdc"))

(use-package org
  :defer t
  :hook
  (org-mode-hook . variable-pitch-mode)
  (org-babel-after-execute-hook . org-redisplay-inline-images)
  :bind
  ("C-c a a" . org-agenda)
  ("C-c a c" . org-capture)
  (:map org-mode-map
        ("C-c a d" . org-archive-all-done))
  :preface
  (defun open-org-agenda ()
    (org-agenda nil "n")
    (delete-other-windows)
    (get-buffer "*Org Agenda*"))
  :custom
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
  :defer t
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

(use-package spacious-padding
  :disabled
  :straight (:host github :repo "protesilaos/spacious-padding")
  :demand
  :bind
  ("C-c o p" . spacious-padding-mode)
  :config
  (spacious-padding-mode))

(use-package indent-bars
  :disabled
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook
  (prog-mode-hook . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.4))
  (indent-bars-highlight-current-depth '(:face default :blend 0.4))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth nil)
  (indent-bars-display-on-blank-lines nil)
  ;; (indent-bars-treesit-support t)
  ;; (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator)))
  )

(use-package gdscript-mode
  :defer t)

(use-package all-the-icons
  :disabled
  :defer 1)

(use-package all-the-icons-dired
  :disabled
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package vscode-icon
  :disabled
  :defer 1)

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

(use-package dired-hacks
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

(use-package diredfl
  :disabled
  :hook (dired-mode-hook . diredfl-mode))

;; live previews? kmacro-x?
(use-package macrursors
  :straight (:host github
                   :repo "corytertel/macrursors")
  :hook
  (macrursors-pre-finish-hook . corfu-mode)
  (macrursors-post-finish-hook . corfu-mode)
  :bind
  (("C->" . macrursors-mark-next-line)
   ("C-<" . macrursors-mark-previous-line)
   ;; TODO: disable active region after this
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
  (isearch-lazy-count t)
  (search-ring-max 200)
  (regexp-search-ring-max 200)
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
  (defun avy-action-zap-to-including-char (pt)
    (if (> pt (point))
        (kill-region (point) (1+ pt))
      (kill-region pt (point))))
  (defun avy-action-zap-to-excluding-char (pt)
    (if (> pt (point))
        (kill-region (point) pt)
      (kill-region (1+ pt) (point))))
  :config
  (defun avy-zap-up-to-char-in-line (char)
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy-jump
       (regexp-quote (string char))
       :beg (line-beginning-position)
       :end (line-end-position)
       :action #'avy-action-zap-to-excluding-char)))
  (defun avy-zap-to-char-in-line (char)
    (interactive (list (read-char "char: " t)))
    (avy-with avy-goto-char
      (avy-jump
       (regexp-quote (string char))
       :beg (line-beginning-position)
       :end (line-end-position)
       :action #'avy-action-zap-to-including-char)))
  :bind
  (("M-t" . avy-goto-char-in-line)
   ("C-t" . avy-goto-word-1)
   ("M-z" . avy-zap-to-char-in-line)
   ("C-z" . avy-zap-up-to-char-in-line)
   ;; :map dired-mode-map
   ;; ("C-t" . nil)
   )
  :custom
  (avy-style 'de-bruijn)
  (avy-keys '(?c ?s ?n ?t ?a ?e ?i ?h))
  (avy-dispatch-alist '((?\w . avy-action-copy)
                        (?\o . avy-action-embark))))

(use-package ace-window
  :disabled
  :custom
  (aw-background nil)
  (aw-scope 'frame)
  (aw-keys avy-keys)
  (aw-ignore-current t)
  :bind
  ([remap other-window] . ace-window))

(use-package rotate
  :config
  (setq
   rotate-functions
   '(rotate:even-horizontal
     rotate:even-vertical))
  :bind
  (("C-x C-o" . rotate-window)
   ("C-x M-o" . rotate-layout)))

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
  :hook (prog-mode-hook . electric-pair-mode))

(use-package wrap-region
  :config
  (wrap-region-global-mode))

(use-package puni
  :bind
  (("M-'"   . puni-expand-region)
   ("M-r"   . puni-raise)
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

(use-package shell
  :straight (:type built-in)
  :bind
  (:map shell-mode-map
        ("C-l" . comint-clear-buffer)))

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
   ("C-c C-l" . eshell/clear-buffer)
   ;; :map eshell-hist-mode-map
   ;; ("<up>" . previous-line)
   ;; ("<down>" . next-line)
   )
  :config
  (require 'em-hist)
  (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
  ;; (remove-hook 'eshell-preoutput-filter-functions 'eshell-handle-ansi-color)
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

(use-package detached
  :disabled)

(use-package org-mime
  :defer t)

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
      (when-let ((_ (fboundp 'magit-get-current-branch))
                 (branch (magit-get-current-branch))
                 (fancy (when (char-displayable-p ?) " ")))
        (concat fancy branch))))
  :config
  (setq magit-git-environment
        (append magit-git-environment
                (list "OVERCOMMIT_COLOR=0"))))

(use-package magit-delta
  :disabled
  :hook (magit-mode-hook . magit-delta-mode))

(use-package forge
  :disabled
  :defer t)

(use-package blamer
  :defer t
  :custom
  (blamer-show-avatar-p nil)
  :config
  (set-face-attribute 'blamer-face nil
                      :foreground 'unspecified
                      :inherit 'custom-comment))

(defun string-repeat (num s)
  "Make a string of S repeated NUM times."
  (declare (pure t) (side-effect-free t))
  (let (ss)
    (while (> num 0)
      (setq ss (cons s ss))
      (setq num (1- num)))
    (apply 'concat ss)))
(defun string-pad-left (len padding s)
  "If S is shorter than LEN, pad it with PADDING on the left."
  (let ((extra (max 0 (- len (length s)))))
    (concat (make-string extra (string-to-char padding)) s)))
(setq-default
 header-line-format
 '((:propertize (:eval (file-directory)) face font-lock-variable-name-face)
   "「 "
   (:propertize (:eval (file-or-buffer-name)) face font-lock-keyword-face)
   " 」"))
(setq my-mode-line-format
      '("%5l:%c"
        (:eval (string-repeat (- 4 (length (number-to-string (current-column)))) " "))
        (:propertize (:eval (file-read-write-indicator)) face font-lock-warning-face)
        "  "
        (:propertize
         (:eval (unless (file-remote-p default-directory)
                  (git-branch)))
         face magit-dimmed)))
(setq-default mode-line-format
              '((:eval (string-pad-left (window-width) " " (format-mode-line my-mode-line-format)))))
(use-package mini-modeline
  ;; :disabled
  ;; :defer 0.1
  :hook
  (after-init-hook . mini-modeline-mode)
  :preface
  :custom
  (mini-modeline-enhance-visual nil)
  (mini-modeline-display-gui-line nil)
  (mini-modeline-r-format
   '("%5l:%c"
     (:eval (string-repeat (- 4 (length (number-to-string (current-column)))) " "))
     (:propertize (:eval (file-read-write-indicator)) face font-lock-warning-face)
     "  "
     (:propertize
      (:eval (unless (file-remote-p default-directory)
               (git-branch)))
      face magit-dimmed))))

(use-package browse-at-remote
  :commands (browse-at-remote))

(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e"))

(use-package vertico
  :custom
  (vertico-resize nil)
  :config
  (vertico-mode)
  ;; (vertico-flat-mode)
  )

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
   ([remap imenu]     . consult-imenu)
   ("M-g M-i"         . consult-imenu-multi)
   ([remap goto-line] . consult-goto-line)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap yank-pop] . consult-yank-pop))
  :config
  (recentf-mode)
  (consult-customize
   consult-bookmark consult-buffer
   :preview-key "C-'"))

(use-package consult-project-extra
  :after consult
  :bind
  ([remap project-find-file] . consult-project-extra-find)
  :config
  (consult-customize
   consult-project-extra-find
   :preview-key "C-'"))

(use-package consult-ls-git
  :bind
  ("M-s g" . consult-ls-git))

(use-package consult-flycheck
  :defer t
  :straight (:host github :repo "minad/consult-flycheck"))

;; Can I somehow use global marks instead or something?
;; Can I limit those to 4 max
;; I probably want to write my own thing
(use-package harpoon
  :bind
  ("C-c h a" . harpoon-add-file)
  ("C-c h f" . harpoon-toggle-quick-menu)
  ("C-c h t" . harpoon-go-to-1)
  ("C-c h n" . harpoon-go-to-2)
  ("C-c h s" . harpoon-go-to-3)
  ("C-c h c" . harpoon-go-to-4)
  ("C-c h d" . harpoon-clear)
  :custom
  (harpoon-project-package 'project)
  (harpoon-separate-by-branch t))

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
   ("M-." . embark-dwim)
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

;; (use-package envrc
;;   :config
;;   (envrc-global-mode))

(use-package c-ts-mode
  :defer t
  :straight (:type built-in)
  :custom
  (c-ts-mode-indent-offset 4))

(use-package smart-tab
  :disabled
  :custom
  (smart-tab-user-provided-completion-function
   'corfu-candidate-overlay-complete-at-point)
  (smart-tab-completion-functions-alist nil)
  :config
  (global-smart-tab-mode))

(use-package corfu
  :straight
  (:host github
         :repo "minad/corfu"
         :files ("*" "extensions/*" (:exclude ".git")))
  :defer 0.1
  :preface
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  :bind
  ("C-<tab>" . completion-at-point)
  :config
  (setq corfu-map
        (let ((m (make-sparse-keymap)))
          (bind-keys :map m
                     ("C-g" . corfu-quit)
                     ("M-s o" . corfu-move-to-minibuffer)
                     ("C-'" . corfu-insert)
                     ;; I don't like C-n and C-p having two functions
                     ("C-n" . corfu-next)
                     ("C-p" . corfu-previous))
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
  ;; (set-face-attribute
  ;;  'corfu-default nil
  ;;  :background 'unspecified
  ;;  :inherit 'default)
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

(use-package corfu-candidate-overlay
  :disabled
  :bind
  ;; TODO: maybe replace?
  ("C-<tab>" . corfu-candidate-overlay-complete-at-point)
  :config
  (corfu-candidate-overlay-mode))

(use-package cape
  :defer 0.3
  :straight (:host github
                   :repo "minad/cape"
                   :files ("*.el" "extensions/*.el"))
  :preface
  (defun cape-setup ()
    (setq-local completion-at-point-functions
                (delete-dups
                 (remove
                  #'ispell-completion-at-point
                  (append (list #'cape-file #'cape-keyword #'cape-dabbrev)
                          completion-at-point-functions)))))
  ;; TODO: use lisp-complete-symbol for elisp
  :hook
  (prog-mode-hook . cape-setup)
  (org-mode-hook . cape-setup)
  (markdown-mode-hook . cape-setup)
  ;; (lsp-completion-mode-hook . cape-lsp-setup)
  :bind
  ("M-/" . cape-dabbrev))

(use-package copilot
  :disabled
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :custom
  (copilot-indent-offset-warning-disable t)
  (copilot-idle-delay 0)
  :hook
  (prog-mode-hook . copilot-mode)
  :bind
  ("C-M-=" . copilot-complete)
  ("M-\\" . copilot-accept-completion)
  ("C-M-\\" . copilot-accept-completion-by-line))

(use-package codeium
  :disabled
  :preface
  ;; :disabled
  :after cape
  :straight (:host github :repo "exafunction/codeium.el")
  :config
  ;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  (keymap-global-set
   "M-\\"
   (cape-capf-interactive #'codeium-completion-at-point)))

(use-package kind-icon
  :disabled
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package nerd-icons-corfu
  :disabled
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package dumb-jump
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package haskell-mode
  :defer t
  :config
  (remove-hook 'haskell-mode-hook #'interactive-haskell-mode)
  :custom
  (haskell-completing-read-function #'completing-read)
  (haskell-process-show-overlays nil)
  (haskell-process-suggest-restart nil)
  (haskell-font-lock-symbols nil))

(use-package flycheck
  :defer 1
  :custom
  (flycheck-indication-mode nil)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-check-syntax-automatically '(save idle-change mode-enable))
  ;; :config
  ;; (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  ;;   [16 48 112 240 112 48 16] nil nil 'center)
  )

(use-package flycheck-posframe
  ;; :disabled
  :hook
  (flycheck-mode-hook . flycheck-posframe-mode)
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
  ;; (flycheck-posframe-position 'point-bottom-left-corner)
  ;; (flycheck-posframe-position 'frame-bottom-opposite-corner)
  (flycheck-posframe-border-width 1)
  ;; :config
  ;; (flycheck-posframe-configure-pretty-defaults)
  )

(use-package flycheck-inline
  :defer t
  :disabled
  ;; :hook (flycheck-mode-hook . flycheck-inline-mode)
  )

(use-package flycheck-pos-tip
  :disabled
  ;; :custom
  ;; (flycheck-pos-tip-timeout 0)
  :hook (flycheck-mode-hook . flycheck-pos-tip-mode))

(use-package tldr
  :defer t)
(use-package devdocs
  :defer t)

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  ;; (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-prefer-doc-buffer 'maybe)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  ;; :disabled
  ;; :hook (eldoc-mode-hook . eldoc-box-hover-at-point-mode)
  :hook
  ;; (eldoc-mode-hook . eldoc-box-hover-mode)
  (spacious-padding-mode-hook . eldoc-set-box-border-fringe)
  :custom
  (eldoc-display-functions '(eldoc-display-in-buffer))
  (eldoc-box-clear-with-C-g t)
  ;; (eldoc-box-offset '(16 16 32))
  :preface
  (defun eldoc-set-box-border-fringe ()
    (interactive)
    ;; (set-face-attribute
    ;;  'eldoc-box-border nil
    ;;  :background 'unspecified
    ;;  :foreground 'unspecified
    ;;  :inherit 'window-divider)
    )
  :bind
  ("C-c h h" . eldoc-box-help-at-point)
  :config
  ;; (set-face-attribute
  ;;  'eldoc-box-body nil
  ;;  :background 'unspecified
  ;;  :foreground 'unspecified
  ;;  :inherit 'default)
  (eldoc-set-box-border-fringe))

(use-package eglot
  :disabled
  :demand
  :straight
  :hook
  (eglot-managed-mode-hook . eglot-inlay-hints-mode)
  (typescript-mode-hook . eglot-ensure)
  (typescript-ts-mode-hook . eglot-ensure)
  (tsx-ts-mode-hook . eglot-ensure)
  (zig-mode-hook . eglot-ensure)
  (haskell-mode-hook . eglot-ensure)
  (rust-mode-hook . eglot-ensure)
  (rust-ts-mode-hook . eglot-ensure)
  :custom
  (read-process-output-max (* 1024 1024 10))
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities ())
  (eglot-stay-out-of '(company))
  :bind
  (("C-c l l" . eglot)
   :map eglot-mode-map
   ("C-c l w r" . eglot-reconnect)
   ("C-c l w q" . eglot-shutdown)
   ("C-c l a" . eglot-code-actions)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format)
   ("C-c l e" . consult-flymake)
   ("C-c l h" . eldoc-print-current-symbol-info))
  :config
  ;; (fset #'eglot--snippet-expansion-fn #'ignore)
  ;; (require 'goto-addr)
  ;; (push '(haskell-mode . ("haskell-language-server" "--lsp")) eglot-server-programs)
  ;; corfu setup
  (cl-defmethod project-root ((project (head eglot-project)))
    (cdr project))
  (defun my-project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))
  (add-hook 'project-find-functions
            'my-project-try-tsconfig-json nil nil)
  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio"))
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

(use-package buffer-env
  :disabled)

(use-package yasnippet
  :defer 0.5
  :config
  (yas-global-mode))

(use-package lsp-mode
  :defer 0.5
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
  :hook
  (lsp-completion-mode-hook . lsp-mode-setup-completion-for-corfu)
  (typescript-mode-hook . lsp-deferred)
  (typescript-ts-mode-hook . lsp-deferred)
  (tsx-ts-mode-hook . lsp-deferred)
  (zig-mode-hook . lsp-deferred)
  (haskell-mode-hook . lsp-deferred)
  (rust-mode-hook . lsp-deferred)
  (rust-ts-mode-hook . lsp-deferred)
  (c-ts-mode-hook . lsp-deferred)
  :bind
  ("C-c l l" . lsp)
  :config
  (set-face-attribute 'lsp-face-highlight-textual nil
                      :inherit 'bold)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (setq lsp-eslint-auto-fix-on-save t)
  (defun lsp--eslint-before-save (orig-fun)
    "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."
    (when (and lsp-eslint-auto-fix-on-save
               (derived-mode-p '(typescript-mode typescript-ts-mode tsx-tx-mode)))
      (lsp-eslint-fix-all))
    (funcall orig-fun))
  (advice-add 'lsp--before-save :around #'lsp--eslint-before-save)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-symbol-highlighting nil)
  (lsp-symbol-highlighting-skip-current t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-lens-enable nil)
  (lsp-lens-place-position 'end-of-line)
  ;; (lsp-lens-place-position 'above-line)
  (lsp-prefer-capf t)
  (lsp-completion-provider :none) ; use corfu instead
  ;; (lsp-completion-provider :capf)
  (lsp-idle-delay 0.75)
  (lsp-inlay-hint-enable t)
  ;; (lsp-enable-snippet t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-semantic-tokens-enable t)
  (read-process-output-max (* 1024 1024 10))
  (lsp-file-watch-threshold 512)
  (lsp-diagnostics-flycheck-default-level 'warning)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t))

(use-package consult-lsp
  :bind
  (:map lsp-mode-map
   ("M-g f" . consult-lsp-diagnostics)))

(use-package lsp-ui
  :disabled
  :bind
  (:map lsp-ui-mode-map
   ("C-c l t s" . lsp-ui-sideline-toggle-symbols-info)
   ("C-c l h o" . lsp-ui-open-docs-link-hack))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-peek-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-diagnostic-max-lines 10)
  (lsp-ui-sideline-show-hover t)
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

(use-package dape
  :defer 1
  :custom
  (dape-buffer-window-arrangement 'right)
  :config
  (repeat-mode))

(use-package lsp-haskell
  :after lsp-mode
  :defer 0.4
  :preface
  :custom
  (lsp-haskell-plugin-class-code-lens-on nil)
  (lsp-haskell-formatting-provider "fourmolu")
  (lsp-haskell-plugin-pragmas-completion-on nil)
  (lsp-haskell-plugin-ghcide-completions-config-auto-extend-on nil)
  (lsp-haskell-plugin-ghcide-completions-config-snippets-on nil))

(use-package tuareg
  :defer t)

(use-package merlin
  :defer t)

(use-package dune
  :defer t)

(use-package eros
  :hook (emacs-lisp-mode-hook . eros-mode))

(use-package sly
  :defer t
  :custom
  ;; (sly-complete-symbol-function 'completion-at-point)
  (inferior-lisp-program "ros -Q run")
  ;; (inferior-lisp-program "sbcl --dynamic-space-size 8Gb")
  :config
  (setq-default sly-symbol-completion-mode nil))

(use-package cider
  :bind
  (:map cider-repl-mode-map
   ("C-l" . cider-repl-clear-buffer)
   :map cider-mode-map
   ("C-c M-c" . cider-debug-defun-at-point)
   ("C-c C-p" . cider-inspect-last-sexp))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-enrich-classpath t))

(use-package sayid
  :disabled
  :hook (clojure-mode-hook . sayid-setup-package))

(use-package cider-eval-sexp-fu
  :after cider)

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
  :disabled
  :hook (lisp-mode-hook . aggressive-indent-mode))

(use-package rust-mode
  :defer t)

(use-package nasm-mode
  :defer t)

(use-package typescript-mode
  :custom
  (typescript-indent-level 2)
  :defer t)

(use-package prisma-ts-mode
  :after treesit
  :config
  (add-to-list
   'treesit-language-source-alist
   '(prisma "https://github.com/victorhqc/tree-sitter-prisma")))

(use-package markdown-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package yuck-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package nginx-mode
  :defer t)

(use-package zig-mode
  :defer t)

(use-package odin-mode
  :straight (:host github :repo "mattt-b/odin-mode")
  :hook (odin-mode-hook . lsp-deferred)
  :config
  (push '(odin-mode . "odin") lsp-language-id-configuration)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "ols")
    			    :major-modes '(odin-mode)
    			    :server-id 'ols
                    ;; This is just so lsp-mode sends the "workspaceFolders" param to the server.
    			    :multi-root t))
  )

(use-package lua-mode
  :defer t)

(use-package graphql-mode
  :defer t)

(use-package forth-mode
  :defer t)

(use-package comint-mime
  :hook
  (inferior-python-mode-hook . comint-mime-mode))

(use-package elixir-mode
  ;; TODO: try out lexical-lsp
  :hook
  (elixir-ts-mode-hook . lsp-deferred)
  (elixir-mode-hook . lsp-deferred)
  )

;; (use-package erlang)
(setq load-path (cons "/usr/lib/erlang/lib/tools-3.6/emacs" load-path))
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
  (delete 'typescript treesit-auto-langs)
  (delete 'tsx treesit-auto-langs)
  (global-treesit-auto-mode))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :hook
  (after-init-hook . global-ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--" ":-->"
     ">=>" "<=<" "<==" "<===" "=>" "=>>" "==>" "===>" "<=>" "<==>" "<===>" "<====>" "<!---"
     "(*" "*)" "[|" "|]" "{|" "|}" "<." "<.>" ".>"
     "</>" "</" "/>"
     "<~~" "<~" "~>" "~~>"
     "::" ":::"
     "==" "==="
     "!=" "!=="
     ">>=" "=<<"
     "<>" ":>"
     "<<" ">>"
     ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

(use-package xah-math-input
  :bind
  ("C-c m i" . xah-math-input-change-to-symbol))

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
  (haskell-mode-hook . apheleia-mode)
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
  (setf (alist-get 'prettier-typescript apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=typescript"))
  (setf (alist-get 'cljstyle      apheleia-formatters) '("cljstyle" "pipe"))
  (setf (alist-get 'clojure-mode  apheleia-mode-alist) 'cljstyle)
  (setf (alist-get 'fourmolu      apheleia-formatters) '("fourmolu" file))
  (setf (alist-get 'haskell-mode  apheleia-mode-alist) 'fourmolu)
  (setf (alist-get 'rebar3-format apheleia-formatters) '("apheleia-from-project-root" "rebar.config" "rebar3" "format" filepath))
  (setf (alist-get 'erlang-mode   apheleia-mode-alist) 'rebar3-format))

(use-package rainbow-mode
  :commands (rainbow-mode))

;; TODO: something's wrong with the font sizes, only 9 works
(use-package screenshot
  :straight (:host github
             :repo "tecosaur/screenshot"
             :build (:not compile))
  :commands (screenshot)
  :custom
  (screenshot-max-width 140))

(use-package nix-mode
  :defer t)

(use-package sudo-edit
  :commands (sudo-edit-find-file sudo-edit sudo-edit-current-file))

(use-package jinx
  :disabled
  :hook
  (emacs-startup-hook . global-jinx-mode))

(provide 'init)
;;; init.el ends here
