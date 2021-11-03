;;; init.el --- My configuration
;;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; That's my (Eugene's) local Emacs configuration

;;; Code:

;; Straight
(defvar comp-deferred-compilation-deny-list ())
(defvar bootstrap-version)
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

;; Use-package
(straight-use-package 'use-package)

(use-package use-package-core
  :straight (:type built-in)
  :custom (use-package-hook-name-suffix nil))

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-check-for-modifications '(watch-files find-when-checking)))

;; TODO: split that into blocks
(use-package emacs
  :straight (:type built-in)
  :custom
  (tab-always-indent 'complete)
  (inhibit-compacting-font-caches t)
  ;; I think it's managed by GHCM now
  (gc-cons-threshold most-positive-fixnum "2^61 bytes")
  (gc-cons-percentage 0.6)
  ;; Long lines stuff
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
  ;; Clean buffers
  (clean-buffer-list-delay-general 1)
  (savehist-file (concat user-emacs-directory "savehist"))
  (history-length t)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :hook (emacs-startup-hook . (lambda ()
                                (setq gc-cons-threshold 536870912 ; 512mb
                                      gc-cons-percentage 0.1)))
  :config
  (let ((tmp-dir (concat user-emacs-directory "tmp")))
    (unless (file-exists-p tmp-dir)
      (make-directory tmp-dir t)))
  ;; Disable visual clutter
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  ;; We don't want to type yes and no all the time so, do y and n
  (fset 'yes-or-no-p 'y-or-n-p)
  (savehist-mode t)
  (setq-default indent-tabs-mode nil
                tab-width 4))

(use-package display-fill-column-indicator
  :straight (:type built-in)
  :config
  ;; (global-display-fill-column-indicator-mode t)
  (setq-default fill-column 100))

(use-package frame
  :straight (:type built-in)
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)
  :config
  (window-divider-mode t)
  (blink-cursor-mode 0))

(use-package faces
  :straight (:type built-in)
  :config
  (defvar used-font "Iosevka Arjaz")
  (add-to-list 'default-frame-alist `(font . ,used-font))
  (set-face-attribute 'default nil :height 130)
  (set-frame-font used-font))

(use-package cus-edit
  :straight (:type built-in)
  :custom
  (custom-file (concat user-emacs-directory "garbage.el")))

(use-package autorevert
  :straight (:type built-in)
  :custom
  (auto-revert-interval 2)
  :config
  ;; Automaticaly revert changes
  (global-auto-revert-mode t))

(use-package general
  :init
  (defvar leader-key "SPC")
  (defvar leader-non-normal-key "M-SPC"))

(use-package compile
  :straight (:type built-in)
  :general
  (:states 'normal
   :prefix leader-key
   "c c" #'compile
   "c r" #'recompile))

(use-package lisp-mode
  :straight (:type built-in)
  :general
  (:keymaps 'emacs-lisp-mode-map
   "C-c C-r" #'eval-region)
  :demand t
  :config
  (let ((path "~/.dotfiles/emacs/elisp-fix-indent.el"))
    (when (file-exists-p path)
      (load path))))

(use-package bookmark
  :straight (:type built-in)
  :custom
  (bookmark-fontify nil)
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   ;; TODO: maybe "b g" ?
   "g" #'bookmark-jump))

(use-package sql
  :demand
  :general
  ("C-l" #'comint-clear-buffer)
  :config
  ;; TODO: ugly
  (setq sql-postgres-login-params
        (append sql-postgres-login-params '(port))))

(use-package simple
  :straight (:type built-in)
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "k" #'kill-current-buffer))

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
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "o" #'find-file)
  :config
  (unless (file-exists-p (concat user-emacs-directory "backups"))
    (make-directory (concat user-emacs-directory "backups") t)))

(use-package gcmh
  :demand
  :custom
  (gcmh-high-cons-threshold (/ 1073741824 2))
  :config
  (gcmh-mode 1))

(use-package so-long
  :config
  (global-so-long-mode))

(use-package ibuffer
  :straight (:type built-in)
  :general
  ("C-x C-b" #'ibuffer))

(use-package prog-mode
  :straight (:type built-in)
  :config
  (global-prettify-symbols-mode))

(use-package dash)

(use-package helpful
  :general
  ("C-h f" #'helpful-callable
   "C-h v" #'helpful-variable
   "C-h k" #'helpful-key))

(use-package visual-regexp)

(use-package org
  :hook (org-babel-after-execute-hook . org-redisplay-inline-images)
  :general
  ("C-c a" #'org-agenda
   "C-c c" #'org-capture)
  :demand
  :custom
  (org-image-actual-width 500)
  (org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-latex-logfiles-extensions
   '("lof" "lot" "tex" "aux" "idx" "log" "out" "toc" "nav" "snm"
     "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl"
     "bbl" "pygtex" "pygstyle"))
  (org-confirm-babel-evaluate nil)
  (org-directory "~/.org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-hide-leading-stars t)
  (org-startup-folded t)
  (org-startup-indented nil)
  (org-agenda-files (list org-default-notes-file))
  (org-capture-templates
   '(("t" "Tasks" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n%u\n" :prepend t)
     ("l" "Look later" entry (file+headline org-default-notes-file "Look later")
      "* TODO %?")
     ("s" "Skills" entry (file+headline org-default-notes-file "Skills")
      "* TODO %?")
     ("g" "Gifts" entry (file+headline org-default-notes-file "Gifts")
      "* TODO %?")))
  :config
  ;; enable python for in-buffer evaluation
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))

(use-package org-roam
  :straight (:host github
             :repo "org-roam/org-roam"
             :files (:defaults "extensions/*"))
  :demand t
  :init
  (make-directory "~/.org/roam" t)
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/.org/roam"))
  :general
  (:states 'normal
   "C-c n f" #'org-roam-node-find
   "C-c n i" #'org-roam-node-insert
   ;; That's a backlinks buffer
   "C-c n l" #'org-roam-buffer-toggle
   "C-c n c" #'org-roam-capture)
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))

(use-package org-jira
  :custom
  (jiralib-url "https://vacuum.atlassian.net")
  :config
  (make-directory "~/.org-jira" t))

(use-package solaire-mode
  :hook (after-init-hook . solaire-global-mode))

;; TODO: check out doom-flatwhite
(use-package doom-themes
  :after (solaire-mode)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :preface
  (defun load-dark-theme ()
    "Load the saved dark theme."
    (interactive)
    (disable-theme light-theme)
    (load-theme dark-theme t))
  (defun load-light-theme ()
    "Load the saved light theme."
    (interactive)
    (disable-theme dark-theme)
    (load-theme light-theme t))
  :init
  (defvar light-theme 'doom-gruvbox-light)
  (defvar dark-theme 'doom-nord)
  (defvar loaded-theme-p nil)
  :hook
  (server-after-make-frame-hook . (lambda ()
                                    (interactive)
                                    (unless loaded-theme-p
                                      (load-dark-theme)
                                      (setq loaded-theme-p t))))
  :config
  ;; (unless server-process
  ;;   (load-dark-theme))
  (doom-themes-org-config))

(use-package feebleline
  :disabled
  :config
  (feebleline-mode))

(use-package mood-line
  :config
  (mood-line-mode t))

(use-package page-break-lines
  :config
  (page-break-lines-mode))

(use-package centaur-tabs
  :disabled
  :hook
  (centaur-tabs-mode . centaur-tabs-headline-match)
  (centaur-tabs-mode . centaur-tabs-group-by-projectile-project)
  :config
  (centaur-tabs-mode)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "⏺"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'under
        centaur-tabs-style "rounded"
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-enable-ido-completion nil
        x-underline-at-descent-line t))

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
  :general
  (:states 'normal
   [remap evil-scroll-down] #'good-scroll-up-half-screen
   [remap evil-scroll-up] #'good-scroll-down-half-screen))

(use-package rainbow-delimiters
  :disabled
  :hook ((prog-mode-hook       . rainbow-delimiters-mode)))
;; (emacs-lisp-mode-hook . (lambda () (rainbow-delimiters-mode -1)))
;; (clojure-mode-hook    . (lambda () (rainbow-delimiters-mode -1)))
;; (hy-mode-hook         . (lambda () (rainbow-delimiters-mode -1)))
;; (sly-mode-hook        . (lambda () (rainbow-delimiters-mode -1)))
;; (lisp-mode-hook       . (lambda () (rainbow-delimiters-mode -1)))
;; (scheme-mode-hook     . (lambda () (rainbow-delimiters-mode -1)))
;; (racket-mode-hook     . (lambda () (rainbow-delimiters-mode -1)))))

(use-package highlight-parentheses
  :disabled
  :custom
  (highlight-parentheses-colors (mapcar #'doom-color '(red orange yellow magenta)))
  :hook ((emacs-lisp-mode-hook
          hy-mode-hook
          clojure-mode-hook
          sly-mode-hook
          lisp-mode-hook
          scheme-mode-hook
          racket-mode-hook)
         .
         highlight-parentheses-mode))

(use-package prism
  :preface
  (defun nice-prism-colors (&rest _)
    (interactive)
    (prism-set-colors
     :lightens '(0)
     :desaturations `(,(if (member light-theme custom-enabled-themes) 0 7.5))
     :colors (mapcar #'doom-color '(red blue magenta green cyan))))
  :custom
  (prism-parens t)
  :config
  (advice-add #'load-light-theme :after #'nice-prism-colors)
  (advice-add #'load-dark-theme  :after #'nice-prism-colors)
  :hook
  ((lisp-mode-hook clojure-mode-hook) . prism-mode)
  (prism-mode-hook . nice-prism-colors))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'bitmap)
  ;; (highlight-indent-guides-responsive 'stack)
  ;; (highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)
  )

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
  (global-hl-todo-mode t))

(use-package git-gutter
  :custom
  (git-gutter:window-width 2)
  (git-gutter:update-interval 1)
  (git-gutter:ask-p nil)
  :config
  (global-git-gutter-mode t))

(use-package git-gutter-fringe
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

(use-package dired
  :straight (:type built-in)
  :hook (dired-mode-hook . auto-revert-mode)
  :custom
  (dired-listing-switches "-alhg")
  (dired-auto-revert-buffer t "don't prompt to revert; just do it")
  (dired-dwim-target t "suggest a target for moving/copying intelligently")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always "always copy/delete recursively")
  (dired-recursive-deletes 'top))

(use-package diredfl
  :hook (dired-mode-hook . diredfl-mode))

(use-package dired-hacks
  :general
  (:keymaps 'dired-mode-map
   "C-c C-d" #'dired-create-directory
   "C-c C-f" #'dired-create-empty-file
   "C-c C-/" #'dired-narrow-fuzzy
   "C-c /"   #'dired-narrow-fuzzy
   "<tab>"   #'dired-subtree-toggle)
  :config
  (advice-add #'dired-subtree-toggle :after (lambda (&rest _) (interactive) (revert-buffer)))
  (dired-async-mode))

;; TODO: That looks interesting
(use-package objed
  :disabled
  :general
  (:keymaps 'objed-mode-map
   "<escape>" #'objed-activate)
  (:keymaps 'objed-map
   "C-e" #'avy-goto-word-1
   "w" #'objed-forward-word
   "W" #'objed-forward-symbol
   "b" #'objed-backward-word
   "B" #'objed-backward-symbol
   "j" #'objed-next-line
   "k" #'objed-previous-line
   "f" #'objed-forward-word
   "c" #'objed-del-insert
   "i" #'objed-quit))

(use-package kakoune
  :disabled
  :custom
  (ryo-modal-cursor-type bar)
  :general
  ("<escape>" #'ryo-modal-mode)
  (:keymaps 'ryo-modal-mode-map
   "u" #'undo-fu-only-undo
   "U" #'undo-fu-only-redo
   "C-r" #'undo-fu-only-redo
   "v" #'er/expand-region)
  :config
  (kakoune-setup-keybinds))

(use-package evil
  :hook (after-change-major-mode-hook . (lambda () (modify-syntax-entry ?_ "w")))
  :demand t
  :preface
  (defun split-window-right+switch ()
    (interactive)
    (split-window-right)
    (other-window 1))
  (defun split-window-below+switch ()
    (interactive)
    (split-window-below)
    (other-window 1))
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :custom
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-undo-system 'undo-fu)
  (evil-want-change-word-to-end t)
  (evil-want-C-d-scroll t)
  (evil-want-C-u-scroll t)
  (evil-move-beyond-eol t)
  :general
  (:states 'insert "C-k" nil)
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "s" #'split-window-right+switch
   "v" #'split-window-below+switch
   "b s" #'save-buffer
   "q" #'delete-window)
  :config
  (evil-mode))

(use-package evil-collection
  :demand t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; TODO: check out embrace
(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-embrace
  :custom
  (evil-embrace-show-help-p t)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package expand-region
  :general
  ("M-V" #'er/expand-region)
  (:states 'normal
   :keymaps 'override
   "v" #'er/expand-region)
  :custom
  (expand-region-smart-cursor t)
  (expand-region-contract-fast-key "M-v"))

(use-package smart-comment
  :general
  ("M-;" #'smart-comment))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode t))

(use-package evil-org
  :after org
  :hook ((org-mode-hook . evil-org-mode)
         (evil-org-mode-hook . evil-org-set-key-theme))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package avy
  :general
  (:states 'normal
   :keymaps 'override
   "g s" #'avy-goto-word-1
   "C-e" #'avy-goto-word-1
   "C-j" #'avy-goto-word-1
   "C-k" #'avy-goto-char-timer)
  :custom
  (avy-background t)
  (avy-keys (list ?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package evil-easymotion
  :demand t
  :config
  (fset 'evilem-map evilem-map)
  :general
  (:states 'normal
   [remap evil-find-char] #'evilem-motion-find-char
   [remap evil-find-char-backward] #'evilem-motion-find-char-backward
   [remap evil-find-char-to] #'evilem-motion-find-char-to
   [remap evil-find-char-to-backward] #'evilem-motion-find-char-to-backward
   "s" #'evilem-map)
  (:states 'normal
   :keymaps 'evil-cleverparens-mode-map
   "s" #'evilem-map))

;; That's weird with redos sometimes
(use-package undo-fu
  :general
  (:states 'normal
   :keymaps 'override
   "u" #'undo-fu-only-undo
   "U" #'undo-fu-only-redo))

(use-package aggressive-indent
  :hook (lisp-mode-hook . aggressive-indent-mode))

(use-package hungry-delete
  :hook (prog-mode-hook . hungry-delete-mode))

(use-package ws-butler
  :config
  (ws-butler-global-mode t))

(use-package popper
  :disabled
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "b P" #'popper-toggle-type
   "b p" #'popper-toggle-latest
   "b C-p" #'popper-cycle)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode)
  (popper-echo-mode))

(use-package dashboard
  :custom
  (show-week-agenda-p t)
  (dashboard-set-heading-icons t)
  (dashboard-startup-banner 3)
  (dashboard-set-navigator t)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook))

(use-package olivetti
  :custom
  (olivetti-body-width 120)
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "n o" #'olivetti-mode))

(use-package smartparens
  :hook ((prog-mode-hook . smartparens-mode)
         (prog-mode-hook . show-smartparens-mode))
  :config
  (require 'smartparens-config))

(use-package dumbparens
  :disabled ; TODO: check this out instead of cleverparens some day
  :hook (prog-mode-hook . dumbparens-mode))

(use-package highlight-sexp
  :disabled
  :hook ((clojure-mode-hook . highlight-sexp-mode)
         (hy-mode-hook . highlight-sexp-mode)
         (emacs-lisp-mode-hook . highlight-sexp-mode)
         (common-lisp-mode-hook . highlight-sexp-mode)
         (scheme-mode-hook . highlight-sexp-mode)
         (lisp-mode-hook . highlight-sexp-mode)
         (racket-mode-hook . highlight-sexp-mode))
  :straight (highlight-sexp
             :repo "daimrod/highlight-sexp"
             :host github))

(use-package evil-cleverparens
  :hook ((clojure-mode-hook . evil-cleverparens-mode)
         (hy-mode-hook . evil-cleverparens-mode)
         (emacs-lisp-mode-hook . evil-cleverparens-mode)
         (common-lisp-mode-hook . evil-cleverparens-mode)
         (scheme-mode-hook . evil-cleverparens-mode)
         (lisp-mode-hook . evil-cleverparens-mode)
         (racket-mode-hook . evil-cleverparens-mode))
  :custom
  (evil-cp-additional-bindings (remove
                                '("M-O" . evil-cp-open-above-form)
                                (remove '("M-o" . evil-cp-open-below-form)
                                        evil-cp-additional-bindings)))
  (evil-cleverparens-use-additional-bindings t)
  :config
  (require 'evil-cleverparens-text-objects))

(use-package symex
  :disabled
  :config
  (symex-initialize)
  :general
  ;; evil normal state <=> symex state on escape
  (:states 'normal
   :keymaps '(clojure-mode-map lisp-mode-map emacs-lisp-mode-map)
   "<escape>" #'symex-mode-interface))

(use-package ansi-color
  :straight (:type built-in)
  :preface
  (defun colorize-compilation-buffer ()
    (read-only-mode)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode))
  :config
  (add-hook #'compilation-filter-hook #'colorize-compilation-buffer))

(use-package xterm-color)

(use-package shell
  :straight (:type built-in)
  :demand
  :general
  (:keymaps 'shell-mode-map
   "C-l" #'comint-clear-buffer)
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "n s" #'shell))

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
  :general
  (:keymaps 'eshell-mode-map
   "C-l" #'eshell/clear-buffer
   "C-r" #'eshell-isearch-backward      ; TODO: does that remap redo?
   "C-s" #'eshell-isearch-forward)
  (:states 'normal
   :prefix leader-key
   :keymaps 'override
   "e"   #'eshell
   "n e" #'eshell-new)
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))

(use-package eshell-up)

(use-package pcmpl-args)

(use-package bash-completion)

(use-package fish-completion
  :if (executable-find "fish")
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
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "n v" #'vterm))

(use-package treemacs
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "n t" (lambda ()
           (interactive)
           (treemacs-add-and-display-current-project)
           (treemacs-display-current-project-exclusively))))

(use-package treemacs-evil)

(use-package treemacs-magit)

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme 'all-the-icons))

(use-package treemacs-projectile)

(use-package org-mime)

(use-package apheleia
  :hook ((clojure-mode-hook haskell-mode-hook) . apheleia-mode)
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "b f" #'apheleia-format-buffer)
  :config
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-l 180"))
  (add-to-list 'apheleia-formatters '(cljstyle . ("cljstyle" "pipe")))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljstyle)))

(use-package elfeed
  :config
  (let ((path "~/.dotfiles/emacs/elfeed-local-feed.el"))
    (when (file-exists-p path)
      (load path)))
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "n f" #'elfeed))

(use-package elpher
  :custom
  (elpher-gemini-link-string "> ")
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "n g" #'elpher))

(use-package erc
  :custom
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (auth-sources '("~/.authinfo.gpg"
                  "~/.authinfo"
                  "~/.netrc"))
  :config
  (erc-services-mode 1)
  (erc-update-modules))

(use-package magit
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "m s" #'magit-status
   "m m" #'magit-status
   ;; "m b" #'magit-blame
   "m c" #'magit-clone))

(use-package magit-todos
  :demand t
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "m l" #'magit-todos-list)
  :config
  (magit-todos-mode))

(use-package magit-delta
  :hook (magit-mode-hook . magit-delta-mode))

(use-package forge
  :after magit)

(use-package blamer
  :straight (blamer :host github
                    :repo "Artawower/blamer.el")
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "m b" #'blamer-mode))

(use-package browse-at-remote)

(use-package projectile
  :custom
  (projectile-project-search-path '("~/Code/"))
  :config
  (projectile-mode t)
  :general
  (:keymaps 'projectile-mode-map
   "C-c p" 'projectile-command-map))

(use-package wgrep
  :custom
  (wgrep-enable-key "r"))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package selectrum-prescient)

(use-package selectrum
  :general
  ("C-c C-b"  #'selectrum-repeat
   "C-c b"    #'selectrum-repeat)
  (:keymaps    'selectrum-minibuffer-map
   "<escape>" #'keyboard-quit
   "C-k"      #'selectrum-previous-candidate
   "C-j"      #'selectrum-next-candidate)
  :custom
  (selectrum-files-select-input-dirs t)
  (selectrum-quick-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (magit-completing-read-function #'selectrum-completing-read)
  :config
  (selectrum-prescient-mode t)
  (selectrum-mode t))

(use-package consult
  :custom
  (consult-line-start-from-top t)
  :general
  ("C-x b" #'consult-buffer)
  (:states 'normal
   "/" #'consult-line
   "?" #'consult-line)
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "a" #'consult-ripgrep
   "r" #'consult-recent-file
   "i" #'consult-imenu
   "b b" #'consult-buffer))

(use-package consult-flycheck
  :after flycheck
  :straight (consult-flycheck :host github
                              :repo "minad/consult-flycheck")
  :general
  (:states 'normal
   :prefix leader-key
   "b e" #'consult-flycheck))

(use-package consult-projectile
  :after (consult projectile)
  :custom
  (consult-project-root-function 'projectile-project-root)
  :straight (consult-projectile :type git
                                :host gitlab
                                :repo "OlMon/consult-projectile")
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "<SPC>" #'consult-projectile))

(use-package marginalia
  :demand
  :general
  (:keymaps 'minibuffer-map
   "M-a" #'marginalia-cycle)
  :config
  (marginalia-mode t))

(use-package embark
  :after marginalia
  :demand
  :preface
  ;; Cider integration
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
  ;; Straight integration
  (embark-define-keymap embark-straight-map
    "Keymar for actions on straight packages"
    ("u" straight-visit-package-website)
    ("r" straight-rebuild-package)
    ("i" straight-use-package)
    ("c" straight-check-package)
    ("F" straight-pull-package-and-deps)
    ("f" straight-fetch-package)
    ("p" straight-push-package)
    ("n" straight-normalize-package)
    ("m" straight-merge-package))
  (add-to-list 'embark-keymap-alist '(straight . embark-straight-map))
  (add-to-list 'marginalia-prompt-categories '("recipe\\|package" . straight))
  :general
  ("C-q" #'embark-dwim
   "C-;" #'embark-act)
  (:keymaps 'selectrum-minibuffer-map
   "C-e" #'embark-export
   "C-s" #'embark-collect-snapshot
   "C-l" #'embark-collect-live
   "C-:" #'embark-dwim
   "C-;" #'embark-act
   "C-o" #'embark-act
   "M-o" #'embark-act)
  (:keymaps 'embark-general-map
   "y" #'kill-new))

(use-package embark-consult
  :demand t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package topsy
  :straight (topsy :type git
                   :host github
                   :repo "alphapapa/topsy.el")
  :hook (prog-mode-hook . topsy-mode))

(use-package direnv)

(use-package org-superstar
  :after org
  :hook (org-mode-hook . org-superstar-mode))

;; TODO: check out multiple cursors
(use-package iedit
  :disabled)

(use-package parinfer-rust-mode
  :hook ((clojure-mode-hook
          hy-mode-hook
          emacs-lisp-mode-hook
          common-lisp-mode-hook
          scheme-mode-hook
          lisp-mode-hook
          racket-mode-hook)
         . parinfer-rust-mode)
  :disabled
  :custom
  (parinfer-rust-auto-download t))
;; (parinfer-rust-troublesome-modes nil))

(use-package paren-face)

(use-package smart-tabs-mode
  :hook (c-mode-common-hook . (lambda ()
                                (setq-local indent-tabs-mode t)))
  :config
  (smart-tabs-insinuate 'c 'c++))

(use-package yasnippet
  :config
  (yas-reload-all)
  (yas-global-mode t))

(use-package dabbrev
  :straight (:type built-in)
  :general
  ("M-/"   #'dabbrev-completion
   "C-M-/" #'dabbrev-expand))

(use-package company
  :demand t
  ;; TODO: cider + eshell
  ;; :hook (prog-mode-hook . company-mode)
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
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
  (company-backends '(company-capf))
  (company-auto-commit nil)
  (company-auto-commit-chars nil)
  :config
  (global-company-mode)
  :general
  (:keymaps 'company-active-map
   "RET" #'company-complete-selection
   "<ret>" #'company-complete-selection))

(use-package corfu
  :disabled
  :custom
  (corfu-auto-prefix 2)
  (corfu-cycle t)              ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)               ;; Enable auto completion
  (corfu-commit-predicate t)   ;; Commit selected candidates on next input
  (corfu-quit-at-boundary nil) ;; Automatically quit at word boundary
  (corfu-quit-no-match t)      ;; Automatically quit if there is no match
  ;; (corfu-echo-documentation nil)) ;; Do not show documentation in the echo area
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :general
  (:keymaps 'corfu-map
   "C-j"     #'corfu-next
   "C-k"     #'corfu-previous
   "TAB"     #'corfu-next
   [tab]     #'corfu-next
   "S-TAB"   #'corfu-previous
   [backtab] #'corfu-previous)
  :demand
  :config
  (corfu-global-mode t))

(use-package company-tabnine
  ;; :disabled
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package yasnippet-snippets)

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom (dumb-jump-default-project "~/Code"))

(use-package flycheck
  :custom
  (flycheck-indication-mode 'right-fringe)
  :config
  ;; TODO: fancy way to set :working-directory for existing checkers
  (global-flycheck-mode t)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)
  :general
  ("C-c C-e" #'flycheck-next-error))

(use-package flycheck-inline
  :hook (flycheck-mode-hook . flycheck-inline-mode))

(use-package flycheck-pos-tip
  :disabled
  :custom
  (flycheck-pos-tip-timeout 0)
  :config
  (flycheck-pos-tip-mode t))

(use-package lsp-mode
  :custom
  (lsp-semantic-highlighting t)
  (lsp-enable-symbol-highlighting t)
  (lsp-lens-enable t)
  (lsp-prefer-capf t)
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.750)
  (lsp-headerline-breadcrumb-enable nil)
  (read-process-output-max (* 1024 1024))
  :config
  (lsp-register-custom-settings '(("pylsp.plugins.pylsp_mypy.enabled" t t)))
  :general
  (:states 'normal
   :prefix leader-key
   "l l" #'lsp)
  (:states 'normal
   :keymaps 'lsp-mode-map
   :prefix leader-key
   "l c" #'lsp-treemacs-call-hierarchy
   "l n" #'lsp-rename
   "l k" #'lsp-describe-thing-at-point
   "l f" #'lsp-format-buffer
   "l d" #'lsp-find-definition
   "l t" #'lsp-find-type-definition
   "l r" #'lsp-find-references
   "l i" #'lsp-find-implementation
   "l a" #'lsp-execute-code-action
   "l g" #'lsp-avy-lens)
  (:states 'normal
   :keymaps 'lsp-mode-map
   "S-k" #'lsp-describe-thing-at-point
   [remap evil-lookup] #'lsp-describe-thing-at-point))

(use-package lsp-ui)

;; TODO: check out structured-haskell-mode
(use-package haskell-mode
  ;; do I need to enable interactive-haskell-mode manually?
  :hook
  (haskell-mode-hook . haskell-indentation-mode)
  (haskell-mode-hook . interactive-haskell-mode)
  :bind (:map haskell-mode-map
         ("C-c c"   . haskell-compile)
         ("C-c C-p" . haskell-check))
  :custom
  (haskell-compile-cabal-build-command "stack build"))

(use-package shakespeare-mode)

(use-package hlint-refactor
  :hook (haskell-mode-hook . hlint-refactor-mode))

(use-package lsp-haskell
  :after lsp-mode
  ;; :custom
  ;; (lsp-haskell-formatting-provider "ormolu")
  )

(use-package tuareg)

(use-package reason-mode)

(use-package idris-mode)

(use-package python-x
  :bind (:map inferior-python-mode-map
         ("C-l" . comint-clear-buffer))
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt --pprint")
  :config
  (python-x-setup))

(use-package cython-mode)

(use-package py-isort
  :hook (before-save-hook . py-isort-before-save)
  :custom
  (py-isort-options '("-l=180" "-m=3" "--tc")))

(use-package python-black
  :hook (python-mode-hook . python-black-on-save-mode)
  :custom
  (python-black-extra-args '("-l 180")))

(use-package pyvenv)

(use-package auto-virtualenv
  :hook (python-mode-hook . auto-virtualenv-set-virtualenv))

(use-package highlight-defined
  :hook (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package eros
  :hook (emacs-lisp-mode-hook . eros-mode))

(use-package sly
  :demand
  :general
  (:keymaps 'sly-mode-map
   "C-c M-i" #'sly-inspect)
  :custom
  (sly-complete-symbol-fuction 'sly-simple-completions)
  (inferior-lisp-program "sbcl")) ;; TODO: ccl

(use-package sly-quicklisp)

(use-package geiser)

(use-package geiser-guile)

(use-package web-mode
  :hook (web-mode-hook . (lambda ()
                           (setq web-mode-markup-indent-offset 2
                                 web-mode-css-indent-offset 2
                                 web-mode-code-indent-offset 2))))

(use-package rjsx-mode
  :mode "\\.jsx?$")

(use-package typescript-mode
  :mode "\\.tsx?$")

(use-package purescript-mode
  :hook (purescript-mode-hook . turn-on-purescript-indentation))

(use-package psc-ide
  :hook (purescript-mode-hook . psc-ide-mode))

(use-package hy-mode
  :bind (:map inferior-hy-mode-map
         ("C-l" . comint-clear-buffer))
  :demand
  :custom
  (hy-jedhy--enable? nil)
  (hy-shell--interpreter-args '("--repl-output-fn" "hy.contrib.hy-repr.hy-repr"))
  :config
  (add-to-list 'hy-indent--exactly "lfor")
  (add-to-list 'hy-indent--exactly "sfor")
  (add-to-list 'hy-indent--exactly "dfor"))

(use-package scala-mode)

(use-package lsp-metals
  :straight (lsp-metals :host github
                        :repo "emacs-lsp/lsp-metals")
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")))

(use-package kotlin-mode)

(use-package flycheck-kotlin
  :config
  (flycheck-kotlin-setup))

(use-package clojure-mode)

(use-package flycheck-clj-kondo)

(use-package kibit-helper)

(use-package cider
  :bind (:map cider-repl-mode-map
         ("C-l" . cider-repl-clear-buffer)
         :map cider-mode-map
         ("C-c M-c" . cider-debug-defun-at-point))
  :custom
  (cider-repl-display-help-banner nil)
  :config
  (advice-add 'cider-find-var :before (lambda (&rest r) (evil-set-jump))))

(use-package sayid
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

(use-package elm-mode
  :hook (elm-mode-hook . elm-format-on-save-mode))

(use-package flycheck-elm
  :after flycheck
  :hook (flycheck-mode-hook . flycheck-elm-setup))

(use-package rust-mode
  :bind (:map rust-mode-map
         ("C-c C-p" . rust-run-clippy)
         ("C-c C-c" . rust-run))
  :custom
  (rust-format-on-save t))

(use-package racket-mode
  :bind (:map racket-mode-map
         ("C-c C-c" . racket-run)
         ("C-c C-r" . racket-send-region)))

(use-package erlang-start
  :load-path "/usr/lib/erlang/lib/tools-3.5.1/emacs/"
  :straight nil
  :custom
  (erlang-root-dir "/usr/lib/erlang/")
  (exec-path (cons "/usr/lib/erlang/bin" exec-path))
  (erlang-man-root-dir "/usr/lib/erlang/man"))

(use-package elixir-mode)

(use-package gdscript-mode
  :custom
  (gdscript-godot-executable "godot-mono"))

(use-package cmake-mode)

(use-package cmake-font-lock)

(use-package jinja2-mode)

(use-package markdown-mode)

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package tree-sitter
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (push '(clojure-mode . clojure) tree-sitter-major-mode-language-alist)
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

;; TODO: check out
(use-package tree-edit
  :disabled
  :straight (:host github
             :repo "ethan-leba/tree-edit"))

(use-package fira-code-mode
  :custom
  (fira-code-mode-disabled-ligatures '("[]" "x" "===" "!=="))
  :hook (prog-mode-hook . fira-code-mode))

(use-package pdf-tools
  :hook (pdf-view-mode-hook . pdf-view-dark-minor-mode)
  :custom
  (pdf-view-display-page 'fit-page)
  :config
  (pdf-tools-install))

(use-package tex-site
  :straight auctex
  :bind ("M-q" . align-current)
  :hook ((LaTeX-mode-hook . LaTeX-math-mode)
         (LaTeX-mode-hook . flyspell-mode)
         (LaTeX-mode-hook . turn-on-reftex)
         (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))
  :custom
  ;; TODO: make it use the emacs itself
  (TeX-view-program-list '(("zathura" "zathura --page=%(outpage) %o")))
  (TeX-view-program-selection '((output-pdf "Zathura")))
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package which-key
  :config
  (which-key-mode t))

(use-package telega
  :straight (telega :branch "master")
  :hook
  (telega-load-hook . telega-notifications-mode)
  :custom
  (telega-auto-download '((photo         opened)
                          (video         opened)
                          (file          opened)
                          (voice-message opened)
                          (video-message opened)
                          (web-page      opened)
                          (instant-view  opened)))
  (telega-completing-read-function 'completing-read)
  (telega-use-images t)
  ;; (telega-symbol-attachment "")
  (telega-root-default-view-function #'telega-view-compact)
  (telega-use-docker nil)
  ;; (telega-symbol-telegram nil)
  (telega-chat-input-markups '("markdown1" nil "markdown2"))
  :bind-keymap
  ("C-c t" . telega-prefix-map)
  :bind
  (:map telega-msg-button-map
   ("q" . telega)
   ("S-d" . telega-msg-delete-marked-or-at-point)
   ("k" . evil-previous-line)
   ("l" . evil-forward-char)
   ;; :map telega-root-mode-map
   ;; ("C-c C-f" . telega-filter-by-name)
   ;; ("C-c C-t" . telega-filter-by-type)
   ;; ("g r" . telega-filters-reset)
   ))

;; should be deferred in case of emacs server
(use-package screenshot
  :general
  (:states 'normal "M-p" #'screenshot)
  :straight (screenshot :host github
                        :type git
                        :repo "tecosaur/screenshot"
                        :files ("*.el")))

(use-package stumpwm-mode)

(use-package activity-watch-mode
  :demand t
  :config
  (global-activity-watch-mode))

(use-package activity-watch-visualize
  :straight (activity-watch-visualize :host github
                                      :repo "arjaz/activity-watch-visualize")
  :general
  (:states 'normal
   :keymaps 'override
   :prefix leader-key
   "b a" #'activity-watch-visualize-as-org))

(use-package cyrillic-dvorak-im
  :straight (cyrillic-dvorak-im :repo "xFA25E/cyrillic-dvorak-im"
                                :host github))

(use-package selected
  :general
  (:keymaps 'selected-keymap
   "<tab>" #'indent-region)
  :demand t
  :config
  (selected-global-mode))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package nano-modeline
  :disabled
  :straight (nano-modeline :host github
                           :repo "rougier/nano-modeline"))

(provide 'init)
;;; init ends here
