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
  :straight nil
  :custom (use-package-hook-name-suffix nil))

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-check-for-modifications '(watch-files find-when-checking)))

;; TODO: split that into blocks
(use-package emacs
  :straight nil
  :custom
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
  ;; Smooth scrolling
  ;; move minimum when cursor exits view, instead of recentering
  (scroll-conservatively 101)
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
  :straight nil
  :config
  (setq-default fill-column 80)
  (global-display-fill-column-indicator-mode t))

(use-package frame
  :straight nil
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'bottom-only)
  :config
  (window-divider-mode t)
  (blink-cursor-mode 0))

(use-package faces
  :straight nil
  :config
  ;; Fonts
  (defvar arjaz/font "Iosevka")
  (add-to-list 'default-frame-alist `(font . ,arjaz/font))
  (set-face-attribute 'default nil :height 150)
  (set-frame-font arjaz/font))

(use-package cus-edit
  :straight nil
  :custom
  (custom-file (concat user-emacs-directory "garbage.el")))

(use-package autorevert
  :straight nil
  :custom
  (auto-revert-interval 2)
  :config
  ;; Automaticaly revert changes
  (global-auto-revert-mode t))

(use-package bookmark
  :straight nil
  :custom
  (bookmark-fontify nil))

(use-package cc-vars
  :straight nil
  :hook (c++-mode-hook . (lambda ()
                           (electric-pair-local-mode 0)
                           (setq-local fill-column 100)))
  :custom
  (c-default-style "k&r")
  (c-basic-offset 4)
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode)))

(use-package display-line-numbers
  :straight nil
  :disabled
  :custom
  (display-line-numbers-type 'relative "relative line numbers")
  :hook ((prog-mode-hook text-mode-hook conf-mode-hook eshell-mode-hook)
         . display-line-numbers-mode))

(use-package hl-line
  :straight nil
  :config
  ;; Highlight current line
  (global-hl-line-mode t))

(use-package files
  :straight nil
  :custom
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

(use-package gcmh
  :demand
  :custom
  (gcmh-high-cons-threshold (/ 1073741824 2))
  :config
  (gcmh-mode 1))

;; TODO: check this out
(use-package system-packages)

(use-package so-long
  :config
  (global-so-long-mode))

(use-package ibuffer
  :straight nil
  :bind ("C-x C-b" . ibuffer)
  :custom
  (add-to-list 'ibuffer-never-show-predicates "^\\*"))

(use-package ibuf-ext
  :straight nil)

(use-package dash)

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;; TODO: move that back to hy-mode, load org config properly
(use-package ob-hy)

(use-package org
  :hook (org-babel-after-execute-hook . org-redisplay-inline-images)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
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
  (org-startup-indented t)
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
     (python . t)
     (hy . t))))

(use-package solaire-mode
  :hook (after-init-hook . solaire-global-mode))

(use-package tree-sitter
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (push '(clojure-mode . clojure) tree-sitter-major-mode-language-alist)
  (push '(haskell-mode . haskell) tree-sitter-major-mode-language-alist)
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(use-package fira-code-mode
  :if (-contains? default-frame-alist '(font . "Fira Code"))
  :custom
  (fira-code-mode-disabled-ligatures '("[]" "x" "===" "!=="))
  :config
  (global-fira-code-mode))

(use-package doom-themes
  :after (solaire-mode)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :init
  (setq arjaz/theme 'doom-nord)
  (defvar arjaz/loaded-theme nil)
  :hook (server-after-make-frame-hook . (lambda ()
                                          (unless arjaz/loaded-theme
                                            (setq arjaz/loaded-theme t)
                                            (load-theme arjaz/theme t))))
  :config
  (load-theme arjaz/theme t)
  (doom-themes-org-config))

(use-package bespoke-themes
  :straight (bespoke-themes :host github
                            :repo "mclear-tools/bespoke-themes")
  :disabled
  :config
  ;; Set header line
  (setq bespoke-set-mode-line 'header)
  ;; Set evil cursor colors
  (setq bespoke-set-evil-cursors t)
  ;; Use mode line visual bell
  (setq bespoke-set-visual-bell t)
  ;; Set use of italics
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t)
    ;; Set initial theme variant
  (setq bespoke-set-theme 'dark)
  ;; Load theme
  (load-theme 'bespoke t))

(use-package doom-modeline
  :disabled
  ;; I guess it's what I use with the daemon
  :hook ((after-init-hook . doom-modeline-mode)
         (doom-modeline-mode-hook . column-number-mode))
  :custom
  (doom-modeline-icon t)
  (doom-modeline-project-detection 'project)
  (doom-modeline-modal-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-lsp t))
  ;; And that's used without a daemon
  ;; (doom-modeline-mode 1))

(use-package feebleline
  :disabled
  :config
  (feebleline-mode))

(use-package mood-line
  :config
  (mood-line-mode t))

(use-package rainbow-delimiters
  :hook ((prog-mode-hook       . rainbow-delimiters-mode)
         (emacs-lisp-mode-hook . (lambda () (rainbow-delimiters-mode -1)))
         (clojure-mode-hook    . (lambda () (rainbow-delimiters-mode -1)))
         (hy-mode-hook         . (lambda () (rainbow-delimiters-mode -1)))
         (sly-mode-hook        . (lambda () (rainbow-delimiters-mode -1)))
         (lisp-mode-hook       . (lambda () (rainbow-delimiters-mode -1)))
         (scheme-mode-hook     . (lambda () (rainbow-delimiters-mode -1)))
         (racket-mode-hook     . (lambda () (rainbow-delimiters-mode -1)))))

(use-package highlight-parentheses
  :custom
  ;; TODO: is there a way to query current theme colors?
  (highlight-parentheses-colors (-map #'doom-color
                                      '(red orange yellow magenta)))
  :hook ((emacs-lisp-mode-hook
          hy-mode-hook
          clojure-mode-hook
          sly-mode-hook
          lisp-mode-hook
          scheme-mode-hook
          racket-mode-hook)
         . highlight-parentheses-mode))

(use-package prism
  :hook ((clojure-mode-hook lisp-mode-hook) . prism-mode)
  :config
  (prism-set-colors
    :lightens '(0)
    :desaturations '(0)
    :colors (-map #'doom-color
                  '(yellow blue magenta green cyan))))

(use-package highlight-indent-guides
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))

(use-package highlight-numbers
  :hook (prog-mode-hook . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode-hook . hes-mode))

(use-package all-the-icons
  :demand
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.tsx$" all-the-icons-fileicon "typescript" :height 1.0 :v-adjust -0.1 :face all-the-icons-blue-alt))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.hy" all-the-icons-fileicon "scheme" :height 1.2 :face all-the-icons-red)))

(use-package all-the-icons-dired
  :demand
  :config
  :hook (dired-mode-hook . (lambda ()
                             (interactive)
                             (unless (file-remote-p default-directory)
                               (all-the-icons-dired-mode t)))))

(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
   `(;; For things that need to be done, just not today.
     ("TODO" warning bold)
     ;; For problems that will become bigger problems later if not
     ;; fixed ASAP.
     ("FIXME" error bold)
     ;; For tidbits that are unconventional and not intended uses of the
     ;; constituent parts, and may break in a future update.
     ("HACK" font-lock-constant-face bold)
     ;; For things that were done hastily and/or hasn't been thoroughly
     ;; tested. It may not even be necessary!
     ("REVIEW" font-lock-keyword-face bold)
     ;; For especially important gotchas with a given implementation,
     ;; directed at another user other than the author.
     ("NOTE" success bold)
     ;; For things that just gotta go and will soon be gone.
     ("DEPRECATED" font-lock-doc-face bold)
     ;; For a known bug that needs a workaround
     ("BUG" error bold)
     ;; For warning about a problematic or misguiding code
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

(use-package evil
  :hook (after-change-major-mode-hook . (lambda () (modify-syntax-entry ?_ "w")))
  :demand
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
  :config
  (evil-mode t))

(use-package evil-leader
  :custom
  (evil-leader/in-all-states 1)
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)
  (evil-leader/set-key
    ;; Windows
    "w b" 'evil-window-bottom-right
    "w t" 'evil-window-top-left
    "w h" 'evil-window-left
    "w j" 'evil-window-down
    "w k" 'evil-window-up
    "w l" 'evil-window-right
    "w o" 'delete-other-windows
    "v" 'evil-window-vsplit
    "h" 'evil-window-split
    "q" 'evil-quit

    ;; Spawning stuff
    "n t" 'terminal-here-launch
    "n m" 'mu4e
    "n v" 'vterm
    "n e" 'eshell-new
    "n f" 'elfeed
    "n g" 'elpher
    "n o" 'olivetti-mode

    ;; Lsp
    "l l" 'lsp
    ;; "l e" 'lsp-ui-flycheck-list
    "l c" 'lsp-treemacs-call-hierarchy
    "l n" 'lsp-rename
    "l s" 'lsp-describe-thing-at-point
    "l f" 'lsp-format-buffer
    "l d" 'lsp-find-definition
    "l t" 'lsp-find-type-definition
    "l r" 'lsp-find-references
    "l i" 'lsp-find-implementation
    "l a" 'lsp-execute-code-action
    ;; "l m" 'lsp-ui-imenu
    "l g" 'lsp-avy-lens

    "k" 'kill-current-buffer

    "f" 'apheleia-format-buffer

    ;; Magit bindings
    "m s" 'magit-status
    "m m" 'magit-status
    "m b" 'magit-blame
    "m c" 'magit-clone

    ;; Eshell
    "e" 'eshell

    ;; Search
    "a" 'consult-ripgrep
    "i" 'consult-imenu

    ;; Projectile
    "<SPC>" 'consult-projectile

    ;; Moving
    "r" 'consult-recent-file
    "b" 'consult-buffer
    "o" 'find-file
    "g" 'consult-bookmark
    "d" 'treemacs))

(use-package evil-indent-plus)

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

;; TODO: look at expand-region
(use-package evil-embrace
  :custom
  (evil-embrace-show-help-p nil)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-args
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-quickscope
  :config
  (global-evil-quickscope-mode t))

(use-package evil-collection
  :custom
  (evil-collection-company-use-tng nil)
  :after (evil vterm)
  :config
  (evil-collection-init))

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
  :bind (:map evil-normal-state-map
              ("C-e" . avy-goto-word-1)
              ("C-j" . avy-goto-word-1)
              ("C-k" . avy-goto-char-timer))
  :custom
  (avy-keys (list ?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package evil-easymotion
  :config
  (evilem-default-keybindings ";"))

(use-package undo-fu
  :after evil
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package aggressive-indent
  :disabled
  :hook (prog-mode-hook . aggressive-indent-mode))

(use-package hungry-delete
  :disabled
  :hook (prog-mode-hook . hungry-delete-mode))

(use-package ws-butler
  :config
  (ws-butler-global-mode t))

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

(use-package eyebrowse
  :disabled
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))

(use-package olivetti
  :custom
  (olivetti-body-width 100))

(use-package smartparens
  :hook ((prog-mode-hook . smartparens-mode)
         (show-smartparens-mode-hook
          .
          (lambda ()
            (interactive)
            (set-face-attribute
              'sp-show-pair-match-content-face
              nil
              :background
              (face-attribute 'sp-show-pair-match-face :background)))))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode))

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

(use-package free-keys)

(use-package xterm-color)

(use-package shell
  :straight nil
  :demand
  :bind (:map shell-mode-map
              ("C-l" . comint-clear-buffer)))

(use-package esh-mode
  :straight nil
  :demand
  :hook (eshell-before-prompt-hook
         .
         (lambda ()
           (setq-local xterm-color-preserve-properties t)))
  :custom
  (eshell-history-size 1024)
  (eshell-scroll-to-bottom-on-input nil)
  (eshell-hist-ignoredups t)
  :init
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
  ;; FIXME: it binds in all maps for some reason
  :bind (:map eshell-mode-map
              ("C-l" . eshell/clear-buffer)
              ;; FIXME: that's ugly
              ([remap undo-fu-only-redo] . eshell-isearch-backward)
              ("C-r" . eshell-isearch-backward)
              ("C-s" . eshell-isearch-forward))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))

(use-package eshell-up)

(use-package esh-autosuggest
  ;; FIXME: there's a conflict with company-tng and evil-integration
  ;; :disabled
  :demand
  :bind (:map esh-autosuggest-active-map
              ("C-e" . company-complete-selection))
  :hook (eshell-mode-hook . esh-autosuggest-mode))

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

(use-package vterm)

(use-package mu4e
  :demand
  :init
  (defun mu4e-revert-main ()
    (interactive)
    (when (s-contains? "*mu4e-main*" (buffer-name))
      (revert-buffer)))
  :hook (mu4e-context-changed-hook . mu4e-revert-main)
  :config
  (setq mu4e-root-maildir "~/Maildir"
        smtpmail-local-domain "gmail.com"
        smtpmail-default-smtp-server "smpt.gmail.com"
        smtpmail-smtp-server "smpt.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-queue-mail nil
        mu4e-attachment-dir "~/Downloads"
        ;; don't save message to Sent Messages, IMAP takes care of this
        mu4e-sent-messages-behavior 'delete
        ;; allow for updating mail
        mu4e-get-mail-command "mbsync -a"
        ;; something about ourselves
        mu4e-view-show-images t
        mu4e-view-prefer-html t
        mu4e-update-interval 180
        mu4e-headers-auto-update t
        mu4e-compose-signature-auto-include nil
        mu4e-compose-format-flowed t
        mu4e-view-image-max-width 800
        mu4e-change-filenames-when-moving t
        mu4e-compose-dont-reply-to-self t
        ;; don't keep message buffers around
        message-kill-buffer-on-exit t
        org-mu4e-convert-to-html t
        mu4e-confirm-quit nil
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'always-ask
        mu4e-contexts
        (list
         (make-mu4e-context
          :name "main"
          :enter-func (lambda () (mu4e-message "Entering main context"))
          :leave-func (lambda () (mu4e-message "Leaving main context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) "art6661322@gmail.com")))
          :vars '((user-mail-address . "art6661322@gmail.com")
                  (user-full-name . "Eugene Rossokha")
                  (mu4e-sent-folder . "/art-gmail/[art].Sent Mail")
                  (mu4e-drafts-folder . "/art-gmail/[art].drafts")
                  (mu4e-trash-folder . "/art-gmail/[art].Bin")
                  (smtpmail-queue-dir . "~/Maildir/art-gmail/queue/cur")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "art6661322")
                  (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                  (smtpmail-auth-credentials . "~/.authinfo.gpg")
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-debug-info . t)
                  (smtpmail-debug-verbose . t)
                  (mu4e-maildir-shortcuts . (("/art-gmail/INBOX"           . ?i)
                                             ("/art-gmail/[art].Sent Mail" . ?s)
                                             ("/art-gmail/[art].Bin"       . ?t)
                                             ("/art-gmail/[art].All Mail"  . ?a)
                                             ("/art-gmail/[art].Starred"   . ?r)
                                             ("/art-gmail/[art].drafts"    . ?d)))))
         (make-mu4e-context
          :name "work"
          :enter-func (lambda () (mu4e-message "Entering work context"))
          :leave-func (lambda () (mu4e-message "Leaving work context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) "eugene.rossokha@vacuumlabs.com")))
          :vars '((user-mail-address . "eugene.rossokha@vacuumlabs.com")
                  (user-full-name . "Eugene Rossokha")
                  (mu4e-sent-folder . "/vacuumlabs-gmail/[vacuumlabs].Sent Mail")
                  (mu4e-drafts-folder . "/vacuumlabs-gmail/[vacuumlabs].drafts")
                  (mu4e-trash-folder . "/vacuumlabs-gmail/[vacuumlabs].Bin")
                  (smtpmail-queue-dir . "~/Maildir/vacuumlabs-gmail/queue/cur")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "eugene.rossokha")
                  (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                  (smtpmail-auth-credentials . "~/.authinfo.gpg")
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-debug-info . t)
                  (smtpmail-debug-verbose . t)
                  (mu4e-maildir-shortcuts . (("/vacuumlabs-gmail/INBOX"                  . ?i)
                                             ("/vacuumlabs-gmail/[vacuumlabs].Sent Mail" . ?s)
                                             ("/vacuumlabs-gmail/[vacuumlabs].Bin"       . ?t)
                                             ("/vacuumlabs-gmail/[vacuumlabs].All Mail"  . ?a)
                                             ("/vacuumlabs-gmail/[vacuumlabs].Starred"   . ?r)
                                             ("/vacuumlabs-gmail/[vacuumlabs].drafts"    . ?d)))))))
  (require 'smtpmail)
  (require 'org-mu4e))

(use-package org-mime)

;; TODO: change for apheleia
(use-package format-all
  :disabled)

;; FIXME: it creates some tmp buffers and I lose the current one for ts
(use-package apheleia
  :hook
  ((clojure-mode-hook haskell-mode-hook) . apheleia-mode)
  :config
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-l 79"))
  (add-to-list 'apheleia-formatters '(cljstyle . ("cljstyle" "pipe")))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljstyle)))
  ;; (apheleia-global-mode t))

(use-package elfeed
  :config
  (load "~/.dotfiles/emacs/elfeed-local-feed.el"))

(use-package elfeed-goodies
  :disabled
  :config
  (elfeed-goodies/setup))

(use-package elpher
  :custom
  (elpher-gemini-link-string "> "))

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

(use-package git-commit
  :custom
  (git-commit-fill-column 50)
  (git-commit-style-convention-checks '(non-empty-second-line
                                        overlong-summary-line)))

(use-package magit)

(use-package magit-delta
  :hook (magit-mode-hook . magit-delta-mode))

(use-package forge
  :after magit)

(use-package browse-at-remote)

(use-package projectile
  :after (evil-leader)
  :custom
  (projectile-project-search-path '("~/Code/"))
  :config
  (evil-leader/set-key "p" projectile-command-map)
  (projectile-mode t))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package selectrum-prescient)

(use-package selectrum
  :bind (:map selectrum-minibuffer-map
              ("<escape>" . keyboard-quit)
              ("C-k" . selectrum-previous-candidate)
              ("C-j" . selectrum-next-candidate))
  :custom
  (magit-completing-read-function #'selectrum-completing-read)
  :config
  (selectrum-prescient-mode t)
  (selectrum-mode t))

(use-package ctrlf
  :config
  (ctrlf-mode t)
  (push '("C-j" . ctrlf-next-match) ctrlf-minibuffer-bindings)
  (push '("C-k" . ctrlf-previous-match) ctrlf-minibuffer-bindings)
  (push '("<escape>" . ctrlf-cancel) ctrlf-minibuffer-bindings)
  (define-key evil-normal-state-map (kbd "/") #'ctrlf-forward-default)
  (define-key evil-normal-state-map (kbd "?") #'ctrlf-backward-default))

(use-package consult)

(use-package consult-projectile
  :after (consult projectile)
  :custom
  (consult-project-root-function 'projectile-project-root)
  :straight (consult-projectile :type git
                                :host gitlab
                                :repo "OlMon/consult-projectile"))

(use-package embark
  :bind (("C-:" . embark-dwim)
         ("C-;" . embark-act)
         :map selectrum-minibuffer-map
         ("C-:" . embark-dwim)
         ("C-;" . embark-act)
         ("C-o" . embark-act)
         ("M-o" . embark-act)))

(use-package embark-consult
  :demand t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package marginalia
  :bind (:map selectrum-minibuffer-map
              ("M-a" . marginalia-cycle))
  :config
  (marginalia-mode t))

(use-package direnv)

(use-package gnuplot)

(use-package org-superstar
  :after org
  :hook (org-mode-hook . org-superstar-mode))

(use-package org-mind-map
  :after (org-ox)
  :custom
  (org-mind-map-engine "dot")
  :config
  (require 'org-ox))

(use-package iedit
  :disabled)

(use-package parinfer-rust-mode
  :hook ((clojure-mode-hook . parinfer-rust-mode)
         (hy-mode-hook . parinfer-rust-mode)
         (emacs-lisp-mode-hook . parinfer-rust-mode)
         (common-lisp-mode-hook . parinfer-rust-mode)
         (scheme-mode-hook . parinfer-rust-mode)
         (lisp-mode-hook . parinfer-rust-mode)
         (racket-mode-hook . parinfer-rust-mode))
  :custom
  (parinfer-rust-auto-download t))
  ;; (parinfer-rust-troublesome-modes nil))

(use-package smart-tabs-mode
  :hook (c-mode-common-hook . (lambda ()
                                (setq-local indent-tabs-mode t)))
  :config
  (smart-tabs-insinuate 'c 'c++))

;; TODO: check use-package-chords
(use-package key-chord
  :config
  ;; TODO: That should be moved somewhere I think
  (key-chord-mode t)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package dired
  :straight nil
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
  :init
  (defun arjaz/dired-subtree-toggle ()
    "Toggle subtree and update all-the-icons."
    (interactive)
    (dired-subtree-toggle)
    (when all-the-icons-dired-mode
      (revert-buffer)))
  :bind ((:map dired-mode-map
               ("C-c C-d" . dired-create-directory)
               ("C-c C-f" . dired-create-empty-file)
               ("C-c C-/" . dired-narrow-fuzzy)
               ("C-c /" . dired-narrow-fuzzy)
               ("<tab>" . arjaz/dired-subtree-toggle)))
  :config
  (dired-async-mode t))

(use-package terminal-here
  :bind ("M-RET" . terminal-here-launch)
  :custom
  (terminal-here-terminal-command '("alacritty" "--")))

(use-package yasnippet
  :config
  (yas-reload-all)
  (yas-global-mode t)
  (defvar my/company-point nil)
  (advice-add 'company-complete-common :before
              (lambda () (setq my/company-point (point))))
  (advice-add 'company-complete-common :after
              (lambda ()
                (when (equal my/company-point (point))
                  (yas-expand)))))

(use-package company
  :hook (prog-mode-hook . company-mode)
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
  (company-auto-commit-chars nil))

(use-package yasnippet-snippets
  :after company)
  ;; :config
  ;; (add-to-list 'company-backends 'company-yasnippet))

(use-package company-flx
  :config
  (company-flx-mode t))

(use-package company-tabnine
  :after (company)
  :disabled
  :config
  (add-to-list 'company-backends 'company-tabnine))

(use-package realgud
  :disabled)

(use-package dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom (dumb-jump-default-project "~/Code"))

(use-package zoom
  :disabled
  :init
  (defun arjaz/zoom-default ()
    (interactive)
    (setq zoom-size '(0.66 . 0.66)))
  (defun arjaz/zoom-term ()
    (interactive)
    (setq zoom-size '(0.66 . 0.8)))
  (defun undo-local-track-mouse (&optional ignored)
    (kill-local-variable 'track-mouse))
  :config
  (zoom-mode t)
  (arjaz/zoom-term)
  (advice-add 'zoom--get-frame-snapshot :before 'undo-local-track-mouse)
  (advice-add 'zoom--handler :before 'undo-local-track-mouse))

(use-package treemacs)

(use-package treemacs-evil)

(use-package treemacs-projectile)

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme 'all-the-icons))

(use-package treemacs-magit)

(use-package flycheck
  :custom
  (flycheck-indication-mode 'right-fringe)
  :config
  (global-flycheck-mode t)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)
  :bind ("C-c C-e" . flycheck-next-error))

(use-package flycheck-pos-tip
  :custom
  (flycheck-pos-tip-timeout 0)
  :config
  (flycheck-pos-tip-mode t))

(use-package lsp-mode
  :custom
  (lsp-semantic-highlighting t)
  (lsp-enable-symbol-highlighting nil)
  (lsp-lens-enable t)
  (lsp-prefer-capf t)
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.750)
  (lsp-headerline-breadcrumb-enable nil)
  (read-process-output-max (* 1024 1024))
  :config
  (lsp-register-custom-settings '(("pyls.plugins.pyls_mypy.enabled" t t)))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote)))

(use-package haskell-mode
  :hook ((haskell-mode-hook . haskell-indentation-mode)
         (haskell-mode-hook . interactive-haskell-mode))
  :bind (:map haskell-mode-map
              ("C-c c" . haskell-compile)
              ("C-c C-p" . haskell-check))
  :custom
  (haskell-compile-cabal-build-command "stack build"))

(use-package shakespeare-mode)

(use-package hlint-refactor
  :hook (haskell-mode-hook . hlint-refactor-mode))

(use-package lsp-haskell
  :after lsp-mode
  :custom
  (lsp-haskell-formatting-provider "ormolu")) ;; REVIEW: does it work?

(use-package idris-mode)

;; TODO: check out comint-clear-buffer and bind it to the inferior mode map
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
  (py-isort-options '("-l=79" "-m=3" "--tc")))

(use-package python-black
  :hook (python-mode-hook . python-black-on-save-mode)
  :custom
  (python-black-extra-args '("-l 79")))

(use-package pytest
  :disabled
  :bind (:map python-mode-map
              ("C-c C-a" . pytest-all)
              ("C-c r r" . pytest-run)
              ("C-c r a" . pytest-again)))

(use-package python-pytest
  :bind (:map python-mode-map
         ("C-c C-a" . python-pytest)
         :map hy-mode-map
         ("C-c r" . python-pytest)))

(use-package pyvenv)

(use-package auto-virtualenv
  :hook (python-mode-hook . auto-virtualenv-set-virtualenv))

(use-package ein
  :demand
  :custom
  (ein:output-area-inline-images t))

(use-package highlight-defined
  :hook (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package highlight-quoted
  :hook (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package eros
  :disabled ;; it shows nil all the time
  :hook (emacs-lisp-mode-hook . eros-mode))

(use-package sly
  :demand
  ;;:bind (:map sly-mrepl-mode-map
  ;;            ("C-l" . sly-mrepl-clear-repl)
  :custom
  (sly-complete-symbol-fuction 'sly-simple-completions)
  (inferior-lisp-program "sbcl"))

(use-package geiser)

(use-package geiser-guile)

(use-package web-mode
  :mode "\\.tsx?$"
  :hook (web-mode-hook . (lambda ()
                           (setq web-mode-markup-indent-offset 2
                                 web-mode-css-indent-offset 2
                                 web-mode-code-indent-offset 2))))

(use-package rjsx-mode
  :mode "\\.jsx?$")

(use-package typescript-mode)

(use-package purescript-mode)

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

(use-package clojure-mode)

(use-package clojure-mode-extra-font-locking)

(use-package flycheck-clojure
  :disabled)

(use-package flycheck-clj-kondo)

(use-package kibit-helper)

(use-package cider
  :bind (:map cider-repl-mode-map
              ("C-l" . cider-repl-clear-buffer))
  :config
  (advice-add 'cider-find-var :before (lambda (&rest r) (evil-set-jump))))

(use-package cider-eval-sexp-fu)

(use-package clj-refactor
  :hook (clojure-mode-hook . (lambda ()
                               (interactive)
                               (clj-refactor-mode t)
                               (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package elm-mode
  :after company
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

(use-package elixir-mode)

(use-package gdscript-mode)

(use-package cmake-mode)

(use-package cmake-font-lock)

(use-package jinja2-mode)

(use-package markdown-mode)

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package pdf-tools
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

(use-package vimish-fold)

(use-package evil-vimish-fold
  :config
  (global-evil-vimish-fold-mode t))

(use-package cd-compile)

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package which-key
  :config
  (which-key-mode t))

(use-package httprepl)

(use-package restclient)

(use-package company-restclient)

(use-package simple-httpd
  :custom
  (httpd-root "/var/www"))

(use-package code-compass
  :after (simple-httpd)
  ;; NOTE: it shows some stuff near the modeline, I don't like it
  :disabled
  :straight (code-compass :type git
                          :host github
                          :files ("*" "*/*")
                          :repo "ag91/code-compass"))

(use-package telega
  :disabled
  :bind-keymap ("C-c t" . telega-prefix-map)
  :bind (:map telega-msg-button-map
              ("k" . evil-previous-line)))
         ;; :map telega-chat-mode-map
         ;;      ("<evil-normal-state-map> q" . telega)))

(use-package screenshot
  :bind (:map evil-normal-state-map
              ("M-p" . screenshot))
  :straight (screenshot :host github
                        :type git
                        :repo "tecosaur/screenshot"
                        :files ("*.el")))

(use-package svg-tag-mode
  :straight (svg-tag-mode :repo "rougier/svg-tag-mode"
                          :type git
                          :host github
                          :files ("svg-tag-mode.el"))
  :disabled)

(use-package stumpwm-mode)

(use-package tshell
  :straight (tshell :repo "TatriX/tshell"
                    :host github
                    :type git
                    :files ("*.el")))

(use-package apply-to-region
  :straight (apply-to-region :repo "arjaz/apply-to-region.el"
                             :host github
                             :files ("*.el")))

(provide 'init)
;;; init ends here
