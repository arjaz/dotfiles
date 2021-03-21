;;; init.el ---  My configuration

;;; Commentary:
;;; That's my (Eugene's) local Emacs configuration

;;; Code:
;; I think it's managed by GHCM now
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 536870912 ; 512mb
                  gc-cons-percentage 0.1)))

;; Long lines stuff
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;; Disable visual clutter
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(setq x-gtk-use-system-tooltips nil
      use-dialog-box nil)

;; Disable startup message and gtk pop-ups
(setq inhibit-startup-message t)

;; Symlinks
(setq vc-follow-symlinks t
      find-file-visit-truename t)

;; Don't create backup files
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 6
      kept-old-versions 2
      make-backup-files nil
      backup-directory-alist '(("." . "~/.config/emacs/saves")))

(setq temporary-file-directory "~/.config/emacs/tmp/")

;; Automaticaly revert changes
(global-auto-revert-mode t)

;; Disable auto-saves
(setq auto-save-default nil)

;; Disable lock files
(setq create-lockfiles nil)

;; We don't want to type yes and no all the time so, do y and n
(fset 'yes-or-no-p 'y-or-n-p)

;; Fonts
(add-to-list 'default-frame-alist '(font . "Iosevka"))
(set-face-attribute 'default nil :height 110)
(set-frame-font "Iosevka")

(setq window-divider-default-places 'bottom-only)
(window-divider-mode t)

(setq savehist-file "~/.config/emacs/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history t)
(savehist-mode t)

;; Remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-display-fill-column-indicator-mode t)
(setq-default fill-column 80)

;; Non-blinking cursor
(blink-cursor-mode 0)

;; Highlight matching brace
(show-paren-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Relative line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'eshell-mode-hook 'display-line-numbers-mode)

;; Don't bother to close the parens
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;; Move flymake garbage
(setq flymake-run-in-place nil)

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq js-indent-level 2)

;; Clean buffers
(setq clean-buffer-list-delay-general 1)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; Relative line numbers
(setq display-line-numbers-type 'relative)

;; Smooth scrolling
(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering

;; Straight
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(watch-files find-when-checking))

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

;; (use-package fira-code-mode
;;   :config
;;   (setq fira-code-mode-disabled-ligatures '("[]" "x" "===" "!=="))
;;   (global-fira-code-mode))

(use-package doom-themes
  :after (solaire-mode)
  :init
  (defvar arjaz/loaded-theme nil)
  :hook (server-after-make-frame . (lambda ()
                                     (interactive)
                                     (unless arjaz/loaded-theme
                                       (setq arjaz/loaded-theme t)
                                       (load-theme 'doom-nord t))))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t)
  (doom-themes-org-config))

;; (use-package doom-modeline
;;   ;; I guess it's what I use with the daemon
;;   :hook (after-init . doom-modeline-mode)
;;   :hook (doom-modeline-mode . column-number-mode)
;;   :init
;;   (setq doom-modeline-icon t
;;         doom-modeline-project-detection 'project
;;         doom-modeline-modal-icon t
;;         doom-modeline-major-mode-icon t
;;         doom-modeline-major-mode-color-icon t
;;         doom-modeline-vcs-max-length 12
;;         doom-modeline-buffer-state-icon t
;;         doom-modeline-buffer-modification-icon t
;;         doom-modeline-env-version t
;;         doom-modeline-lsp t))
;;   ;; And that's used without a daemon
;;   ;; (doom-modeline-mode 1))

(use-package feebleline
  :config
  (feebleline-mode))

(use-package rainbow-delimiters
  ;; TODO: remove that from lisps
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap))

(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.tsx$" all-the-icons-fileicon "typescript" :height 1.0 :v-adjust -0.1 :face all-the-icons-blue-alt))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.hy" all-the-icons-fileicon "scheme" :height 1.2 :face all-the-icons-red)))

(use-package all-the-icons-dired
  :config
  :hook (dired-mode . (lambda ()
                        (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode t)))))

(use-package hl-todo
  :config
  (global-hl-todo-mode t)
  (setq hl-todo-keyword-faces
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
          ("XXX" font-lock-constant-face bold))))

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  (setq git-gutter:window-width 2
        git-gutter:update-interval 1
        git-gutter:ask-p nil))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
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

(use-package gcmh
  :config
  (setq gcmh-high-cons-threshold (/ 1073741824 2))
  (gcmh-mode 1))

(use-package evil
  :hook (after-change-major-mode . (lambda () (modify-syntax-entry ?_ "w")))
  :bind (:map evil-normal-state-map
              ("C-u" . (lambda ()
                         (interactive)
                         (evil-scroll-up nil)))
              ("C-d" . (lambda ()
                         (interactive)
                         (evil-scroll-down nil))))
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :config
  (evil-mode t)
  (setq evil-split-window-below t
        evil-vsplit-window-right t))

;; (use-package evil-dvorak
;;   :config
;;   (global-evil-dvorak-mode t))

(use-package evil-numbers
  :bind (:map evil-normal-state-map
              ("C-c j" . evil-numbers/inc-at-pt)
              ("C-c k" . evil-numbers/dec-at-pt)))

(use-package evil-leader
  :config
  (setq evil-leader/in-all-states 1)
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
    "n r" 'counsel-tramp

    ;; Lsp
    "l l" 'lsp
    "l e" 'lsp-ui-flycheck-list
    "l c" 'lsp-treemacs-call-hierarchy
    "l n" 'lsp-rename
    "l s" 'lsp-describe-thing-at-point
    "l f" 'lsp-format-buffer
    "l d" 'lsp-find-definition
    "l t" 'lsp-find-type-definition
    "l r" 'lsp-find-references
    "l i" 'lsp-find-implementation
    "l a" 'lsp-execute-code-action
    "l m" 'lsp-ui-imenu
    "l g" 'lsp-avy-lens

    "k" 'kill-current-buffer

    "f" 'format-all-buffer

    ;; Magit bindings
    "m s" 'magit-status
    "m m" 'magit-status
    "m b" 'magit-branch
    "m c" 'magit-clone

    ;; Eshell
    "t" 'eshell-toggle
    "e" 'eshell

    ;; Search
    "s" 'swiper-isearch
    "a" 'counsel-projectile-rg

    ;; Projectile
    "p p" 'counsel-projectile-switch-project
    "p c" 'projectile-compile-project
    "p d" 'projectile-dired
    "j" 'projectile-find-file-other-window
    "<SPC>" 'counsel-projectile-find-file

    ;; Moving
    "r" 'counsel-buffer-or-recentf
    "b" 'ivy-switch-buffer
    "o" 'counsel-find-file
    "g" 'counsel-bookmark
    "d" 'dired-sidebar-toggle-with-current-directory))

(use-package evil-indent-plus)

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-embrace
  :config
  (setq evil-embrace-show-help-p nil)
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

(use-package evil-iedit-state
  :after (iedit evil)
  :hook (iedit-mode . evil-iedit-state)
  :config
  (defalias 'iedit-cleanup 'iedit-lib-cleanup))

(use-package evil-quickscope
  :config
  (global-evil-quickscope-mode t))

(use-package evil-goggles
  :hook (evil-mode . evil-goggles-mode)
  :config
  (setq evil-goggles-duration 0.025))

(use-package evil-collection
  :after (evil vterm)
  :config
  (evil-collection-init))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode t))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :hook (evil-org-mode . (lambda ()
                           (evil-org-set-key-theme)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package avy
  :config
  (define-key evil-normal-state-map "S" 'avy-goto-char-timer)
  (define-key evil-motion-state-map "m" 'avy-goto-char-timer))

(use-package undo-fu
  :after evil
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

;; (use-package aggressive-indent
;;   :hook (prog-mode . aggressive-indent-mode))

;; (use-package hungry-delete
;;   :hook (prog-mode . hungry-delete-mode))

(use-package ws-butler
  :config
  (ws-butler-global-mode t))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq show-week-agenda-p t
        dashboard-set-heading-icons t
        dashboard-startup-banner 3
        dashboard-set-navigator t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;;(use-package eyebrowse
;;  :config
;;  (eyebrowse-mode t)
;;  (eyebrowse-setup-opinionated-keys))

;; (use-package smartparens
;;   :hook (prog-mode . smartparens-mode)
;;   :config
;;   (require 'smartparens-config))

(use-package xterm-color)

(setq eshell-history-size 1024)

(defun eshell-new ()
  "Open a new eshell session."
  (interactive)
  (eshell 'N))

(defun eshell-clear-buffer ()
  "Clear terminal."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t))

(use-package shrink-path)

;; FIXME: That doesn't work for some reason
;; (use-package esh-autosuggest)

(use-package bash-completion)

(use-package fish-completion
  :after bash-completion
  :config
  (global-fish-completion-mode)
  (setq fish-completion-fallback-on-bash-p t))

(use-package eshell-prompt-extras
  :config
  (setq eshell-prompt-function 'epe-theme-lambda
        eshell-highlight-prompt t))

(use-package eshell-syntax-highlighting
  :config
  (eshell-syntax-highlighting-global-mode t))

(use-package vterm)

(use-package mu4e
  :config
  (setq mu4e-maildir "~/Maildir"
        mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder "/[Gmail].Sent Mail"
        mu4e-trash-folder "/[Gmail].Trash"
        smtpmail-local-domain "gmail.com"
        smtpmail-default-smtp-server "smpt.gmail.com"
        smtpmail-smtp-server "smpt.gmail.com"
        smtpmail-smtp-service 587)

  (setq mu4e-maildir-shortcuts
        '(("/INBOX"               . ?i)
          ("/[Gmail].Sent Mail"   . ?s)))

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; allow for updating mail in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; something about ourselves
  (setq user-mail-address "art6661322@gmail.com"
        user-full-name "Eugene Rossokha")

  (setq mu4e-view-show-images t
        mu4e-view-image-max-width 800)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t))

;; TODO: change for apheleia
(use-package format-all)

;; (use-package apheleia
;;   :config
;;   (setf (alist-get 'black apheleia-formatters)
;;         '("black" "-l 79"))
;;   (apheleia-global-mode t))

(use-package elfeed
  :config
  (load "~/.dotfiles/emacs/elfeed-local-feed.el"))

(use-package elfeed-dashboard
  :config
  (setq elfeed-dashboard-file "~/.org/elfeed-dashboard.org")
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after 'elfeed-dashboard-update-links))

(use-package elpher
  :config
  (setq elpher-gemini-link-string "> "))

(use-package erc
  :custom
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  :config
  (erc-services-mode 1)
  (erc-update-modules)
  (setq auth-sources '("~/.authinfo.gpg"
                       "~/.authinfo"
                       "~/.netrc")))

(use-package git-commit
  :config
  (setq git-commit-fill-column 50
        git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line)))

(use-package magit)

(use-package magit-todos
  :hook (prog-mode . magit-todos-mode))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/Code/"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

(use-package ivy
  :after evil
  :straight (ivy :type git
                 :flavor melpa
                 :files (:defaults
                         (:exclude "swiper.el" "counsel.el" "ivy-hydra.el")
                         "doc/ivy-help.org" "ivy-pkg.el")
                 :host github
                 :repo "abo-abo/swiper")
  :bind (:map ivy-mode-map
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line))
  :config
  (setq projectile-completion-system 'ivy
        ivy-magic-slash-non-match-action nil
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-display-style 'fancy
        ivy-on-del-error-function 'ignore
        ivy-format-function 'ivy-format-function-line
        ivy-sort-max-size 7500)
  ;; ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-mode))

(use-package swiper
  :after evil
  :straight (swiper :type git
                    :flavor melpa
                    :files ("swiper.el" "swiper-pkg.el")
                    :host github
                    :repo "abo-abo/swiper")
  :config
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "?") 'swiper-backward))

(use-package counsel
  :after evil
  :straight (counsel :type git
                     :flavor melpa
                     :files ("counsel.el" "counsel-pkg.el")
                     :host github
                     :repo "abo-abo/swiper")
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         ("C-c i" . counsel-imenu))
  :config
  (counsel-mode))

(use-package ivy-hydra
  :after evil
  :straight (ivy-hydra :type git
                       :flavor melpa
                       :files ("ivy-hydra.el")
                       :host github
                       :repo "abo-abo/swiper"))

;; (use-package ivy-posframe
;;   :config
;;   (ivy-posframe-mode t)
;;   (setq ivy-posframe-display-functions-alist
;;         '((t . ivy-posframe-display-at-window-center))
;;         ivy-posframe-parameters '((left-fringe 8)
;;                                   (right-fringe 8)
;;                                   (top-fringe 2)
;;                                   (bottom-fringe 2))))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (setq counsel-projectile-preview-buffers t)
  (counsel-projectile-mode t))

(use-package prescient
  :after counsel)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode t))

(use-package counsel-tramp
  :config
  (setq remote-file-name-inhibit-cache nil
        vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)
        tramp-default-method "ssh"
        create-lockfiles nil)
  :hook (counsel-tramp-pre-command . (lambda ()
                                       (projectile-mode 0)))
  :hook (counsel-tramp-quit . (lambda ()
                                (projectile-mode 1))))

(use-package direnv)

;; (use-package imenu-list)

(use-package org
  :straight (org :type built-in)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-logfiles-extensions
      '("lof" "lot" "tex" "aux" "idx" "log" "out" "toc" "nav" "snm"
        "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl"
        "bbl" "pygtex" "pygstyle"))

(setq org-confirm-babel-evaluate nil)
;; enable python for in-buffer evaluation
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; all python code be safe
(setq org-confirm-babel-evaluate '(lambda (lang body)
                                    (not (string= lang "python"))))

(setq org-directory "~/.org/"
      org-default-notes-file (concat org-directory "notes.org")
      org-hide-leading-stars t
      org-startup-folded t
      org-startup-indented t
      org-agenda-files (list org-default-notes-file))

(setq org-capture-templates
      '(("t" "Tasks" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n%u\n" :prepend t)
        ("l" "Look later" entry (file+headline org-default-notes-file "Look later")
         "* TODO %?")
        ("s" "Skills" entry (file+headline org-default-notes-file "Skills")
         "* TODO %?")
        ("g" "Gifts" entry (file+headline org-default-notes-file "Gifts")
         "* TODO %?")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-ref
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq reftex-default-bibliography '("~/Documents/bibliography/references.bib")))

(use-package org-mind-map
  :after (org-ox)
  :config
  (require 'org-ox)
  (setq org-mind-map-engine "dot"))

(use-package iedit)

(use-package parinfer-rust-mode
  :hook (clojure-mode . parinfer-rust-mode)
  :hook (hy-mode . parinfer-rust-mode)
  :hook (emacs-lisp-mode . parinfer-rust-mode)
  :hook (common-lisp-mode . parinfer-rust-mode)
  :hook (scheme-mode . parinfer-rust-mode)
  :hook (lisp-mode . parinfer-rust-mode)
  :hook (racket-mode . parinfer-rust-mode)
  :init
  (setq parinfer-rust-auto-download t
        parinfer-rust-troublesome-modes nil))

(use-package smart-tabs-mode
  :hook (c-mode-common . (lambda ()
                           (setq indent-tabs-mode t)))
  :config
  (smart-tabs-insinuate 'c 'c++))

(use-package key-chord
  :config
  (key-chord-mode t)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(setq dired-listing-switches "-alhg"
      dired-auto-revert-buffer t  ; don't prompt to revert; just do it
      dired-dwim-target t  ; suggest a target for moving/copying intelligently
      dired-hide-details-hide-symlink-targets nil
      ;; Always copy/delete recursively
      dired-recursive-copies 'always
      dired-recursive-deletes 'top)

(add-hook 'dired-mode-hook 'auto-revert-mode)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(defun arjaz/dired-subtree-toggle ()
  "Toggle subtree and update all-the-icons."
  (interactive)
  (dired-subtree-toggle)
  (when all-the-icons-dired-mode
    (revert-buffer)))

(use-package dired-hacks
  :bind ((:map dired-mode-map
               ("C-c C-d" . dired-create-directory)
               ("C-c C-f" . dired-create-empty-file)
               ("C-c C-/" . dired-narrow-fuzzy)
               ("C-c /" . dired-narrow-fuzzy)
               ("<tab>" . arjaz/dired-subtree-toggle)))
  :config
  (dired-async-mode t))

(use-package dired-sidebar
  :config
  (add-to-list 'dired-sidebar-display-alist '(side . right)))

(use-package terminal-here
  :bind ("M-RET" . terminal-here-launch)
  :config
  (setq terminal-here-terminal-command '("alacritty" "--")))

(use-package yasnippet
  :config
  (yas-reload-all)
  (yas-global-mode t)
  (defvar my/company-point nil)
  (advice-add 'company-complete-common :before (lambda () (setq my/company-point (point))))
  (advice-add 'company-complete-common :after (lambda ()
                                                (when (equal my/company-point (point))
                                                  (yas-expand)))))

(use-package yasnippet-snippets)

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (company-tng-configure-default)
  (setq company-idle-delay 0
        company-show-numbers t
        company-eclim-auto-save nil
        company-dabbrev-downcase nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-require-match 'never
        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '(company-capf)

        company-auto-complete nil
        company-auto-complete-chars nil))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-flx
  :config
  (company-flx-mode t))

;; (use-package company-tabnine
;;   :after (company)
;;   :config
;;   (add-to-list 'company-backends 'company-tabnine))

(use-package realgud)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate)
  (setq dumb-jump-default-project "~/Code"))

(use-package zoom
  :after (dired-sidebar)
  :config
  (zoom-mode t)
  (setq zoom-size '(0.66 . 0.66)
        dired-sidebar-toggle-hidden-commands nil)

  (defun undo-local-track-mouse (&optional ignored)
    (kill-local-variable 'track-mouse))

  (advice-add 'zoom--get-frame-snapshot :before 'undo-local-track-mouse)
  (advice-add 'zoom--handler :before 'undo-local-track-mouse))

(use-package flycheck
  :config
  (global-flycheck-mode t)
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)
  :bind ("C-c C-e" . flycheck-next-error))

(use-package flycheck-pos-tip
  :config
  (setq flycheck-pos-tip-timeout 0)
  (flycheck-pos-tip-mode t))

(use-package lsp-mode
  :config
  (lsp-register-custom-settings '(("pyls.plugins.pyls_mypy.enabled" t t)))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote))
  (setq lsp-semantic-highlighting t
        lsp-enable-symbol-highlighting t
        lsp-lens-enable t
        lsp-prefer-capf t
        lsp-completion-provider :capf
        lsp-idle-delay 0.750
        read-process-output-max (* 1024 1024)))

(use-package lsp-ivy)

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t))

(use-package dap-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))
(add-hook 'c++-mode-hook '(lambda ()
                            (interactive)
                            (electric-pair-local-mode 0)
                            (set-fill-column 100)))

(use-package haskell-mode
  :hook (haskell-mode . haskell-indentation-mode)
  :hook (haskell-mode . interactive-haskell-mode)
  :bind (:map haskell-mode-map
              ("C-c c" . haskell-compile)
              ("C-c C-p" . haskell-check))
  :config
  (setq haskell-compile-cabal-build-command "stack build"))

(use-package shakespeare-mode)

(use-package hlint-refactor
  :hook (haskell-mode . hlint-refactor-mode))

(use-package lsp-haskell
  :after lsp-mode
  :config
  (setq lsp-haskell-formatting-provider "ormolu")) ;; REVIEW: does it work?

(use-package idris-mode)

;; TODO: check out comint-clear-buffer and bind it to the inferior mode map
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --pprint")

(use-package python-x
  :config
  (python-x-setup))

(use-package cython-mode)

(use-package py-isort
  :hook (before-save . py-isort-before-save)
  :config
  (setq py-isort-options '("-l=79" "-m=3" "--tc")))

(use-package python-black
  :hook (python-mode . python-black-on-save-mode)
  :config
  (setq python-black-extra-args '("-l 79")))

(use-package pytest
  :bind (:map python-mode-map
              ("C-c C-a" . pytest-all)
              ("C-c r r" . pytest-run)
              ("C-c r a" . pytest-again)))

(use-package pyvenv)

(use-package auto-virtualenv
  :hook (python-mode . auto-virtualenv-set-virtualenv))

(use-package web-mode
  :mode "\\.tsx?$"
  :hook (web-mode . (lambda ()
                      (setq web-mode-markup-indent-offset 2
                            web-mode-css-indent-offset 2
                            web-mode-code-indent-offset 2))))

(use-package rjsx-mode
  :mode "\\.jsx?$")

(use-package prettier-js
  :hook (js-mode . prettier-js-mode)
  :hook (typescript-mode . prettier-js-mode)
  :hook (web-mode . prettier-js-mode)
  :hook (rjsx-mode . prettier-js-mode))

(use-package typescript-mode)

(use-package purescript-mode)

(use-package hy-mode
  :config
  (setq hy-jedhy--enable? nil))

(use-package clojure-mode)

(use-package clojure-mode-extra-font-locking)

(use-package cider)

(use-package cider-eval-sexp-fu)

(use-package elm-mode
  :after company
  :hook (elm-mode . elm-format-on-save-mode))

(use-package flycheck-elm
  :after flycheck
  :hook (flycheck-mode . flycheck-elm-setup))

(use-package rust-mode
  :bind (:map rust-mode-map
              ("C-c C-p" . rust-run-clippy)
              ("C-c C-c" . rust-run))
  :config
  (setq rust-format-on-save t))

(use-package racket-mode
  :bind (:map racket-mode-map
              ("C-c C-c" . racket-run)
              ("C-c C-r" . racket-send-region)))

(use-package elixir-mode)

(use-package makefile-executor
  :hook (makefile-mode . makefile-executor-mode))

(use-package cmake-mode)

(use-package cmake-font-lock)

(use-package jinja2-mode)

(use-package markdown-mode)

(use-package mermaid-mode)

(use-package yaml-mode)

(use-package bnf-mode)

(use-package dockerfile-mode)

(use-package emmet-mode
  :hook (mhtml-mode . emmet-mode)
  :hook (html-mode . emmet-mode)
  :hook (jinja2-mode . emmet-mode))

;; Zathura for pdf
(setq TeX-view-program-list '(("zathura" "zathura --page=%(outpage) %o")))
(setq TeX-view-program-selection '((output-pdf "Zathura")))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package tex-site
  :straight auctex
  :bind ("M-q" . align-current)
  :hook (LaTeX-mode . LaTeX-math-mode)
  :hook (LaTeX-mode . flyspell-mode)
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (add-hook 'TeX-after-compilation-finished-functions 'TeX-revert-document-buffer)
  (setq TeX-PDF-mode t
        TeX-auto-save t
        TeX-parse-self t
        reftex-plug-into-AUCTeX t))

(use-package vimish-fold)

(use-package evil-vimish-fold
  :config
  (global-evil-vimish-fold-mode t))

(use-package run-command)

(use-package cd-compile)

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package which-key
  :config
  (which-key-mode t))

;; (use-package which-key-posframe
;;   :config
;;   (which-key-posframe-mode t))

;; (use-package eaf
;;   :straight nil
;;   :load-path "/usr/share/emacs/site-lisp/eaf"
;;   :config
;;   (require 'eaf-evil)
;;   (eaf-setq eaf-browser-dark-mode "true")
;;   (setq eaf-evil-leader-keymap spacemacs-cmds)
;;   (setq eaf-browser-continue-where-left-off t)
;;   (eaf-setq eaf-browser-enable-adblocker "true")
;;   (setq eaf-browser-default-search-engine "duckduckgo")
;;   (setq eaf-evil-leader-key "SPC")
;;   (setq eaf-find-alternate-file-in-dired t))
;;   ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding))

;; (use-package nano
;;   :straight nil
;;   :load-path "~/Programs/nano-emacs/"
;;   :config
;;   (require 'nano-theme-dark)
;;   (require 'nano-layout)
;;   (require 'nano-modeline))

(use-package screenshot
  :straight (screenshot :type git
                        :repo "tecosaur/screenshot"
                        :file ("*.el")))

(provide 'init)
;;; init ends here
