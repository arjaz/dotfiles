;; Not my shit
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (terminal-here diminish rainbow-delimiters rainvow-delimiters fill-column-indicator autopair hlinum linum-highlight-current-line-number smooth-scrolling linum-relative relative-line-numbers use-package try powerline org-evil org-bullets nordless-theme nord-theme helm evil-visual-mark-mode evil-surround evil-leader evil-commentary))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; My shit
;; Disable startup message
(setq inhibit-startup-message t)
;; Disable menu-bar
(menu-bar-mode -1)
;; Disable scroll-bar
(scroll-bar-mode -1)
;; Disable tool-bar
(tool-bar-mode -1)
;; Disable tooltip
(tooltip-mode -1)

;; Don't create backup files
(setq make-backup-files nil)

;; Line numbers
;; (global-display-line-numbers-mode)
;; Highlight current line
(global-hl-line-mode 1)

(package-initialize)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmoderog/elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)

(package-initialize)

(load-theme 'nord t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Wind Mode
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;; Helm
(use-package helm
  :ensure t
  :config
  (require 'helm)
  (require 'helm-config)
  (helm-mode 1))

;; Org-mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; Evil

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (require 'evil-commentary)
  (evil-commentary-mode))

(use-package evil-leader
  :ensure t
  :config
  (require 'evil-leader)
  (setq evil-leader/in-all-states 1)
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "b" 'helm-buffers-list
    "v" 'split-window-right
    "h" 'split-window-below))

;; Learn org-mode
(use-package org-evil
  :ensure t
  :config
  (require 'org-evil))

;; Relative line numbers
(use-package linum-relative
  :ensure t
  :config
  (require 'linum-relative)
  (setq linum-relative-backend 'display-line-number-mode)
  (linum-relative-global-mode))

;; Highlight current line number
;; TODO: set color (face)
;; (use-package hlinum
;;   :ensure t
;;   :config
;;   (hlinum-activate)
;;   (linum-))

(use-package evil
  :ensure t
  :config
  (require 'evil)
  (define-key evil-normal-state-map (kbd "C-u") (lambda()
                                                  (interactive)
                                                  (evil-scroll-up nil)))
  (define-key evil-normal-state-map (kbd "C-d") (lambda()
                                                  (interactive)
                                                  (evil-scroll-down nil)))
  (evil-mode 1))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode))

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (fci-mode))

;; Configure theme
(use-package powerline
  :ensure t
  :config
  (require 'powerline)
  (powerline-default-theme))

;; Clean up powerline
(use-package diminish
  :ensure t
  :config
  (diminish 'visual-line-mode))

;; Open i3-sensible-terminal
(global-set-key (kbd "M-RET") (kbd "M-! i3-sensible-terminal RET"))

;; Remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight matching brace
(show-paren-mode t)

;; <esc> quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; use spaces
(setq-default tab-width 4 indent-tabs-mode nil)
