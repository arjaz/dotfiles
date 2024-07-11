;; Make sure to use emacs lsp booster
(setenv "LSP_USE_PLISTS" "true")

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(defvar bootstrap-version)
(setq straight-check-for-modifications '(watch-files))
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 7))
  (load bootstrap-file nil 'nomessage))
(defvar use-package-enable-imenu-support t)
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

(use-package savehist
  :straight (:type built-in)
  :custom
  (savehist-file (concat user-emacs-directory "savehist"))
  (savehist-save-minibuffer-history t)
  :config
  (savehist-mode)
  (push 'use-dark-theme-p savehist-additional-variables)
  (push 'the-font-height savehist-additional-variables))

(use-package solaire-mode
  :commands (solaire-global-mode))
(use-package doom-themes
  :defer t
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil))

(use-package modus-themes
  :defer t
  :preface
  (defun my-modus-themes-custom-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
       `(solaire-default-face ((,c :inherit default :background ,bg-dim :foreground ,fg-dim)))
       `(solaire-line-number-face ((,c :inherit solaire-default-face :foreground ,fg-dim)))
       `(solaire-hl-line-face ((,c :background ,bg-active)))
       `(solaire-org-hide-face ((,c :background ,bg-dim :foreground ,bg-dim))))))
  :custom
  (modus-operandi-palette-overrides
   '((bg-paren-match bg-dim)
     (preprocessor green-faint)
     (string green-faint)
     (keyword blue-faint)
     (bg-region fg-dim)
     (fg-region bg-main)))
  (modus-operandi-tinted-palette-overrides
   '((bg-paren-match bg-green-nuanced)
     (keyword cyan-faint)
     ;; (preprocessor blue-faint)
     (string green)))
  (modus-vivendi-palette-overrides
   '((fg-main "#e0e0e0")
     ;; (bg-main "#111111")
     (bg-main "#1d1f21")
     ;; (preprocessor fg)
     (keyword cyan-faint)
     (bg-paren-match fg-dim)
     (string green-faint)
     (bg-region fg-dim)
     (fg-region bg-main)))
  (modus-themes-common-palette-overrides
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     (fringe unspecified)
     (bg-mode-line-active bg)
     (bg-mode-line-inactive bg)
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
  (modus-themes-mixed-fonts t))

(custom-set-faces
 '(region ((t :extend nil))))

(setq light-theme 'modus-operandi-tinted)
(setq dark-theme 'modus-vivendi)
(defun load-dark-theme ()
  "Load the saved dark theme."
  (interactive)
  (setq use-dark-theme-p t)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme dark-theme t)
  (solaire-global-mode t))
(defun load-light-theme ()
  "Load the saved light theme."
  (interactive)
  (setq use-dark-theme-p nil)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme light-theme t)
  ;; (my-modus-themes-custom-faces)
  ;; (set-face-attribute font-lock-keyword-face nil
  ;;                     :weight 'semibold)
  ;; (solaire-global-mode t)
  )
(defvar use-dark-theme-p t)

(setq the-font "Iosevka")
(setq the-nice-font "Iosevka")
(setq the-font-height 110)

(add-hook
 'before-make-frame-hook
 (lambda ()
   (set-face-attribute 'default
                       nil
                       :family the-font
                       :height the-font-height)
   (set-face-attribute 'fixed-pitch-serif
                       nil
                       :family the-font
                       :height the-font-height)
   (set-face-attribute 'fixed-pitch
                       nil
                       :family the-font
                       :height the-font-height)
   (set-face-attribute 'variable-pitch
                       nil
                       :family the-nice-font
                       :height the-font-height)
   (if use-dark-theme-p
       (load-dark-theme)
     (load-light-theme))))
