;; Make sure to use emacs lsp booster
(setenv "LSP_USE_PLISTS" "true")

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq tool-bar-mode nil
      scroll-bar-mode nil
      menu-bar-mode nil)
(tooltip-mode -1)
(horizontal-scroll-bar-mode -1)

(defvar bootstrap-version)
(setq straight-check-for-modifications '(watch-files))
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
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
  (history-length 100)
  (history-delete-duplicates t)
  :config
  (savehist-mode)
  (push 'use-dark-theme-p savehist-additional-variables)
  (push 'the-font-height savehist-additional-variables))

(use-package saveplace
  :straight (:type built-in)
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600)
  :config
  (save-place-mode))

(use-package modus-themes
  :defer t
  :custom
  (modus-operandi-palette-overrides
   '(;; (bg-main "#f6f8fa")
     ;; (fg-main "#24292f")
     (bg-paren-match bg-green-nuanced)
     (bg-region bg-blue-subtle)
     (fg-region fg-main)
     (string fg-main)
     (keyword fg-main)
     (docstring fg-main)
     (docmarkup fg-main)
     (bg-hover bg-magenta-nuanced)
     (bg-search-current bg-yellow-nuanced)
     (bg-search-lazy bg-cyan-nuanced)
     (bg-search-replace bg-red-nuanced)
     (bg-search-rx-group-0 bg-blue-nuanced)
     (bg-search-rx-group-1 bg-green-nuanced)
     (bg-search-rx-group-2 bg-red-subtle)
     (bg-search-rx-group-3 bg-magenta-subtle)))
  (modus-vivendi-palette-overrides
   '(;; (fg-main "#f4f4f4")
     ;; (fg-dim "#aeafad")
     ;; (bg-main "#1e1e1e")
     ;; (bg-main "#111111")
     (string fg-main)
     (keyword fg-main)
     (bg-paren-match fg-dim)
     (string cyan-faint)
     (docstring fg-main)
     (docmarkup fg-main)
     (bg-region fg-dim)
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
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     (fringe unspecified)
     (keybind cyan-faint)
     (accent-0 cyan-faint)
     (accent-1 yellow-faint)
     (accent-2 blue-faint)
     (accent-3 red-faint)
     (fg-prompt cyan-faint)
     (rx-construct cyan-faint)
     (rx-backslash magenta-faint)
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
     (docstring fg-dim)
     (docmarkup fg-dim)
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

(setq light-theme 'modus-operandi)
(setq dark-theme 'modus-vivendi)
(defun load-dark-theme ()
  "Load the saved dark theme."
  (interactive)
  (setq use-dark-theme-p t)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme dark-theme t))
(defun load-light-theme ()
  "Load the saved light theme."
  (interactive)
  (setq use-dark-theme-p nil)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme light-theme t))
(defvar use-dark-theme-p t)

(setq ring-bell-function 'ignore)

(defun set-fonts (font-height)
  (interactive "nFont height: ")
  (setq the-font "Iosevka")
  (setq the-nice-font "Iosevka")
  (setq the-font-height font-height)
  (setq-default line-spacing nil)
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
                      :height the-font-height))
(set-fonts 140)

(if use-dark-theme-p
    (load-dark-theme)
  (load-light-theme))
