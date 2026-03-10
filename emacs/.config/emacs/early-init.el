;; -*- lexical-binding: t; -*-
;; (setenv "LSP_USE_PLISTS" "true")

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
(setq straight-check-for-modifications '())
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

(setq ring-bell-function 'ignore)

(defun set-fonts (font-height)
  (interactive "nFont height: ")
  ;; (setq the-font "CommitMonoArjaz")
  (setq the-font "Ioskeley Mono")
  ;; (setq the-font "Iosevka")
  (setq the-nice-font "Iosevka Aile")
  (setq the-font-height font-height)
  (setq-default line-spacing 0.0)
  ;; TODO: custom-set-faces
  (set-face-attribute 'default
                      nil
                      :weight 'normal
                      :family the-font
                      :height the-font-height)
  (set-face-attribute 'fixed-pitch-serif
                      nil
                      :weight 'normal
                      :family the-font
                      :height the-font-height)
  (set-face-attribute 'fixed-pitch
                      nil
                      :weight 'normal
                      :family the-font
                      :height the-font-height)
  (set-face-attribute 'variable-pitch
                      nil
                      :weight 'normal
                      :family the-nice-font
                      :height the-font-height))
(set-fonts 110)

;; (defun set-safe-composition-table ()
;;   (interactive)
;;   (set-char-table-range composition-function-table t `(["[,-.;A-Z_a-z]+" 0 font-shape-gstring])))
;; (defun unset-safe-composition-table ()
;;   (interactive)
;;   (set-char-table-range composition-function-table t `(["" 0 font-shape-gstring])))
;; (defun toggle-safe-composition-table--around (old-fn &rest args)
;;   "Disable the composition table around a function invocation. Useful to prevent weird avy artifacts."
;;   (let ((visible-buffers (mapcar #'window-buffer (window-list))))
;;     (dolist (b visible-buffers)
;;       (with-current-buffer b
;;         (unset-safe-composition-table)))
;;     (let ((res (apply old-fn args)))
;;       (dolist (b visible-buffers)
;;         (with-current-buffer b
;;           (set-safe-composition-table)))
;;       res)))
;; (add-hook 'minibufer-setup-hook 'set-safe-composition-table)
;; (add-hook 'special-mode-hook 'set-safe-composition-table)
;; (add-hook 'text-mode-hook 'set-safe-composition-table)
;; (add-hook 'fundamental-mode-hook 'set-safe-composition-table)
;; (add-hook 'prog-mode-hook 'set-safe-composition-table)

;; (advice-add 'avy-process
;;             :around
;;             #'toggle-safe-composition-table--around)

;; (advice-add 'flash-jump
;;             :around
;;             #'toggle-safe-composition-table--around)
