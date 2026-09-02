;; -*- lexical-binding: t; -*-
;; (setenv "LSP_USE_PLISTS" "true")

(defvar cpu-arch "znver4")
(setq native-comp-compiler-options '("-Os"
                                     "-g0"
                                     "-fno-omit-frame-pointer"
                                     "-fno-finite-math-only"))
(setq native-comp-driver-options `(,(format "-mtune=%s" cpu-arch)
                                   ,(format "-march=%s" cpu-arch)))

(setq load-prefer-newer t)

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
(setq straight-check-for-modifications '(find-when-checking))
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
;; (setq straight-use-package-by-default t)

;; (setq use-package-compute-statistics t)

(use-package use-package-core
  :custom
  (use-package-hook-name-suffix nil))

(use-package benchmark-init
  :disabled
  :straight t
  :hook (after-init-hook . benchmark-init/deactivate)
  :demand)

(use-package savehist
  :custom
  (savehist-file (concat user-emacs-directory "savehist"))
  (savehist-save-minibuffer-history t)
  (history-length 100)
  (history-delete-duplicates t)
  :hook
  (savehist-save-hook
   . (lambda ()
       (setq kill-ring
             (mapcar #'substring-no-properties
                     (cl-remove-if-not #'stringp kill-ring)))))
  :config
  (savehist-mode)
  (push 'kill-ring savehist-additional-variables)
  (push 'regexp-search-ring savehist-additional-variables)
  (push 'search-ring savehist-additional-variables))

(use-package saveplace
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600)
  :config
  (save-place-mode))

(setq ring-bell-function 'ignore)

(defvar the-font)
(defvar the-nice-font)
(defvar the-font-height)
(defvar the-font-width)
(defvar the-font-weight)
(defun set-fonts (font-height)
  (interactive "nFont height: ")
  (setq the-font "Aporetic Sans Mono")
  ;; (setq the-font "IoskeleyMono Nerd Font")
  ;; (setq the-nice-font "Iosevka Aile")
  (setq the-nice-font "Aporetic Sans")
  (setq the-font-height font-height)
  (setq the-font-width 'normal)
  (setq the-font-weight 'normal)
  ;; (setq-default line-spacing '(0.05 . 0.05))
  (custom-set-faces
   `(default
     ((t (:family ,the-font :height ,the-font-height :weight ,the-font-weight :width ,the-font-width))))
   `(fixed-pitch-serif
     ((t (:family ,the-font :height ,the-font-height :weight ,the-font-weight :width ,the-font-width))))
   `(fixed-pitch
     ((t (:family ,the-font :height ,the-font-height :weight ,the-font-weight :width ,the-font-width))))
   `(variable-pitch
     ((t (:family ,the-nice-font :height ,the-font-height :weight ,the-font-weight :width ,the-font-width))))))
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
