(unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))
(org-babel-load-file (expand-file-name "~/.dotfiles/litemacs.org"))

;;; emacs ends here
(provide 'emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-toggle-size-fraction 5)
 '(package-selected-packages
   '(smooth-scrolling perfect-margin centered-window flycheck-elm emmet-mode yasnippet smartparens projectile markdown-mode magit lsp-mode ivy haskell-mode git-gutter flycheck evil company ccls all-the-icons docker tablist json-mode json-snatcher json-reformat docker-tramp dockerfile-mode evil-goggles quelpa-use-package quelpa company-box mixed-pitch org-superstar-mode dap-mode emacs-ccls yasnippet-snippets yapfify yaml-mode wisi which-key use-package uniquify-files typescript-mode terminal-here shakespeare-mode scala-mode rust-mode rjsx-mode rainbow-delimiters pyvenv purescript-mode prettier-js plantuml-mode org-bullets mermaid-mode lsp-ui lsp-ivy lsp-haskell ledger-mode kotlin-mode key-chord jinja2-mode ivy-hydra hlint-refactor hl-todo hindent highlight-indent-guides git-gutter-fringe flycheck-rust flycheck-pos-tip flycheck-kotlin flycheck-haskell evil-surround evil-smartparens evil-org evil-magit evil-leader evil-commentary evil-collection eshell-toggle elm-mode eglot doom-themes doom-modeline dired-sidebar csharp-mode counsel-projectile company-lsp company-jedi cmake-mode bnf-mode auto-package-update auctex all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
