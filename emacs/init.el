(org-babel-load-file (expand-file-name "~/.dotfiles/emacs/litemacs.org"))

;; (when (window-system)
;;   (set-frame-font "Fira Code"))
;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                ; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))
;;
;; (let ((alist '(
;;   (33 . ".\\(?:\\(?:\\(?:!\\.\\|!\\)\\|\\(?:==\\|=\\)\\)\\)") ;; !
;;   (35 . ".\\(?:\\(?:[!(:={]\\|\\(?:#\\(?:##\\|#\\)\\|#\\)\\|\\?\\|\\[\\|\\(?:_(\\|_\\)\\)\\)") ;; #
;;   (36 . ".\\(?:>\\)") ;; $
;;   (37 . ".\\(?:%\\)") ;; %
;;   (38 . ".\\(?:&\\)") ;; &
;;   (42 . ".\\(?:\\(?:[/>]\\|\\(?:\\*\\*\\|\\*\\)\\)\\)") ;; *
;;   (43 . ".\\(?:\\(?:\\(?:\\+\\+\\|\\+\\)\\|>\\)\\)") ;; +
;;   (45 . ".\\(?:\\(?:[|~]\\|\\(?:-\\(?:-\\|>\\)\\|-\\)\\|\\(?:<<\\|<\\)\\|\\(?:>>\\|>\\)\\)\\)") ;; -
;;   ; (46 . ".\\(?:\\(?:-\\|\\(?:\\.\\(?:\\.\\|<\\)\\|\\.\\)\\|=\\|\\?\\)\\)") ;; .
;;   (47 . ".\\(?:\\(?:[>\\]\\|\\*\\|\\(?://\\|/\\)\\|\\(?:==\\|=\\)\\)\\)") ;; /
;;   (48 . ".\\(?:x[0-9A-Fa-f]\\)") ;; 0
;;   (58 . ".\\(?:\\(?:[<->]\\|\\(?::[:=]\\|:\\)\\)\\)") ;; :
;;   (59 . ".\\(?:;\\)") ;; ;
;;   (60 . ".\\(?:\\(?:[:>]\\|!--\\|\\(?:\\$>\\|\\$\\)\\|\\(?:\\*>\\|\\*\\)\\|\\(?:\\+>\\|\\+\\)\\|\\(?:-\\(?:[<>|]\\|-\\)\\|-\\)\\|\\(?:/>\\|/\\)\\|\\(?:<\\(?:[<=]\\|-\\)\\|<\\)\\|\\(?:=\\(?:[<>|]\\|\\(?:=>\\|=\\)\\)\\|=\\)\\|\\(?:|\\(?:>\\|\\(?:||\\||\\)\\)\\||\\)\\|\\(?:~[>~]\\|~\\)\\)\\)") ;; <
;;   (61 . ".\\(?:\\(?:!=\\|/=\\|:=\\|<<\\|\\(?:=[=>]\\|=\\)\\|\\(?:>>\\|>\\)\\)\\)") ;; =
;;   (62 . ".\\(?:\\(?:\\(?:->\\|-\\)\\|:\\|\\(?:=>\\|=\\)\\|\\(?:>\\(?:[=>]\\|-\\)\\|>\\)\\)\\)") ;; >
;;   (63 . ".\\(?:\\(?:[:=]\\|\\.\\|\\?\\)\\)") ;; ?
;;   (70 . ".\\(?:l\\)") ;; F
;;   (84 . ".\\(?:l\\)") ;; T
;;   (91 . ".\\(?:\\(?::]\\||\\)\\)") ;; [
;;   (92 . ".\\(?:/\\)") ;; \
;;   (93 . ".\\(?:#\\)") ;; ]
;;   (94 . ".\\(?:=\\)") ;; ^
;;   (95 . ".\\(?:\\(?:_\\||_\\)\\)") ;; _
;;   (102 . ".\\(?:l\\)") ;; f
;;   (119 . ".\\(?:ww\\)") ;; w
;;   (123 . ".\\(?:|\\)") ;; {
;;   (124 . ".\\(?:\\(?:[>}]\\|\\(?:->\\|-\\)\\|\\(?:=>\\|=\\)\\|]\\|\\(?:|\\(?:[=>]\\|-\\||>\\)\\||\\)\\)\\)") ;; |
;;   (126 . ".\\(?:\\(?:[=>@]\\|-\\|\\(?:~>\\|~\\)\\)\\)") ;; ~
;;              )))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))


;;; emacs ends here
(provide 'emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   '("https://www.youtube.com/feeds/videos.xml?channel_id=UCZAENaOaceQUMd84GDc26EA" "https://lukesmith.xyz/rss.xml"))
 '(fira-code-mode-disabled-ligatures '("[]" "x"))
 '(package-selected-packages
   '(magit-todos evil-indent-plus evil-escape evil-args evil-embrace parinfer solaire-mode fish-completion bash-completion zlc lsp-ui gdscript-mode clojure-mode-extra-font-locking cython-mode lsp-mode eshell-prompt-extras eshell-prompt-extra elfeed yasnippet-snippets yapfify yaml-mode ws-butler wisi which-key vimrc-mode use-package uniquify-files typescript-mode terminal-here smooth-scrolling smart-tabs-mode shakespeare-mode scala-mode rust-mode rjsx-mode rainbow-delimiters racket-mode purescript-mode prettier-js plantuml-mode perfect-margin org-bullets mpv mixed-pitch mermaid-mode lsp-ivy lsp-haskell ledger-mode kotlin-mode key-chord jinja2-mode ivy-hydra indent-guide hy-mode hlint-refactor hl-todo hindent highlight-indent-guides hasklig-mode git-gutter-fringe focus flycheck-rust flycheck-pos-tip flycheck-kotlin flycheck-haskell flycheck-elm fira-code-mode evil-surround evil-smartparens evil-quickscope evil-org evil-numbers evil-magit evil-leader evil-iedit-state evil-goggles evil-commentary evil-collection eshell-toggle esh-autosuggest emmet-mode elpy elm-mode eglot dumb-jump doom-themes doom-modeline dockerfile-mode docker dired-sidebar dashboard darkroom dap-mode csharp-mode counsel-projectile company-tabnine company-lsp company-jedi cmake-mode cider centered-window centaur-tabs bnf-mode auto-package-update auctex all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
