;;; custom-file.el --- custom file -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments '("--smart-case" "--stats" "--vimgrep"))
 '(auto-save-visited-interval 40)
 '(column-number-mode t)
 '(compilation-error-regexp-alist
    '(go-test ant bash python-tracebacks-and-caml comma java javac maven
       cucumber shellcheck))
 '(compilation-scroll-output t)
 '(custom-safe-themes t)
 '(eglot-connect-timeout nil t)
 '(global-display-line-numbers-mode t)
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(grep-command "rg --vimgrep ")
 '(grep-find-command
    '("find . -type f -exec grep --color=auto -nH --null -e  \\{\\} +"
       . 54))
 '(grep-find-template
    "find -H <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
 '(grep-find-use-xargs 'exec-plus)
 '(grep-highlight-matches 'auto)
 '(grep-template "grep <X> <C> -nH --null -e <R> <F>")
 '(grep-use-null-device nil)
 '(grep-use-null-filename-separator t)
 '(js-indent-level 2)
 '(magit-auto-revert-mode t)
 '(menu-bar-mode nil)
 '(mode-require-final-newline t)
 '(org-agenda-files '("~/notes/todo.md"))
 '(package-selected-packages
    '(back-button blacken bm cape catppuccin-theme corfu dockerfile-mode
       doom-modeline dumb-jump eglot-java embark-consult expand-region
       git-gutter-fringe git-timemachine go-mode gptel-commit
       gptel-magit json-mode k8s-mode llm lua-mode magit-todos
       marginalia markdown-mode markdown-preview-eww multiple-cursors
       orderless pandoc projectile rainbow-mode request rg smartparens
       tablist treemacs ultra-scroll vertico vscode-dark-plus-theme))
 '(package-vc-selected-packages
    '((ultra-scroll :url "https://github.com/jdtsmith/ultra-scroll")))
 '(require-final-newline 'visit-save)
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))))
 '(scroll-margin 3)
 '(tab-bar-mode t)
 '(tab-bar-show 1)
 '(treemacs-collapse-dirs 3)
 '(treemacs-filewatch-mode t)
 '(treemacs-follow-mode t)
 '(treemacs-fringe-indicator-mode nil)
 '(treemacs-git-mode nil)
 '(treemacs-no-png-images t)
 '(treemacs-project-follow-mode t)
 '(treemacs-select-when-already-in-treemacs 'next-or-back)
 '(use-package-always-ensure t)
 '(use-package-expand-minimally t)
 '(whitespace-style
    '(face trailing tabs spaces missing-newline-at-eof empty indentation
       space-after-tab space-before-tab space-mark tab-mark))
 '(window-divider-default-right-width 6)
 '(window-divider-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "CTDB" :family "JetBrains Mono NL Medium"))))
 '(font-lock-comment-face ((t (:inherit font-lock-constant-face))))
 '(gptel-context-highlight-face ((t (:extend t :background "#1e2030"))))
 '(whitespace-space ((t (:inherit whitespace-indentation))))
 '(window-divider ((t (:foreground "#303030" :box (:line-width (4 . 4) :color "#444d56" :style released-button)))))
 '(window-divider-first-pixel ((t (:foreground "#303030")))))
