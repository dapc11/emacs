(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments '("--smart-case" "--stats" "--vimgrep"))
 '(auto-save-visited-interval 40)
 '(before-save-hook '(delete-trailing-whitespace))
 '(compilation-scroll-output t)
 '(custom-safe-themes t)
 '(eglot-connect-timeout nil)
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(grep-command "rg --vimgrep ")
 '(grep-find-command
    '("find . -type f -exec grep --color=auto -nH --null -e  \\{\\} +" . 54))
 '(grep-find-template
    "find -H <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
 '(grep-find-use-xargs 'exec-plus)
 '(grep-highlight-matches 'auto)
 '(grep-template "grep <X> <C> -nH --null -e <R> <F>")
 '(grep-use-null-device nil)
 '(grep-use-null-filename-separator t)
 '(ignored-local-variable-values
    '((lsp-diagnostics-disabled-modes python-mode)
       (eval flycheck-add-next-checker 'python-flake8
         '(warning . python-pylint))
       (flycheck-checker . python-flake8)
       (add-hook 'python-mode-hook
         (lambda nil
           (setq indent-tabs-mode t)
           (setq tab-width 4)
           (setq python-indent-offset 4)))))
 '(magit-auto-revert-mode t)
 '(mode-require-final-newline t)
 '(org-agenda-files '("~/notes/todo.md"))
 '(package-selected-packages
    '(treemacs-projectile bm git-timemachine treemacs-all-the-icons all-the-icons treemacs git-blamed rgb markdown-mode blacken python-mode eterm-256color pandoc github-dark-vscode-theme eglot-java eglot embark highlight-indentation hydra xterm-color flycheck dumb-jump rainbow-mode tabbar go-mode cape corfu embark-consult consult smartparens expand-region json-mode k8s-mode yaml-mode lua-mode dockerfile-mode rg ripgrep exec-path-from-shell ag projectile magit vertico orderless multiple-cursors))
 '(require-final-newline 'visit-save)
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))))
 '(tabbar-separator '(0.5))
 '(tabbar-use-images nil)
 '(treemacs-collapse-dirs 3)
 '(treemacs-filewatch-mode t)
 '(treemacs-follow-mode t)
 '(treemacs-git-mode t)
 '(treemacs-project-follow-mode t)
 '(treemacs-select-when-already-in-treemacs 'next-or-back)
 '(use-package-always-ensure t)
 '(use-package-expand-minimally t)
 '(whitespace-style
    '(face trailing tabs spaces missing-newline-at-eof empty indentation space-after-tab space-before-tab space-mark tab-mark))
 '(window-divider-default-right-width 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treemacs-hl-line-face ((t (:inherit hl-line :background "#24292e" :weight bold))))
 '(treemacs-window-background-face ((t (:background "#2f363d"))))
 '(whitespace-space ((t (:background "#24292e" :foreground "#444d56"))))
 '(window-divider ((t (:foreground "#444d56")))))
