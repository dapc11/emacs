(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments '("--smart-case" "--stats" "--vimgrep"))
 '(auto-save-visited-interval 40)
 '(compilation-error-regexp-alist
    '(go-test ant bash python-tracebacks-and-caml comma java javac maven
       cucumber shellcheck))
 '(compilation-scroll-output t)
 '(custom-safe-themes t)
 '(eglot-connect-timeout nil)
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
 '(ignored-local-variable-values
    '((lsp-diagnostics-disabled-modes python-mode)
       (eval flycheck-add-next-checker 'python-flake8
         '(warning . python-pylint))
       (flycheck-checker . python-flake8)
       (add-hook 'python-mode-hook
         (lambda nil (setq indent-tabs-mode t)
           (setq tab-width 4) (setq python-indent-offset 4)))))
 '(magit-auto-revert-mode t)
 '(mode-require-final-newline t)
 '(org-agenda-files '("~/notes/todo.md"))
 '(package-selected-packages
    '(ag all-the-icons blacken bm cape consult corfu dockerfile-mode
       dumb-jump eglot eglot-java embark embark-consult eterm-256color
       exec-path-from-shell expand-region flycheck git-blamed
       git-timemachine github-dark-vscode-theme go-mode gptel
       highlight-indentation hydra json-mode k8s-mode lua-mode magit
       markdown-mode multiple-cursors orderless pandoc projectile
       python-mode rainbow-mode rg rgb ripgrep smartparens tabbar
       transpose-frame treemacs treemacs-all-the-icons
       treemacs-projectile vertico xterm-color yaml-mode))
 '(require-final-newline 'visit-save)
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))))
 '(tabbar-separator '(0.5))
 '(tabbar-use-images nil)
 '(treemacs-collapse-dirs 3)
 '(treemacs-filewatch-mode t)
 '(treemacs-follow-mode t)
 '(treemacs-fringe-indicator-mode nil)
 '(treemacs-git-mode nil)
 '(treemacs-project-follow-mode t)
 '(treemacs-select-when-already-in-treemacs 'next-or-back)
 '(use-package-always-ensure t)
 '(use-package-expand-minimally t)
 '(whitespace-style
    '(face trailing tabs spaces missing-newline-at-eof empty indentation
       space-after-tab space-before-tab space-mark tab-mark))
 '(window-divider-default-right-width 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t))))
 '(window-divider ((t (:foreground "#444d56" :box (:line-width (2 . 2) :color "#444d56" :style released-button))))))
