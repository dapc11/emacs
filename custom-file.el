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
    '(ag ansi back-button blacken bm cape corfu dockerfile-mode dumb-jump
       eglot-java embark-consult eterm-256color exec-path-from-shell
       expand-region flycheck git-timemachine go-mode gptel
       highlight-indentation json-mode k8s-mode lua-mode magit
       markdown-mode multiple-cursors orderless org-modern pandoc
       projectile python-mode rainbow-mode rg ripgrep smartparens
       tabbar treemacs treesit-auto vertico vscode-dark-plus-theme))
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
 '(highlight ((t (:foreground "#4db2ff" :underline nil))))
 '(hl-line ((t (:extend t))))
 '(lazy-highlight ((t (:background "dark slate gray" :foreground "gray"))))
 '(mode-line ((t (:background "DeepSkyBlue4" :foreground "#fafafa" :weight normal))))
 '(mode-line-inactive ((t (:background "gray24" :foreground "#d4d4d4" :weight normal))))
 '(whitespace-hspace ((t (:inherit whitespace-space))))
 '(whitespace-space ((t (:foreground "gray22"))))
 '(whitespace-tab ((t (:inherit whitespace-space))))
 '(window-divider ((t (:foreground "#444d56" :box (:line-width (2 . 2) :color "#444d56" :style released-button))))))
