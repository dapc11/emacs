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
 '(eglot-connect-timeout nil)
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
 '(mode-require-final-newline t)
 '(org-agenda-files '("~/notes/todo.md"))
 '(package-selected-packages
    '(ag ansi back-button blacken bm cape corfu dockerfile-mode dumb-jump
       eglot-java embark-consult eterm-256color exec-path-from-shell
       expand-region flycheck git-timemachine go-mode gptel
       highlight-indentation json-mode k8s-mode lua-mode magit
       markdown-mode multiple-cursors orderless org-modern pandoc
       projectile rainbow-mode rg ripgrep smartparens treemacs
       treesit-auto vertico vscode-dark-plus-theme))
 '(require-final-newline 'visit-save)
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))))
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
 '(highlight ((t (:foreground "#4db2ff" :underline nil))))
 '(hl-line ((t (:extend t))))
 '(lazy-highlight ((t (:background "dark slate gray" :foreground "gray"))))
 '(match ((t (:background "gray18" :weight medium))))
 '(mode-line ((t (:background "#303030" :foreground "#fafafa" :weight normal))))
 '(mode-line-inactive ((t (:background "#202020" :foreground "#d4d4d4" :weight normal))))
 '(tab-bar ((t (:background "#303030"))))
 '(tab-bar-tab ((t (:background "#434343" :foreground "#ffffff"))))
 '(whitespace-hspace ((t (:inherit whitespace-space))))
 '(whitespace-space ((t (:foreground "gray22"))))
 '(whitespace-tab ((t (:inherit whitespace-space))))
 '(window-divider ((t (:foreground "#303030" :box (:line-width (4 . 4) :color "#444d56" :style released-button)))))
 '(window-divider-first-pixel ((t (:foreground "#303030")))))
