(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook '(delete-trailing-whitespace))
 '(compilation-scroll-output t)
 '(custom-safe-themes
    '("283f9c7e709629a69635256b38d163c60025c7397ce0ab2012b5aa447688f052" "3f5397595ce0cab1951ef74cd9f76c0a34e9c6c7183a9c93eca11dccb7a39093" "b9f44212b4be6f0466811c5d8a297dda3c40dbf4c4cfd97c1686fceb2043b617" "6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901" "2119133daf4639a0151886f2a13ca32284ed7388a1b7d2401e8886d376a4b0a8"))
 '(eglot-connect-timeout nil t)
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
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
 '(org-agenda-files '("~/notes/todo.md"))
 '(package-selected-packages
    '(github-dark-vscode-theme catppuccin-theme eglot-java eglot embark highlight-indentation hydra xterm-color flycheck dumb-jump rainbow-mode tabbar go-mode cape corfu embark-consult consult smartparens expand-region json-mode k8s-mode yaml-mode lua-mode dockerfile-mode rg ripgrep exec-path-from-shell ag projectile magit vertico orderless multiple-cursors))
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))))
 '(tabbar-separator '(0.5))
 '(whitespace-style
    '(face tabs trailing space-before-tab newline empty space-after-tab tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#191919" :foreground "#ccd1db" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight ultra-light :height 140 :width normal :foundry "JB" :family "JetBrains Mono")))))
