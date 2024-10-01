(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook '(delete-trailing-whitespace))
 '(compilation-scroll-output t)
 '(custom-safe-themes
    '("b0fda28799663e26d5a800c70f245d255933f2ad66eeade2c986707d3fa5c957" default))
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
 '(package-selected-packages
    '(highlight-indentation hydra xterm-color flycheck dumb-jump rainbow-mode tabbar go-mode cape corfu embark-consult consult smartparens expand-region json-mode k8s-mode yaml-mode lua-mode dockerfile-mode rg ripgrep exec-path-from-shell ag projectile magit vertico orderless multiple-cursors))
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))))
 '(tabbar-separator '(0.5))
 '(whitespace-style
    '(face tabs trailing space-before-tab newline empty space-after-tab tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
