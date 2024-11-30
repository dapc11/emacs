(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments '("--smart-case" "--stats" "--vimgrep"))
 '(before-save-hook '(delete-trailing-whitespace))
 '(compilation-scroll-output t)
 '(custom-safe-themes
    '("b3eb51cae07003745332262b7289a16668df30384e7bcb14b38047584d59e2c2" "222a759131931b61b2f2f583bc34e1c43856053a76f1665f462914515c686018" "392d66a7a4b38a79f308adcc1f1b9bc59ea770c30d450fd515e71e429bf08cc8" "4aacfcb106ce7b37e51deef43180a82cb97d7857929ceeb4940ac0f9e3877984" "a049903f21f58e72cc528b74f0785c076de5cb7eeab92bde3eadcb4093b4dcbd" "d806f518aa7ff581cc1920e2132da978395aef85a1ebfc11291a83314ff031a7" "87a2b43835f3487e4387bc138c8a5dc00985fc927741ff072bc56c9d10b4f9b0" "471668594ebaa7e6ac194577d020fffb62c68d639f69836b976f4d5586eb3825" "f236389db80b49658dd7237840c661868d7176835f01ce66a1f1c3389ff7975f" "2e38d9cd62101b1813317fa16500a1f45dc975916cc0e565a24998e91f48f68e" "82da5861984da03345ed8fa68c16578fd624b2cb75fe0235926d7c28eea4e4d6" "dcd608ec5a4069066c2dae3c9228b6cdb386256e08503f9dfc38db89037a54cc" "16a246c07973d7a0da2c3efb17cdf8f4b5d1ae35a20d9b7b09cb7ece00054390" "f4bb6287708d57aed0d792d21c7df80470f703233819b89a9cf0841c814337ee" "22bac168b03a7bc65a417179b029afa5d459621aa8ae4975e5f715a05a421827" "2f701863f04f09787b4ac74d7113cad625b3ae5eaef2b4f18866f22a6c2a0608" "f5b895e86b90addfae4a6f564567d3778d3a20cff8d7878f362e31ee8ca9fba4" "8cdb331c38f5719a29a13094d3703c9545ccf9cceddefa2a4cdd2c9d42dc1dff" "b5c56c7c78cd937b6b18ac3ed1f1682ead8ec88475be23ce8822acb638cd3a6a" "ee33cf262c1582f6edafdcbf742503969f3c1bf90a9f847e64819c96d39f2ab5" "24b6ade0e3cabdfee9fa487961b089d059e048d77fe13137ea4788c1b62bd99d" "968f921a942756576efadb721bbf23f5d983b4186251920a1d5762a49a2d00e6" "c4847a17d607ae479425b4180f912a5f8b15aaf6602dcdbdf80b45f4f7b5c86f" "ab0e5658892d375ac996b68a4b32721e327f10246d62e1ba59e539477d167831" "283f9c7e709629a69635256b38d163c60025c7397ce0ab2012b5aa447688f052" "3f5397595ce0cab1951ef74cd9f76c0a34e9c6c7183a9c93eca11dccb7a39093" "b9f44212b4be6f0466811c5d8a297dda3c40dbf4c4cfd97c1686fceb2043b617" "6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901" "2119133daf4639a0151886f2a13ca32284ed7388a1b7d2401e8886d376a4b0a8"))
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
    '(git-timemachine treemacs-all-the-icons all-the-icons treemacs git-blamed rgb markdown-mode blacken python-mode eterm-256color pandoc github-dark-vscode-theme eglot-java eglot embark highlight-indentation hydra xterm-color flycheck dumb-jump rainbow-mode tabbar go-mode cape corfu embark-consult consult smartparens expand-region json-mode k8s-mode yaml-mode lua-mode dockerfile-mode rg ripgrep exec-path-from-shell ag projectile magit vertico orderless multiple-cursors))
 '(require-final-newline 'visit-save)
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))))
 '(tabbar-separator '(0.5))
 '(tabbar-use-images nil)
 '(use-package-always-ensure t)
 '(use-package-expand-minimally t)
 '(whitespace-style
    '(face tabs trailing space-before-tab newline empty space-after-tab tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
