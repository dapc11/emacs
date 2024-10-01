(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook '(delete-trailing-whitespace))
 '(compilation-scroll-output t)
 '(custom-safe-themes
    '("15b663428c3143bc9768ba668bbb95d3b10ac2bd8572a214b4db3e39d6e7513b" "26fc7e9dfcb2cd391b32a39eead4648b9fca83338d38faaf6ff2328aa7470f8a" "fd80f99cbd9077ea9f823d154f9c2cd7b8c433785c472d6376b6d19962bd02d4" "0646f6ee570ac4d067770f303b17f6103157c868fbdedde0561e6970115179f6" "5f47ed690794835ea94cc19624e9903c8affbea1bc366204354cf8bb33ece315" "e65dfa3b123acd893cb639a0fe6a3b05761eb61111ca7f141339817ed0f041ed" "57e5066a44e5bca03d4dc1e04f09f212300a869800cd985003438a1c4661d810" "f2bacb4ca24e71f638676b12e21585faeb80c8773917418591150503b6b3e44d" "ecabbcea4a3d424dccf1283d7f5e6e53c94d5bb82530ef571e3315aff132631f" "7ecffef64b4195bf1f491c7490e97d5a7b58382e130ced4e3279fadf24085b4d" "a2be864d5e1c1814cf0a6c70b3557a75ea0f0c392e7c4e7b54b341aa6a9eefed" "7fcb92fcad64676fe9b60eb883a32f7934a9bc375eb723e4341ba39516526cb8" "829be058f8c2dce410edc6596771eaf85f0d3cd560610d61de7036abfaf24143" "fa66d0bff4a3f0cde56d35e2c563c824335364527ed786141cbb5358fc8d9e79" "1637e3d4ea9607527dda4c302fa7fcab113ac66c0a0a958453937c7a12b5a695" "20aa6aa1aeb07ee2cc1f452a1e296a625c7f97fbb6bee050736217e975be26f0" "21396557b2d101aff827b684ecffed273bca013b1557638ffded81a821ab425c" "9321ad35b2b8cf8a3c3884bfaf78442d5194c20ccb6c260a85fb69413ad14669" "df5917c377cf64ac848573f3378ca5829e72c7ab674b013f573c7ae0802f9f3b" "74ba82021837ce3d882610de540c3fa20a0d2a7109890c59360ae0d1a3a43604" "019975b0c9c49d85c667e652a8fa07ee1f39b821b78af9b214880d569d1408aa" "3bd918439e75ce50d7a03cddcb4e51e8fc1850c8bb579851861ba31f5f025281" "08f3d2398295e9d6642a1e039e4d96b40f0ed9373876e6ada34f541667bc911e" "57282922be355a14a690313610c8e39df0be8a4dc42bb5004271eff07f57faae" "f331ecac8026270a5258bc2fdaa7c69e5ff81e3ee83ac7b43354d88c332cc862" "bec42717de1cc8ccad568df4e8486f0e9e2a14447cf85099222343fb31994641" "8574cbb5a72369efa5bf9c9bc43c4e5b7190c03e5a6e9ec831b7e9569a5eaf54" "75224260e1614142d3b07c74cabf67fe2d91f4c174c843762c886b851faa45f5" "0a2c1b0c0b4ddae5118781e1633593bbd5b55672301dce72a231cb2bb2e2a149" "72c6d0771b0575a9ad4065f4e466494aa8f19d3c15f9554a1e7a2756e626854f" "01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "c7f838704d7caa88bc337464867c22af0a502e32154558b0f6c9c3c6e8650122" "d92c1c36a5181cf629749bf6feee1886cf6bce248ab075c9d1b1f6096fea9539" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
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
    '(xterm-color flycheck dumb-jump rainbow-mode tabbar go-mode cape corfu embark-consult treemacs-magit treemacs-projectile consult treemacs smartparens expand-region json-mode git-gutter-fringe k8s-mode yaml-mode lua-mode dockerfile-mode rg ripgrep git-gutter exec-path-from-shell ag projectile magit vertico orderless multiple-cursors which-key))
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
