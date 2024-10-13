(setq inhibit-startup-message t)
(setq lisp-indent-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-basic-indent 4)
(setq column-number-mode t)
(setq backup-directory-alist '(("." . "~/.emacsbackup")))
(setq line-spacing 0.1)
(setq ring-bell-function 'ignore)
(setq custom-file "~/.emacs.d/custom-file.el")
(setopt use-short-answers t)

(defvar dt/user-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")

(defun dt/load-user-init (filename)
  "Execute a file of Lisp code named FILENAME."
  (let ((user-init-file
          (expand-file-name filename
            dt/user-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t))))

(set-frame-font "JetBrains Mono" nil)
(tool-bar-mode 0)
(show-paren-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(delete-selection-mode)
(global-hl-line-mode)
(auto-save-visited-mode)
(global-display-line-numbers-mode)
(advice-add 'risky-local-variable-p :override #'ignore)
(xterm-mouse-mode t)
(load-file custom-file)

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
    use-package-expand-minimally t))

(dt/load-user-init "gruber-darker-theme.el")

(load-theme 'gruber-darker)

(dt/load-user-init "utils.el")


(use-package savehist)

(use-package multiple-cursors
  :ensure t
  :config
  :bind (
          ("M-<right>". mc/mark-next-like-this)
          ("M-<left>" . mc/mark-previous-like-this)))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :custom
  (ediff-diff-options "")
  (ediff-custom-diff-options "-u")
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-vertically)
  (byte-compile-warnings '(not docstrings))

  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-blame-echo-style 'headings)
  (add-to-list 'git-commit-finish-query-functions
    #'dt/git-commit-check-style-conventions))

(use-package git-commit
  :ensure nil
  :preface
  (defun dt/git-commit-set-fill-column ()
    (setq-local comment-auto-fill-only-comments nil)
    (set-face-foreground 'fill-column-indicator "salmon")
    (setq fill-column 72)
    (display-fill-column-indicator-mode))
  :config
  (advice-add 'git-commit-turn-on-auto-fill :before #'dt/git-commit-set-fill-column))

(use-package smartparens
  :init
  (smartparens-mode))

(use-package expand-region
  :bind (
          ("M-S-<right>" . er/expand-region)
          ("M-S-<left>" . er/contract-region)))

(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'next-line)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-c v") 'split-window-right)
(global-set-key (kbd "C-c q") 'dt/kill-and-close-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-S-r") 'isearch-query-replace)
(global-set-key (kbd "C-n") 'next-error)
(global-set-key (kbd "C-b") 'previous-error)
(global-set-key (kbd "C-t") 'treemacs)
(global-set-key (kbd "C-S-j") 'join-line)
(global-set-key (kbd "C-c n") 'projectile-find-file)
(global-set-key [prior] 'move-beginning-of-line)
(global-set-key [next] 'move-end-of-line)
(global-set-key (kbd "M-;") 'dabbrev-expand)
(global-set-key (kbd "C-c C-d") 'dt/duplicate-line)
(global-set-key (kbd "C-c d") 'dt/duplicate-line)
(global-set-key (kbd "C-c C-x") 'dt/unpop-to-mark-command)
(global-set-key (kbd "M-<down>") 'dt/move-text-down)
(global-set-key (kbd "M-<up>") 'dt/move-text-up)

(use-package projectile
  :init
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories "^bob$")
  (add-to-list 'projectile-globally-ignored-directories "^batteries$")
  (add-to-list 'projectile-globally-ignored-directories "^\\.bob$")
  (add-to-list 'projectile-globally-ignored-directories "^automation$")
  (add-to-list 'projectile-globally-ignored-directories "^httpprobe$")
  (add-to-list 'projectile-globally-ignored-directories "^__pycache__$")
  (add-to-list 'projectile-globally-ignored-directories "^vendor$")
  (add-to-list 'projectile-globally-ignored-directories "^target$")

  :config
  (setq
    projectile-indexing-method 'native
    projectile-completion-system 'auto
    projectile-project-search-path '(("~/repos" . 1))))

(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (go-mode . eglot-ensure))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (when (daemonp)
    (exec-path-from-shell-initialize))
  :config
  (exec-path-from-shell-copy-envs '("PATH" "SONARQUBE_TOKEN_CODEANALYZER" "JAVA_HOME" "M2_HOME" "M2" "MAVEN_OPTS" "GOPRIVATE" "GOPROXY")))

(use-package yaml-mode)
(use-package json-mode)
(use-package go-mode)
(use-package dockerfile-mode)
(use-package lua-mode)
(use-package k8s-mode)
(use-package ansi-color)

(setq auto-mode-alist
  (append
    '(
       ("\\.el\\'" . emacs-lisp-mode)
       ("\\.yaml\\'" . yaml-mode)
       ("\\.yml\\'" . yaml-mode)
       ("\\.tpl\\'" . k8s-mode)
       ("\\.go\\'" . go-mode)
       ("\\.py\\'" . python-mode)
       ("\\.py\\'" . smartparens-mode)
       ("\\.yaml\\'" . smartparens-mode)
       ("\\.yml\\'" . smartparens-mode)
       ("\\.tpl\\'" . smartparens-mode)
       ("\\.go\\'" . smartparens-mode)
       ("\\.lua\\'" . smartparens-mode)
       ("\\.json\\'" . smartparens-mode)
       ("\\.sh\\'" . smartparens-mode)
       ("\\.sh\\'" . shell-script-mode)
       ("Dockerfile\\'" . dockerfile-mode)
       )
    auto-mode-alist)
  )

(add-hook 'yaml-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'json-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'git-commit-setup-hook 'dt/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'compilation-filter-hook 'dt/apply-ansi-colors)

;; Load post-init.el
(dt/load-user-init "post-init.el")
