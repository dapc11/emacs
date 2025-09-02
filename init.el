;;; init.el --- init file -*- lexical-binding: t; -*-

(setopt
  native-comp-async-report-warnings-errors 'silent
  inhibit-startup-message t
  lisp-indent-offset 2
  column-number-mode t
  line-spacing 0.1
  tool-bar-mode nil
  ring-bell-function 'ignore
  use-short-answers t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-basic-indent 4)
(setq-default cursor-type 'bar)

(setq backup-directory-alist '(("." . "~/.emacsbackup")))
(setq custom-file "~/.emacs.d/custom-file.el")
(defconst dt/home-dir (expand-file-name "~/")
  "User's home directory.")
;; (setq debug-on-error t)

(defvar dt/user-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")

(defun dt/load-user-init (filename)
  "Execute a file of Lisp code named FILENAME."
  (let ((user-init-file
          (expand-file-name filename
            dt/user-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t))))

(which-key-mode 1)
(global-font-lock-mode 1)
(tool-bar-mode 0)
(show-paren-mode 1)
(menu-bar-mode 1)
(scroll-bar-mode 1)
(delete-selection-mode 1)
(doom-modeline-mode 1)
(global-hl-line-mode)
(auto-save-visited-mode)
(global-display-line-numbers-mode)
(advice-add 'risky-local-variable-p :override #'ignore)
(xterm-mouse-mode t)
(load-file custom-file)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)           ; Don't show revert messages
(setq global-auto-revert-non-file-buffers t)  ; Also revert dired buffers
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")

(require 'package)
(setq package-archives
  '(("gnu"   . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
    use-package-expand-minimally t))

;; (load-theme 'vscode-dark-plus)
(use-package catppuccin-theme)
(setq catppuccin-flavor 'macchiato) ;; or 'latte, 'macchiato, or 'mocha
(load-theme 'catppuccin t)

(dt/load-user-init "utils.el")

(use-package savehist
  :init
  (savehist-mode 1))

(use-package recentf
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never))

(use-package multiple-cursors
  :ensure t
  :config
  :bind (
          ("M-<right>". mc/mark-next-like-this)
          ("M-<left>" . mc/mark-previous-like-this)))
(use-package magit
  :defer t
  :bind (("C-c g" . magit-status))
  :commands (magit-status)
  :custom
  (ediff-diff-options "")
  (ediff-custom-diff-options "-u")
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-vertically)
  (byte-compile-warnings '(not docstrings))

  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (setq magit-blame-echo-style 'headings)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

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

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons)

(setq doom-modeline-height 1)
(if (facep 'mode-line-active)
  (set-face-attribute 'mode-line-active nil :family "JetBrains Mono NL Medium" :height 140) ; For 29+
  (set-face-attribute 'mode-line nil :family "JetBrains Mono NL Medium" :height 140))
(set-face-attribute 'mode-line-inactive nil :family "JetBrains Mono NL Medium" :height 140)

(use-package expand-region
  :bind (
          ("M-S-<right>" . er/expand-region)
          ("M-S-<left>" . er/contract-region)))

(defun join-line-above ()
  "Join the current line with the line above."
  (interactive)
  (forward-line 1)
  (end-of-line)
  (delete-indentation))

(keymap-global-set "C-S-j"       #'join-line-above)
(keymap-global-set "<mouse-4>"   #'previous-line)
(keymap-global-set "<mouse-5>"   #'next-line)
(keymap-global-set "C-c <right>" #'windmove-right)
(keymap-global-set "C-c <left>"  #'windmove-left)
(keymap-global-set "C-c <up>"    #'windmove-up)
(keymap-global-set "C-c <down>"  #'windmove-down)
(keymap-global-set "C-c -"       #'split-window-below)
(keymap-global-set "C-c v"       #'split-window-right)
(keymap-global-set "C-c q"       #'dt/kill-and-close-buffer)
(keymap-global-set "C-c k"       #'kill-this-buffer)
(keymap-global-set "C-r"         #'query-replace)
(keymap-global-set "C-f"         #'occur)
(keymap-global-set "C-c n"       #'projectile-find-file)
(keymap-global-set "<prior>"     #'move-beginning-of-line)
(keymap-global-set "<next>"      #'move-end-of-line)
(keymap-global-set "M-;"         #'dabbrev-expand)
(keymap-global-set "C-c C-d"     #'dt/duplicate-line)
(keymap-global-set "C-c d"       #'dt/duplicate-line)
(keymap-global-set "C-c C-x"     #'dt/unpop-to-mark-command)
(keymap-global-set "M-<down>"    #'dt/move-text-down)
(keymap-global-set "M-<up>"      #'dt/move-text-up)
(keymap-global-set "C-c M-<left>"  #'windmove-swap-states-left)
(keymap-global-set "C-c M-<right>" #'windmove-swap-states-right)
(keymap-global-set "C-c M-<up>"    #'windmove-swap-states-up)
(keymap-global-set "C-c M-<down>"  #'windmove-swap-states-down)
(keymap-global-set "M-e"         #'treemacs)
(keymap-global-set "C-+"         #'my-increase-font-size)
(keymap-global-set "C--"         #'my-decrease-font-size)

(setq magit-blame-styles
  '((margin
      (margin-format "%h %-3C %-10a")
      (margin-width . 37)
      (margin-face . magit-blame-margin)
      (margin-body-face magit-blame-dimmed))))

(defun scroll-half-page-down ()
  "Scroll down half the page."
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "Scroll up half the page."
  (interactive)
  (scroll-up (/ (window-body-height) 2)))
(keymap-global-set "C-v" #'scroll-half-page-up)
(keymap-global-set "M-v" #'scroll-half-page-down)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind (("C-S-f" . projectile-ripgrep))
  :custom
  (projectile-indexing-method 'native)
  (projectile-completion-system 'auto)
  (projectile-generic-command "fd . --type f --color=never")
  (projectile-enable-caching t)
  (projectile-project-search-path '(("~/repos" . 1)))
  (projectile-globally-ignored-directories
    '("bob"
       "batteries"
       ".bob"
       "automation"
       "httpprobe"
       "__pycache__"
       "vendor"
       "target")))


(use-package go-mode)
(use-package python
  :ensure nil)
(use-package json-mode)
(use-package dockerfile-mode)
(use-package lua-mode)
(use-package k8s-mode)
(use-package ansi-color)
(use-package blacken)
(use-package markdown-mode)
(with-eval-after-load 'markdown-mode
  (setq markdown-mode-map (make-sparse-keymap)))


(use-package eglot
  :init
  (setq eglot-sync-connect 3
    eglot-connect-timeout nil
    eglot-autoshutdown t
    eglot-send-changes-idle-time 0.5
    eglot-report-progress t
    eglot-ignored-server-capabilities '(:documentHighlightProvider
                                         :foldingRangeProvider)
    ;; NOTE Disable eglot-auto-display-help-buffer because :select t
    ;;      its popup rule causes eglot to steal focus too often.
    eglot-auto-display-help-buffer nil)
  :bind (
          ("C-c c a" . eglot-code-actions)
          ("C-c c f" . eglot-format)
          ("C-c c r" . eglot-rename)
          ("M-n" . flymake-goto-next-error)
          ("M-b" . flymake-goto-prev-error)
          ("C-c c c" . eglot-code-action-quickfix)
          ("C-c c e" . eglot-code-action-extract)
          ("C-c c i" . eglot-code-action-inline)))

(dolist
  (mode-spec
    '(("Dockerfile\\'" . dockerfile-mode)
       ("\\.el\\'"     . emacs-lisp-mode)
       ("\\.go\\'"     . go-mode)
       ("\\.java\\'"   . java-mode)
       ("\\.py\\'"     . python-mode)
       ("\\.sh\\'"     . bash-mode)
       ("\\.tpl\\'"    . k8s-mode)
       ("\\.yaml\\'"   . yaml-ts-mode)
       ("\\.yml\\'"    . yaml-ts-mode)))
  (add-to-list 'auto-mode-alist mode-spec))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    '(python-mode . ("pyright-langserver" "--stdio"))))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    '(go-mode . ("gopls" "serve"))))

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)

;; Whitespace mode
(defun dt/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'compilation-filter-hook 'dt/apply-ansi-colors)
(add-hook 'html-mode-hook          'dt/apply-ansi-colors)
(add-hook 'emacs-lisp-mode-hook    'dt/set-up-whitespace-handling)
(add-hook 'git-commit-setup-hook   'dt/set-up-whitespace-handling)
(add-hook 'go-mode-hook            'dt/set-up-whitespace-handling)
(add-hook 'java-mode-hook          'dt/set-up-whitespace-handling)
(add-hook 'json-mode-hook          'dt/set-up-whitespace-handling)
(add-hook 'lua-mode-hook           'dt/set-up-whitespace-handling)
(add-hook 'python-mode-hook        'dt/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook          'dt/set-up-whitespace-handling)

(setq org-todo-keywords
  '((sequence "TODO" "ONGOING" "TESTING" "IN REVIEW" "ON HOLD" "DONE" "ABANDONED")))

(add-hook 'treemacs-mode-hook (lambda()(display-line-numbers-mode -1)))
(window-divider-mode 1)

(defun my-increase-font-size ()
  "Increase the font size globally."
  (interactive)
  (let ((current-size (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ current-size 10))))

(defun my-decrease-font-size ()
  "Decrease the font size globally."
  (interactive)
  (let ((current-size (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- current-size 10))))

(add-hook 'after-save-hook
  (lambda ()
    (when (string= (file-name-extension buffer-file-name) "el")
      (byte-compile-file buffer-file-name))))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq
    scroll-conservatively 101 ; important!
    scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(defun my-projectile-replace-advice (original-fun &rest args)
  "Advice to ensure `projectile-replace` uses Bash for subprocess."
  (let
    ((shell-file-name "/bin/bash"))
    (apply original-fun args))
  (let
    ((shell-command-switch "-ic"))
    (apply original-fun args)))

;; Add advice around `projectile-replace`
(advice-add 'projectile-replace :around #'my-projectile-replace-advice)

(defun markdown-to-jira ()
  "Convert the current buffer from Markdown to Jira using Pandoc."
  (interactive)
  (let* ((input-file (make-temp-file "md2jira" nil ".md"))
          (output-file (make-temp-file "md2jira" nil ".jira"))
          (pandoc-command (format "pandoc --from markdown --to jira -o %s %s"
                            output-file input-file)))
    (write-region (point-min) (point-max) input-file)
    (shell-command pandoc-command)
    (erase-buffer)
    (insert-file-contents output-file)
    (delete-file input-file)
    (delete-file output-file)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)     ;; Use Embark as an action menu
   ("C-;" . embark-dwim))   ;; Smart actions
  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if prefix
             (format "Embark %s %s"
                     (plist-get (car targets) :type)
                     prefix)
           (format "Embark %s"
                   (plist-get (car targets) :type)))
         keymap nil nil t))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Load post-init.el
(dt/load-user-init "post-init.el")
