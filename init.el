;;; init.el --- init file -*- lexical-binding: t; -*-

(setopt
  native-comp-async-report-warnings-errors 'silent
  inhibit-startup-message t
  lisp-indent-offset 2
  column-number-mode t
  line-spacing 0.1
  ring-bell-function 'ignore
  use-short-answers t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-basic-indent 4)
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

(defun dt/set-font-based-on-dpi ()
  "Set font based on screen DPI (HiDPI vs standard)."
  (let ((font-name "Jetbrains Mono")
         (hidpi-font-size 16)          ;; Font size for HiDPI screens
         (standard-font-size 15)        ;; Font size for standard DPI screens
         (dpi-threshold 100))           ;; DPI threshold for HiDPI
    (let* ((dpi (/ (display-pixel-width) (/ (display-mm-width) 25.4))) ;; Calculate DPI
            (font-size (if (> dpi dpi-threshold)
                         hidpi-font-size
                         standard-font-size)))
      (if (member font-name (font-family-list))
        (set-face-attribute 'default nil :font (format "%s-%d" font-name font-size))
        (message "Font %s is not available on this system" font-name)))))
(dt/set-font-based-on-dpi)

(which-key-mode 1)
(global-font-lock-mode 1)
(tool-bar-mode 0)
(show-paren-mode 1)
(menu-bar-mode 1)
(scroll-bar-mode 1)
(delete-selection-mode)
(global-hl-line-mode)
(auto-save-visited-mode)
(global-display-line-numbers-mode)
(advice-add 'risky-local-variable-p :override #'ignore)
(xterm-mouse-mode t)
(load-file custom-file)
;; (add-to-list 'default-frame-alist '(undecorated . t))

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

;; (dt/load-user-init "gruber-darker-theme.el")
(load-theme 'vscode-dark-plus)
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
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
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


(defun my-next-error-or-flymake-next ()
  "Invoke `next-error`, falling back to `flymake-goto-next-error` if no errors."
  (interactive)
  (condition-case nil
    (next-error)
    (error
      (when (bound-and-true-p flymake-mode)
        (flymake-goto-next-error)))))

(defun my-previous-error-or-flymake-prev ()
  "Invoke `previous-error`, falling back to `flymake-goto-prev-error` if no errors."
  (interactive)
  (condition-case nil
    (previous-error)
    (error
      (when (bound-and-true-p flymake-mode)
        (flymake-goto-prev-error)))))

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
(keymap-global-set "C-n"         #'my-next-error-or-flymake-next)
(keymap-global-set "C-b"         #'my-previous-error-or-flymake-prev)
(keymap-global-set "C-c n"       #'projectile-find-file)
(keymap-global-set "<prior>"     #'move-beginning-of-line)
(keymap-global-set "<next>"      #'move-end-of-line)
(keymap-global-set "M-;"         #'dabbrev-expand)
(keymap-global-set "C-c C-d"     #'dt/duplicate-line)
(keymap-global-set "C-c d"       #'dt/duplicate-line)
(keymap-global-set "C-c C-x"     #'dt/unpop-to-mark-command)
(keymap-global-set "M-<down>"    #'dt/move-text-down)
(keymap-global-set "M-<up>"      #'dt/move-text-up)
(keymap-global-set "C-M-<left>"  #'windmove-swap-states-left)
(keymap-global-set "C-M-<right>" #'windmove-swap-states-right)
(keymap-global-set "C-M-<up>"    #'windmove-swap-states-up)
(keymap-global-set "C-M-<down>"  #'windmove-swap-states-down)
(keymap-global-set "M-e"         #'treemacs)

(setq magit-blame-styles
  '((margin
      (margin-format " %s%f" " %C %a" " %H")
      (margin-width . 42)
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
(use-package python-mode)
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

(use-package exec-path-from-shell
  :if (display-graphic-p) ; Only necessary for GUI
  :init
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (exec-path-from-shell-copy-envs
    '("PYTHONDONTWRITEBYTECODE"
       "PATH"
       "SONARQUBE_TOKEN_CODEANALYZER"
       "JAVA_HOME"
       "M2_HOME"
       "M2"
       "MAVEN_OPTS"
       "GOPRIVATE"
       "GOPROXY"))
  :config
  ;; Ensure environment is loaded on GUI and daemon sessions
  (when (or (daemonp) (display-graphic-p))
    (exec-path-from-shell-initialize)))

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

;; Functions to increase and decrease font size
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

;; Keybindings for Ctrl-+ and Ctrl--
(keymap-global-set "C-+" #'my-increase-font-size)
(keymap-global-set "C--" #'my-decrease-font-size)

(add-hook 'after-save-hook
  (lambda ()
    (when (string= (file-name-extension buffer-file-name) "el")
      (byte-compile-file buffer-file-name))))

(defun dt/guess-indent-width ()
  "Guess the indentation width by analyzing the current buffer."
  (or (and (boundp 'tab-width) tab-width) ; Use `tab-width` if set.
    4)) ; Default to 4 spaces if `tab-width` is unavailable.

(defun dt/deindent-line-or-region ()
  "De-indent the current line or the active region based on current file's indentation width."
  (interactive)
  (let ((indent-width (dt/guess-indent-width))) ; Get the current file's indentation width.
    (if (use-region-p)
      ;; If a region is active, de-indent the entire region.
      (let ((deactivate-mark nil)) ; Preserve the region after de-indenting.
        (indent-rigidly (region-beginning) (region-end) (- indent-width)))
      ;; If no region is active, de-indent the current line.
      (let ((current-indentation (current-indentation)))
        (if (> current-indentation 0)
          (indent-line-to (max 0 (- current-indentation indent-width))))))))

;; Bind the function to Shift-Tab.
(keymap-global-set "<backtab>" #'dt/deindent-line-or-region)

;; Load post-init.el
(dt/load-user-init "post-init.el")
