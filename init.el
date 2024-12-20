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
(setq my/home-dir (getenv "HOME"))
(setopt use-short-answers t)
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

(set-frame-font "Fira Code" nil)
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

(dt/load-user-init "dt-github-theme.el")
(load-theme 'dt-github)
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

(global-set-key (kbd "C-S-j") 'join-line-above)
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
(global-set-key (kbd "C-n") #'my-next-error-or-flymake-next)
(global-set-key (kbd "C-b") #'my-previous-error-or-flymake-prev)
(global-set-key (kbd "C-c n") 'projectile-find-file)
(global-set-key [prior] 'move-beginning-of-line)
(global-set-key [next] 'move-end-of-line)
(global-set-key (kbd "M-;") 'dabbrev-expand)
(global-set-key (kbd "C-c C-d") 'dt/duplicate-line)
(global-set-key (kbd "C-c d") 'dt/duplicate-line)
(global-set-key (kbd "C-c C-x") 'dt/unpop-to-mark-command)
(global-set-key (kbd "M-<down>") 'dt/move-text-down)
(global-set-key (kbd "M-<up>") 'dt/move-text-up)
(global-set-key (kbd "M-e") 'treemacs)

(setq magit-blame-styles
           '((margin
              (margin-format " %s%f" " %C %a" " %H")
              (margin-width . 42)
              (margin-face . magit-blame-margin)
              (margin-body-face magit-blame-dimmed))))

(defun isearch-forward-at-point-or-region ()
  "Start isearch forward with the selected region, or the word under the cursor if no region is selected.
Deactivate the mark after starting the search."
  (interactive)
  (let ((search-string
         (if (use-region-p)
             (progn
               (let ((region (buffer-substring-no-properties (region-beginning) (region-end))))
                 (deactivate-mark)
                 region))
           (thing-at-point 'word t))))
    (if search-string
        (progn
          (isearch-forward nil t)
          (isearch-yank-string search-string))
      (isearch-forward nil t))))

(global-set-key (kbd "C-S-s") 'isearch-forward-at-point-or-region)

(defun scroll-half-page-down ()
  "Scroll down half the page."
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "Scroll up half the page."
  (interactive)
  (scroll-up (/ (window-body-height) 2)))
(global-set-key (kbd "C-v") 'scroll-half-page-up)
(global-set-key (kbd "M-v") 'scroll-half-page-down)

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


;; (use-package eglot
;;   :init
;;   (setq eglot-sync-connect 3
;;         eglot-connect-timeout nil
;;         eglot-autoshutdown t
;;         eglot-send-changes-idle-time 0.5
;;         eglot-report-progress t
;;         eglot-ignored-server-capabilities '(:documentHighlightProvider
;;                                             :foldingRangeProvider)
;;         ;; NOTE We disable eglot-auto-display-help-buffer because :select t
;;         ;;      its popup rule causes eglot to steal focus too often.
;;     eglot-auto-display-help-buffer nil)
;;   :bind (
;;           ("C-c c a" . eglot-code-actions)
;;           ("C-c c f" . eglot-format)
;;           ("C-c c r" . eglot-rename)
;;           ("M-n" . flymake-goto-next-error)
;;           ("M-b" . flymake-goto-prev-error)
;;           ("C-c c c" . eglot-code-action-quickfix)
;;           ("C-c c e" . eglot-code-action-extract)
;;           ("C-c c i" . eglot-code-action-inline)))

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . k8s-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

;; JAVA START

(defvar my/local-dir (concat user-emacs-directory ".local/") "Local state directory")
(defvar lsp-java-workspace-dir (expand-file-name "lsp/workspace/data" my/local-dir) "LSP data directory for Java")
(defvar java-home (format "%s/.sdkman/candidates/java/current" my/home-dir) "The home dir of the jdk")
(defvar java-bin (format "%s/bin/java" java-home) "The path to the java binary")
(defvar jdtls-home "/opt/eclipse.jdt.ls" "The path to eclipse.jdt.ls installation")
(defvar jdtls-config (format "%s/config_linux" jdtls-home) "The path to eclipse.jdt.ls installation")

(defun my/jdtls-start-command (arg)
  "Creates the command to start jdtls"
  (let ((jdtls-jar (replace-regexp-in-string "\n\\'" "" (shell-command-to-string (format "find %s/plugins -iname '*launcher_*.jar'" jdtls-home)) "The jar file that starts jdtls")))
    `(,java-bin "-jar" ,jdtls-jar "-data" ,(format "%s/.cache/lsp/project/%s" my/home-dir (project-name (project-current))) "-configuration" ,jdtls-config
       "--add-modules=ALL-SYSTEM"
       "--add-opens java.base/java.util=ALL-UNNAMED"
       "--add-opens java.base/java.lang=ALL-UNNAMED"
       "-XX:+UseAdaptiveSizePolicy" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Xmx8G" "-Xms2G" "-Xverify:none")))

(defun my/java-setup-project-workspace ()
  "Setup a local java workspace for the current project."
  (interactive)
  (let* ((project-root (project-root (project-current)))
          (file-path (concat project-root ".dir-locals.el"))
          (data-dir (concat (project-root (project-current)) ".lsp/workspace/data"))
          (cache-dir (concat (project-root (project-current)) ".lsp/workspace/cache"))
          (content '((java-mode
                       (eval . (progn
                                 (setq lsp-session-file (concat (project-root (project-current)) ".lsp/session")
                                   lsp-java-workspace-dir (concat (project-root (project-current)) ".lsp/workspace/data")
                                   lsp-java-workspace-cache-dir (concat (project-root (project-current)) ".lsp/workspace/cache"))))))))
    (make-directory data-dir t)
    (make-directory cache-dir t)
    (with-temp-buffer
      (setq-local enable-local-variables :all)
      (insert (format "%s\n" (pp-to-string content)))
      (write-file file-path))))

(defun my/java-clear-project-workspace ()
  "Setup a local java workspace for the current project."
  (interactive)
  (let ((directory lsp-java-workspace-dir))
    (when (file-exists-p directory)
      (delete-directory directory 'recursive))
    (make-directory directory t)))

;; mkdir -p ~/.emacs.d/.local/lsp/eclipse.jdt.ls
;; pushd ~/.emacs.d/.local/lsp/eclipse.jdt.ls
;; curl -s -L https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.37.0/jdt-language-server-1.37.0-202406271335.tar.gz | tar zxv
;; mkdir bundles
;; pushd bundles
;; curl -O https://github.com/dgileadi/vscode-java-decompiler/raw/master/server/dg.jdt.ls.decompiler.cfr-0.0.3.jar
;; curl -O https://github.com/dgileadi/vscode-java-decompiler/raw/master/server/dg.jdt.ls.decompiler.common-0.0.3.jar
;; curl -O https://github.com/dgileadi/vscode-java-decompiler/raw/master/server/dg.jdt.ls.decompiler.fernflower-0.0.3.jar
;; curl -O https://github.com/dgileadi/vscode-java-decompiler/raw/master/server/dg.jdt.ls.decompiler.procyon-0.0.3.jar
;; popod
;; popd

(use-package eglot-java
  :custom
  (eglot-java-eclipse-jdt-ls-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.37.0/jdt-language-server-1.37.0-202406271335.tar.gz")
  (eglot-java-server-install-dir (file-name-concat my/local-dir "lsp" "eclipse.jdt.ls"))
  (eglot-java-eclipse-jdt-args '("-XX:+UseAdaptiveSizePolicy" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Xmx8G" "-Xms2G"))
  (eglot-java-eclipse-jdt-data-root-dir (file-name-concat my/local-dir "lsp" "eclipse.jdt.ls" "data"))
  :init

  (defun jdtls-initialization-options ()
    (let ((setting-json-file (file-name-concat user-emacs-directory "config.json")))
      (with-temp-buffer
        (insert-file-contents setting-json-file)
        (json-parse-buffer :object-type 'plist :false-object :json-false))))

  :config
  ;; Override existing options
  (cl-defmethod eglot-initialization-options ((server eglot-java-eclipse-jdt))
    (jdtls-initialization-options))
  ;; eglot-java registers in 'project-find-functions a function that lookus up for .project
  (advice-add 'eglot-java--init :after (lambda() (remove-hook 'project-find-functions  #'eglot-java--project-try)))
  :hook (java-mode . eglot-java-mode))

;; JAVA END


;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;     '(python-mode . ("pyright-langserver" "--stdio"))))
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;     '(java-mode . ("jdtls"))))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
    '(go-mode . ("gopls" "serve"))))

;; (add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)
;; (add-hook 'java-mode-hook 'eglot-ensure)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (when (daemonp)
    (exec-path-from-shell-initialize))
  :config
  (exec-path-from-shell-copy-envs '("PYTHONDONTWRITEBYTECODE" "PATH" "SONARQUBE_TOKEN_CODEANALYZER" "JAVA_HOME" "M2_HOME" "M2" "MAVEN_OPTS" "GOPRIVATE" "GOPROXY")))

;; Whitespace mode
(defun dt/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'compilation-filter-hook 'dt/apply-ansi-colors)
(add-hook 'html-mode-hook 'dt/apply-ansi-colors)
(add-hook 'emacs-lisp-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'git-commit-setup-hook 'dt/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'java-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'json-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'dt/set-up-whitespace-handling)

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
(global-set-key (kbd "C-+") 'my-increase-font-size)
(global-set-key (kbd "C--") 'my-decrease-font-size)

(add-hook 'after-save-hook
          (lambda ()
            (when (string= (file-name-extension buffer-file-name) "el")
              (byte-compile-file buffer-file-name))))

;; Load post-init.el
(dt/load-user-init "post-init.el")
