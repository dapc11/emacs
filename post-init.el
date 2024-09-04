;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'recentf-mode)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)


(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :ensure t
  :bind (
          ("C-c e" . embark-export)
          ("C-." . embark-act)
          ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun dt/consult-line ()
  (interactive)
  (if (minibufferp)
    ;; when in minibuffer..
    (progn
      (vertico-next)
      )
    ;; when not in minibuffer..
    (progn
      (let ((vertico-count 10)    )
        (consult-line))
      )
    )
  )

(defun dt/consult-ripgrep-region-or-prompt ()
  "Call `consult-ripgrep' with the selected region as the initial input if any.
If no region is selected, call `consult-ripgrep' without pre-populating the input."
  (interactive)
  (if (use-region-p)
    (consult-ripgrep nil (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-ripgrep))
  )

(use-package consult
  :bind (
          ([remap Info-search] . consult-info)
          ("C-x b" . consult-buffer)
          ("C-x B" . consult-project-buffer)
          ("C-c b" . consult-bookmark)
          ("C-c <deletechar>" . bookmark-delete)
          ("M-y" . consult-yank-pop)
          ("M-g i" . consult-imenu)
          ("C-c l" . consult-goto-line)
          ("C-c r" . consult-recent-file)
          ("C-c N" . consult-fd)
          ("C-c F" . dt/consult-ripgrep-at-point)
          ("C-S-f" . dt/consult-ripgrep-region-or-prompt)

          :map minibuffer-local-map
          ("M-s" . consult-history)
          ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
    register-preview-function #'consult-register-format
    xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (consult-customize
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    preview-key '([S-up] [S-down]))
)

(autoload 'projectile-project-root "projectile")
(setq consult-project-function (lambda (_) (projectile-project-root)))


(defvar-local consult-toggle-preview-orig nil)

(defun consult-toggle-preview ()
  "Command to enable/disable preview."
  (interactive)
  (if consult-toggle-preview-orig
      (setq consult--preview-function consult-toggle-preview-orig
            consult-toggle-preview-orig nil)
    (setq consult-toggle-preview-orig consult--preview-function
          consult--preview-function #'ignore)))

(define-key vertico-map (kbd "C-p") #'consult-toggle-preview)
(define-key vertico-map (kbd "S-<up>") #'vertico-previous)
(define-key vertico-map (kbd "S-<down>") #'vertico-next)

(global-set-key (kbd "C-f") 'dt/consult-line)

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode))

  ;; Enable Corfu
  :config
  (global-corfu-mode)

  :bind (:map corfu-map
          ("M-SPC"      . corfu-insert-separator)
          ("TAB"        . corfu-next)
          ([tab]        . corfu-next)
          ("S-TAB"      . corfu-previous)
          ([backtab]    . corfu-previous)
          ("S-<return>" . nil)
          ("RET"        . corfu-insert))

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.4 . 0.2)))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-symbol cape-elisp-block)

  :config
  (setq cape-dabbrev-check-other-buffers nil)

  ;; Set up CAPF with `cape-dabbrev` and other sources
  (defun dt/setup-capf ()
    "Set up completion-at-point-functions with cape and dabbrev."
    (setq-local completion-at-point-functions
                (list
               ;; Combine eglot with cape sources
               (cape-capf-buster
                #'eglot-completion-at-point
                #'cape-dabbrev
                #'cape-file
                #'cape-symbol))))
  ;; Add CAPF setup to programming modes
  (add-hook 'prog-mode-hook #'dt/setup-capf))

;; Ripgrep the current word from project root
(defun dt/consult-ripgrep-at-point ()
  (interactive)
  (select-text-in-delimiters)
  (let ((search-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (consult-ripgrep nil (concat search-string (char-to-string ? )))))

(defun select-text-in-delimiters ()
  "Select text between the nearest left and right delimiters."
  (interactive)
  (let (start end)
    (skip-chars-backward "^<>([{\"'")
    (setq start (point))
    (skip-chars-forward "^<>)]}\"'")
    (setq end (point))
    (push-mark start)))

(use-package tabbar
  :ensure t
  :bind
  ("<C-S-iso-lefttab>" . tabbar-backward)
  ("<C-tab>" . tabbar-forward)

  :init
  (progn
    (tabbar-mode t)

    (set-face-attribute
      'tabbar-default nil
      :background "#21252B"
      :box '(:line-width 1 :color "#282C34" :style nil))
    (set-face-attribute
      'tabbar-unselected nil
      :background "#282C34"
      :box '(:line-width 5 :color "#282C34" :style nil))
    (set-face-attribute
      'tabbar-selected nil
      :background "#2C323C"
      :foreground "#ABB2BF"
      :box '(:line-width 5 :color "#2C323C" :style nil))
    (set-face-attribute
      'tabbar-highlight nil
      :background "#2C323C"
      :foreground "#ABB2BF"
      :box '(:line-width 5 :color "#2C323C" :style nil))
    (set-face-attribute
      'tabbar-button nil
      :box '(:line-width 1 :color "#282C34" :style nil))
    (set-face-attribute
      'tabbar-separator nil
      :background "#282C34"
      :height 0.6)

    (defun tabbar-buffer-tab-label (tab)
      "Return a label for TAB that includes the Git repository name, if available."
      (let* ((buffer (tabbar-tab-value tab))
              (label (format "%s" (buffer-name buffer)))
              (git-root (with-current-buffer buffer
                          (vc-root-dir)))
              (repo-name (if git-root
                           (file-name-nondirectory (directory-file-name git-root))
                           "")))
        (if (and git-root (not (string= repo-name "")))
          (format "[%s] %s " repo-name label)
          label)))

    (custom-set-variables
      '(tabbar-separator (quote (0.5))))))

(setq tabbar-buffer-groups-function (lambda () (list "All")))

(defun dt/tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
   Exclude buffers whose names start and end with '*', and magit buffers."
  (delq nil
        (mapcar #'(lambda (b)
                    (let ((name (buffer-name b)))
                      (if (and (not (string-prefix-p " " name))
                               (not (string-match "^\\*.*\\*$" name))
                               (not (string-prefix-p "magit" name)))
                          b)))
                (buffer-list))))

(setq tabbar-buffer-list-function 'dt/tabbar-buffer-list)

(use-package indent-bars
  :load-path "~/.emacs.d/indent-bars"
  :hook ((yaml-mode . indent-bars-mode)
            (python-mode . indent-bars-mode))
  :custom
  (indent-bars-prefer-character t))
(setq
  indent-bars-color '(highlight :face-bg t :blend 0.5)
  indent-bars-pattern "."
  indent-bars-width-frac 0.1
  indent-bars-pad-frac 0.1
  indent-bars-zigzag nil
  indent-bars-color-by-depth nil
  indent-bars-highlight-current-depth nil
  indent-bars-display-on-blank-lines nil)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :init
  ;; Set directory to store undo-tree history files
  (setq undo-tree-history-directory-alist
    `(("." . ,(concat user-emacs-directory "undo-history/"))))

  ;; Ensure the directory exists
  (make-directory (concat user-emacs-directory "undo-history/") t)

  ;; Enable persistent undo
  (setq undo-tree-auto-save-history t)

  :bind (("C-z" . undo-tree-undo)
          ("C-S z" . undo-tree-redo)))
