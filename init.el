(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode t)

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(load-theme 'spacemacs-dark)
(set-frame-font "JetBrains Mono 12" nil t)
(setq ring-bell-function 'ignore)

(use-package vertico
  :init
  (setq vertico-cycle t)
  :config
  (vertico-mode)
  )

(use-package which-key
  :config
  (which-key-mode)
  )

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match 1)
  (setq helm-buffers-fuzzy-matching 1)
  (setq helm-recentf-fuzzy-match 0)
  :config
  (helm-mode 1)
  :bind (
         ("M-y" . helm-show-kill-ring)
	 ("M-x" . helm-M-x)
	 ("C-c r" . helm-recentf)
	 )
  )

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (company-mode)
  )

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") (lambda () (interactive) (company-complete-common-or-cycle -1)))
  (define-key company-active-map (kbd "TAB") (lambda () (interactive) (company-complete-common-or-cycle -1))))

(use-package multiple-cursors
  :ensure t
  :bind(("C-S-c C-S-c" . mc/edit-lines)
        ("M-<down>" . mc/mark-next-like-this)
        ("M-<up>" . mc/mark-previous-like-this)
        ("C-c C-<" . mc/mark-all-like-this)
        ("C-<" . mc/mark-next-word-like-this)
	("C->" . mc/mark-previous-word-like-this)
	)
  :config
  (setq mc/insert-numbers-default 1)
  )

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c .") 'next-error)
(global-set-key (kbd "C-c ,") 'previous-error)
(global-set-key (kbd "C-c v") 'split-window-right)
(global-set-key (kbd "C-c -") 'split-window-below)

(setq package-check-signature nil)
(use-package magit
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
  :init
  (progn
    (delete 'Git vc-handled-backends)
    ;; make magit status go full-screen but remember previous window
    ;; settings
    ;; from: http://whattheemacsd.com/setup-magit.el-01.html
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    ;; Close popup when commiting - this stops the commit window
    ;; hanging around
    ;; From: http://git.io/rPBE0Q
    (defadvice git-commit-commit (after delete-window activate)
      (delete-window))

    (defadvice git-commit-abort (after delete-window activate)
      (delete-window))
    )
  )

(setq byte-compile-warnings '(not docstrings))
(setq-default cursor-type 'bar)

(use-package projectile
  :init
  (setq projectile-project-search-path '("~/repos"))
  :bind (
	 ("C-c p" . projectile-command-map)
	 ("C-c C-p" . projectile-switch-project)
	 ("C-c s" . projectile-ag))
  :config
  (projectile-mode +1)
  )

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t)
  (ivy-mode)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :bind (("C-s" . swiper-isearch)
	 ("M-s" . swiper-isearch-thing-at-point)
	 ("C-c C-r" . ivy-resume)
      	 ("C-x C-f" . counsel-find-file))
  )
(setq enable-local-variables :safe)
