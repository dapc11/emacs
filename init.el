(setq inhibit-startup-message t)
(setq lisp-indent-offset 2)
(setq tool-bar-mode -1)
(setq show-paren-mode 1)
(setq menu-bar-mode -1)
(setq scroll-bar-mode -1)
(setq delete-selection-mode 1)
(setq global-hl-line-mode t)
(setq recentf-mode 1)
(setq global-display-line-numbers-mode t)
(setq advice-add 'risky-local-variable-p :override #'ignore)
(setq column-number-mode t)
(setq backup-directory-alist '(("." . "~/.emacsbackup")))

;; 4 spaces rather than tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-basic-indent 4)

(setq custom-file "~/.emacs.d/custom-file.el")
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

(load-theme 'atom-one-dark)

(set-frame-font "JetBrains Mono 11" nil)
(setq line-spacing 0.1)
(setq ring-bell-function 'ignore)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package which-key
  :init
  (which-key-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package corfu
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.4 . 0.2))

  :bind (:map corfu-map
          ("M-SPC"      . corfu-insert-separator)
          ("TAB"        . corfu-next)
          ([tab]        . corfu-next)
          ("S-TAB"      . corfu-previous)
          ([backtab]    . corfu-previous)
          ("S-<return>" . nil) ;; leave my entry as it is
          ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
)

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-dabbrev
                     #'cape-file))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

(require 'multiple-cursors)
(global-set-key (kbd "M-<right>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-<left>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'magit)

(setq
  ediff-diff-options ""
  ediff-custom-diff-options "-u"
  ediff-window-setup-function 'ediff-setup-windows-plain
  ediff-split-window-function 'split-window-vertically
  byte-compile-warnings '(not docstrings))

(use-package smartparens
  :init
  (smartparens-mode))

(use-package expand-region)
(global-set-key (kbd "M-S-<right>") 'er/expand-region)
(global-set-key (kbd "M-S-<left>") 'er/contract-region)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-c v") 'split-window-right)

(defun kill-and-close-buffer ()
  "Kill and Close current active buffer"
  (interactive)
  (kill-this-buffer)
  (delete-window)
)

(global-set-key (kbd "C-c q") 'kill-and-close-buffer)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-S-r") 'isearch-query-replace)
(global-set-key (kbd "C-c g") 'magit)
(global-set-key (kbd "C-.") 'next-error)
(global-set-key (kbd "C-,") 'previous-error)
(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-t") 'treemacs)
(global-set-key (kbd "C-S-u") 'undo-redo)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-c n") 'projectile-find-file)

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq
      treemacs-deferred-git-apply-delay 0.5
      treemacs-no-png-images t
      treemacs-sorting 'mod-time-desc
      treemacs-text-scale -0.5)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
    ("C-t"   . treemacs)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)


(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
         (line (let ((s (thing-at-point 'line t)))
                 (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-c C-d") 'rc/duplicate-line)
(global-set-key (kbd "C-c d") 'rc/duplicate-line)

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "C-c C-x") 'unpop-to-mark-command)

(defun move-text-internal (arg)
  (cond
    ((and mark-active transient-mark-mode)
      (if (> (point) (mark))
        (exchange-point-and-mark))
      (let ((column (current-column))
             (text (delete-and-extract-region (point) (mark))))
        (forward-line arg)
        (move-to-column column t)
        (set-mark (point))
        (insert text)
        (exchange-point-and-mark)
        (setq deactivate-mark nil)))
    (t
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [M-down] 'move-text-down)
(global-set-key [M-up] 'move-text-up)

(use-package projectile
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq
    projectile-completion-system 'auto
    projectile-project-search-path '(("~/repos" . 1))))

;; (defun my-set-background-color (&optional frame)
;;   "Set custom background color."
;;   (with-selected-frame (or frame (selected-frame))
;;     (set-background-color "#171717")))

;; ;; Run later, for client frames...
;; (add-hook 'after-make-frame-functions 'my-set-background-color)
;; ;; ...and now, for the initial frame.
;; (my-set-background-color)

(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)

(use-package exec-path-from-shell
  :init
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package yaml-mode)
(use-package json-mode)
(use-package python-mode)
(use-package go-mode)
(use-package dockerfile-mode)
(use-package lua-mode)
(use-package k8s-mode)

(setq auto-mode-alist
  (append
    '(
       ("\\.el\\'" . emacs-lisp-mode)
       ("\\.yaml\\'" . yaml-mode)
       ("\\.yml\\'" . yaml-mode)
       ("\\.tpl\\'" . k8s-mode)
       ("\\.go\\'" . go-mode)
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

;;; Whitespace mode
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'json-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode-hook 'rc/set-up-whitespace-handling)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun goto-last-change (&optional mark-point minimal-line-distance)
  "Set point to the position of the last change.
Consecutive calls set point to the position of the previous change.
With a prefix arg (optional arg MARK-POINT non-nil), set mark so \
\\[exchange-point-and-mark]
will return point to the current position."
  (interactive "P")
  ;; (unless (buffer-modified-p)
  ;;   (error "Buffer not modified"))
  (when (eq buffer-undo-list t)
    (error "No undo information in this buffer"))
  (when mark-point
    (push-mark))
  (unless minimal-line-distance
    (setq minimal-line-distance 10))
  (let ((position nil)
	     (undo-list (if (and (eq this-command last-command)
			              goto-last-change-undo)
		              (cdr (memq goto-last-change-undo buffer-undo-list))
		              buffer-undo-list))
	     undo)
    (while (and undo-list
             (or (not position)
               (eql position (point))
               (and minimal-line-distance
                 ;; The first invocation always goes to the last change, subsequent ones skip
                 ;; changes closer to (point) then minimal-line-distance.
                 (memq last-command '(goto-last-change
                                       goto-last-change-with-auto-marks))
                 (< (count-lines (min position (point-max)) (point))
                   minimal-line-distance))))
      (setq undo (car undo-list))
      (cond ((and (consp undo) (integerp (car undo)) (integerp (cdr undo)))
	          ;; (BEG . END)
	          (setq position (cdr undo)))
	    ((and (consp undo) (stringp (car undo))) ; (TEXT . POSITION)
	      (setq position (abs (cdr undo))))
	    ((and (consp undo) (eq (car undo) t))) ; (t HIGH . LOW)
	    ((and (consp undo) (null (car undo)))
	      ;; (nil PROPERTY VALUE BEG . END)
	      (setq position (cdr (last undo))))
	    ((and (consp undo) (markerp (car undo)))) ; (MARKER . DISTANCE)
	    ((integerp undo))		; POSITION
	    ((null undo))		; nil
	    (t (error "Invalid undo entry: %s" undo)))
      (setq undo-list (cdr undo-list)))
    (cond (position
	        (setq goto-last-change-undo undo)
	        (goto-char (min position (point-max))))
	  ((and (eq this-command last-command)
		 goto-last-change-undo)
	    (setq goto-last-change-undo nil)
	    (error "No further undo information"))
	  (t
	    (setq goto-last-change-undo nil)
	    (error "Buffer not modified")))))

(global-set-key (kbd "C-c C-c") 'goto-last-change)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(setopt use-short-answers t)

(defun skeez/consult-line () ; conslut-line
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

;; limit preview
;; when C-r in minibuffer, attempt to tell vertico to just go backwards one
(defun skeez/consult-line-reverse ()
  (interactive)
  (if (minibufferp)
    ;; when in minibuffer..
    (progn
      (vertico-previous)
      )
    ;; when not in minibuffer..
    (progn
      (let ((vertico-count 10)   )
        (consult-line))
      )
    )
  )

(define-key vertico-map [S-up] #'vertico-previous)
(define-key vertico-map [S-down] #'vertico-next)

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
          ([remap Info-search] . consult-info)
          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
          ("C-c b" . consult-buffer)                ;; orig. switch-to-buffer
          ("C-c B" . consult-project-buffer)                ;; orig. switch-to-buffer
          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
          ("M-g i" . consult-imenu)
          ("C-c l" . consult-goto-line)
          ("C-c r" . consult-recent-file)
          ("C-c N" . consult-fd)
          ("C-c F" . consult-focus-lines)
          ("C-c f" . consult-ripgrep))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    preview-key '([S-up] [S-down]))
)

(global-set-key (kbd "C-s") 'skeez/consult-line)         ; normally isearch-forward
(global-set-key (kbd "C-r") 'skeez/consult-line-reverse) ; normally isearch-backward

(autoload 'projectile-project-root "projectile")
(setq consult-project-function (lambda (_) (projectile-project-root)))

(use-package embark
  :ensure t
  :bind (("M-e" . embark-export))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
