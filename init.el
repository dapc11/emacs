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

(defvar dt/user-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")

(defun dt/load-user-init (filename)
  "Execute a file of Lisp code named FILENAME."
  (let ((user-init-file
         (expand-file-name filename
                           dt/user-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t))))

(set-frame-font "JetBrains Mono 11" nil)
(tool-bar-mode -1)
(show-paren-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode)
(global-hl-line-mode)
(auto-save-visited-mode)
(global-display-line-numbers-mode)
(advice-add 'risky-local-variable-p :override #'ignore)
(xterm-mouse-mode t)
(global-set-key (kbd "<mouse-4>") 'previous-line)
(global-set-key (kbd "<mouse-5>") 'next-line)'
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

(use-package wgrep)

(use-package which-key
  :init
  (which-key-mode))

(use-package savehist)

(use-package multiple-cursors
  :ensure t
  :bind (
          ("M-<right>". mc/mark-next-like-this)
          ("M-<left>" . mc/mark-previous-like-this)
          ("C-<" . mc/mark-next-like-this)
          ("C->" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this))
  )

(setq imperative-verb-file "~/.emacs.d/imperatives.txt")
(defun get-imperative-verbs ()
  "Return a list of imperative verbs."
  (let ((file-path imperative-verb-file))
    (with-temp-buffer
      (insert-file-contents file-path)
      (split-string (buffer-string) "\n" t)
      )))

(defcustom dt/git-commit-style-convention-checks '(summary-starts-with-capital
                                                   summary-does-not-end-with-period
                                                   summary-uses-imperative)
  "List of checks performed by `dt/git-commit-check-style-conventions'.
Valid members are `summary-starts-with-capital',
`summary-does-not-end-with-period', and
`summary-uses-imperative'. That function is a member of
`git-commit-finish-query-functions'."
  :options '(summary-starts-with-capital
             summary-does-not-end-with-period
             summary-uses-imperative)
  :type '(list :convert-widget custom-hood-convert-widget)
  :group 'git-commit)

;; Parallels `git-commit-check-style-conventions'
(defun dt/git-commit-check-style-conventions (force)
  "Check for violations of certain basic style conventions.

For each violation ask the user if she wants to proceed anway.
Option `dt/git-commit-check-style-conventions' controls which
conventions are checked."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (git-commit-summary-regexp) nil t)
    (let ((summary (match-string 1))
           (first-word))
      (and (or (not (memq 'summary-starts-with-capital
                      dt/git-commit-style-convention-checks))
             (let ((case-fold-search nil))
               (string-match-p "^[[:upper:]]" summary))
             (y-or-n-p "Summary line does not start with capital letter.  Commit anyway? "))
        (or (not (memq 'summary-does-not-end-with-period
                   dt/git-commit-style-convention-checks))
          (not (string-match-p "[\\.!\\?;,:]$" summary))
          (y-or-n-p "Summary line ends with punctuation.  Commit anyway? "))
        (or (not (memq 'summary-uses-imperative
                   dt/git-commit-style-convention-checks))
          (progn
            (string-match "^\\([[:alpha:]]*\\)" summary)
            (setq first-word (downcase (match-string 1 summary)))
            (car (member first-word (get-imperative-verbs))))
          (when (y-or-n-p "Summary line should use imperative.  Does it? ")
            (when (y-or-n-p (format "Add `%s' to list of imperative verbs?" first-word))
              (with-temp-buffer
                (insert first-word)
                (insert "\n")
                (write-region (point-min) (point-max) imperative-verb-file t)))
            t))))))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)
  (ediff-diff-options "")
  (ediff-custom-diff-options "-u")
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-vertically)
  (byte-compile-warnings '(not docstrings))

  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (add-to-list 'git-commit-finish-query-functions
               #'dt/git-commit-check-style-conventions))

(use-package smartparens
  :init
  (smartparens-mode))

(use-package expand-region
  :bind (
          ("M-S-<right>" . er/expand-region)
          ("M-S-<left>" . er/contract-region)))

(defun dt/kill-and-close-buffer ()
  "Kill and Close current active buffer"
  (interactive)
  (kill-this-buffer)
  (delete-window)
)

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
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-S-u") 'undo-redo)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-c n") 'projectile-find-file)
;; Rebind Page Up (prior) to act as Home
(global-set-key [prior] 'move-beginning-of-line)
;; Rebind Page Down (next) to act as End
(global-set-key [next] 'move-end-of-line)
;; Rebind M-/ functionality to M-;
(global-set-key (kbd "M-;") 'dabbrev-expand)

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq
    treemacs-deferred-git-apply-delay 0.5
    treemacs-no-png-images t
    treemacs-sorting 'mod-time-desc
    treemacs-text-scale -0.5)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode nil)
  :bind
  (:map global-map
    ("C-t" . treemacs)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(defun dt/duplicate-line ()
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

(global-set-key (kbd "C-c C-d") 'dt/duplicate-line)
(global-set-key (kbd "C-c d") 'dt/duplicate-line)

(defun dt/unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "C-c C-x") 'dt/unpop-to-mark-command)

(defun dt/move-text-internal (arg)
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

(defun dt/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (dt/move-text-internal arg))

(defun dt/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (dt/move-text-internal (- arg)))

(global-set-key (kbd "M-<down>") 'dt/move-text-down)
(global-set-key (kbd "M-<up>") 'dt/move-text-up)

(use-package projectile
  :init
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :config
  (setq
    projectile-completion-system 'auto
    projectile-project-search-path '(("~/repos" . 1))))

(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(use-package eglot
  :ensure t
  :hook ((prog-mode . eglot-ensure)))

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
       ("\\.py\\'" . python-mode)
       ;; ("\\.py\\'" . smartparens-mode)
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

;; Whitespace mode
(defun dt/set-up-whitespace-handling ()
  (interactive)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun dt/fill-column-git ()
  (interactive)
  (set-face-foreground 'fill-column-indicator "salmon")
  (lambda ()
    (set-fill-column 72)
    (display-fill-column-indicator-mode))
  )

(add-hook 'git-commit-setup-hook 'dt/fill-column-git)
(add-hook 'yaml-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'json-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode-hook 'dt/set-up-whitespace-handling)
(add-hook 'git-commit-setup-hook 'dt/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'dt/set-up-whitespace-handling)

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

;; Load post-init.el
(dt/load-user-init "post-init.el")
