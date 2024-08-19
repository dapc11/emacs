(setq inhibit-startup-message t)
(setq lisp-indent-offset 2)
(tool-bar-mode -1)
(show-paren-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(global-hl-line-mode t)
(global-display-line-numbers-mode t)

;; 4 spaces rather than tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq c-basic-indent 4)

(setq column-number-mode t)
(setq backup-directory-alist '(("." . "~/.emacsbackup")))

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
  :demand t
  :config

  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$"
        consult--tofu-char
        (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
      ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
      ((string-suffix-p "$" word)
        `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
      ;; File extensions
      ((and (or minibuffer-completing-file-name
              (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
        `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-styles '(orderless basic)
    completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
    ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
    completion-category-overrides
    '(
       (file (styles partial-completion)) ;; partial-completion is tried first
       ;; enable initialism by default for symbols
       (command (styles +orderless-with-initialism))
       (variable (styles +orderless-with-initialism))
       (symbol (styles +orderless-with-initialism)))
    orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
    orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                  #'orderless-affix-dispatch)))

(use-package savehist
  :init
  (savehist-mode))

(use-package company
  :init
  (company-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-c C-SPC") 'company-complete)
  (setq company-backends '((company-capf company-dabbrev-code)))
  (setq company-require-match nil))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") (lambda () (interactive) (company-complete-common-or-cycle -1)))
  (define-key company-active-map (kbd "TAB") (lambda () (interactive) (company-complete-common-or-cycle -1))))

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(require 'multiple-cursors)
(global-set-key (kbd "M-<right>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-<left>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'magit)

(setq ediff-diff-options "")
(setq ediff-custom-diff-options "-u")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-vertically)

(setq byte-compile-warnings '(not docstrings))

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
(global-set-key (kbd "C-c q") 'delete-window)
(global-set-key (kbd "C-c r") 'recentf)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-S-r") 'isearch-query-replace)
(global-set-key (kbd "C-c g") 'magit)
(global-set-key (kbd "C-c C-g") 'magit)
(global-set-key (kbd "C-c n") 'projectile-find-file)
(global-set-key (kbd "C-c f") 'projectile-ripgrep)
(global-set-key (kbd "C-.") 'next-error)
(global-set-key (kbd "C-,") 'previous-error)
(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-S-u") 'undo-redo)

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
(global-set-key (kbd "C-c C-c") "\C-u\C- ")

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

(global-set-key [M-S-down] 'move-text-down)
(global-set-key [M-S-up] 'move-text-up)

(use-package projectile
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'auto)
  (setq projectile-project-search-path '(("~/repos" . 1)))
  )

(defun my-set-background-color (&optional frame)
  "Set custom background color."
  (with-selected-frame (or frame (selected-frame))
    (set-background-color "#171717")))

;; Run later, for client frames...
(add-hook 'after-make-frame-functions 'my-set-background-color)
;; ...and now, for the initial frame.
(my-set-background-color)

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
  (whitespace-mode)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'json-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode-hook 'rc/set-up-whitespace-handling)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point))
  )
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)


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


(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(setopt use-short-answers t)
