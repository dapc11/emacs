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

(defun dt/consult-line ()
  "Search for a line using `consult-line`.

If the function is called within the minibuffer, it moves to the next candidate
in the `vertico` completion menu using `vertico-next`.

Region will pre the prepopulated input if active.
The region is deactivated after the selected text is grabbed."
  (interactive)
  (if (minibufferp)
      ;; When in minibuffer..
      (progn
        (vertico-next))
    ;; When not in minibuffer..
    (progn
      (let* ((vertico-count 10)
             (selected-text (when (region-active-p)
                              (buffer-substring-no-properties
                               (region-beginning)
                               (region-end)))))
        (deactivate-mark)  ;; Deactivate the region after grabbing the text
        (if selected-text
            (consult-line selected-text) ;; Pre-populate if text is selected
          (consult-line)))))  ;; Default behavior if no region is selected
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
          ("C-S-f" . ag-project)
          ("C-c z b" . dt/fd-notes)
          ("C-c z f" . dt/rg-notes)
          ("C-c z n" . dt/new-note)

          :map minibuffer-local-map
          ("M-s" . consult-history)
          ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
    register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (defun dt/fd-notes ()
    (interactive)
    (consult-fd "~/notes"))
  (defun dt/rg-notes ()
    (interactive)
    (consult-ripgrep "~/notes"))
  (defun dt/new-note ()
    "Create a new markdown file in ~/notes with a title.
The title will be injected as the first header (# Title),
capitalized according to Chicago style, and the filename
will be snake_case with a .md extension."
    (interactive)
    (let* ((title (read-string "Title: "))   ;; Prompt user for the title
            (chicago-title (with-temp-buffer
                             (insert title)
                             (goto-char (point-min))
                             (capitalize-region (point-min) (point-max))
                             (string-trim (buffer-string))))  ;; Chicago-style capitalization
            (filename (concat "~/notes/"
                        (replace-regexp-in-string " " "_" (downcase title)) ".md"))) ;; Create snake_case filename
      ;; Create new file with title as the first header
      (find-file filename)
      (insert (concat "# " chicago-title "\n\n")) ;; Insert title as header
      (save-buffer)
      (message "Markdown file created: %s" filename)))

  (consult-customize
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    preview-key '([S-up] [S-down]))
)

(autoload 'projectile-project-root "projectile")
(setq consult-project-function (lambda (_) (projectile-project-root)))

(defun dt/yaml-mode-keybindings ()
  "YAML mode specific keymaps."
  (define-key yaml-mode-map (kbd "M-.") 'dt/ag-at-point))

(add-hook 'yaml-mode-hook 'dt/yaml-mode-keybindings)

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

(defun dt/ag-at-point ()
  "Search for the text between delimiters using ag in the project."
  (interactive)
  (select-text-in-delimiters)
  (let ((search-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (ag-project search-string)))

(defun select-text-in-delimiters ()
  "Select text between the nearest left and right delimiters."
  (interactive)
  (let (start end)
    (skip-chars-backward "^<>([{\"'")  ;; Move to the nearest left delimiter
    (setq start (point))               ;; Mark the start position
    (skip-chars-forward "^<>)]}\"'")   ;; Move to the nearest right delimiter
    (setq end (point))                 ;; Mark the end position
    (set-mark start)                   ;; Set the mark at the start
    (goto-char end)))                  ;; Move point to the end position


(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defun dt/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above. Also, further
consecutive invocations of this command will inherit the negative
argument.

If region is active, comment lines in active region instead.
Unlike `comment-dwim', this always comments whole lines."
  (interactive "p")
  (if (use-region-p)
      (progn
        (comment-or-uncomment-region
         (save-excursion
           (goto-char (region-beginning))
           (line-beginning-position))
         (save-excursion
           (goto-char (region-end))
           (line-end-position)))
        (setq deactivate-mark nil))  ;; Keep region active after commenting
    (when (and (eq last-command 'comment-line-backward)
               (natnump n))
      (setq n (- n)))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (unless (natnump n) (setq this-command 'comment-line-backward))))

(global-set-key (kbd "C-/") nil)
(global-set-key (kbd "C-/") 'dt/comment-line)

(add-to-list 'compilation-error-regexp-alist
             '("^\\([0-9.]+\\-[a-z0-9]+\\.*\\): digest: sha256:\\([a-f0-9]+\\) size: \\([0-9]+\\)$" 1 2 3))

(defun parse-env-file (file)
  "Parse a .env FILE and set the environment variables in Emacs."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "\\([^#\n]+\\)=" nil t)
        (let ((key (match-string 1))
              (value (progn (re-search-forward "\"\\([^\"]+\\)\"" nil t)
                            (match-string 1))))
          (setenv key value)
          (message "Set environment variable: %s=%s" key value))))))

(defun projectile-load-env-vars ()
  "Load environment variables from the .env file in the root of the Projectile project."
  (let ((env-file (expand-file-name ".env" (projectile-project-root))))
    (parse-env-file env-file)))

;; Hook to load .env when entering a new Projectile project
(add-hook 'projectile-after-switch-project-hook #'projectile-load-env-vars)

;; Auto completion
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :init
  (global-corfu-mode)
  :config
  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)
                                      #'corfu-send)))))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package cape
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
)

;;;post-init.el ends here
