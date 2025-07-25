;;; post-init.el --- Post init file -*- lexical-binding: t; -*-

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
  :ensure t
  :custom
  (completion-styles '(orderless basic))  ;; use orderless as the main style
  (completion-category-defaults nil)      ;; disable all default category overrides
  (completion-category-overrides
    '((file (styles partial-completion))))) ;; use partial-completion for file paths

(defun dt/consult-project-ripgrep ()
  "Run `consult-ripgrep` in the current project."
  (interactive)
  (consult-ripgrep (project-root (project-current t))))

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
          ("C-S-M-f" . dt/consult-project-ripgrep)
          ("C-c N" . consult-fd)
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
    "Create a new markdown note in ~/notes with a title.
- Title becomes the first-level header.
- Filename is snake_case.
- Ensures the notes directory exists."
    (interactive)
    (let* ((title (read-string "Title: "))
            (title-cap (capitalize title))
            (filename (concat (expand-file-name "~/notes/")
                        (replace-regexp-in-string " " "_" (downcase title)) ".md")))
      (make-directory (file-name-directory filename) :parents)
      (find-file filename)
      (when (= (point-max) (point-min)) ;; Only insert if file is new/empty
        (insert (concat "# " title-cap "\n\n"))
        (save-buffer))
      (message "Markdown note created: %s" filename)))

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


(keymap-global-set "C-z" #'undo)
(keymap-global-set "C-S-z" #'undo-redo)

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defun dt/comment-line (n)
  "Comment or uncomment line(s) in a consistent way.

- With active region: comment or uncomment all lines in the region.
- With prefix arg N:
  - Positive N: apply to current line and N-1 lines downward.
  - Negative N: apply to current line and -N-1 lines upward.
  - Repeated invocations inherit negative prefix.
- Keeps point at the end of the last affected line.
- Always comments whole lines (not partial)."
  (interactive "p")
  (if (use-region-p)
    (let ((start (save-excursion
                   (goto-char (region-beginning))
                   (line-beginning-position)))
           (end (save-excursion
                  (goto-char (region-end))
                  (line-end-position))))
      (comment-or-uncomment-region start end)
      (setq deactivate-mark nil)) ;; keep region active
    ;; Handle negative prefix logic
    (when (and (eq last-command 'comment-line-backward)
            (natnump n))
      (setq n (- n)))
    (let* ((start (line-beginning-position))
            (end (save-excursion
                   (forward-line (1- n))
                   (line-end-position))))
      (comment-or-uncomment-region (min start end) (max start end))
      (goto-char (line-end-position)))
    ;; Set command to inherit negative motion
    (unless (natnump n)
      (setq this-command 'comment-line-backward))))


(keymap-global-set "C-/" nil)
(keymap-global-set "C-/" #'dt/comment-line)

(add-to-list 'compilation-error-regexp-alist-alist
             '(pytest
               "^\\(.*\\.py\\):\\([0-9]+\\):"
               1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist-alist
             '(pytest-warning
               " \\(.*\\.py\\):\\([0-9]+\\):"
               1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist-alist
             '(pytest-double-colon
               "FAILED \\(.*\\.py\\):\\([0-9]+\\) "
               1 2 nil 0))

(add-to-list 'compilation-error-regexp-alist 'pytest)
(add-to-list 'compilation-error-regexp-alist 'pytest-warning)
(add-to-list 'compilation-error-regexp-alist 'pytest-double-colon)

(setq compilation-find-file-no-prompt t) ;; Always use default without asking

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
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  )

(require 'dired)
(defun dt/dired-up-directory ()
  "Go up to the parent directory in dired."
  (interactive)
  (dired-up-directory))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<backspace>") 'dt/dired-up-directory))

(defun dt/dired-unpack-archive-to-directory ()
  "Unpack the archive file under the cursor in dired to a hardcoded directory with a unique index and open dired there."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
          (base-dir "/home/epedape/test_reports/")  ; Change this to your desired directory
          (unique-dir (dt/generate-sequential-dir-name base-dir))
          (command (dt/get-extract-command file unique-dir)))
    (if command
      (progn
        (make-directory unique-dir t)
        (shell-command command)
        (message "Unpacked %s to %s" file unique-dir)
        (dired unique-dir))  ; Open dired in the unpacked directory
      (message "Not a valid archive file."))))

(defun dt/get-extract-command (file target-dir)
  "Return the appropriate shell command to extract FILE into TARGET-DIR based on the file extension."
  (cond
    ((string-match-p "\\.tar\\'" file)
      (format "tar -xf %s -C %s" (shell-quote-argument file) (shell-quote-argument target-dir)))
    ((or (string-match-p "\\.tar\\.gz\\'" file)
       (string-match-p "\\.tgz\\'" file))
      (format "tar -xzf %s -C %s" (shell-quote-argument file) (shell-quote-argument target-dir)))
    ((string-match-p "\\.zip\\'" file)
      (format "unzip %s -d %s" (shell-quote-argument file) (shell-quote-argument target-dir)))
    (t nil)))

(defun dt/generate-sequential-dir-name (base-dir)
  "Generate a sequentially numbered unique directory name under BASE-DIR."
  (let* ((existing-dirs (directory-files base-dir t "unpacked-[0-9]+"))
          (indices (mapcar (lambda (dir)
                             (string-to-number (replace-regexp-in-string ".*unpacked-\\([0-9]+\\)" "\\1" dir)))
                     existing-dirs))
          (max-index (if indices (apply 'max indices) -1)))
    (concat base-dir "unpacked-" (number-to-string (1+ max-index)))))

;; Bind the function to a key in dired mode
(define-key dired-mode-map (kbd "U") 'dt/dired-unpack-archive-to-directory)

(defun close-other-buffers ()
  "Close all buffers except the current one."
  (interactive)
  (mapc (lambda (buffer)
          (unless (eq buffer (current-buffer))
            (kill-buffer buffer)))
    (buffer-list))
  (message "Closed all other buffers."))
(defun replace-all (pattern replacement)
  (goto-char (point-min))
  (while (search-forward pattern nil t)
    (replace-match replacement nil t)))

(defun replace-escaped-newlines-br-tags-and-url-encoded-chars ()
  "Replace escaped newlines (\\n), <br/> tags, &gt; and &lt;, and URL-encoded characters with their textual representations."
  (interactive)
  (replace-all "\\\\n" "\n")
  (replace-all "<br\\s-*/?>" "\n")
  (replace-html-entities-in-buffer)
  (message "Processed newlines, <br/> tags, HTML entities, and URL-encoded characters."))

(defun replace-html-entities-in-buffer ()
  "Replace HTML character entities in the current buffer with their corresponding characters."
  (interactive)
  (let ((content (buffer-string)))
    (delete-region (point-min) (point-max))
    (insert (replace-html-entities-with-characters content))))

(defun replace-html-entities-with-characters (string)
  "Replace HTML character entities in STRING with their corresponding characters."
  (let ((entities '(("&lt;" . "<")
                     ("&gt;" . ">")
                     ("&amp;" . "&")
                     ("&quot;" . "\"")
                     ("&apos;" . "'")
                     ("&nbsp;" . " ")
                     ("&#x27;" . "'")
                     )))
    (dolist (pair entities string)
      (setq string (replace-regexp-in-string (car pair) (cdr pair) string)))))

(defun open-current-file-in-chrome ()
  "Open the current file in Google Chrome."
  (interactive)
  (if (buffer-file-name)
    (start-process "open-chrome" nil "google-chrome" (buffer-file-name))
    (message "Buffer is not visiting a file")))

(defun sanitize-html-report ()
  "Render ANSI color codes and replace HTML entities (&gt; and &lt;) in the current buffer."
  (interactive)
  ;; Replace &gt; with > and &lt; with <
  (replace-escaped-newlines-br-tags-and-url-encoded-chars)
  (dt/apply-ansi-colors)
  (open-current-file-in-chrome)
  (message "Processed ANSI colors and sanitized report."))

(defun open-jira-issue ()
  "Open the Jira issue at the cursor in the default web browser."
  (interactive)
  (let ((jira-base-url "https://eteamproject-alpha.internal.ericsson.com/browse/")
         (jira-id-regex "\\bADPPRG-[0-9]+\\b"))
    (save-excursion
      ;; Get the entire line as a string
      (let ((line (thing-at-point 'line t)))
        (if (and line (string-match jira-id-regex line))
          (let ((jira-id (match-string 0 line)))
            (browse-url (concat jira-base-url jira-id)))
          (message "No Jira ID found on the current line!"))))))

(use-package bm
  :ensure t
  :demand t

  :init
  (setq bm-restore-repository-on-load t
    bm-cycle-all-buffers t
    bm-repository-file (expand-file-name "bm-repository" user-emacs-directory)
    bm-buffer-persistence t)

  :config
  ;; Restore bookmarks after init
  (add-hook 'after-init-hook #'bm-repository-load)

  ;; Save bookmarks at appropriate lifecycle points
  (add-hook 'kill-buffer-hook     #'bm-buffer-save)
  (add-hook 'after-save-hook      #'bm-buffer-save)
  (add-hook 'after-revert-hook    #'bm-buffer-restore)
  (add-hook 'find-file-hook       #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook
    (lambda ()
      (bm-buffer-save-all)
      (bm-repository-save)))

  :bind (("C-c ." . bm-next)
          ("C-c ," . bm-previous)
          ("C-c <up>" . bm-show-all)
          ("C-c m" . bm-toggle)))


(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun cc/--next-sexp-raw ()
  "Raw implementation to move point to the beginning of the next sexp.

This function has no error checking."
  (forward-sexp 2)
  (backward-sexp))

(defun cc/next-sexp ()
  "Move point to beginning of the next balanced expression (sexp)."
  (interactive)
  (condition-case nil
    (cc/--next-sexp-raw)
    (error (condition-case nil
             (forward-sexp)
             (error
               (message
                 "Unable to move point to next balanced expression (sexp)."))))))

(defun my-isearch-forward-prepopulate ()
  "Start isearch with the active region as initial input, if any."
  (interactive)
  (let ((initial (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                   "")))
    (when (and initial (not (string= initial "")))
      (deactivate-mark))
    (isearch-mode t nil nil nil)
    (when (and initial (not (string= initial "")))
      (isearch-yank-string initial))))

(keymap-global-set "C-s" #'my-isearch-forward-prepopulate)

(use-package back-button
  :ensure t
  :hook (after-init . back-button-mode)
  :config
  (define-key back-button-mode-map (kbd "C-i") nil)
  :bind (:map back-button-mode-map
          ("M-o"     . back-button-local-backward)
          ("M-C-o"   . back-button-global-backward)
          ("M-i"     . back-button-local-forward)
          ("M-C-i"   . back-button-global-forward)))

(defun jump-to-matching-delimiter ()
  "Jump to the matching delimiter ((), {}, [], '', \"\")."
  (interactive)
  (cond
   ;; If on an opening delimiter, move forward to match
   ((member (char-after) '(?\( ?\[ ?\{ ?\" ?\'))
    (forward-sexp 1))
   ;; If on a closing delimiter, move backward to match
   ((member (char-before) '(?\) ?\] ?\} ?\" ?\'))
    (backward-sexp 1))
   (t
    (message "Not on a delimiter."))))

(global-set-key (kbd "M-%") 'jump-to-matching-delimiter)


(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(use-package git-gutter-fringe
  :ensure t
  :config
  (setq git-gutter-fr:side 'left-fringe)

  ;; Define fringe bitmaps to indicate changes
  (fringe-helper-define 'git-gutter-fr:added nil
    "......."
    "......."
    "......."
    "......."
    "......."
    "......."
    "......."
    ".......")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "......."
    "......."
    "......."
    "......."
    "......."
    "......."
    "......."
    ".......")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "......."
    "......."
    "......."
    "......."
    "......."
    "......."
    "......."
    "......."))

(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)

(global-set-key (kbd "M-n") #'next-error)
(global-set-key (kbd "M-p") #'previous-error)

(defun my/save-all-buffers ()
  "Save all modified file-visiting buffers without prompting."
  (interactive)
  (save-some-buffers t))

(use-package gptel
  :ensure t
  :config
  (setq gptel-log-level 'debug)
)

;; Key in ~/.authinfo
(setq
  gptel-model 'gpt-4o
  gptel-backend (gptel-make-gpt4all
                  "GPT4All"
                  :protocol "https"
                  :host "gateway.eli.gaia.gic.ericsson.se/api/openai"
                  :models '(gpt-4o)
                  :key gptel-api-key
                  ))

(gptel-make-preset 'gpt4coding
  :description "A preset optimized for coding tasks"
  :backend "GpT4All"
  :model 'gpt-4o
  :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
  :tools '("read_buffer" "modify_buffer"))


(setq gptel-default-preset 'gpt4coding)

(load-file "~/.emacs.d/gptel-prompts.el")

(use-package ediff
  :ensure nil
  :init
  (setq ediff-split-window-function 'split-window-horizontally
    ediff-window-setup-function 'ediff-setup-windows-plain))

(defun ediff-current-windows ()
  "Run ediff on the buffers displayed in the current frame's two windows."
  (interactive)
  (let ((windows (window-list)))
    (if (= (length windows) 2)
       (let ((buf1 (window-buffer (car windows)))
             (buf2 (window-buffer (cadr windows))))
         (ediff-buffers buf1 buf2))
     (error "This function requires exactly 2 windows"))))

;;;post-init.el ends here
