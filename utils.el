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

(defun dt/kill-and-close-buffer ()
  "Kill and Close current active buffer"
  (interactive)
  (kill-this-buffer)
  (delete-window)
  )

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

(defun dt/unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

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

(defun dt/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1))

(defun dt/apply-ansi-colors ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun dt/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
    (concat
      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun dt/add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'dt/ediff-copy-both-to-C))

(add-hook 'ediff-keymap-setup-hook 'dt/add-d-to-ediff-mode-map)
