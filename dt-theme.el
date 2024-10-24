(defun face-exists-p (face)
  "Check if FACE is defined."
  (facep face))

(defun my-set-face-attribute (face &rest args)
  "Set face attribute only if FACE exists.
FACE is the face name (symbol) and ARGS are the attributes for `set-face-attribute`."
  (if (facep face)
    (apply 'set-face-attribute face args)
    (message "Face '%s' does not exist. Skipping..." face)))

(setq my-foreground "#b6bdca")
(setq my-background "#23252e")
(setq my-background-darker "#21252b")
(setq my-blue "#729fdd")
(setq my-green "#57ab5a")
(setq my-red "#d46a76")
(setq my-orange "#ffa657")

(defun set-custom-faces ()
  "Set custom faces using dt/builtin."
  (when (face-exists-p 'default)
    (my-set-face-attribute 'default nil
      :inherit nil
      :extend nil
      :stipple nil
      :background my-background
      :foreground my-foreground
      :inverse-video nil
      :box nil
      :strike-through nil
      :overline nil
      :underline nil
      :slant 'normal
      :weight 'regular
      :height 138
      :width 'normal
      :family "Fira Code"))

  (my-set-face-attribute 'ansi-color-blue nil
    :background my-blue
    :foreground my-blue)

  (my-set-face-attribute 'ansi-color-bright-blue nil
    :background my-blue
    :foreground my-blue)

  (my-set-face-attribute 'ansi-color-bright-green nil
    :background my-green
    :foreground my-green)

  (my-set-face-attribute 'ansi-color-bright-red nil
    :background my-red
    :foreground my-red)

  (my-set-face-attribute 'ansi-color-green nil
    :background my-green
    :foreground my-green)

  (my-set-face-attribute 'ansi-color-red nil
    :background my-red
    :foreground my-red)

  (my-set-face-attribute 'compilation-mode-line-fail nil
    :inherit 'compilation-error
    :weight 'bold)

  (my-set-face-attribute 'isearch-fail nil
    :inherit 'warning)

  (my-set-face-attribute 'hl-line nil
    :background "#31353f")

  (my-set-face-attribute 'line-number nil
    :background "#31353f")

  (my-set-face-attribute 'line-number-current-line nil
    :background "#3b3f4c"
    :foreground my-blue)


  (my-set-face-attribute 'mode-line nil
    :background my-background-darker
    :foreground my-foreground)

  (my-set-face-attribute 'mode-line-inactive nil
    :background "#21252b"
    :foreground my-foreground
    :box nil)

  (my-set-face-attribute 'mode-line-active nil
    :inherit 'mode-line
    :box nil)

  (my-set-face-attribute 'mode-line-buffer-id nil
    :foreground my-blue)

  (my-set-face-attribute 'font-lock-warning-face nil
    :inherit 'warning)

  (my-set-face-attribute 'info-menu-star nil
    :inherit 'warning)

  (my-set-face-attribute 'mode-line-highlight nil
    :foreground my-blue)


  (my-set-face-attribute 'warning nil
    :foreground my-red)

  (my-set-face-attribute 'org-todo nil
    :foreground my-red
    :weight 'bold)

  (my-set-face-attribute 'success nil
    :foreground my-blue
    :weight 'bold)

  (my-set-face-attribute 'minibuffer-prompt nil
    :foreground my-blue)

  (my-set-face-attribute 'consult-line-number-wrapped nil
    :foreground my-red)

  (my-set-face-attribute 'magit-section-highlight nil
    :background "#31353f")

  (my-set-face-attribute 'corfu-current nil
    :background "#3b3f4c"
    :weight 'bold)

  (my-set-face-attribute 'corfu-default nil
    :background "#31353f")

  (my-set-face-attribute 'isearch nil
    :foreground my-orange
    :background "#31353f"
    :underline (list :color my-orange :style 'line)
    :weight 'extrabold)

  (my-set-face-attribute 'isearch-fail nil
    :foreground my-red
    :background my-background
    :weight 'extrabold)

  (my-set-face-attribute 'tabbar-default nil
    :background my-background-darker
    :foreground my-foreground
    :weight 'semibold
    :height '133
    :box nil)

  (my-set-face-attribute 'tabbar-button nil
    :inherit 'tabbar-default
    :box nil)

  (my-set-face-attribute 'tabbar-highlight nil
    :inherit 'tabbar-default
    :background my-background-darker
    :box nil
    :foreground my-orange)

  (my-set-face-attribute 'tabbar-modified nil
    :inherit 'tabbar-default
    :foreground my-blue
    :box nil)

  (my-set-face-attribute 'tabbar-selected nil
    :inherit 'tabbar-default
    :foreground my-blue
    :box nil)

  (my-set-face-attribute 'tabbar-selected-modified nil
    :inherit 'tabbar-default
    :foreground my-red
    :box nil)

  (my-set-face-attribute 'tabbar-separator nil
    :inherit 'tabbar-default
    :background my-background-darker
    :box nil)

  (my-set-face-attribute 'tabbar-unselected nil
    :inherit 'tabbar-default
    :box nil)

  (my-set-face-attribute 'region nil
    :background "#3b3f4c"
    :foreground 'unspecified))

(set-custom-faces)  ; Call the function to set the faces
