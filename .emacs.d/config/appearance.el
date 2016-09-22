;; load and set the color theme
(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-night t))

;; show number of search matches in the mode line
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode +1))

;; enable relative line numbers
(use-package linum-relative
  :diminish linum-relative-mode)

(use-package vi-tilde-fringe
  :diminish vi-tilde-fring-mode
  :config (global-vi-tilde-fringe-mode))

(use-package powerline
  :config (powerline-vim-theme))

(setq-default cursor-type 'bar)

;; amount of line spacing in a buffer
(setq-default line-spacing 0.2)

;; shorter confirmation prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; show matching parens
(setq show-paren-delay 0)
(show-paren-mode t)

;; give a visual indication of an empty line
(setq indicate-empty-lines t)

;; show the column number in the mode line
(setq-default column-number-mode t)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; don't wrap long lines
(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows t)

;; allow the emacs window to be resized nicely
(setq frame-resize-pixelwise t)
(setq window-combination-resize t)

; ;; set the editor font
(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :height 100
                    :weight 'normal)

(setq indent-guide-char "|")
