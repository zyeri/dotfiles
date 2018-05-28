;;; -*- lexical-binding: t; -*-
;; here be dragons

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(eval-when-compile
  (require 'cl-lib))

(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(setq load-prefer-newer t)

(when (not package-archive-contents)
  (package-refresh-contents))

;; make sure use-package is available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'delight)
  (package-install 'delight))

(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(require 'diminish)
(require 'bind-key)
(require 'delight)

(setq use-package-always-ensure t
      use-package-verbose nil)

;; store customizations in an external file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror 'nomessage)

(setq visible-bell t)

;; minibuffer garbage collection
(defun dotemacs/minibuffer-setup-hook ()
  "Set `GC-CONS-THRESHOLD' to `MOST-POSITIVE-FIXNUM'."
  (setq gc-cons-threshold most-positive-fixnum))

(defun dotemacs/minibuffer-exit-hook ()
  "Set `GC-CONS-THRESHOLD' to a reasonable number."
  (setq gc-cons-threshold (* 64 1024 1024)))

(add-hook 'minibuffer-setup-hook #'dotemacs/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook  #'dotemacs/minibuffer-exit-hook)

;; startup profiler
(use-package esup
  :defer t)

;; utility libraries
(use-package dash
  :config
  (eval-after-load 'dash '(dash-enable-font-lock)))

(use-package dash-functional
  :after dash)

(use-package s)
(use-package f)

;; custom paths for Emacs to store stuff
(defconst dotemacs/user-home-directory
  (f-expand (getenv "HOME"))
  "Home directory for the current user.")

(defconst dotemacs/user-emacs-directory
  (f-join dotemacs/user-home-directory ".emacs.d")
  "Emacs directory for the current user.")

(defconst dotemacs/user-dropbox-directory
  (f-join dotemacs/user-home-directory "Dropbox")
  "Dropbox directory for the current user.")

(defconst dotemacs/user-org-directory
  (f-join dotemacs/user-dropbox-directory "org")
  "Directory to look for Org files.")

(defconst dotemacs/user-local-directory
  (f-join dotemacs/user-emacs-directory "local")
  "Root directory for local Emacs files.")

(defconst dotemacs/user-host-directory
  (f-join dotemacs/user-local-directory (format "@%s" (system-name)))
  "Directory for hostname-specific files.")

(defconst dotemacs/user-etc-directory
  (f-join dotemacs/user-host-directory "etc")
  "Location for permanent files to be stored.")

(defconst dotemacs/user-cache-directory
  (f-join dotemacs/user-host-directory "cache")
  "Location for cache files to be stored.")

(defun dotemacs/ensure-local-folders () 
  (dolist (dir (list dotemacs/user-local-directory dotemacs/user-host-directory dotemacs/user-etc-directory dotemacs/user-cache-directory))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(dotemacs/ensure-local-folders)

;; shorter confirmation prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; general defaults
(setq-default ad-redefinition-action 'accept
              apropos-do-all t
              compilation-always-kill t
              compilation-ask-about-save nil
              compilation-scroll-output t
              confirm-nonexistent-file-or-buffer t
              enable-recursive-minibuffers nil
              ;; keep the point out of the minibuffer
              minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
              vc-follow-symlinks t
              save-interprogram-paste-before-kill t
              sentence-end-double-space nil
              ;; scrolling
              scroll-conservatively 1001
              scroll-margin 0
              scroll-preserve-screen-position t
              ;; whitespace
              indent-tabs-mode nil
              require-file-newline t
              tab-always-indent t
              tab-width 4
              echo-keystrokes 0.1
              browse-url-browser-function 'browse-url-generic
              browse-url-generic-program "google-chrome-stable"
              delete-by-moving-to-trash t
              calendar-week-start-day 1
              frame-title-format '(buffer-file-name "%f" ("%b"))
              custom-safe-themes t
              ;; wrapping
              truncate-lines t
              truncate-partial-width-windows 50)

(setq display-line-numbers-type 'relative)

;; paths
(setq-default abbrev-file-name             (f-join dotemacs/user-local-directory "abbrev.el")
              auto-save-list-file-name     `((".*" . ,(f-join dotemacs/user-cache-directory "autosave")))
              pcache-directory             (f-join dotemacs/user-cache-directory "pcache")
              server-auth-dir              (f-join dotemacs/user-cache-directory "server")
              url-cache-directory          (f-join dotemacs/user-cache-directory "url")
              url-configuration-directory  (f-join dotemacs/user-etc-directory   "url")
              backup-directory-alist `((".*" . ,(f-join dotemacs/user-cache-directory "backups"))))

;; history and backups
(setq-default auto-save-default nil
              create-lockfiles nil
              history-length 500
              history-delete-duplicates t
              history-save-minibuffer-history t
              make-backup-files nil)

(setq savehist-file (f-join dotemacs/user-cache-directory "savehist")
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (f-join dotemacs/user-cache-directory "saveplace"))

(use-package unkillable-scratch
  :config
  (setq unkillable-scratch-behavior 'bury)
  (add-hook 'after-init-hook 'unkillable-scratch))

;; save-place.el
(setq save-place-file (f-join dotemacs/user-cache-directory "save-place")
      save-place-forget-unreadable-files t)
(save-place-mode 1)

(setq-default bookmark-default-file (f-join dotemacs/user-cache-directory "bookmarks")
              bookmark-save-flag t)

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*")

;; recent files
(use-package recentf
  :init (add-hook 'after-init-hook #'recentf-mode)
  :config
  (setq recentf-save-file (f-join dotemacs/user-cache-directory "recentf")
        recentf-max-saved-items 300
        recentf-max-menu-items 0
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$")))

(setq eshell-directory-name (concat dotemacs/user-cache-directory "eshell"))

(use-package editorconfig
  :demand t
  :diminish editorconfig-mode
  :mode ("\\.?editorconfig$" . editorconfig-conf-mode)
  :config
  (add-hook 'after-init-hook #'editorconfig-mode))

;; unicode
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when (string-equal system-type "gnu/linux")
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
        select-enable-clipboard t
        select-enable-primary t))

;; always prettify symbols
(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; show column and line numbers in the mode line
(column-number-mode)
(line-number-mode)

;; highlight matching parens
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(show-paren-mode)

(auto-compression-mode t)
(random t)

;; reload files that have been modified outside of Emacs
(global-auto-revert-mode t)
(setq auto-revert-verbose nil)

;; shift+{up,down,left,right} to navigate between windows
(require 'windmove)
(windmove-default-keybindings)

;; show number of matches
(use-package anzu
  :defer t
  :diminish anzu-mode
  :init
  (setq anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode +1)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; disable annoying startup stuff
(advice-add #'display-startup-echo-area-message :override 'ignore)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; run Emacs as a server
(require 'server)
(unless (server-running-p)
  (server-start))

(use-package color-theme-sanityinc-tomorrow
  :defer t)

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-neotree-config)

  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)

  (doom-themes-org-config))

(use-package all-the-icons
  :disabled )

(use-package all-the-icons-dired
  :disabled
  :hook (dired-mode . all-the-icons-dired-mode)
  :after (:all all-the-icons))

(use-package neotree
  :disabled
  :after (:all all-the-icons projectile)
  :init
  (setq neo-create-file-auto-open t
        neo-banner-message nil
        neo-smart-open nil
        neo-show-updir-line nil
        neo-mode-line-type 'neotree
        neo-dont-be-alone t
        neo-window-width 32
        neo-show-hidden-files t
        neo-auto-indent-point t
        neo-modern-sidebar t)

  (setq neo-theme
        (if window-system 'icons 'arrow))
  
  (setq projectile-switch-project-action 'neotree-projectile-action)

  :bind (([f2] . neotree-toggle)
         ("C-c f t" . neotree-toggle)))

(unless (package-installed-p 'hydra)
  (package-install 'hydra))

(use-package hydra
  :load-path "site-lisp/hydra")

(use-package counsel)

(use-package counsel-projectile
  :after (:all counsel projectile)
  :config
  (counsel-projectile-mode))

(use-package swiper)

(use-package avy)

(use-package ivy
  :load-path "site-lisp/swiper"
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-x C-f" . counsel-find-file)
         ("C-c C-s" . counsel-ag)
         ("C-c j" . counsel-git-grep)
         ("C-x l" . counsel-locate)
         ("C-c C-r" . ivy-resume)
         :map ivy-mode-map
              ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)

  (setq projectile-completion-system 'ivy)
  (setq magit-completing-read-function 'ivy-completing-read)

  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10))

(defconst dotemacs/themes
  '((sanityinc-tomorrow-day 'light)
    (sanityinc-tomorrow-night 'dark)
    (doom-tomorrow-night 'dark))
  "List of available themes.")

(defun dotemacs/disable-all-themes ()
  "Disable all active themes."
  (interactive)
  (dolist (theme dotemacs/themes)
    (disable-theme (car theme))))

(defun dotemacs/generate-theme-fn (theme-name dark)
  (let ((theme-fn-name (intern (format "load-theme/%s" theme-name))))
    `(defun ,theme-fn-name ()
       (interactive)
       ;; (setq dark-theme (equal ,dark 'dark))
       (dotemacs/disable-all-themes)
       (load-theme ',theme-name t)
       (with-eval-after-load 'smart-mode-line
         (sml/apply-theme ,dark)))))

(defmacro dotemacs/generate-all-theme-fns ()
  (declare (indent 4))
  `(progn ,@(mapcar
             (lambda (x)
               (dotemacs/generate-theme-fn (nth 0 x) (nth 1 x)))
             dotemacs/themes)))

(dotemacs/generate-all-theme-fns)

(defun dotemacs/ivy-load-theme-action (x)
  (funcall (intern (format "load-theme/%s" (car x)))))

(defun dotemacs/ivy-load-theme ()
  "Use `ivy-read` select and load a theme."
  (interactive)
  (ivy-read "Select theme: " dotemacs/themes
            :action (lambda (x)
                      (funcall (intern (format "load-theme/%s" (car x)))))))

; ;; begin helm
; (use-package helm
;   :diminish helm-mode
;   :config
;   (require 'helm)
;   (require 'helm-config)

;   (global-set-key (kbd "C-c h") 'helm-command-prefix)
;   (global-unset-key (kbd "C-x c"))

;   (setq helm-quick-update t
;         helm-M-x-fuzzy-match t
;         helm-apropos-fuzzy-match t
;         helm-locate-fuzzy-sort-fn t
;         helm-recentf-fuzzy-match t
;         helm-buffers-fuzzy-matching t
;         helm-move-to-line-cycle-in-source nil
;         helm-display-header-line nil
;         helm-display-function 'pop-to-buffer
;         helm-ff-newfile-prompt-p nil
;         helm-ff-skip-boring-files t
;         helm-ff-search-library-in-sexp t
;         helm-ff-file-name-history-use-recentf t
;         helm-echo-input-in-header-line t
;         helm-reuse-last-window-split-state nil
;         helm-split-window-in-side-p t
;         helm-autoresize-min-height 10)

;   (when (and (eq system-type 'gnu/linux)
;              (executable-find "locate"))
;     (setq helm-locate-command "locate %s -e -A --regex %s"))

;   (when (and (eq system-type 'gnu/linux)
;              (executable-find "git"))
;     (setq helm-ls-git-grep-command "git grep -n%cH --color=always --full-name -e %p %f"))

;   (helm-mode t)
;   (helm-autoresize-mode)
;   (helm-adaptive-mode t)
;   :bind (("M-x"     . helm-M-x)
;          ("M-y"     . helm-show-kill-ring)
;          ("C-h a"   . helm-apropos)
;          ("C-x b"   . helm-mini)
;          ("C-x C-b" . helm-buffers-list)
;          ("C-x C-l" . helm-locate)
;          ("C-x C-m" . helm-man-woman)
;          ("C-x C-f" . helm-find-files)
;          ("C-x C-r" . helm-recentf)
;          ;; ("C-c h o" . helm-occur)
;          ;; ("C-c h x" . helm-register)
;          ("C-c p h" . helm-projectile)
;          ("C-c b p" . helm-browse-project)
;          :map helm-map
;          ("TAB" . helm-execute-persistent-action)
;          ("C-z" . helm-select-action)
;          ("M-i" . helm-previous-line)
;          :map helm-command-prefix
;          ("b" . helm-buffers-list)
;          ("f" . helm-find-files)
;          ("m" . helm-mini)
;          ("o" . helm-imenu)))

; (use-package helm-projectile
;   :after (:all helm projectile)
;   :init
;   (setq projectile-completion-system 'helm
;         projectile-switch-project-action 'helm-projectile)
;   :config
;   (helm-projectile-on))

; (use-package helm-swoop
;   :after (:all helm)
;   :init
;   (setq helm-swoop-use-fuzzy-match t
;         helm-swoop-split-direction 'split-window-vertically
;         helm-swoop-split-with-multiple-windows nil
;         helm-swoop-split-window-function #'pop-to-buffer
;         helm-swoop-use-fuzzy-match t
;         helm-swoop-speed-or-color t)
;   :bind (("M-i" . helm-swoop)
;          ("C-s" . helm-swoop)
;          ("C-c M-i" . helm-multi-swoop)
;          ("C-x M-i" . helm-multi-swoop-all)))


; (use-package helm-descbinds
;   :after (:all helm)
;   :config
;   (add-hook 'after-init-hook #'helm-descbinds-mode)
;   :bind (("C-h b" . helm-descbinds)))

; (use-package helm-ls-git
;   :after helm)

; (use-package helm-flx
;   :init
;   (setq helm-flx-for-helm-find-files t
;         helm-flx-for-helm-locate t)
;   :config
;   (helm-flx-mode +1))

; (use-package helm-ag
;   :commands (helm-ag helm-projectile-ag)
;   :init
;   (setq helm-ag-insert-at-point 'symbol)
;   :bind (("M-p" . helm-projectile-ag)))

; ;; end helm

(use-package ag
  :init
  (setq ag-highlight-search t
        ag-reuse-window t
        ag-reuse-buffers t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-copy-envs
   '("WORKON_HOME" "PROJECT_HOME"))
  (exec-path-from-shell-initialize))

;; python setup
(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-completion-native-enable nil
        python-shell-interpreter "python"))

(use-package elpy
  :config
  (elpy-enable))

;; don't use native tooltips
(cond ((eq system-type 'gnu/linux)
       (setq x-gtk-use-system-tooltips nil)))

;; font Emacs should use
(cond ((member "Iosevka" (font-family-list))
       (set-frame-font "Iosevka-12")
       (set-face-attribute 'default nil :font "Iosevka-12")))

;; prettier mode line
(use-package smart-mode-line
  :init
  (setq-default sml/theme 'dark
                sml/vc-mode-show-backend t)
  :config
  (sml/setup))

;; dired
(require 'dired)
(require 'dired-x)

(put 'dired-find-alternative-file 'disabled nil)

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-dwim-target t
      dired-listing-switches "-laGh1v --group-directories-first")

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package peep-dired
  :after dired
  ;; don't access `dired-mode-map' until `peep-dired' is loaded
  :defer t
  :init
  (setq peep-dired-cleanup-on-disable t)
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; manage popups
(use-package shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-rules
        '(("*eshell*" :select t :other t)
          ("*Help*" :select t :inhibit-window-quit t :other t)
          ("*Messages*" :select nil :inhibit-window-quit t)
          ("*magit-dispatch-popup*" :select t :align t)
          (" *NeoTree*" :select t :align left :static t)
          ("*compilation*" :size 0.25 :noselect t :autokill t :autoclose t)
          ("*ert*" :same t :modeline t)
          ("*info*" :size 0.5 :select t :autokill t)
          ("*Backtrace*" :size 20 :noselect t)
          ("*Warnings*"  :size 12 :noselect t :autofit t)
          ("*Messages*"  :size 12 :noselect t)
          ("*Help*" :size 0.3)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 20 :noselect t :autokill t)
          (apropos-mode :size 0.3 :autokill t :autoclose t)
          (Buffer-menu-mode :size 20 :autokill t)
          (comint-mode :noesc t)
          (grep-mode :size 25 :noselect t :autokill t)
          (profiler-report-mode :size 0.3 :regexp t :autokill t :modeline minimal)
          (tabulated-list-mode :noesc t)
          ("^ ?\\*" :regexp t :size 15 :noselect t :autokill t :autoclose t)
          ("^\\*magit" :regexp t :size 0.5 :noesc t :autokill t)))
  :config
  (add-hook 'after-init-hook #'shackle-mode))

;; evil mode
(use-package evil
  :disabled
  :init
  (setq evil-want-fine-undo 'fine
        evil-default-cursor 'box
        evil-move-beyond-eol t
        evil-esc-delay 0
        evil-ex-substitute-global t
        evil-ex-visual-char-range t
        evil-symbol-word-search  t
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-magic t
        evil-echo-state t)
  :config
  ;; set initial state for specific modes
  (cl-loop for (mode . state)
           in '((help-mode . emacs)
                (debugger-mode . emacs)
                (dired-mode . emacs)
                (tabulated-list-mode . emacs)
                (view-mode . emacs)
                (comint-mode . emacs)
                (term-mode . emacs)
                (calendar-mode . emacs)
                (Man-mode . emacs)
                (grep-mode . emacs)
                (image-mode . emacs)
                (paradox-menu-mode . emacs)
                (epa-key-list-mode . emacs)
                (dashboard-mode . emacs)
                (neotree-mode . emacs)
                (prodigy-mode . emacs)
                (special-mode . emacs)
                (messages-buffer-mode . emacs)
                (flymake-diagnostics-buffer-mode . emacs)
                (TeX-output-mode . emacs)
                (esup-mode . emacs))
           do (evil-set-initial-state mode state))
  (evil-mode)
  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package evil-commentary
  :diminish
  :after evil
  :config
  (evil-commentary-mode +1))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))

(use-package evil-anzu
  :after evil)

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-search-highlight-persist
  :after evil
  :config
  (global-evil-search-highlight-persist t))

(use-package undo-tree
  :diminish
  :init
  (setq undo-tree-history-directory-alist `((".*" . ,(f-join dotemacs/user-cache-directory "undo-tree-history")))
        undo-tree-auto-save-history t))

(use-package general
  :init
  (setq general-default-keymaps 'evil-normal-state-map)
  :config
  (require 'general)

  (general-define-key "[ SPC" 'insert-newline-above
                      "] SPC" 'insert-newline-below))

(use-package move-text
  :after general
  :config
  (general-define-key "[ e" 'move-text-up
                      "] e" 'move-text-down))

(use-package ag
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers t))

(use-package flycheck
  :diminish flycheck-mode)

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  (setq ispell-program-name (executable-find "aspell")
        ispell-extra-args '("--sug-mode=ultra")))

(use-package magit
  :init
  (setq magit-section-show-children-count t
        magit-diff-arguments '("--histogram")
        magit-ediff-dwim-show-on-hunks t
        magit-repository-directories `(( ,(f-join (getenv "HOME") "src" "projects"))
                                       ( ,(f-join (getenv "HOME") "src" "repos"))))
  :config
  (require 'with-editor)
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)
  :bind (("C-c g"   . magit-status)))

(use-package evil-magit
  :disabled
  :after (:all evil magit))

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-popup-type 'minibuffer
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5
        which-key-sort-uppercase-first nil
        which-key-sort-order #'which-key-prefix-then-key-order)
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'after-init-hook #'which-key-mode))

(use-package expand-region
  :bind (("C-c C-+" . er/expand-region)
         ("C-c C--" . er/contract-region)))

(use-package eldoc
  :diminish
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package projectile
  :diminish
  :init
  (setq projectile-enable-caching t
        projectile-cache-file (f-join dotemacs/user-cache-directory "projectile-cache.el")
        projectile-known-projects-file (f-join dotemacs/user-cache-directory "projectile-projects.el"))
  :config
  (projectile-mode))

(use-package company
  :diminish
  :init
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-dabbrev-code-everywhere t
        company-idle-delay 1
        company-show-numbers t
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-limit 10
        company-begin-commands '(self-insert-command)
        company-backends '((company-files
                            company-keywords
                            company-capf
                            company-yasnippet)
                           (company-abbrev))
        company-disabled-backends '(dabbrev)
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))
  :config
  (global-company-mode)
  :bind (("C-." . company-files)
         ("M-<tab>" . company-complete)
         ("C-c C-y" . company-yasnippet)))

(use-package company-statistics
  :after company
  :config
  (setq company-statistics-file (f-join dotemacs/user-cache-directory "company-statistics-cache.el"))
  (add-hook 'after-init-hook #'company-statistics-mode))

(use-package cmake-mode)

(use-package org
  :diminish org-indent-mode
  :config
  (setq org-directory (f-expand "~/Dropbox/org")
        org-agenda-files '("~/Dropbox/org/agenda.org")
        org-default-notes-file "~/Dropbox/org/notes.org")

  ;; source code
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t)

  (setq org-agenda-custom-commands
        '(("D" "Deadlines"
           ((tags-todo "DEADLINE=\"<today>\""
                       ((org-agenda-overriding-header "Due today")))
            (tags-todo "DEADLINE<\"<today>\""
                       ((org-agenda-overriding-header "Late")))
            (tags-todo "+DEADLINE<\"<+5d>\"+DEADLINE>\"<today>\""
                       ((org-agenda-overriding-header "Very late")))))
          ("@" "Contexts"
           ((tags-todo "@email"
                       ((org-agenda-overriding-header "Emails")))
            (tags-todo "@phone"
                       ((org-agenda-overriding-header "Phone")))))))
 
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "SCHEDULED(s)" "ACTIVE(a)" "|" "DONE(d)" "CANCELED(c)")))

  ;; capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/org/agenda.org" "Tasks")
           "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)))

  ;; agenda
  (setq org-agenda-show-all-dates t
        org-deadline-warning-days 14)

  ;; dim blocked tasks
  (setq org-agenda-dim-blocked-tasks t)

  ;; compact block agenda view
  (setq org-agenda-compact-blocks t)

  ;; time
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-report-include-clocking-task t)
  (setq org-pretty-entities t)

  ;; resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t
        ;; Save clock data and state changes and notes in the LOGBOOK drawer
        org-clock-into-drawer t
        ;; don't prompt to resume active clock
        org-clock-persist-query-resume nil
        ;; auto clock resolution for finding open clocks
        org-clock-auto-clock-resolution '(when-no-clock-is-running)
        ;; include current clocking task in clock reports
        org-clock-report-include-clocking-task t
        ;; round time
        org-time-stamp-rounding-minutes '(1 1)
        ;; keep tasks with dates on the global todo lists
        org-agenda-todo-ignore-with-date nil
        ;; keep tasks with deadlines on the global todo lists
        org-agenda-todo-ignore-deadlines nil
        ;; keep tasks with scheduled dates on the global todo lists
        org-agenda-todo-ignore-scheduled nil
        ;; keep tasks with timestamps on the global todo lists
        org-agenda-todo-ignore-timestamp nil
        ;; remove completed deadline tasks from the agenda view
        org-agenda-skip-deadline-if-done t
        ;; remove completed scheduled tasks from the agenda view
        org-agenda-skip-scheduled-if-done t
        ;; remove completed items from search results
        org-agenda-skip-timestamp-if-done t
        ;; show all future entries for repeating tasks
        org-agenda-repeating-timestamp-show-all t
        ;; show all agenda dates - even if they are empty
        org-agenda-show-all-dates t)

  ;; sorting order for agenda tasks
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
          (todo category-up priority-down effort-up)
          (tags category-up priority-down effort-up)
          (search category-up)))

  ;; weekly agenda starts on Monday
  (setq org-agenda-start-on-weekday 1)

  ;; don't hide leading stars
  (setq org-hide-leading-stars nil)

  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto)))

  (setq org-id-method (quote uuidgen))
  (setq org-deadline-warning-days 30)
  ;; (setq org-table-export-default-format "orgtbl-to-tsv")

  ;; logging time
  (setq org-log-done '(time))
  (setq org-log-into-drawer "LOGBOOK")

  (setq org-plantuml-jar-path "~/bin/plantuml.jar")

  (add-to-list 'org-structure-template-alist
               (list "p" (concat ":PROPERTIES:\n"
                                 "?\n"
                                 ":END:")))

  (add-to-list 'org-structure-template-alist
               (list "eh" (concat ":EXPORT_FILE_NAME: ?\n"
                                  ":EXPORT_TITLE:\n"
                                  ":EXPORT_OPTIONS: toc:nil html-postamble:nil num:nil")))

  ;; use my gpg key to encrypt any heading with the :crypt: tag
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)

  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key "1C534F4138D5CCE4")

  (require 'ox-latex)

  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))                 

  (use-package ob-http)

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t)
     (haskell . t)
     (python . t)
     (sql . t)
     (sqlite . t)
     (plantuml . t)))
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)))

(use-package htmlize
  :after (:all org))

(use-package elfeed
  :commands elfeed
  :bind ("C-x w" . elfeed))

(use-package elfeed-org
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list (f-expand "~/Dropbox/org/elfeed.org")))
  :config
  (elfeed-org))

(use-package tramp
  :init
  (setq-default tramp-auto-save-directory    (f-join dotemacs/user-cache-directory "tramp-auto-save/")
                tramp-backup-directory-alist backup-directory-alist
                tramp-persistency-file-name  (f-join dotemacs/user-cache-directory "tramp-persistency.el"))
  (setq tramp-default-method "ssh"
        tramp-use-ssh-controlmaster-options ""))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

(use-package plantuml-mode
  :init
  (setq plantuml-jar-path "~/bin/plantuml.jar")
  :mode (("\\.plantuml\\'" . plantuml-mode)
         ("\\.puml\\'" . plantuml-mode)))

(use-package prodigy
  :bind (("<f12>" . prodigy)))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/private")
        yas-indent-line nil)
  :config
  (yas-reload-all)
  (yas-global-mode 1)

  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package hippie-exp
  :ensure nil
  :defer t
  :bind (("<C-return>" . hippie-expand)
         ("C-j" . hippie-expand))
  :config
  (setq-default hippie-expand-try-functions-list '(yas-hippie-try-expand)))

(use-package tex
  :mode (("\\.tex\\'" . latex-mode))
  :ensure auctex
  :config
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'TeX-revert-document-buffer)

  (setq TeX-save-query nil
        TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex
        LaTeX-fill-break-at-separators nil)

  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))

  (setq-default TeX-master nil
                TeX-engine 'luatex)

  ;; enable reftex integration
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)

  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))

(use-package auctex-latexmk
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package company-auctex
  :mode (("\\.tex\\'" . latex-mode))
  :after (:all company auctex)
  :config
  (add-hook 'LaTeX-mode-hook #'company-auctex-init)
  (add-hook 'TeX-mode-hook #'company-auctex-init))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package haskell-mode
  :mode (("\\.hs\"" . haskell-mode))
  :commands haskell-mode
  :config
  (require 'haskell-mode)

  (setq haskell-interactive-popup-errors nil
        haskell-ask-also-kill-buffers nil
        haskell-process-type 'auto)

  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package hindent
  :diminish hindent-mode
  :after haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package ini-mode
  :mode ("\\.ini\\'" . ini-mode))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)
         ("Pipfile" . toml-mode)))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode))

(use-package tern
  :after js2-mode)

(use-package company-tern
  :init
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode))
  :init
  (setq web-mode-style-padding 4
        web-mode-script-padding 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-markup-indent-offset 2
        web-mode-enable-auto-quoting nil
        web-mode-comment-keywords t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-heredoc-fontification t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-enable-column-highlight t
        web-mode-engines-alist
        '(("jinja" . "\\.html\\'")))
  :config
  (add-hook 'web-mode-hook #'emmet-mode))

(use-package company-web
  :after (:all company web-mode)
  :config
  (require 'company-web-html))

(use-package css-mode
  :after web-mode
  :mode ("\\.css$" . css-mode)
  :config
  (setq css-indent-offset 4))

(use-package sass-mode
  :after web-mode
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :after web-mode
  :mode ("\\.scss\\'" . scss-mode))

(use-package emmet-mode
  :after web-mode
  :init
  (setq-default emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)

  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t))

(use-package macrostep
  :defer t
  :bind (:map emacs-lisp-mode-map
         ("C-c e" . macrostep-expand)))

(use-package systemd)

(use-package nginx-mode)

(defun insert-newline-above (count)
  "Insert one or several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun insert-newline-below (count)
  "Insert one or several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun dotemacs/scale-font-size (direction)
  "Scale the font. When DIRECTION is positive or zero, the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale)))))

(defun dotemacs/increase-font-size ()
  "Increase font size."
  (interactive)
  (dotemacs/scale-font-size 1))

(defun dotemacs/decrease-font-size ()
  "Decrease font size."
  (interactive)
  (dotemacs/scale-font-size -1))

(defun dotemacs/reset-font-size ()
  "Reset font size."
  (interactive)
  (dotemacs/scale-font-size 0))

(defun dotemacs/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

