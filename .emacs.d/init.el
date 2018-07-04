;; -*- lexical-binding: t-*-

(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; (when (string-equal system-name "akemi")
;;   (setq tls-checktrust t)
;;   (setq gnutls-verify-error t))

(use-package diminish
  :ensure t)

;; some useful utility libraries
(use-package dash
  :ensure t)

(use-package dash-functional
  :ensure t
  :after dash)

(use-package s
  :ensure t)

(use-package f
  :ensure t)

;; startup profiling
(use-package esup
  :ensure t)

;; enter pins in emacs
(use-package pinentry
  :disabled t
  :ensure t
  :config
  (pinentry-start))

;; personal info
(setq user-full-name "Alex Schmidt"
      user-mail-address "zyeryi@gmail.com")

;; better window resizing
(setq frame-resize-pixelwise t)

;; font setup
(cond ((member "Iosevka" (font-family-list))
       (set-frame-font "Iosevka-12")
       (set-face-attribute 'default nil :font "Iosevka-12")))

;; shorter confirmation prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; always load latest elisp files
(setq load-prefer-newer t)

;; display bell visually
(setq visible-bell t)

;; follow symlinks in version control
(setq vc-follow-symlinks t)

;; better scrolling
(setq scroll-conservatively 1001
      scroll-margin 0
      scroll-preserve-screen-position t)

;; handle whitespace sanely
(setq indent-tabs-mode nil
      require-file-newline t
      tab-always-indent t
      tab-width 4)

(setq echo-keystrokes 0.1)

;; delete moves to trash
(setq delete-by-moving-to-trash t)

;; weeks start on Monday
(setq calendar-week-start-day 1)

;; frame title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; don't prompt to load themes, just load them
(setq custom-safe-themes t)

;; line wrapping
(setq truncate-lines t
      truncate-partial-width-windows 50)

(setq save-interprogram-paste-before-kill t
      sentence-end-double-space nil
      compilation-scroll-output t)

;; configure relative line numbers on supported Emacs versions
(unless (version< emacs-version "26.1")
  (setq display-line-numbers-type 'relative))

;; compressed file support
(auto-compression-mode t)

;; seed the PRNG
(random t)

;; hide some startup stuff
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; useful constants
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

(defconst dotemacs/user-cache-directory
  (f-join dotemacs/user-emacs-directory "cache")
  "Directory for cache files.")

(setq abbrev-file-name
      (f-join dotemacs/user-cache-directory "abbrev.el"))

(setq auto-save-list-file-name
      `((".*" . ,(f-join dotemacs/user-cache-directory "autosave"))))

(setq pcache-directory
      (f-join dotemacs/user-cache-directory "pcache"))

(setq server-auth-dir
      (f-join dotemacs/user-cache-directory "server"))

(setq url-cache-directory
      (f-join dotemacs/user-cache-directory "url-cache"))

(setq url-configuration-directory
      (f-join dotemacs/user-cache-directory "url-config"))

(setq backup-directory-alist
      `((".*" . ,(f-join dotemacs/user-cache-directory "backups"))))

;; always use unicode
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; configure clipboard
(cond ((string-equal system-type "gnu/linux")
       (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
             select-enable-clipboard t
             select-enable-primary t)))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :init
  (setq auto-revert-verbose nil))

(use-package paren
  :hook (after-init . show-paren-mode)
  :init
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t))

(use-package smart-mode-line
  :ensure t
  :init
  (setq-default sml/theme 'dark
                sml/vc-mode-show-backend t)
  :config
  ;; display line and column numbers in the mode line
  (line-number-mode)
  (column-number-mode)

  ;; display battery in the mode line too
  (display-battery-mode)

  ;; activate smart-mode-line
  (sml/setup))

;; prevent the scratch buffer from being killed
(use-package unkillable-scratch
  :ensure t
  :hook (after-init . unkillable-scratch)
  :init
  (setq unkillable-scratch-behavior 'bury))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator "/"
        uniquify-ignore-buffers-re "^\\*"))

(use-package recentf
  :ensure t
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-save-file (f-join dotemacs/user-cache-directory "recentf")
        recentf-max-saved-items 300
        recentf-max-menu-items 0
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$")))

(use-package savehist
  :ensure t
  :config
  (setq savehist-file (f-join dotemacs/user-cache-directory "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1))

(use-package saveplace
  :ensure t
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file (f-join dotemacs/user-cache-directory "saveplace")
        save-place-forget-unreadable-files t))

(use-package bookmark
  :init
  (setq bookmark-default-file (f-join dotemacs/user-cache-directory "bookmarks")
        bookmark-save-flag t))

(use-package editorconfig
  :ensure t
  :hook (after-init . editorconfig-mode)
  :diminish)

(use-package anzu
  :ensure t
  :diminish
  :config
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode +1)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-neotree-config)

  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)

  (doom-themes-org-config))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :after (:all all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package neotree
  :ensure t
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
        neo-modern-sidebar t
    neo-theme (if window-system 'icons 'arrow))
    :bind (([f2] . neotree-toggle)))

(use-package ag
  :ensure t
  :init
  (setq ag-highlight-search t
        ag-reuse-window t
        ag-reuse-buffers t))

(use-package hydra
  :ensure t
  :load-path "site-lisp/hydra")

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :load-path "site-lisp/swiper"
  :hook ((after-init . ivy-mode))
  :init
  (setq projectile-completion-system 'ivy
    magit-completing-read-function 'ivy-completing-read
    ivy-count-format "(%d/%d) "
    ivy-use-virtual-buffers t
    ivy-height 10
    enable-recursive-minibuffers t)
  :bind (("C-c C-r" . ivy-resume)))

(use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
     ("C-c C-s" . counsel-ag)
     ("C-c k" . counsel-ag)
     ("M-x" . counsel-M-x)
     ("C-c j" . counsel-git-grep)
     ("C-x l" . counsel-locate)))

(use-package counsel-projectile
  :ensure t
  :hook ((after-init . counsel-projectile-mode)))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-copy-envs '("WORKON_HOME" "PROJECT_HOME"))
  (exec-path-from-shell-initialize))

(use-package python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq python-shell-completion-native-enable nil
        python-shell-interpreter "python"))

(use-package elpy
  :ensure t
  :init
  (setq-default elpy-rpc-python-command "python3")
  :config
  (elpy-enable))

(use-package dired
  :init
  (put 'dired-find-alternative-file 'disabled nil)
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t
        dired-listing-switches "-laGh1v --group-directories-first"))

(use-package dired-x
  :after dired)

(use-package dired-narrow
  :after (:all dired dired-x)
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package peep-dired
  :after (:all dired dired-x)
  :ensure t
  :defer t
  :init
  (setq peep-dired-cleanup-on-disable t)
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
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
          ("^\\*magit" :regexp t :size 0.5 :noesc t :autokill t))))

(use-package evil
  :ensure t
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
  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package evil-commentary
  :ensure t
  :diminish
  :after evil
  :config
  (evil-commentary-mode +1))

(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode))

(use-package evil-anzu
  :ensure t
  :after evil)

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-search-highlight-persist
  :ensure t
  :after evil
  :config
  (global-evil-search-highlight-persist t))

(use-package undo-tree
  :ensure t
  :diminish
  :init
  (setq undo-tree-history-directory-alist `((".*" . ,(f-join dotemacs/user-cache-directory "undo-tree-history")))
        undo-tree-auto-save-history t))

(use-package general
  :ensure t
  :init
  (setq general-default-keymaps 'evil-normal-state-map)
  :config
  (general-define-key "[ SPC" 'insert-newline-above
                      "] SPC" 'insert-newline-below))

(use-package move-text
  :ensure t
  :after general
  :config
  (general-define-key "[ e" 'move-text-up
                      "] e" 'move-text-down))
(use-package magit
  :ensure t
  :init
  (setq magit-section-show-children-count t
        magit-diff-arguments '("--histogram")
        magit-ediff-dwim-show-on-hunks t
    magit-repository-directories `(( ,(f-join (getenv "HOME") "src" "projects"))
                       ( ,(f-join (getenv "HOME") "src" "repos"))))
  :bind (("C-c g" . magit-status)))

(use-package with-editor
  :ensure t
  :after magit
  :hook ((shell-mode . with-editor-export-editor)
         (term-exec . with-editor-export-editor)
         (eshell-mode . with-editor-export-editor)))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :diminish
  :init
  (setq which-key-popup-type 'minibuffer
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5
        which-key-sort-uppercase-first nil
        which-key-sort-order #'which-key-prefix-then-key-order)
  :config
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom))

(use-package expand-region
  :ensure t
  :bind (("C-c C-+" . er/expand-region)
         ("C-c C--" . er/contract-region)))

(use-package projectile
  :ensure t
  :diminish
  :init
  (setq projectile-enable-caching t
        projectile-cache-file (f-join dotemacs/user-cache-directory "projectile-cache.el")
        projectile-known-projects-file (f-join dotemacs/user-cache-directory "projectile-projects.el"))
  :config
  (projectile-mode))

(use-package company
  :ensure t
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
  :ensure t
  :after company
  :config
  (setq company-statistics-file (f-join dotemacs/user-cache-directory "company-statistics-cache.el"))
  (add-hook 'after-init-hook #'company-statistics-mode))

(use-package org
  :ensure t
  :diminish org-indent-mode
  :config
  (setq org-directory (f-expand "~/Dropbox/org")
        org-default-notes-file "~/Dropbox/org/notes.org")
  
  (setq org-agenda-files
        '("~/Dropbox/org/agenda.org"
          "~/Dropbox/org/archive.org"
          "~/Dropbox/org/contacts.org"
          "~/Dropbox/org/misc.org"
          "~/Dropbox/org/notes.org"
          "~/Dropbox/org/refile.org"))

  (setq org-refile-targets
	'((org-agenda-files . (:maxlevel . 1))))

  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "SCHEDULED(s)" "ACTIVE(a)" "|" "DONE(d)" "CANCELED(c)")))

  ;; capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/org/agenda.org" "Tasks")
           "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)))

  (setq org-latex-listings 'minted
        org-latex-packages-alist
	'(("" "minted" t)
	  ("" "tikz" t)))

  (setq org-latex-pdf-process
        '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-preview-latex-default-process 'imagemagick
        org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (python . t)
     (sql . t)
     (sqlite . t)
     (ditaa . t)
     (dot . t)
     (gnuplot . t)
     (latex . t)
     (plantuml . t)))
  

  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)))

(use-package org-projectile
  :ensure t
  :disabled
  :init
  (setq org-projectile-projects-file
        (f-join dotemacs/user-dropbox-directory "org" "projects.org"))
  (push (org-projectile-project-todo-entry) org-capture-templates)

  (setq org-agenda-files
        (append org-agenda-files (org-projectile-todo-files)))

  :bind (("C-c n p" . org-projectile-project-todo-completing-read)))

(use-package org-super-agenda
  :ensure t
  :disabled
  :after org-mode
  :config
  (org-super-agenda-mode +1)

  (setq org-super-agenda-groups
        '((:order-multi (1 (:name "High priority"
                                  :priority> "C")))
          (:order-multi (1 (:name "Done today"
                                  :and (:regexp "State \"DONE\""
                                                :log t)))))))

(use-package elfeed
  :ensure t
  :commands elfeed
  :bind ("C-x w" . elfeed))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list (f-expand "~/Dropbox/org/elfeed.org")))
  :config
  (elfeed-org))

(use-package tramp
  :ensure t
  :init
  (setq-default tramp-auto-save-directory (f-join dotemacs/user-cache-directory "tramp-auto-save/")
                tramp-backup-directory-alist backup-directory-alist
                tramp-persistency-file-name (f-join dotemacs/user-cache-directory "tramp-persistency.el"))
  (setq tramp-default-method "ssh"
        tramp-use-ssh-controlmaster-options ""))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package plantuml-mode
  :ensure t
  :mode (("\\.plantuml\\'" . plantuml-mode)
         ("\\.puml\\'" . plantuml-mode))
  :init
  (setq plantuml-jar-path "~/bin/plantuml.jar"))

(use-package prodigy
  :ensure t
  :defer t
  :bind (([f12] . prodigy)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/private")
        yas-indent-line nil)
  :config
  (yas-reload-all)
  (yas-global-mode 1)

  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map))

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after yasnippet)

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
  :ensure t
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package company-auctex
  :ensure t
  :mode (("\\.tex\\'" . latex-mode))
  :after (:all company auctex)
  :hook ((LaTeX-mode . company-auctex-init)
         (TeX-mode . company-auctex-init)))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . haskell-indentation-mode)
         (haskell-mode . interactive-haskell-mode))
  :mode (("\\.hs\"" . haskell-mode))
  :commands haskell-mode
  :init
  (setq haskell-interactive-popup-errors nil
        haskell-ask-also-kill-buffers nil
        haskell-process-type 'auto))

(use-package hindent
  :ensure t
  :diminish hindent-mode
  :after haskell-mode
  :hook ((haskell-mode . hindent-mode)))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package ini-mode
  :ensure t
  :mode ("\\.ini\\'" . ini-mode))

(use-package toml-mode
  :ensure t
  :mode (("\\.toml\\'" . toml-mode)
         ("Pipfile" . toml-mode)))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode))

(use-package tern
  :ensure t
  :after js2-mode)

(use-package company-tern
  :ensure t
  :after (:all company tern)
  :hook (js2-mode . tern-mode)
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package web-mode
  :ensure t
  :hook (web-mode . emmet-mode)
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
        '(("jinja" . "\\.html\\'"))))

(use-package company-web
  :ensure t
  :after (:all company web-mode)
  :config
  (require 'company-web-html))

(use-package css-mode
  :ensure t
  :after web-mode
  :mode ("\\.css$" . css-mode)
  :config
  (setq css-indent-offset 4))

(use-package sass-mode
  :ensure t
  :after web-mode
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :ensure t
  :after web-mode
  :mode ("\\.scss\\'" . scss-mode))

(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode))
  :after web-mode
  :init
  (setq-default emmet-move-cursor-between-quotes t)
  :config
  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap))

(use-package vue-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (setq rust-format-on-save t))

(use-package macrostep
  :ensure t
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package systemd
  :ensure t)

(use-package nginx-mode
  :ensure t)

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
