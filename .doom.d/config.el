;;; .doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Alex Schmidt"
      user-mail-address "zyeryi@gmail.com"
      epa-file-encrypt-to user-mail-address)

;; disable automatic completion
(setq company-idle-delay nil)

;; disable lsp-ui-sideline stuff
(setq lsp-ui-sideline-enable nil
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-file-watchers nil)

;; disable mouse-overs for mode-line segments
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; disable confirmation on quit
(setq confirm-kill-emacs nil)

;; tell projectile where projects are stored
(setq projectile-project-search-path '("~/projects" "~/src/repos"))

;; location where Emacs can find its own source code
(setq source-directory "~/src/repos/emacs/")

;; prevent message about iedit during startup (annoying)
(setq iedit-toggle-key-default nil)

;;
;;; UI
(setq doom-theme 'doom-one)

;; line numbers are distracting if we don't need them
(setq display-line-numbers-type nil)

;; don't yell at us if the fonts aren't installed yet
(ignore-errors
  (setq doom-font (font-spec :family "Iosevka" :size 14)
        doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13)))

;;; Frames/Windows
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;
;;; Keybinds
(map! :leader
      (:prefix "f"
        "t" #'find-in-dotfiles
        "T" #'browse-dotfiles)

      ;; my arrow keys aren't working, so set up some temporary bindings to move
      ;; lines around until I can get them fixed/replaced
      "[ e" #'drag-stuff-up
      "] e" #'drag-stuff-down

      (:prefix "p"
        "/" #'+ivy/project-search)

      (:prefix "t"
        "m" #'toggle-frame-maximized))

(map! :gnvime "M-K" #'drag-stuff-up
      :gnvime "M-J" #'drag-stuff-down)

;;; :completion ivy
(setq +ivy-buffer-preview t)

;;; :editor evil
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; o/O keys shouldn't continue commented lines
(setq +evil-want-o/O-to-continue-comments nil)

;;; :tools magit
(setq magit-repository-directories '(("~/projects" . 1))
      magit-save-repository-buffers nil
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-commit "--gpg-sign=0x1C534F4138D5CCE4")
                         (magit-rebase "--autosquash" "--gpg-sign=0x1C534F4138D5CCE4")
                         (magit-pull "--rebase" "--gpg-sign=0x1C534F4138D5CCE4")))

;;; :lang org
(setq org-directory "~/org/"
      org-bullets-bullet-list '("*"))

(setq-default org-hide-leading-stars nil)

;; revert TAB behavior (cycle subtree visibility state recursively)
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

;;; :ui vc-gutter
(setq +vc-gutter-in-remote-files nil)

;;; :lang latex
(setq bibtex-completion-library-path (list "~/org/refs/pdfs"))

(setq +latex-bibtex-file "~/org/refs/references.bib"
      +latex-enable-unicode-math t)

(after! bibtex
  (setq bibtex-dialect 'biblatex))

(when IS-MAC
  (setq +latex-viewers '(skim pdf-tools)))
