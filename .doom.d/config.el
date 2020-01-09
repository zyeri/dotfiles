;;; .doom.d/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Alex Schmidt"
      user-mail-address "zyeryi@gmail.com"
      epa-file-encrypt-to user-mail-address)

;; auto refresh ibuffer
(add-hook! 'ibuffer-mode-hook #'ibuffer-auto-mode)

;; disable mouse-overs for mode-line segments
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; UI
(setq doom-theme 'doom-one)

;; hide line numbers
(setq display-line-numbers-type nil)

;; don't yell at us if the fonts aren't installed yet
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13))

;;; Frames/Windows
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; disable confirmation on quit
(setq confirm-kill-emacs nil)

;; tell projectile where projects are stored
(setq projectile-project-search-path '("~/projects" "~/src/repos"))

;; location where Emacs can find its own source code
(when (file-directory-p "~/src/repos/emacs")
  (setq source-directory "~/src/repos/emacs"))

;; prevent message about iedit during startup (annoying)
(setq iedit-toggle-key-default nil)

;;; :completion ivy
(setq +ivy-buffer-preview t)

;;; :editor evil
(after! evil
  ;; don't use 'jk' to escape insert mode
  (setq evil-escape-key-sequence nil))

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

;;; :ui vc-gutter
(setq +vc-gutter-in-remote-files nil)

;;; :lang latex
(setq bibtex-completion-library-path (list "~/org/bibliography/pdfs"))

(setq +latex-bibtex-file "~/org/bibliography/references.bib"
      +latex-enable-unicode-math t)

;;; :tools pdf
(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-width))

(load! "+org")
(load! "+bindings")
