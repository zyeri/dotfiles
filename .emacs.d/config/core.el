;; start the emacs server if it's not already running

(use-package server
  :ensure server
  :config (unless (server-running-p) (server-start)))

(use-package indent-guide
    :config (indent-guide-global-mode))

;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-setup-exit-hook)

;; strip trailing whitespace when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; scrolling settings
(setq scroll-margin 5
      scroll-conservatively 999
      scroll-step 1)

;; require a newline at the end of every file
(setq require-final-newline)

;; ignore case when searching
(setq-default case-fold-search t)

;; don't insert tabs
(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; don't make backup files
(setq-default make-backup-files nil)

(setq compilation-ask-about-save nil)

;; week starts on monday
(setq calendar-week-start-day 1)

;; spell checking
(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")
