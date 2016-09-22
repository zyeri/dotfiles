(use-package ido
  :config
  (progn
    (setq ido-enable-prefix nil)
    (setq ido-use-virtual-buffers t)
    (setq ido-use-url-at-point t)
    (setq ido-max-prospects 10)
    (setq ido-create-new-buffer 'always)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-save-directory-list-file "~/.emacs.d/ido.last")

    (ido-mode t)
    (ido-everywhere t)))

(use-package ido-ubiquitous
  :after ido
  :config (ido-ubiquitous-mode t))

(use-package flx-ido
  :after ido
  :config
  (progn
    (setq ido-enable-flex-matching t)
    (flx-ido-mode t)))

(use-package ido-vertical-mode
  :after ido
  :config
  (progn
    (ido-vertical-mode)
    (setq ido-vertical-show-count t)))

(use-package smex
  :init
  (progn
    (smex-initialize)

    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
