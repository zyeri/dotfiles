(use-package magit
  :bind ("C-x g" . magit-status))

(use-package gist
  :after magit
  :config (global-git-gutter-mode t)
  :diminish git-gutter-mode)
