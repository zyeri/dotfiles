(use-package flycheck
  :diminish flycheck-mode
  :init
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (setq flycheck-check-syntax-automatically '(save mode-enabled))

    (global-flycheck-mode)))
