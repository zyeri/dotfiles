(use-package haskell-mode
  :config
  (progn
    (setq-default haskell-stylish-on-save t)

    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))
