(use-package pandoc-mode
  :init
  (progn
    (add-hook 'markdown-mode-hook 'pandoc-mode)
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
    ))
