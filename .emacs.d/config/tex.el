(use-package tex
  :ensure auctex
  :config
  (progn
    (setq TeX-save-query nil)
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq TeX-PDF-mode t)

    (setq-default TeX-master nil)
    (setq-default TeX-engine 'xetex)

    (setq TeX-source-correlate-mode t)
    (setq TeX-source-correlate-method 'synctex)

    (setq font-latex-fontify-sectioning 'color)
    (setq font-latex-script-display nil)

    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)))
