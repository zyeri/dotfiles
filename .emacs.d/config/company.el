(use-package company
  :demand
  :commands company-mode
  :init (global-company-mode)
  :config
  (progn

    (setq company-show-numbers t)

    (setq company-idle-delay 0.2)

    (setq company-tooltip-limit 20)

    (setq company-require-match nil)

    (setq company-minimum-prefix-length 1)

    (setq company-tooltip-align-annotations t)

    (setq company-tooltip-flip-when-above t)

    (setq company-global-modes '(not term-mode)))

  :diminish company-mode)

(use-package company-quickhelp
  :after company
  :config
  (progn
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode)))

(use-package company-statistics
  :after company
  :config (company-statistics-mode))

(use-package company-math
  :after company
  :config
  (progn
    (add-to-list 'company-backends 'company-math-symbols-unicode)
    (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package company-auctex
  :after company
  :config (company-auctex-init))
