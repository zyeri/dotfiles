(use-package helm
  :config
  (progn
    (require 'helm)
    (require 'helm-config)

    (setq helm-candidate-number-limit 100)

    (setq helm-autoresize-min-height 10)
    (setq helm-autoresize-min-height 20)
    (helm-autoresize-mode 1)))

(use-package helm-descbinds
  :after helm
  :config (helm-descbinds-mode))

(use-package helm-flx
  :after helm
  :config (helm-flx-mode t))

(use-package helm-fuzzier
  :after helm
  :config (helm-fuzzier-mode t))

(use-package helm-ag
  :after helm
  :config (setq helm-ag-fuzzy-match t))

(use-package helm-swoop
  :after helm
  :config
  (progn
    (setq helm-swoop-pre-input-function #'ignore)
    (setq helm-swoop-use-line-number-face t)
    (setq helm-swoop-split-with-multiple-windows t)
    (setq helm-swoop-speed-or-color t)
    (setq helm-swoop-use-fuzzy-match t)))
