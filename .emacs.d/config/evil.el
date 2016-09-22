(use-package evil
  :config
  (progn

    (setq evil-default-cursor t)
    (setq evil-want-fine-undo t)

    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)

    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

    (setq-default evil-insert-state-cursor 'bar)

    ;; activate evil-mode
    (evil-mode t)))

(use-package evil-leader
  :after evil
  :config
  (progn
    (setq evil-leader/in-all-states t)

    (evil-leader/set-leader ",")
    (evil-leader/set-key
     "T" 'eshell
     "k" 'kill-buffer
     "K" 'kill-this-buffer
     "b" 'ido-switch-buffer
     "w" 'save-buffer
     "o" 'helm-occur)

    (global-evil-leader-mode)))

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode t))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode t))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-mode))

(use-package evil-snipe
  :after evil
  :diminish evil-snipe-mode
  :diminish evil-snipe-local-mode
  :config (evil-snipe-mode t))
