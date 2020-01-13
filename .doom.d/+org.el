;;; ~/projects/dotfiles/.doom.d/+org.el -*- lexical-binding: t; -*-

(after! org
  (add-to-list 'org-modules 'org-habit t))

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(setq org-bullets-bullet-list '("*")
      org-refile-allow-creating-parent-nodes 'confirm)

(use-package! org-ref
  :after org
  :init
  (setq org-ref-bibliography-notes "~/org/bibliography/notes.org"
        org-ref-default-bibliography '("~/org/bibliography/references.bib")
        org-ref-pdf-directory "~/org/bibliography/pdfs")

  (when (featurep! :completion ivy)
    (setq org-ref-completion-library 'org-ref-ivy-cite)))
