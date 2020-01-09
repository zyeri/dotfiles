;;; .doom.d/packages.el -*- no-byte-compile: t; -*-

(package! org-ref)
(package! org-ql)
(package! org-sidebar)
(package! org-super-agenda)

(package! elfeed-web)
(package! elfeed-goodies)

(package! mpdel :ignore)
(when (featurep! :completion ivy)
  (package! ivy-mpdel :ignore))
