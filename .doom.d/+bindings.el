;;; ~/projects/dotfiles/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;;; bindings
(map! :gnvime "M-K" #'drag-stuff-up
      :gnvime "M-J" #'drag-stuff-down

      :leader
      ;; alternative bindings
      "[ e" #'drag-stuff-up
      "] e" #'drag-stuff-down

      (:prefix "f"
        "t" #'find-in-dotfiles
        "T" #'browse-dotfiles)

      (:prefix "t"
        "m" #'toggle-frame-maximized)

      (:when (featurep! :completion ivy)
        (:prefix "p"
          "/" #'+ivy/project-search))

      (:when (featurep! :completion helm)
        (:prefix "p"
          "/" #'+helm/project-search)))
