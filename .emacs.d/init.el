;; init.el
;; Yeah, my emacs config is a mess from me experimenting the operating
;; sys-- I mean editor.  Please ignore the horrible elisp I wrote (still
;; learning) and it's also probably safe to assume nothing works. I'll
;; improve this eventually and rewrite it all so that it makes sense.

;; disable a few annoying things before initializing
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq inhibit-default-init t
      inhibit-startup-screen t
      ring-bell-function #'ignore)

;; packages
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
     '("melpa" . "http://melpa.org/packages/")
     '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(setq load-prefer-newer t)

(mapc 'load (file-expand-wildcards "~/.emacs.d/config/*.el"))

(diminish 'indent-guide-mode "")
