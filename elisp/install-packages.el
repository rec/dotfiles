;; (package-install 'cython-mode)
(package-install 'dash)
(package-install 'dash-functional)
;;(package-install 'git-gutter)
(package-install 'google-c-style)
(package-install 'markdown-mode)
(package-install 's)
(package-install 'saveplace)
(package-install 'uniquify)
(package-install 'yaml-mode)

(use-package clipetty
  :hook (after-init . global-clipetty-mode))

(use-package codex-ide
  :vc (:url "https://github.com/dgillis/emacs-codex-ide" :rev :newest)
  :bind ("C-c C-;" . codex-ide-menu))
