(setq elisp-path (expand-file-name "~/git/dotfiles/elisp"))

(setq elisp-library-path (concat elisp-path "/libraries"))
(setq load-path (cons elisp-path load-path))
(setq load-path (cons elisp-library-path load-path))

(load-library "startup")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-always-kill t)
 '(compile-command "NO_COLOR=1 d run-tests")
 '(cursor-type (quote box))
 '(etags-table-search-up-depth 10)
 '(fill-column 88)
 '(global-mark-ring-max 256)
 '(global-whitespace-mode t)
 '(grep-command "egrep --exclude-dir={build,htmlcov} -nHIR * --include \\*.py -e ")
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
