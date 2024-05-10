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
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(kill-ring-max 1024)
 '(large-file-warning-threshold nil)
 '(mark-ring-max 256)
 '(py-indent-offset 4)
 '(python-indent 4)
 '(python-indent-offset 4)
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(shell-cd-regexp "\\(f\\|cd\\)")
 '(standard-indent 4)
 '(tab-width 4)
 '(template-auto-insert t)
 '(template-subdirectories (quote ("./" "Templates/" "~/.emacs.d/Templates")))
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(visual-line-mode nil t)
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
