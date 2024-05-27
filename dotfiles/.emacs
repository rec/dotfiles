(setq elisp-path (expand-file-name "~/code/dotfiles/elisp"))

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
 '(cursor-type 'box)
 '(etags-table-search-up-depth 10)
 '(fill-column 88)
 '(global-mark-ring-max 256)
 '(global-whitespace-mode t)
 '(grep-command
   "egrep --exclude-dir={build,htmlcov} -nHIR * --include \\*.py --include \\*.h --include \\&*.cpp -e ")
 '(grep-find-command
   '("find . -type f -exec egrep --exclude-dir={build,htmlcov} -nHIR * --include \\*.py -e  \\{\\} +" . 85))
 '(grep-find-template
   "find <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
 '(grep-find-use-xargs 'exec-plus)
 '(grep-highlight-matches 'auto)
 '(grep-template "grep <X> <C> -nH --null -e <R> <F>")
 '(grep-use-null-device nil)
 '(grep-use-null-filename-separator t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(kill-ring-max 1024)
 '(large-file-warning-threshold nil)
 '(mark-ring-max 256)
 '(py-indent-offset 4)
 '(python-indent 4)
 '(python-indent-offset 4)
 '(require-final-newline t)
 '(safe-local-variable-values '((encoding . utf-8)))
 '(shell-cd-regexp "\\(f\\|cd\\)")
 '(standard-indent 4)
 '(tab-width 4)
 '(template-auto-insert t)
 '(template-subdirectories '("./" "Templates/" "~/.emacs.d/Templates"))
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(visual-line-mode nil t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-variable-tag ((t (:foreground "light blue3" :weight bold))))
 '(diff-added ((t (:inherit diff-changed :extend t :background "color-193"))))
 '(diff-file-header ((t (:extend t :background "grey112" :weight bold))))
 '(diff-header ((t (:extend t :background "grey80"))))
 '(diff-mode-default ((t (:inherit autoface-default :height 120 :family "Menlo"))) t)
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "color-217"))))
 '(diff-removed ((t (:inherit diff-changed :extend t :background "color-224"))))
 '(font-lock-comment-face ((t (:foreground "color-172"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine3"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue3"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan3"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "color-106"))))
 '(font-lock-string-face ((t (:foreground "color-100"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen4"))))
 '(link ((t (:foreground "cyan3" :underline t))))
 '(whitespace-empty ((t (:background "white smoke" :foreground "firebrick"))))
 '(whitespace-line ((t (:background "gray93" :foreground "black"))))
 '(whitespace-tab ((t (:background "White" :foreground "lightgray"))))
 '(whitespace-trailing ((t (:background "gray97" :foreground "black" :weight bold)))))
