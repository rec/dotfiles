;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; (setq elisp-path (expand-file-name "~/code/dotfiles/elisp"))

;; (setq elisp-library-path (concat elisp-path "/libraries"))
;; (setq load-path (cons elisp-path load-path))
;; (setq load-path (cons elisp-library-path load-path))

(setq code-root (expand-file-name "~/code"))
(setq elisp-path (concat code-root "/dotfiles/elisp"))
(setq melpa-path (concat code-root "/dotfiles/elisp"))
(setq elisp-library-path (concat elisp-path "/libraries"))
(setq load-path (cons elisp-path load-path))
(setq load-path (cons elisp-library-path load-path))

(load-library "startup")
(xterm-mouse-mode +1)

;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :machine "server")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(compile-command "NO_COLOR=1 d run-tests")
 '(cursor-type 'box)
 '(delete-selection-mode t)
 '(dirtrack-list '("^.*@.*:\\(.*\\)\\$" 1))
 '(etags-table-search-up-depth 10)
 '(explicit-shell-file-name "/bin/bash")
 '(fill-column 88)
 '(global-mark-ring-max 256)
 '(global-whitespace-mode nil)
 '(grep-command
   "egrep -nHIR * --include \\*.py --include \\*.pyi --include \\*.pyi.in --include \\*.h --include \\*.cpp  --include \\*.c  --include \\*.cu --include \\*.yaml --include \\*.yml --exclude-dir torch/include --exclude-dir third_party --exclude-dir=build -we ")
 '(grep-find-command
   '("find . -type f -exec egrep --exclude-dir={build,htmlcov} -nHIR * --include \\*.py -e  \\{\\} +"
     . 85))
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
 '(magit-auto-revert-mode nil)
 '(mark-ring-max 256)
 '(package-selected-packages
   '(blacken counsel cython-mode dash-at-point dash-functional electric-operator git-gutter
             git-gutter+ golden-ratio-scroll-screen google-c-style live-preview
             magit-filenotify markdown-mode nhexl-mode protobuf-mode pylint python-mode
             realgud s saveplace shell-here template yaml-mode))
 '(py-indent-offset 4)
 '(python-indent 4)
 '(python-indent-offset 4)
 '(remote-shell-program "/bin/bash")
 '(require-final-newline t)
 '(safe-local-variable-values '((encoding . utf-8)))
 '(save-place-mode t)
 '(shell-cd-regexp "\\(f\\|cd\\)")
 '(split-height-threshold 0)
 '(standard-indent 4)
 '(tab-width 4)
 '(template-auto-insert t)
 '(template-subdirectories '("./" "Templates/" "~/.emacs.d/Templates"))
 '(tool-bar-mode nil)
 '(visible-bell nil)
 '(visual-line-mode nil t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "nil" :slant normal :weight medium :height 130 :width normal))))
 '(custom-variable-tag ((t (:foreground "light blue3" :weight bold))))
 '(diff-added ((t (:inherit diff-changed :extend t :background "#bbffbb"))))
 '(diff-file-header ((t (:extend t :background "grey80" :weight bold))))
 '(diff-header ((t (:extend t :background "grey80"))))
 '(diff-mode-default ((t (:inherit autoface-default :height 120 :family "Menlo"))) t)
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "LightGreen"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "LightPink"))))
 '(diff-removed ((t (:inherit diff-changed :extend t :background "Pink"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "dark gray"))))
 '(font-lock-comment-face ((t (:foreground "chartreuse4"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine3"))))
 '(font-lock-function-name-face ((t (:foreground "turquoise4"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan3"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "color-106"))))
 '(font-lock-string-face ((t (:foreground "SlateBlue2"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen4"))))
 '(font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
 '(link ((t (:foreground "cyan3" :underline t))))
 '(whitespace-empty ((t (:background "white smoke" :foreground "firebrick"))))
 '(whitespace-line ((t (:background "gray93" :foreground "black"))))
 '(whitespace-tab ((t (:background "White" :foreground "lightgray"))))
 '(whitespace-trailing ((t (:background "gray97" :foreground "black" :weight bold)))))
