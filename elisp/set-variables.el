;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))

(eval-after-load "markdown-mode"
  '(progn
    (define-key markdown-mode-map (kbd "M-<left>") nil)
    (define-key markdown-mode-map (kbd "M-<right>") nil)
    (define-key markdown-mode-map (kbd "M-S-<right>") nil)
    (define-key markdown-mode-map (kbd "M-S-<left>") nil)))

(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("\\.(inl|proto)\\'"   . c-mode))
(add-to-list 'auto-mode-alist '("\\.(js|json)\\'"   . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.grep\\'" . grep-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; http://www.emacswiki.org/emacs/AnsiColor
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 85))

;; (global-auto-revert-mode t)
;; (global-font-lock-mode t)
;; (global-whitespace-mode t)

(put 'dired-find-alternate-file 'disabled nil)
(setq
  auto-revert-use-notify nil
  auto-revert-interval 20
  backup-by-copying-when-linked t   ; handle links correctly
  backup-by-copying-when-mismatch t
  backup-directory-alist (list (cons "." backup-dir))
  delete-auto-save-files t          ; leave no "#" files in home directory
  delete-old-versions t             ; delete excess backups silently
  dired-recursive-copies t
  grep-use-null-device nil
  kept-new-versions 10              ; keep 10 backups plus 2 oldest backups
  swirly-default-prefix ""
  uniquify-buffer-name-style 'post-forward-angle-brackets
  version-control t                 ; make backup versions unconditionally
  whitespace-style '(face empty tabs lines-tail trailing)
  zoom-font-frame-local-flag nil
  dired-omit-files (concat dired-omit-files "\\|^.DS_Store$\\|.pyc$")
  shell-file-name "bash"
  shell-command-switch "-ic"
)

(setq-default
 dired-listing-switches "-alhv"
 line-spacing 3
 dired-omit-files-p t
)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
