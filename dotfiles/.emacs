;; (package-initialize)

(if (string-equal system-type "darwin")
    (setq code-root "/code")
    (setq code-root (expand-file-name "~/git"))
    )

(setq elisp-path (concat code-root "/dotfiles/elisp"))
(setq elisp-library-path (concat elisp-path "/libraries"))
(setq load-path (cons elisp-path load-path))
(setq load-path (cons elisp-library-path load-path))

(load-library "startup")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
