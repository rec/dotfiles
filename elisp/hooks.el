;; Comment this out to not change whitespace on save.
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(add-hook 'shell-mode-hook #'(lambda () (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode t)))

(add-hook 'python-mode-hook (lambda () (git-gutter-mode t)))

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook (lambda () (git-gutter-mode t)))

;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; (add-hook 'python-mode-hook #'electric-operator-mode)
;; (add-hook 'javascript-mode-hook #'electric-operator-mode)

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)
            ))


;; (add-hook 'c-mode-common-hook #'electric-operator-mode)

;; (defun my-python-mode-common-hook ()
;;   (setq python-basic-offset 4)
;; ;;  (define-key python-mode-map "\C-m" 'newline-and-indent)
;; )

;; (add-hook 'python-mode-common-hook 'my-python-mode-common-hook)
;;(add-hook 'c-mode-common-hook 'electric-pair-mode)
;;(add-hook 'python-mode-hook 'electric-pair-mode)
