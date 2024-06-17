;; Comment this out to not change whitespace on save.
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(add-hook 'shell-mode-hook #'(lambda () (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode t)))

(add-hook 'python-mode-hook (lambda () (git-gutter-mode t)))

(setq-default c-basic-offset 4)
(defun my-c-mode-common-hook ()
 ;; (c-set-offset 'substatement-open 0)
 (c-set-offset 'innamespace 0)

 (setq c++-tab-always-indent t)
 (setq c-basic-offset 4)
 (setq c-indent-level 4)
;; (git-gutter-mode t)
 )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; not used.
(defun my-c++-mode-hook ()
    (define-key c++-mode-map ")" 'self-insert-command)
    (define-key c++-mode-map "(" 'self-insert-command)
    (define-key c++-mode-map "[" 'self-insert-command)
    (define-key c++-mode-map "]" 'self-insert-command)
    (define-key c++-mode-map "{" 'self-insert-command)
    (define-key c++-mode-map "}" 'self-insert-command)
    (define-key c++-mode-map ":" 'self-insert-command)
    (define-key c++-mode-map ";" 'self-insert-command)
    (define-key c++-mode-map "," 'self-insert-command)
    )

;; (add-hook 'c++-mode-hook 'my-c++-mode-hook)


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

;; (add-hook 'python-mode-hook 'blacken-mode)
