;; Comment this out to not change whitespace on save.
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'eglot-managed-mode-hook
          (lambda () (eglot-inlay-hints-mode -1)))
;; (add-hook 'shell-mode-hook #'(lambda () (dirtrack-mode 1)))

(defconst swirly-shell-prompt-directory-regexp
  "^[^#$\n]* [^#$\n]*@[^:\n]+:\\(?:\033\\[[0-9;]*m\\)*\\([^#$\033\n]+\\)[$#]\\(?:\033\\[[0-9;]*m\\)* ")

(defun swirly-track-shell-directory (input)
  "Track shell directory from INPUT."
  (when (string-match swirly-shell-prompt-directory-regexp input)
    (let* ((directory (match-string 1 input))
           (expanded-directory (file-name-as-directory (expand-file-name directory)))
           (current-directory (file-name-as-directory (expand-file-name default-directory))))
      (when (and (not (string= directory ""))
                 (not (string= expanded-directory current-directory))
                 (file-accessible-directory-p expanded-directory))
        (shell-process-cd directory))))
  input)

(add-hook 'shell-mode-hook
          (lambda ()
            (shell-dirtrack-mode -1)
            (dirtrack-mode -1)
            (remove-hook 'comint-preoutput-filter-functions 'swirly-track-shell-directory t)
            (add-hook 'comint-preoutput-filter-functions 'swirly-track-shell-directory nil t)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode t)))
;; (add-hook 'python-mode-hook (lambda () (git-gutter-mode t)))

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

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)
            ))
