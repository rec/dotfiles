
;===================================== 
; installing all code from stef-elisp
;=====================================
;; last updated: February 9, 2008

(add-to-list 'load-path
             (file-name-directory (or load-file-name
                                      (buffer-file-name)))
             t)

(require 'stef-elisp-compatibility nil t)

(require 'embedded-elisp-library nil t) 
(require 'query-sheet nil t) 
(require 'comm-tests nil t)

;; ====== keykit-mode:

(autoload 'keykit-mode "keykit-mode" "KeyKit major mode." t)
(setq auto-mode-alist
      (append '(("\\.\\(k[pc]?\\|tyk\\|toy\\)$" . keykit-mode)) auto-mode-alist))
(add-hook 'keykit-mode-hook 'kk-untabify-on-saving)
;(add-hook 'keykit-mode-hook '(lambda () (embedded-elisp-mode 1)))

(autoload 'timidity-cfg-mode "timidity-cfg" 
  "major mode for editing Timidity configuration files" t)
(setq auto-mode-alist
      (append '(("\\.cfg$" . timidity-cfg-mode)) auto-mode-alist))

;; ======= csound-x:

(require 'org-mode nil t)

(let ((csound-x-directory 
       (or (and (locate-library "csound-x.el")
		(file-name-directory (locate-library "csound-x.el")))
           ;; in public distribution, csound-x is just below stef-elisp:
           (expand-file-name 
            "csound-x" 
            (file-name-directory (locate-library "stef-elisp.el"))))))
  (add-to-list 'load-path csound-x-directory)
  (when (require 'info nil t)
    (add-to-list 'Info-additional-directory-list csound-x-directory)))

(require 'csound-x)

;; ======= entry point for customization:

(defgroup spfa nil
  "SPFA code customization")

(defgroup spfa-paths nil
  "Executables, command lines and directories"
  :group 'spfa)

;=====================================
;; that's it
(provide 'stef-elisp)