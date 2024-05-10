;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________

(package-initialize)

(if (string-equal system-type "darwin")
    (setq code-root "/code")
    (setq code-root (expand-file-name "~/git"))
    )

(setq elisp-path (concat code-root "/dotfiles/elisp"))
(message elisp-path)

;;(setq load-path (cons (concat code-root "/dotfiles/elisp") load-path))
;;(setq load-path (cons (concat code-root "/dotfiles/elisp/libraries") load-path))

;;(setq load-path (cons (expand-file-name "/code/dotfiles/elisp") load-path))
;;(setq load-path (cons (expand-file-name "/code/dotfiles/elisp/libraries") load-path))

(setq load-path (cons elisp-path load-path))
(setq load-path (cons "/code/dotfiles/elisp/libraries" load-path))

(load-library "startup")
