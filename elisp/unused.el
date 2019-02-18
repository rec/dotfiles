;; (setq dired-omit-files
;;       (rx (or
;;            (seq bol (? ".") "#")         ;; emacs autosave files
;;            (seq "~" eol)                 ;; backup-files
;;            (seq ".pyc" eol)              ;; compiled python files
;;            (seq ".pyo" eol)              ;; compiled python files
;;            (seq bol "CVS" eol)           ;; CVS dirs
;; )))

;; (setq dev-project  (or (getenv "EMACS_PROJECT") "fbme"))

;; (setq-default
;;    desktop-dirname (expand-file-name (concat "/code/dotfiles/elisp/desktop/" dev-project))
;;    desktop-path    (list desktop-dirname)
;;    save-place-file (concat desktop-dirname "/saved-places")
;;    dev-root        (concat "/code/" dev-project)
;;    )

;; (if (not (file-readable-p desktop-dirname))
;;     (make-directory desktop-dirname))

;; (setq-default fringe-color
;;       (cond
;;        ;; red
;;        ((string= dev-project "fbme")
;;         '(fringe ((t (:background "#FFFFFF")))))
;;        ;; orange
;;        ((string= dev-project "fbme2")
;;         '(fringe ((t (:background "#FFC590")))))
;;        ;; yellow
;;        ((string= dev-project "fbme3")
;;         '(fringe ((t (:background "#FFFFA0")))))
;;        ;; green
;;        ((string= dev-project "fbme4")
;;         '(fringe ((t (:background "#D0FFD0")))))
;;        ;; Blue
;;        ((string= dev-project "fbme5")
;;         '(fringe ((t (:background "#D8D8FF")))))
;;        ;; violet
;;        ((string= dev-project "fbme6")
;;         '(fringe ((t (:background "#DF8FFF")))))
;;        ;; grey
;;        ((string= dev-project "grit")
;;         '(fringe ((t (:background "#FFF"))))))
;; )


;; (setq tags-file (concat dev-root "/TAGS"))
;; (if (file-readable-p tags-file)
;;     (visit-tags-table tags-file))

;; (tags-query-replace "\"strict\"" "jss::strict" nil)
