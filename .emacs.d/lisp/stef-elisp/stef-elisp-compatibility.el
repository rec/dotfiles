
;;; ensure compatibility from Emacs 21.1 upward
;;; (tested with 22.0.99.1)


(require 'cl)

;; giving 'split-string its emacs 22 semantics and arglist
(condition-case nil
    (split-string "abc" "a" t)  ; if no error, nothing to do
  (error
   (unless (fboundp 'some-name-for-old-split-string)
     (fset 'some-name-for-old-split-string (symbol-function 'split-string))
     (defun split-string (str &optional sep bool)
       (delete-if (lambda (s) (string= s ""))
		  (some-name-for-old-split-string str sep))))))

;; 'region-exists-p is not defined in later emacsen
(unless (fboundp 'region-exists-p)
  (defun region-exists-p ()
    (condition-case nil
        (and (region-end) (region-beginning))
      (error nil))))

;; atomic-change-group does not exists before version 22
(unless (fboundp 'atomic-change-group)
  (defalias 'atomic-change-group 'progn))

;; 'toggle-truncate-lines takes no argument in emacs 21.2
;; ... see in csound-mx.el


(provide 'stef-elisp-compatibility)

