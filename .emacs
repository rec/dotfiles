(server-start)

(setq load-path (cons (expand-file-name "~/.emacs.d/lisp") load-path))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(compile-command "cd /development/rippled && source ~/.bashrc && scons -j7 && build/rippled --unittest && npm test")
 '(cursor-type (quote box) t)
 '(dirtrack-list (quote ("^[^:]*:\\(.*\\)\\$" 1)))
 '(etags-table-search-up-depth 10)
 '(fill-column 80)
 '(global-mark-ring-max 256)
 '(global-whitespace-mode t)
 '(grep-command "grep --exclude \\*.pyc -nHR * -e ")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(java-indent 4)
 '(js-indent-level 4)
 '(kill-ring-max 1024)
 '(large-file-warning-threshold nil)
 '(mark-ring-max 256)
 '(ns-antialias-text nil)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(py-indent-offset 4)
 '(python-indent 4)
 '(python-indent-offset 4)
 '(shell-cd-regexp "\\(f\\|cd\\)")
 '(standard-indent 4)
 '(tab-width 4)
 '(template-auto-insert t)
 '(template-subdirectories (quote ("./" "Templates/" "~/.emacs.d/Templates")))
 '(tool-bar-mode nil)
 '(visual-line-mode nil t))

(setq-default line-spacing 3)

(require 'dired-x)
(require 'git-gutter)
(require 'google-c-style)
(require 'guess-style)
(require 'ido)
(require 'protobuf-mode)
(require 'saveplace)
(require 'template)
(require 'uniquify)
(require 'yaml-mode)

(require 'whitespace)
 (setq whitespace-style '(face empty tabs lines-tail trailing))
 (global-whitespace-mode t)

(setq grep-use-null-device nil)

(setq-default dired-listing-switches "-alhv")
(setq dired-recursive-copies 'always)

(add-hook 'shell-mode-hook
          #'(lambda ()
              (dirtrack-mode 1)))


;; (require 'sure-tags)
(require 'etags-table)
(setq etags-table-alist
      (list
       '("/development/rippled" "/development/rippled/TAGS")
       '("/development/rippled2" "/development/rippled2/TAGS")
       '("/development/rippled3" "/development/rippled3/TAGS")
       '("/development/rippled4" "/development/rippled4/TAGS")
       '("/development/rippled5" "/development/rippled5/TAGS")
       '("/development/rippled6" "/development/rippled6/TAGS")
       '("/development/rippled7" "/development/rippled7/TAGS")
       '("/development/rippled8" "/development/rippled8/TAGS")
       '("/development/rippled9" "/development/rippled9/TAGS")
       ))
(setq split-height-threshold 0)
(setq compilation-window-height 20)

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file)
                        (parent-directory possible-file)) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))

(defun ffu ()
     (interactive)
     (message (find-file-upwards ".git"))
)

(defun cd-compile()
  "Run compile in a specific directory.
If cd-compile-directory is set then compile will be run in that directory,
otherwise the user will be prompted to enter a directory."
  (interactive)
  (let ((default-directory (find-file-upwards ".git")))
    (call-interactively 'compile)))

;; (setq dired-omit-files
;;       (rx (or
;;            (seq bol (? ".") "#")         ;; emacs autosave files
;;            (seq "~" eol)                 ;; backup-files
;;            (seq ".pyc" eol)              ;; compiled python files
;;            (seq ".pyo" eol)              ;; compiled python files
;;            (seq bol "CVS" eol)           ;; CVS dirs
;; )))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode t)
            ))

(add-hook 'python-mode-hook
          (lambda ()
            (git-gutter-mode t)
            ))

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)
            ))

(add-hook 'write-file-hooks
          'delete-trailing-whitespace
)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; (defun my-python-mode-common-hook ()
;;   (setq python-basic-offset 4)
;; ;;  (define-key python-mode-map "\C-m" 'newline-and-indent)
;; )

;; (add-hook 'python-mode-common-hook 'my-python-mode-common-hook)

(global-font-lock-mode t)
(column-number-mode t)
(auto-compression-mode t)
(desktop-save-mode t)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-to-list 'auto-mode-alist '("\\.(inl|proto)\\'"   . c-mode))
(add-to-list 'auto-mode-alist '("\\.(js|json)\\'"   . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))

(template-initialize)
(blink-cursor-mode nil)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq zoom-font-frame-local-flag nil)

(setq backup-by-copying-when-mismatch t)

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))


;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))

(setq backup-directory-alist (list (cons "." backup-dir)))

;;;     Auto save and backup file configuration.

;;; newer backup code....
(setq backup-by-copying-when-linked t   ; handle links correctly
     delete-auto-save-files t          ; leave no "#" files in home directory
     kept-new-versions 10              ; keep 10 backups plus 2 oldest backups
     delete-old-versions t             ; delete excess backups silently
     version-control t)                ; make backup versions unconditionally

(defun normal-backup-enable-predicate (name)
 "Return T so that auto save files are always created, no matter where
the original file is stored."
 t)

;;  Make all auto-save files go into my garbage directory.  When root or others
;;  load my .emacs file, $_GARBAGE will not be set, so auto-save files will be
;;  put in /tmp.
(defvar auto-save-file-prefix
 (cond ((equal system-type 'windows-nt)
        (expand-file-name (concat (getenv "TMPDIR") "/")))
       ;; Root may not be able to write to the current user's backup
       ;; directory, since it may be NFS mounted from another host.
       ((equal (user-real-login-name) "root") "/tmp/")
       ((getenv "_GARBAGE") (concat (getenv "_GARBAGE") "/"))
       (t "/tmp/"))
     "*Prefix to prepend to all auto-save filenames.")

(defun make-auto-save-file-name ()
 "Return file name to use for auto-saves of current buffer."
 (concat auto-save-file-prefix
         "#"
         (if buffer-file-name
             (file-name-nondirectory buffer-file-name)
           (concat "%" (buffer-name)))
         "#"))

(defun auto-save-file-name-p (filename)
 "Return t if FILENAME can be yielded by make-auto-save-file-name.
FILENAME should lack slashes."
 (string-match (concat "^" auto-save-file-prefix "#.*#$") filename))

(defadvice switch-to-buffer (before existing-buffers-only activate)
  "When called interactively, switch to existing buffers only except when called with a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg)))))

(defadvice find-file (before existing-files-only activate)
  "When called interactively, only open existing files except when called with a prefix argument."
  (interactive
   (list (read-file-name "Find file: "
                         default-directory
                         nil
                         (null current-prefix-arg)))))

(put 'dired-find-alternate-file 'disabled nil)

(defun cycle-windows (&optional reverse)
   "Cycle the windows' buffers. If given a prefix argument, cycle in reverse."
   (interactive "P")
   (dolist (window (butlast (if reverse (reverse (window-list)) (window-list))))
     (let ((next-window-buffer (window-buffer (next-window window 0))))
       (set-window-buffer (next-window window 0) (window-buffer window))
       (set-window-buffer window next-window-buffer))))

(defun mapcar-head (fn-head fn-rest list)
      "Like MAPCAR, but applies a different function to the first element."
      (if list
          (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun camelize-method (s)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun camelcase   (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore  (s) (mapconcat 'upcase     (split-name s) "_"))
;; (defun underscored (s) (mapconcat 'downcase   (split-name s) "_"))
;; (defun dasherize   (s) (mapconcat 'downcase   (split-name s) "-"))
;; (defun colonize    (s) (mapconcat 'capitalize (split-name s) "::"))
(defun locamel     (s)
  (mapconcat 'identity
             (mapcar-head 'downcase 'capitalize (split-name s))
             ""))

(defun camelscore (s)
  (cond ;; ((string-match-p "\\(?:[a-z]+_\\)+[a-z]+" s)	(dasherize  s))
        ((string-match-p "\\(?:[A-Z]+_\\)+[A-Z]+" s)	(locamel  s))
        ((string-match-p "\\(?:[A-Z][a-z]+\\)+$"  s)	(underscore  s)) ;; (locamel   s))
        (t						(camelcase s)) ))

(defun camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]_") (point)))
         (end (and (skip-chars-forward  "[:alnum:]_") (point)))
         (txt (buffer-substring beg end))
         (cml (camelscore txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

(defun rotate-file-suffix (file)
  "Returns one rotation through the file"
  (let* ((patterns
        '(
          ("_test\\.cpp" ".pyx")
          ("_test\\.cpp" ".h")
          ("\\.h" ".cpp")
          ("\\.cpp" ".proto")
          ("\\.proto" "_test.cpp")
          ("\\.cc" ".h")
          ("\\.cpp" "_test.cpp")
          ("\\.cpp" ".pyx")
          ("\\.pyx" ".h")
          ("_test\\.py" ".py")
          ("\\.py" "_test.py")
          ))
         (working t))
  (while (and patterns working)
    (setq pattern (pop patterns))
    (if (string-match (car pattern) file)
        (progn
          (setq file
                (replace-regexp-in-string (car pattern)
                                          (cadr pattern)
                                          file))
          (setq working nil)))))
  file)

(defun try-file-directories (file)
  "Tries different possibilities to see if a file exists."
  (let* ((
        '(
          ("_test\\.cpp" ".pyx")
          ("_test\\.cpp" ".h")
          ("\\.h" ".cpp")
          ("\\.cpp" ".proto")
          ("\\.proto" "_test.cpp")`
          ("\\.cc" ".h")
          ("\\.cpp" "_test.cpp")
          ("\\.cpp" ".pyx")
          ("\\.pyx" ".h")
          ("_test\\.py" ".py")
          ("\\.py" "_test.py")
          ))
         (working t))
  (while (and patterns working)
    (setq pattern (pop patterns))
    (if (string-match (car pattern) file)
        (progn
          (setq file
                (replace-regexp-in-string (car pattern)
                                          (cadr pattern)
                                          file))
          (setq working nil)))))
  file)


(defun rotate-tests ()
  "Rotate between a file and its test file."
  (interactive)
  (let* ((filename (buffer-file-name))
         (new-file filename)
         (working2 t)
         (found nil))
    (while working2
      (setq old-file new-file)
      (setq new-file (rotate-file-suffix new-file))
      (if (or (string= new-file filename)
              (string= new-file old-file))
          (progn (setq working2 nil))  ;; We're done!
        (progn
          (if (file-readable-p new-file)
              (progn (find-file new-file) (setq working2 nil))))))))

(defun to-two()
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
)

(defun do-list-buffers()
  (interactive)
  (list-buffers)
  (other-window '1)
)

(defun do-two-list-buffers()
  (interactive)
  (to-two)
  (do-list-buffers)
)

(defun rotate-in-two ()
  "Kill all the other windows, split this one, rotate the other window, come back here."
  (interactive)
  (to-two)
  (other-window 1)
  (rotate-tests)
  (other-window 1)
)

(defun default-dired ()
  (interactive)
  (dired default-directory)
)

(defun default-dired-two ()
  (interactive)
  (to-two)
  (default-dired)
)

(defun home-dired ()
  (interactive)
  (dired "/Users/tom/Documents/development/rec/src/rec")
)

(defun home-dired-two ()
  (interactive)
  (to-two)
  (other-window 1)
  (home-dired)
)

(defun grep-root ()
  (interactive)
  (home-dired-two)
  (grep)
)

(defun reload-file()
  (interactive)
  (find-alternate-file (buffer-file-name))
)

(defun shell-two()
  (interactive)
  (to-two)
  (shell)
)

(defun switch-to-buffer-two()
  (interactive)
  (to-two)
  (switch-to-buffer)
)

(defun find-file-two()
  (interactive)
  (to-two)
  (find-file)
)

(global-set-key [f1] 'next-error)
(global-set-key [s-f1] 'goto-line)

(global-set-key [f2] 'rotate-tests)
(global-set-key [s-M-up] 'rotate-tests)
(global-set-key [s-f2] 'rotate-windows)

(global-set-key [f3] 'do-list-buffers)

(global-set-key [f4] 'switch-to-buffer)
(global-set-key [s-f4] 'shell)

(global-set-key [f5] 'find-file)
(global-set-key [s-f5] 'reload-file)

(global-set-key [f7] 'cd-compile)

(global-set-key [s-f6] 'home-dired)
(global-set-key [f6] 'default-dired)

(global-set-key [s-f8] 'grep)
;;(global-set-key [s-f8] 'grep-root)

(defun to-grep() (interactive) (switch-to-buffer "*grep*"))
(global-set-key [f8] 'to-grep)

(global-set-key [s-f9] 'kmacro-call-macro)
(global-set-key [f9] 'enlarge-window)

(global-set-key [f10] 'query-replace)
(global-set-key [s-f10] 'tags-query-replace)

(global-set-key [s-f11] 'yank-pop)
(global-set-key [f11] 'yank)

(global-set-key [s-f12] 'append-next-kill)
(global-set-key [f12] 'kill-line)

(global-set-key [f13] 'delete-other-windows)
(global-set-key [s-f13] 'to-two)

(global-set-key [f14] 'split-window-vertically)
(global-set-key [s-f14] 'split-window-horizontally)

(global-set-key [f15] 'exchange-point-and-mark)
(global-set-key [s-f15] 'balance-windows)

(defun back-window()
  (interactive)
  (other-window -1)
)

(global-set-key [s-up] 'back-window)
(global-set-key [s-down] 'other-window)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal  :height 76 :width normal :foundry "unknown" :family "DejaVuSans Mono"))))
 '(whitespace-empty ((t (:background "white smoke" :foreground "firebrick"))))
 '(whitespace-line ((t (:background "gray93" :foreground "black"))))
 '(whitespace-trailing ((t (:background "gray93" :foreground "black" :weight bold)))))

;; '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)

;; DejaVu Sans Liberation Mono
