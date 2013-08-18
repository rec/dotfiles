(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(compile-command "cd ~/Documents/development/Max && make -k")
 '(fill-column 80)
 '(global-mark-ring-max 256)
 '(grep-command "grep --exclude \\*.pyc -nHR * -e ")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(indent-tabs-mode nil)
 '(java-indent 2)
 '(kill-ring-max 1024)
 '(large-file-warning-threshold nil)
 '(mark-ring-max 256)
 '(python-indent 2)
 '(standard-indent 2)
 '(tab-width 2)
 '(template-auto-insert t)
 '(template-subdirectories (quote ("./" "Templates/" "~/.emacs.d/Templates")))
)
;; cd /development/rec/projects/slow/Builds/MacOSX && xcodebuild -project Slow.xcodeproj -configuration Debug

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  ;; '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Courier"))))
)

;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; (electric-pair-mode +1)

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; (require 'switch-window)

(require 'dired-x)

(setq grep-use-null-device nil)

(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
             (seq "~" eol)                 ;; backup-files
             (seq ".pyc" eol)              ;; compiled python files
             (seq ".pyo" eol)              ;; compiled python files
             (seq bol "CVS" eol)           ;; CVS dirs
             )))


(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Make sizes human-readable by default, sort version numbers
;; correctly, and put dotfiles and capital-letters first.
(setq-default dired-listing-switches "-alhv")

(setq dired-recursive-copies 'always)

(setq load-path (cons (expand-file-name "~/.emacs.d/lisp") load-path))

;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;;(require 'gnugo)
;;(setq auto-mode-alist (cons '("\\.csd$" . csound-csd-mode) auto-mode-alist))
;;(autoload 'csound-csd-mode "csound-csd" "Csound CSD major mode." t)

(require 'stef-elisp "stef-elisp/stef-elisp")

(require 'git-gutter)
;;(global-git-gutter-mode nil)

(defun ggm()
  (git-gutter-mode t)
)
(add-hook 'python-mode-hook 'ggm)

(global-font-lock-mode t)
(column-number-mode t)
(auto-compression-mode 1)

(require 'template)
;;(require 'zencoding-mode)
(require 'protobuf-mode)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; (require 'eproject-0.4/eproject)

(template-initialize)
(blink-cursor-mode nil)
(set-cursor-color 'DeepSkyBlue)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq zoom-font-frame-local-flag nil)
(require 'saveplace)

(setq backup-by-copying-when-mismatch t)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(defun my-c-mode-common-hook ()
  (setq c-basic-offset 2)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'access-label -1)
  (c-set-offset 'case-label -1)
  (c-set-offset 'statement-case-intro -1)
  (c-set-offset 'member-init-intro 4)
  ;; (subword-mode 0)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-python-mode-common-hook ()
  (setq python-basic-offset 2)
;;  (define-key python-mode-map "\C-m" 'newline-and-indent)
)

(add-hook 'python-mode-common-hook 'my-python-mode-common-hook)

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

(desktop-save-mode 1)

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

;; Three functions from http://steve.yegge.googlepages.com/my-dot-emacs-file

(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME."
 (interactive "sNew name: ")
 (let ((name (buffer-name))
       (filename (buffer-file-name)))
 (if (not filename)
     (message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
     (message "A buffer named '%s' already exists!" new-name)
   (progn
     (rename-file name new-name 1)
     (rename-buffer new-name)
     (set-visited-file-name new-name)
     (set-buffer-modified-p nil))))))


(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR."
 (interactive "DNew directory: ")
 (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (dir
         (if (string-match dir "\\(?:/\\|\\\\)$")
             (substring dir 0 -1) dir))
        (newname (concat dir "/" name)))

   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (progn (copy-file filename newname 1)
            (delete-file filename)
            (set-visited-file-name newname)
            (set-buffer-modified-p nil) t))))


(defun cycle-windows (&optional reverse)
   "Cycle the windows' buffers. If given a prefix argument, cycle in reverse."
   (interactive "P")
   (dolist (window (butlast (if reverse (reverse (window-list)) (window-list))))
     (let ((next-window-buffer (window-buffer (next-window window 0))))
       (set-window-buffer (next-window window 0) (window-buffer window))
       (set-window-buffer window next-window-buffer))))

(defun rotate-windows ()
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond

  ((= (count-windows) 2)
   (let* ((w1 (first (window-list)))
          (w2 (second (window-list)))
          (b1 (window-buffer w1))
          (b2 (window-buffer w2))
          (s1 (window-start w1))
          (s2 (window-start w2)))
     (set-window-buffer w1 b2)
     (set-window-buffer w2 b1)
     (set-window-start w1 s2)
     (set-window-start w2 s1)))

  ((= (count-windows) 3)
   (let* ((w1 (first (window-list)))
          (w2 (second (window-list)))
          (w3 (third (window-list)))

          (b1 (window-buffer w1))
          (b2 (window-buffer w2))
          (b3 (window-buffer w3))

          (s1 (window-start w1))
          (s2 (window-start w2))
          (s3 (window-start w3))
          )
     (set-window-buffer w1 b3)
     (set-window-buffer w2 b1)
     (set-window-buffer w3 b2)

     (set-window-start w1 s3)
     (set-window-start w2 s1)
     (set-window-start w3 s2)
     ))

  ((= (count-windows) 4)
   (let* ((w1 (first (window-list)))
          (w2 (second (window-list)))
          (w3 (third (window-list)))
          (w4 (fourth (window-list)))

          (b1 (window-buffer w1))
          (b2 (window-buffer w2))
          (b3 (window-buffer w3))
          (b4 (window-buffer w4))

          (s1 (window-start w1))
          (s2 (window-start w2))
          (s3 (window-start w3))
          (s4 (window-start w4))
          )
     (set-window-buffer w1 b4)
     (set-window-buffer w2 b1)
     (set-window-buffer w3 b2)
     (set-window-buffer w4 b3)

     (set-window-start w1 s4)
     (set-window-start w2 s1)
     (set-window-start w3 s2)
     (set-window-start w4 s3)
     ))))

(defun rotate-file-suffix (file)
  "Returns one rotation through the file"
  (let* ((patterns
        '(("_test\\.cpp" ".h")
          ("\\.h" ".cpp")
          ("\\.cpp" ".proto")
          ("\\.proto" "_test.cpp")
 ;;         ("\\.js" "_test.js")
 ;;         ("_test\\.js" ".js")
          ("\\.cc" ".h")
          ("\\.cpp" "_test.cpp")
          ("_test\\.py" ".py")
          ("\\.py" "_test.py")))
         (working t))
  (while (and patterns working)
    (setq pattern (pop patterns))
    (if (string-match (car pattern) file)
        (progn
          (setq file (replace-regexp-in-string (car pattern)
                                               (cadr pattern)
                                               file))
          (setq working nil)))))
  file)


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
      (if (or (string= new-file filename) (string= new-file old-file))
          (progn (setq working2 nil))
        (progn
        (if (file-readable-p new-file)
            (progn  (find-file new-file) (setq working2 nil))))))))

;; (defun open-file-at-point()
;;   ""
;;   (interactive)
;;   (if (string-match "include \"\\(.*\\)\".*" (thing-at-point 'line))
;;       ;; (if (file-readable-p (match-string 1))
;;           ())));;)
;; ;;          (find-file (match-string 1))));;)


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
(global-set-key [A-f1] 'goto-line)

(global-set-key [f2] 'rotate-tests)
(global-set-key [A-M-up] 'rotate-tests)
(global-set-key [A-f2] 'rotate-windows)

(global-set-key [f3] 'do-list-buffers)

(global-set-key [f4] 'switch-to-buffer)
(global-set-key [A-f4] 'shell)

(global-set-key [f5] 'find-file)
(global-set-key [A-f5] 'reload-file)

(global-set-key [f7] 'compile)

(global-set-key [A-f6] 'home-dired)
(global-set-key [f6] 'default-dired)

(global-set-key [A-f8] 'grep)
;;(global-set-key [A-f8] 'grep-root)

(defun to-grep() (interactive) (switch-to-buffer "*grep*"))
(global-set-key [f8] 'to-grep)

(global-set-key [A-f9] 'kmacro-call-macro)
(global-set-key [f9] 'enlarge-window)

(global-set-key [f10] 'query-replace)
(global-set-key [A-f10] 'tags-query-replace)

(global-set-key [A-f11] 'yank-pop)
(global-set-key [f11] 'yank)

(global-set-key [A-f12] 'append-next-kill)
(global-set-key [f12] 'kill-line)

(global-set-key [f13] 'delete-other-windows)
(global-set-key [A-f13] 'to-two)

(global-set-key [f14] 'split-window-vertically)
(global-set-key [A-f14] 'split-window-horizontally)

(global-set-key [f15] 'exchange-point-and-mark)
(global-set-key [A-f15] 'balance-windows)

(defun back-window()
  (interactive)
  (other-window -1)
)

(global-set-key [A-up] 'back-window)
(global-set-key [A-down] 'other-window)

(fset 'make-cpp
   [?\C-x ?\C-v ?\C-a ?\C-k ?\C-y return ?\C-x ?\C-w ?\C-y backspace ?c ?p ?p return ?\C-k ?\C-k ?\C-s ?n ?a ?m ?e ?s ?p ?a ?c ?e ?  ?\C-a ?\C-  escape ?< ?\C-w return return up up ?# ?i ?n ?c ?l ?u ?d ?e ?  ?\" ?\C-y ?\M-y ?\M-y ?\" ?\C-a ?\M-f right right ?\M-d ?\M-d kp-delete ?\C-a escape ?> ?\C-r ?# ?e ?n ?d ?\C-a ?\C-k backspace ?\C-  escape ?< escape ?- ?2 ?\C-x ?\C-i ?\A-s escape ?% ?v ?i ?r ?t ?u ?a ?l ?  return return ?! ?\A-s f2])


;; ;;   open current directory (split-and-
;; ;;   open file
;; ;;   reload file
;; ;;   grep at the root directory!
;; ;;   shell
;; ;;   compile
;; ;;   next error
;; ;;   autocomplete
;; ;;   find file
;; ;;   file buffer
;; ;;   buffer list
;; ;;   query-replace (tags-)
;; ;;   goto-line
;; ;;   open file other window
;; ;;   view buffer other window


;; (global-set-key [f1] 'other-window)
;; (global-set-key [f2] 'rotate-tests)
;; (global-set-key [f3] 'switch-to-buffer)
;; (global-set-key [f4] 'find-file)
;; (global-set-key [S-f4] 'reload-file)

;; (global-set-key [f5] 'scroll-up)
;; (global-set-key [f6] 'scroll-down)
;; (global-set-key [f7] 'do-list-buffers)
;; (global-set-key [f7] 'goto-line)
;; (global-set-key [f8] 'next-error)

;; (global-set-key [f9] 'query-replace)
;; (global-set-key [f10] 'grep)
;; (global-set-key [f11] 'shell)
;; (global-set-key [f12] 'compile)

;; (global-set-key [f13] 'save-buffer)
;; (global-set-key [f14] 'save-some-buffers)

(add-to-list 'auto-mode-alist '("\\.(inl|proto)\\'"   . c-mode))
(add-to-list 'auto-mode-alist '("\\.(js|json)\\'"   . javascript-mode))
(put 'dired-find-alternate-file 'disabled nil)
