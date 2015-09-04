(defalias 'gsk 'global-set-key)

(defun shrink-window() (interactive) (enlarge-window -1))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

(defun swirly-to-root() (interactive)
  (dired (trim-string (shell-command-to-string "/development/grit/Root.py"))))

;; Clear can't be used.
(gsk [kp-equal] 'balance-windows)
(gsk [kp-divide] 'kmacro-call-macro)
(gsk [kp-multiply] 'keyboard-escape-quit)

(gsk [home] 'swirly-to-root)
(gsk [kp-up] 'back-window)
(gsk [prior] 'shrink-window)
(gsk [kp-subtract] 'delete-window)

(gsk [kp-left] 'jump-to-prev-pos)
(gsk [kp-begin] 'exchange-point-and-mark)
(gsk [kp-right] 'jump-to-next-pos)
(gsk [kp-add] 'split-window-vertically)

;; End can't be used.
(gsk [kp-down] 'other-window)
(gsk [next] 'enlarge-window)
(gsk [kp-enter] 'repeat-complex-command)


;;;; YES!

(gsk [f1] 'next-error)            (gsk [s-f1] 'goto-line)
(gsk [f2] 'rotate-tests)
(gsk [f3] 'shell)
(gsk [f4] 'switch-to-buffer)      (gsk [s-f4] 'do-list-buffers)
(gsk [f5] 'find-file)             (gsk [s-f5] 'reload-file)
(gsk [f6] 'git-commit-commit)     (gsk [s-f6] 'magit-status)
(gsk [f7] 'swirly-compile)        (gsk [s-f7] 'to-compile)
(gsk [f8] 'swirly-grep)           (gsk [s-f8] 'to-grep)
(gsk [f9] 'dabbrev-expand)
(gsk [f10] 'query-replace)
;;;
(gsk [f12] 'kill-line)            (gsk [s-f12] 'kill-ring-save)
(gsk [f13] 'yank)                 (gsk [s-f13] 'yank-pop)

(gsk [f14] 'save-buffer)
(gsk [Scroll_Lock] 'save-buffer)

(gsk [f15] 'undo)
(gsk [pause] 'undo)


;; navigation

(gsk [s-z] 'undo)

;; TODO:
;; switch to compilation buffer.
;; (gsk [s-f11] 'append-next-kill)
(gsk [s-f4] 'switch-to-buffer-other-frame)


(gsk [M-z] 'zop-to-char)

;; (provide 'keyboard-shortcuts)


(dolist (word  '("fight" "foo" "for" "food!"))
  (message word))
