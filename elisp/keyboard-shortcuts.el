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
(gsk [kp-subtract] 'delete-window)
(gsk [kp-add] 'split-window-vertically)
(gsk [kp-enter] 'repeat-complex-command)

(gsk [f1] 'next-error)
(gsk [f2] 'rotate-tests)
(gsk [f3] 'shell)
(gsk [f4] 'switch-to-buffer)
(gsk [f5] 'find-file)
(gsk [f6] 'git-commit-commit)
(gsk [f7] 'swirly-compile)
(gsk [f8] 'swirly-grep)
(gsk [f9] 'dabbrev-expand)
(gsk [f10] 'query-replace)
;;; [f11]
(gsk [f12] 'kill-line)
(gsk [f13] 'yank)
(gsk [f14] 'save-buffer)
(gsk [Scroll_Lock] 'save-buffer)

(gsk [f15] 'undo)
(gsk [pause] 'undo)

;; End can't be used.
(gsk [kp-down] 'other-window) (gsk [kp-2] 'other-window)
(gsk [next] 'enlarge-window)  (gsk [kp-3] 'enlarge-window)

;; End can't be used.
(gsk [kp-down] 'other-window) (gsk [kp-2] 'other-window)
(gsk [next] 'enlarge-window)  (gsk [kp-3] 'enlarge-window)

(if (string-equal system-type "darwin")
    (progn
      (gsk [kp-7] 'swirly-to-root)
      (gsk [kp-8] 'back-window)
      (gsk [kp-9] 'shrink-window)

      (gsk [kp-4] 'jump-to-prev-pos)
      (gsk [kp-5] 'exchange-point-and-mark)
      (gsk [kp-6] 'jump-to-next-pos)

      (gsk [kp-3] 'enlarge-window)
      (gsk [kp-2] 'other-window)

      (gsk [A-f1] 'goto-line)
      (gsk [A-f4] 'do-list-buffers)
      (gsk [A-f5] 'reload-file)
      (gsk [A-f6] 'magit-status)
      (gsk [A-f7] 'to-compile)
      (gsk [A-f8] 'to-grep)
      (gsk [A-f12] 'kill-ring-save)
      (gsk [A-f13] 'yank-pop)
      )
  (progn
    (gsk [home] 'swirly-to-root)
    (gsk [kp-up] 'back-window)
    (gsk [prior] 'shrink-window)
    (gsk [kp-left] 'jump-to-prev-pos)
    (gsk [kp-begin] 'exchange-point-and-mark)
    (gsk [kp-right] 'jump-to-next-pos))

    (gsk [s-f1] 'goto-line)
    (gsk [s-f4] 'do-list-buffers)
    (gsk [s-f5] 'reload-file)
    (gsk [s-f6] 'magit-status)
    (gsk [s-f7] 'to-compile)
    (gsk [s-f8] 'to-grep)
    (gsk [s-f12] 'kill-ring-save)
    (gsk [s-f13] 'yank-pop)
  )

;; navigation

(gsk [s-z] 'undo)

;; TODO:
;; switch to compilation buffer.
;; (gsk [s-f11] 'append-next-kill)
;; (gsk [s-f4] 'switch-to-buffer-other-frame)


(gsk [M-z] 'zop-to-char)
