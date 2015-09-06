(defalias 'gsk 'global-set-key)

(defun shrink-window()
  (interactive)
  (enlarge-window -1))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

(defun run-shell(x) (trim-string (shell-command-to-string x)))

(defun swirly-to-root()
  (interactive)
  (dired (run-shell "/development/grit/Root.py"))
)

(setq default-prefix "")

(defun swirly-find-file-body(direction)
  (let ((file (run-shell
               (concat "/development/grit/Grit.py efind "
                       default-prefix
                       " "
                       (buffer-file-name)
                       " "
                       direction))))
    (if (string-equal file "")
        (error "Pattern not found")
      (if (file-exists-p file)
          (find-file file)
        (error "File not found")))))


(defun swirly-find-file(prefix)
  (interactive (list (read-string "Find by prefix: ")) )
  (setq default-prefix prefix)
  (swirly-find-file-body "+")
)

(defun swirly-find-file-next()
  (interactive)
  (swirly-find-file-body "+")
)

(defun swirly-find-file-prev()
  (interactive)
  (swirly-find-file-body "-")
)

(defun swirly-run-python()
  (interactive)
  (let ((buf (get-buffer "*Python*")))
    (if (eq nil buf)
        (run-python "/usr/bin/python -i" nil t)
      (switch-to-buffer "*Python*"))))

(defun swirly-dired()
  (interactive)
  (dired "." nil))

(defun swirly-kill-buffer()
  (interactive)
  (kill-buffer))


;; Clear can't be used.
(gsk [kp-equal] 'balance-windows)
(gsk [kp-divide] 'kmacro-call-macro)

(gsk [kp-multiply] 'keyboard-escape-quit)
(gsk [kp-subtract] 'delete-window)
(gsk [kp-add] 'split-window-vertically)
(gsk [kp-enter] 'repeat-complex-command)

(gsk [f1] 'next-error)
(gsk [f2] 'rotate-tests)
(gsk [f3] 'switch-to-buffer)
(gsk [f4] 'do-list-buffers)
(gsk [f5] 'find-file)
(gsk [f6] 'git-commit-commit)
(gsk [f7] 'swirly-compile)
(gsk [f8] 'swirly-grep)
(gsk [f9] 'swirly-dired)
(gsk [f10] 'query-replace)
(gsk [f11] 'shell)
(gsk [f12] 'kill-line)
(gsk [f13] 'yank)
(gsk [f14] 'save-buffer)
(gsk [Scroll_Lock] 'save-buffer)

(gsk [f14] 'save-buffer)
(gsk [Scroll_Lock] 'save-buffer)
(gsk [f15] 'undo)
(gsk [pause] 'undo)

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
      (gsk [A-f3] 'swirly-kill-buffer)
      (gsk [A-f4] 'do-list-buffers)
      (gsk [A-f5] 'reload-file)
      (gsk [A-f6] 'magit-status)
      (gsk [A-f7] 'to-compile)
      (gsk [A-f8] 'to-grep)
      (gsk [A-f12] 'kill-ring-save)
      (gsk [A-f11] 'swirly-run-python)
      (gsk [A-f13] 'yank-pop)
      (gsk [A-f14] 'save-some-buffers)

      (gsk [A-up] 'back-window)
      (gsk [A-down] 'other-window)
      )
  (progn
    (gsk [home] 'swirly-to-root)
    (gsk [kp-up] 'back-window)
    (gsk [prior] 'shrink-window)
    (gsk [kp-left] 'jump-to-prev-pos)
    (gsk [kp-begin] 'exchange-point-and-mark)
    (gsk [kp-right] 'jump-to-next-pos))

    (gsk [s-f1] 'goto-line)
    (gsk [s-f3] 'swirly-kill-buffer)
    (gsk [s-f4] 'do-list-buffers)
    (gsk [s-f5] 'reload-file)
    (gsk [s-f6] 'magit-status)
    (gsk [s-f7] 'to-compile)
    (gsk [s-f8] 'to-grep)
    (gsk [s-f11] 'swirly-run-python)
    (gsk [s-f12] 'kill-ring-save)
    (gsk [s-f13] 'yank-pop)
    (gsk [s-f14] 'save-some-buffers)
    (gsk [s-Scroll_Lock] 'save-some-buffers)

    (gsk [s-up] 'back-window)
    (gsk [s-down] 'other-window)
    )

;; navigation

(gsk [s-z] 'undo)

;; TODO:
;; switch to compilation buffer.
;; (gsk [s-f11] 'append-next-kill)
;; (gsk [s-f4] 'switch-to-buffer-other-frame)

(gsk [M-z] 'zop-to-char)
