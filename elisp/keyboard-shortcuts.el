(define-key (current-global-map) "\C-c!" 'shell-here)

(fset 'newfile
   [escape ?4 ?\M-x ?f ?i ?l ?e ?- ?f ?i ?l ?e])

(defun swirly-save-all()
  (interactive)
  (save-some-buffers t)
)

(defun to-grep()
  (interactive)
  (switch-to-buffer "*grep*")
)

(defun swirly-grep()
  "Run grep in the *grep* buffer."
  (interactive)
  (let ()
    (switch-to-buffer "*grep*")
    (call-interactively 'grep)
    ))

(defun to-compile()
  (interactive)
  (switch-to-buffer "*compilation*")
)

(defun back-window()
  (interactive)
  (other-window -1)
  )

;;(define-key key-translation-map (kbd "<kp-multiply>") "\C-g")
(define-key key-translation-map (kbd "<kp-1>") "\C-s")
(define-key key-translation-map (kbd "<kp-3>") "\C-r")

(global-unset-key [C-z])
(defalias 'gsk 'global-set-key)

(gsk [M-z] 'zop-to-char)
(gsk [Scroll_Lock] 'undo)

(gsk [f1] 'next-error)
(gsk [f2] 'kmacro-call-macro)
(gsk [f3] 'switch-to-buffer)
(gsk [f4] 'goto-line)
(gsk [f5] 'find-file)
(gsk [f6] 'reload-file)

(gsk [f7] 'swirly-recompile)
(gsk [C-f7] 'swirly-compile)
(gsk [M-f7] 'swirly-kill-compilation)

(gsk [f8] 'swirly-grep)
(gsk [f9] 'dabbrev-expand)  ;; swirly-dired)
(gsk [f10] 'query-replace)

(defun to-shell()
  (interactive)
  (switch-to-buffer "*shell*")
  )

(gsk [f11] 'to-shell)


(gsk [print] 'raise-next-frame)
(gsk [f13] 'ns-next-frame)
(gsk [s-f13] 'ns-prev-frame)

(gsk [f14] 'save-buffer)

(gsk [f15] 'undo)
(gsk [pause] 'undo)

(gsk [f16] 'list-buffers)

(gsk [kp-equal] 'balance-windows)

(gsk [kp-add] 'split-window-vertically)
(gsk [kp-divide] 'dabbrev-expand)
(gsk [kp-enter] 'repeat-complex-command)
(gsk [kp-multiply] 'keyboard-quit)
(gsk [kp-subtract] 'delete-window)
(gsk [kp-decimal] 'kill-word)

(gsk [C-kp-add] 'split-to-unit-test)
(gsk [C-kp-multiply] 'cycle-windows)

(gsk [s-down] 'other-window)
(gsk [s-up] 'back-window)
(gsk [A-M-down] 'other-window)
(gsk [A-M-up] 'back-window)
(gsk [s-z] 'undo)

(gsk [M-down] 'move-line-down)
(gsk [M-up] 'move-line-up)


(if (string-equal system-type "darwin")
    (progn
      (gsk [kp-7] 'yank)
      (gsk [kp-8] 'back-window)
      (gsk [kp-9] 'yank-pop)

      (gsk [kp-4] 'move-beginning-of-line)  ;; raise-next-frame)
      (gsk [kp-5] 'kill-region)
      (gsk [kp-6] 'move-end-of-line)  ;; raise-previous-frame)

      (gsk [kp-1] 'isearch-forward)
      (gsk [kp-2] 'other-window)
      (gsk [kp-3] 'isearch-backward)

      (gsk [kp-0] 'kill-line)

      ;; A- means "Mac command key"
      (gsk [A-left] 'previous-tab-or-buffer)
      (gsk [A-right] 'next-tab-or-buffer)

      (gsk [A-f1] 'do-list-buffers)
      (gsk [A-f3] 'switch-to-buffer-other-frame)
      (gsk [A-f4] 'do-list-buffers)
      (gsk [A-f5] 'swirly-get-file-name)
      (gsk [A-f6] 'magit-status)
      (gsk [A-f7] 'to-compile)
      (gsk [A-f8] 'grep)
      (gsk [A-f9] 'magit-file-rename)
      ;;f10
      (gsk [4194411] 'kill-this-buffer)

      ;;(gsk [A-f11] 'swirly-run-python)
      (gsk [A-f12] 'dabbrev-expand)
      (gsk [A-f13] 'yank-pop)
      (gsk [A-f14] 'swirly-save-all)
      (gsk [A-f15] 'redo)

      (gsk [A-up] 'back-window)
      (gsk [A-down] 'other-window)

      (gsk [A-kp-divide] 'apply-macro-to-region-lines)
      (gsk [home] 'beginning-of-buffer)
      (gsk [end] 'end-of-buffer)
      (gsk [M-a] 'mark-whole-buffer)
      (gsk [S-a] 'mark-whole-buffer)
      )

  (progn ;; this is totally old...
    (gsk [home] 'swirly-to-root)
    (gsk [kp-up] 'back-window)
    (gsk [kp-left] 'jump-to-prev-pos)
    (gsk [kp-begin] 'exchange-point-and-mark)
    (gsk [kp-right] 'jump-to-next-pos))

    (gsk [s-f1] 'do-list-buffers)
    (gsk [s-f3] 'switch-to-buffer-other-frame)
    (gsk [s-f4] 'do-list-buffers)
    (gsk [s-f5] 'swirly-get-file-name)
    (gsk [s-f6] 'magit-status)
    (gsk [s-f7] 'to-compile)
    (gsk [s-f8] 'grep)
    (gsk [s-f9] 'magit-file-rename)
      ;;f10
    ;;(gsk [s-f11] 'swirly-run-python)
    (gsk [s-f12] 'kill-ring-save)
    (gsk [s-f13] 'yank-pop)
    (gsk [s-f14] 'swirly-save-all)
    (gsk [s-pause] 'redo)

    (gsk [s-up] 'back-window)
    (gsk [s-down] 'other-window)
    (gsk [s-kp-divide] 'apply-macro-to-region-lines)
    )


;; (gsk [kp-0] 'aquamacs-move-beginning-of-line)
;; (gsk [f13] 'cycle-windows) where to put this?
;; (gsk [f6] 'git-commit-commit)  ;; why doesn't this work!?
;; (gsk [f12] 'speedbar)
;; (gsk [kp-decimal] 'aquamacs-move-end-of-line)



;; TODO:
;; is this file up to date?
;; integrate "swirly-find".

;; switch to compilation buffer.
;; (gsk [s-f11] 'append-next-kill)
;; (gsk [s-f4] 'switch-to-buffer-other-frame)
;; merge-all-frames
;; open in other frame
