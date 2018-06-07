(defun existing-file (filename)
  (if (file-exists-p filename) filename))

(defun rotate-through-files (filename)
  (execvp "/development/dotfiles/python/rotate_file.py" filename))

(defun rotate-tests ()
  "Rotate between a file and its test file."
  (interactive)
  (-if-let* ((filename (buffer-file-name))
             (x1 (message filename))
             (rotated (rotate-through-files filename))
             (x2 (message rotated))
             (x4 (not (find-file rotated)))
             (x5 (message ff))
             )
      nil))


(defun execvp (&rest args)
  "Simulate C's execvp() function.
   Quote each argument seperately, join with spaces and
   call shell-command-to-string to run in a shell.

   From https://www.emacswiki.org/emacs/ExecuteExternalCommand
   "

  (let ((cmd (mapconcat 'shell-quote-argument args " ")))
    (shell-command-to-string cmd)))

;; (message (shell-command-to-string "ls"))
;; (message (execvp "ls"))
;; (message (execvp "ls" "-l"))
(message (rotate-through-files "/development/arthash/python/test/arthash/detect_spam_test.py"))
