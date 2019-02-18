(defun existing-file (filename)
  (if (file-exists-p filename) filename))

(defun rotate-through-files (filename)
  (execvp "/code/dotfiles/python/rotate_file.py" filename))

(defun rotate-tests ()
  "Rotate between a file and its test file."
  (interactive)
  (-if-let* ((filename (buffer-file-name))
             (rotated (rotate-through-files filename))
             (x (not (find-file rotated)))
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
