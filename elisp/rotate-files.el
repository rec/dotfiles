(defun existing-file (filename)
  (if (file-exists-p filename) filename))

(defun rotate-through-files (filename)
  "Rotate through the suffixes we understand."
  (-if-let* ((suffixes '(".cpp" "_inl.h" ".pyx" "_test.cpp" ".h"))
             (len (length suffixes))
             (index (--find-index (s-ends-with? it filename) suffixes))
             (suffix (nth index suffixes))
             (rotated (-rotate (- (+ 1 index)) suffixes))
             (root (s-chop-suffix suffix filename)))
      (--some (existing-file (s-concat root it)) rotated)))

(defun rotate-tests ()
  "Rotate between a file and its test file."
  (interactive)
  (-if-let* ((filename (buffer-file-name))
             (rotated (rotate-through-files filename))
             (ne (not (string= rotated filename)))
             (ff (not (find-file rotated))))
      nil))
