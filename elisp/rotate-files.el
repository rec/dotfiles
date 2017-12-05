(defun existing-file (filename)
  (if (file-exists-p filename) filename))

(defun rotate-through-files-old (filename)
  "Rotate through the suffixes we understand."
  (-if-let* ((suffixes '(".cpp" "_inl.h" ".pyx" "_test.cpp" ".h"))
             (len (length suffixes))
             (index (--find-index (s-ends-with? it filename) suffixes))
             (suffix (nth index suffixes))
             (rotated (-rotate (- (+ 1 index)) suffixes))
             (root (s-chop-suffix suffix filename)))
      (--some (existing-file (s-concat root it)) rotated)))

;; Toggle between:
;; /development/XXX/test/a/b/some_test.py and
;; /development/XXX/a/b/some.py and
;; If it's a test, it's easy.

(defun rotate-through-files (filename)
  "Rotate between test and files in .python."
  (if (string-match-p "_test.py" filename)
      (replace-regexp-in-string "_test" ""
       (replace-regexp-in-string "/test/" "/" filename))
    (replace-regexp-in-string
     "\.py$"
     "_test.py"
     (replace-regexp-in-string
      "/development/.*?/"
      (lambda(s) (concat s "test/"))
      filename))
    ))

(defun rotate-through-files-3 (filename)
  "Rotate between test and files in .python."
  (if (string-match-p "_test.py" filename)
      (replace-regexp-in-string "_test" ""
       (replace-regexp-in-string "/test/" "/" filename))
    (replace-regexp-in-string
       "/development/.*?/"
       (lambda(s) (concat s "test/"))
       filename)
    ))

(defun rotate-through-files-4 (filename)
  "Rotate between test and files in .python."
  (if (string-match-p "_test.py" filename)
      (replace-regexp-in-string "_test" ""
       (replace-regexp-in-string "/test/" "/" filename))
     (replace-regexp-in-string ".py" "_test.py" filename)
     ))

(defun rotate-tests ()
  "Rotate between a file and its test file."
  (interactive)
  (-if-let* ((filename (buffer-file-name))
             (rotated (rotate-through-files filename))
             (existing (existing-file rotated))
             (ff (not (find-file rotated))))
      nil))
