(defun rotate-through-files (file)
  "Rotate through the suffixes we understand."
  (let* ((suffixes '(".h" ".cpp" "_inl.h" ".pyx" "_test.h" ".h")))
    (dotimes (i (- (length suffixes) 1))
      (message "hello world %d: %s" i (nth i suffixes)))))

(rotate-through-files "")
