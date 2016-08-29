(defun rotate-through-files (filename)
  "Rotate through the suffixes we understand."
  (let* ((suffixes '(".h" ".cpp" "_inl.h" ".pyx" "_test.h"))
         (len (length suffixes)))
    (dotimes (i len)
      (let* ((suffix (nth i suffixes))
             (chopped (s-chop-suffix suffix filename)))
        (if (not (string= chopped filename))
            (dotimes (j len)
              (let*

;; (defun rotate-through-files (filename)
;;   "Rotate through the suffixes we understand."
;;   (let* (
;;          (suffixes '(".h" ".cpp" "_inl.h" ".pyx" "_test.h"))
;;          (len (length suffixes)))
;;     (dotimes (i (length suffixes))
;;       (let* ((x i))
;;         (message "hello world %d" i)))))

;; (defun rotate-through-files (filename)
;;   "Rotate through the suffixes we understand."
;;   (let* (
;;          (suffixes '(".h" ".cpp" "_inl.h" ".pyx" "_test.h"))
;;          (len (length suffixes)))
;;     (dotimes (i len)
;;       (let* ((x i))
;;         (message "hello world %d" i)))))

(rotate-through-files "")
(rotate-through-files "file.cpp")
