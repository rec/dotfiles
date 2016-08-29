(defun rotate-file-suffix (file)
  "Returns one rotation through the file"
  ;;; .cpp _inl, .pyx, _test, .h,
  (let* ((patterns
        '(
          ("_test\\.cpp" ".h")  ;; last choice

          ("\\.pyx" "_test.cpp")  ;; second last
          ("\\.pyx" ".h")

          ("_inl\\.h" ".pyx")
          ("_inl\\.h" "_test.cpp")
          ("_inl\\.h" ".h")

          ("\\.h" "_inl.h")
          ("\\.h" ".pyx")
          ("\\.h" "_test.cpp")

          ;; ("_test\\.cpp" "_inl.h")
          ;; ("_test\\.cpp" ".h")

          ;; ("_inl\\.h" ".h")

          ;; ("\\.h" ".cpp")
          ;; ("\\.h" "_test.cpp")
          ;; ("\\.h" "_inl.cpp")

          ;; ("\\.cpp" "_test.cpp")
          ;; ("\\.cpp" "_inl.h")
          ;; ("\\.cpp" ".h")
          ))
         (working t))
  (while (and patterns working)
    (setq pattern (pop patterns))
    (if (string-match (car pattern) file)
        (progn
          (setq file
                (replace-regexp-in-string (car pattern)
                                          (cadr pattern)
                                          file))
          (setq working nil)))))
  file)


(defun try-file-directories (file)
  "Tries different possibilities to see if a file exists."
  (let* ((body (file-name-nondirectory file))
         (directory (file-name-directory file))

         (super (file-name-directory (directory-file-name directory)))

         (result-api (concat directory (file-name-as-directory "api") body))
         (result-impl (concat directory (file-name-as-directory "impl") body))
         (result-test (concat directory (file-name-as-directory "tests") body))
         (result-super (concat super body))
         (result-super-api (concat super (file-name-as-directory "api") body))
         (result-super-impl (concat super (file-name-as-directory "impl") body))
         (result-super-test (concat super (file-name-as-directory "tests") body))
         )
    (progn
      ;; (message (concat "1. " file " -> " result-impl ", "
      ;;                  result-super ", " result-super-api ", " result-super-impl))
      (cond
       ((file-readable-p result-api) result-api)
       ((file-readable-p result-impl) result-impl)
       ((file-readable-p result-test) result-test)
       ((file-readable-p result-super) result-super)
       ((file-readable-p result-super-api) result-super-api)
       ((file-readable-p result-super-impl) result-super-impl)
       ((file-readable-p result-super-test) result-super-test)
       (t file))
      )
    )
  )

(defun rotate-tests ()
  "Rotate between a file and its test file."
  (interactive)
  (let* ((filename (buffer-file-name))
         (new-file filename)
         (working2 t)
         (found nil))
    (while working2
      (setq old-file new-file)
      (setq new-file (rotate-file-suffix new-file))
      (if (or (string= new-file filename)
              (string= new-file old-file))
          (progn (setq working2 nil))  ;; Didn't find a buffer - we're done!
        (progn
          (if (not (file-readable-p new-file))
              (setq new-file (try-file-directories new-file))
            )
          (if (file-readable-p new-file)
              (progn (find-file new-file) (setq working2 nil))))))))

(defun rotate-in-two ()
  "Kill all the other windows, split this one, rotate the other window, come back here."
  (interactive)
  (to-two)
  (other-window 1)
  (rotate-tests)
  (other-window 1)
)
