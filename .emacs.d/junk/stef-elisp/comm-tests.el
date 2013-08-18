;;; -*- auto-recompile: t -*-

;;; comm-tests.el --- simple utilities for testing elisp code

;; This file is not part of GNU Emacs.
;; 
;; comm-tests.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; comm-tests.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; Installation:
;;-------------
;;
;;         (require 'comm-tests)
;;
;; Usage:
;;-------
;;
;; in emacs lisp code, use comment lines such as the following to 
;; set up a test:

; TEST (concat "this " "is" " a test") => "this is a test"

;; those lines are automatically recognized. use the "Tests" menu 
;; to perform one or all tests.
;;
;; if you have embedded-elisp-library.el installed, the menu
;; also allows you to have a button for each test.
;;
;; evaluation of arbitrary forms can be requested in order to complete
;; the test using one or more following ;TRACE statements. 
;; the results appear only upon failure: 

;TEST (buffer-file-name) => "/some/name/for/dir/comm-tests.el"
;TRACE (expand-file-name "~") (locate-library "comm-tests")
;TRACE (featurep 'comm-tests)

;; author: Stéphane Rollandin <hepta@zogotounga.net>
;; last modified January 16, 2009

(require 'cl)
(require 'lisp-mode)
(require 'easymenu)
(require 'query-sheet)

;; ===================================================================
;;                 core functions
;; ===================================================================

(defstruct comm-test
  buffer
  file
  position
  form
  value
  traces)

(defvar tests-output-buffer "*Tests*")
(add-to-list 'special-display-buffer-names tests-output-buffer)

(defvar tests-auto-clear-p t)

(defmacro within-tests-output-buffer (&rest body)
  `(save-excursion
     (set-buffer (get-buffer-create tests-output-buffer))
     (goto-char (point-max))
     ,@body))

(defun runtests (tests-list)
  (get-buffer-create tests-output-buffer)
  (let ((nerrors 0) (ntests 0))
    (dolist (test tests-list)
      (incf ntests)
      (unless (run-comm-test test)
	(incf nerrors)))
    (within-tests-output-buffer
     (let ((conclusion (format "%d test(s) performed. %d failure(s)" 
				ntests nerrors)))
       (insert (format-time-string "[%H:%M] ") conclusion "\n\n")
       (message conclusion)))
    (unless (zerop nerrors)
      (switch-to-buffer-other-window tests-output-buffer)
      (goto-char (if tests-auto-clear-p (point-min) (point-max))))
    (list ntests nerrors)))

(defun signal-test-failure (atest)
  (within-tests-output-buffer
   (insert (propertize "Failure:\n" 
		       'face '(:weight bold :foreground "red"))
	   (propertize (format "%S" (comm-test-form atest))
		       'mouse-face 'highlight
		       'local-map
		       (qsheet-action-keymap
			`(lambda () 
			   (interactive)
			   (if ,(comm-test-file atest)
			       (find-file-other-frame ,(comm-test-file atest))
			     (switch-to-buffer-other-frame
			      ,(comm-test-buffer atest)))
			   (goto-char ,(comm-test-position atest)))))
	   "\n")))

(defun run-comm-test (test)
  "Same as a plain `equalp' except that in case of failure the results 
from evaluating FORM and all optional forms in TRACE are displayed in
`tests-output-buffer'"
  (let ((vals (mapcar (lambda (s)
			(condition-case spec
			    (eval s) 
			  (error spec)))
		      (append (list (comm-test-form test))
			      (comm-test-traces test))))
	(val (comm-test-value test)))
    (if (equalp (car vals) val) 
	t
      (signal-test-failure test)
      (within-tests-output-buffer
	(loop for ti in (comm-test-traces test)
	      for v in (cdr vals)
	      initially do
	      (insert (format "%s\n%S\n%s\n%S\n" 
			      (propertize "Expecting:" 'face 'bold)
			      val 
			      (propertize "Saw" 'face 'bold)
			      (car vals)))
	      initially do 
	      (when (cdr vals) 
		(insert (propertize "Trace:" 'face 'bold) "\n"))
	      do (insert (format "%S => %S\n" ti v)))
	(insert ?\n)))))

(defun tests-clear-buffer ()
  (interactive)
  (within-tests-output-buffer
    (erase-buffer)))


;; ==================================================================
;;                 comment-lines tests
;; ===================================================================

;TEST (1+ 1) => 2
;TEST (1+ 2) => 2
;TEST (concat "a" "heu") => "aheu"
;TEST (concat "a" (make-string 5 ?h)) => "abon?"
;TRACE (make-string 5 ?h) ?h (concat "a" "b")
;TRACE 'end-of-example

(defun go-to-next-commented-test (&optional upto)
  (interactive)
  (unless (and upto (> (point) upto))
    (or (re-search-forward "^[; \t]*TEST\\(.*\\)=>\\(.*\\)" upto t)
        (when (and (interactive-p)
                   (y-or-n-p "No more tests. Scan again from the beginning ?"))
          (goto-char (point-min))
          (go-to-next-commented-test upto)))))

(defun go-to-previous-commented-test ()
  (interactive)
  (or (re-search-backward "^[; \t]*TEST\\(.*\\)=>\\(.*\\)" nil t)
      (when (and (interactive-p)
                 (y-or-n-p "No more tests. Scan again from the end ?"))
        (goto-char (point-max))
        (go-to-previous-commented-test))))

(defun tests-are-defined-p ()
  (save-excursion
    (goto-char (point-min))
    (go-to-next-commented-test)))

(defun run-commented-tests (&optional beg end)
  (interactive)
  (when tests-auto-clear-p (tests-clear-buffer))
  (runtests (collect-commented-tests beg end)))

(defun collect-commented-tests (&optional beg end)
  (interactive)
  (save-excursion
    (goto-char (or beg (point-min)))
    (loop while (go-to-next-commented-test end)
	  collect (make-comm-test
		   :form (read (match-string 1))
		   :value (eval (read (match-string 2)))
		   :buffer (current-buffer)
		   :file (buffer-file-name (current-buffer))
		   :position (match-beginning 0)
		   :traces 
		   (loop while (and (forward-line 1)
				    (looking-at "^[; \t]*TRACE\\(.*\\)"))
			 append (read (format "(%s)" (match-string 1)))
			 into traces
			 finally return traces)))))

(defun run-commented-tests-in-region (&optional beg end)
  (interactive "r")
  (when tests-auto-clear-p (tests-clear-buffer))
  (run-commented-tests beg end))

(defun run-commented-tests-in (&rest filenames)
  (when tests-auto-clear-p (tests-clear-buffer))
  (switch-to-buffer-other-window tests-output-buffer) 
  (let (file (nerrors 0) (ntests 0))
    (dolist (fn filenames)
      (sit-for 0.01)
      (when (setq file (locate-file fn load-path '("" ".el")))
	(within-tests-output-buffer
	 (insert 
	  (propertize (format "in \"%s\"\n" file)
		      'mouse-face 'highlight
		      'local-map
		      (qsheet-action-keymap
		       `(lambda ()
			  (interactive)
			  (find-file-other-frame ,file))))))
	(with-temp-buffer
	  (insert-file-contents file)
	  (set-visited-file-name file t)
	  (set-buffer-modified-p nil)
	  (destructuring-bind 
	      (nntests nnerrors) (runtests (collect-commented-tests))
	    (setq nerrors (+ nerrors nnerrors)
		  ntests (+ ntests nntests))))))
    (within-tests-output-buffer
     (insert (format "Done. %s test(s) performed; %s failure(s)." 
		     ntests nerrors)))))

  (destructuring-bind (un deux) '(1 2)
    (list un deux))

;(apply 'run-commented-tests-in (directory-files (file-name-directory (locate-library "csound-x")) nil "\\.el$"))

(defun tests-wake-up-buttons ()
  (interactive)
  (when (require 'embedded-elisp-library nil t)     
    (save-excursion
      (goto-char (point-min))
      (while (go-to-next-commented-test)
	(save-excursion
	  (beginning-of-line)
	  (eel-wake-up-buttons '(("TEST" . 'run-test-at-point))))))))

(defun run-test-at-point ()
  (interactive)
  (run-commented-tests (point-at-bol) (point-at-eol)))

(defun test-from-region (&optional beg end)
  (interactive "r")
  (let* ((end (min end (point-at-eol)))
         (form (buffer-substring-no-properties beg end))
	 (value (let ((print-escape-newlines t))
		  (prin1-to-string (eval (read form))))))
    (unless (condition-case nil
		(equalp (eval (read value)) (eval (read form)))
	      (error nil))
      (setq value (concat "'" value))
      (condition-case nil
	  (equalp (eval (read value)) (eval (read form)))
	(error (message "formatting failed ! please check the test"))))
    (kill-region beg end)
    (unless (bolp) (insert ?\n))
    (insert ";TEST " form " => " value)
    (unless (eolp) (insert ?\n))
    (when tests-auto-trace-p
      (insert "\n;TRACE "))
    (tests-wake-up-buttons)))


;;  misc. utilities 

(defmacro with-temp-buffer-to-string (&rest body) 
  "Evaluate BODY in a temporary buffer then return its contents as a string"
  (declare (indent 0))
  `(with-temp-buffer
       ,@body
       (buffer-string)))

;TEST (with-temp-buffer-to-string (insert "blep"))  => "blep"


;; ===================================================================
;;                 menu
;; ===================================================================

(defvar tests-auto-trace-p nil
  "When t, insert ;TRACE after a commented test made from the menu")
(make-variable-buffer-local 'tests-auto-trace-p)

(defvar tests-auto-clear-p nil
  "When t, clear the log buffer before a new run")

(easy-menu-define tests-menu emacs-lisp-mode-map
  "Menu provided by tests.el for emacs-lisp-mode"
  (let ((treg "^[; \t]*TEST.*=>.*"))
  `("Tests"
    ["Go to next test" go-to-next-commented-test (tests-are-defined-p)]
    ["Go to previous test" go-to-previous-commented-test (tests-are-defined-p)]
    ["Show all tests" (occur ,treg ) (tests-are-defined-p)]
    "--"
    ["Make a test from region" test-from-region (region-exists-p)]
    ["Auto-insert ;TRACE" (setq tests-auto-trace-p (not tests-auto-trace-p))
     :style toggle :selected tests-auto-trace-p]
    ["Wake up test buttons" tests-wake-up-buttons (and (tests-are-defined-p)
						   (require 'embedded-elisp-library nil t))]
    ["Show all tests" (occur "^;[ ]*TEST ") t]
    "--"
    ["Clear log" tests-clear-buffer t]
    ["Clear log automatically" (setq tests-auto-clear-p (not tests-auto-clear-p))
     :style toggle :selected tests-auto-clear-p]
    "--"
    ["Run test at point" run-test-at-point 
     (string-match ,treg
                   (buffer-substring (point-at-bol) (point-at-eol)))]
    ["Run tests in region"  run-commented-tests-in-region (region-exists-p)]
    ["Run all tests" run-commented-tests (tests-are-defined-p)])))


;; === this is it.
(provide 'comm-tests)

;; tests.el ends here




