;; Mode for editing Csound Scores
;; John Fitch 26 Oct 1996 

;; Copyright (C) 1996 by Codemist Ltd

;; Author: John Fitch <jpff@cs.bath.ac.uk>
;; Keywords: Csound, score
;; Version: 1.05
(defconst csound-sco-version " 1.06")
;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; I N S T A L L A T I O N
;;; In order to arrange for autoloading add the following to .emacs
;;; (setq auto-mode-alist (cons '("\\.sco$" . csound-sco-mode) auto-mode-alist))
;;; (autoload 'csound-sco-mode "csound-sco" "Csound Score major mode." t)
;;;
;;; The running of the mode can be adjusted by various flags best set in
;;; the mode hook.  A typical example is
;;;
;;; (setq csound-sco-mode-hook
;;;       '(lambda ()
;;; 	     (setq font-lock-maximum-decoration t)
;;; 	     (turn-on-font-lock)
;;; 	     (setq csound-sco-mode-do-write-file-hooks t)
;;; 	     (setq csound-sco-mode-build-new-buffer t)
;;; 	     (setq csound-sco-mode-address-string "jpff@cs.bath.ac.uk")
;;; ))


;; Revisions: 
;;            Bug report submission 9 Dec 96
;; 1.02	      Correction of pattern in functions for decimals
;; 1.03	      Correction of pattern in functions for negative generators 6Jan97
;; 1.04	      Protect creation of bold face 16Jan97
;;            Improve documentation slightly 19Jan97
;; 1.05       XEmacs support, and power-of-2 function
;; 1.06       Experimental support for ffitch/Padget experiments

;; Notes: This code used to use hl319 but that is now replaced by font-lock

;; Wish List:
;; Knowledge of numbers of argument for instruments

;;; font-lock fixes for XEmacs: Steve Kersten 
;;; contact steve-k@gmx.net

;; I recommend you turn these on.

(defvar csound-sco-mode-do-write-file-hooks nil
  "*If not nil, then to do timestamps on file saving.
Thi sis done by modifying the local-write-file-hooks.")

(defvar csound-sco-mode-build-new-buffer nil
  "*If not nil, then insert csound-sco-mode-new-buffer-strings when new buffers are generated")

(defvar csound-sco-mode-never-indent nil
  "*If t, the indentation code for csound-sco-mode is turned off.")

(defvar csound-sco-mode-hook nil
  "*Hook run when csound-sco-mode is started.")

(defvar csound-sco-mode-load-hook nil
  "*Hook run when csound-sco-mode is loaded.")

(defvar csound-sco-mode-timestamp-hook 'csound-sco-default-insert-timestamp
  "*Hook called for timestamp insertion.
Override this for your own timestamp styles.")


;; strings you might want to change

(defvar csound-sco-mode-address-string "Score File"
  "*The default author string of each file.")

(defvar csound-sco-mode-new-buffer-template
  '(
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
    ";;" csound-sco-mode-address-string "\n"
    csound-sco-timestamp-start
    csound-sco-timestamp-end
    ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n"
    "\n\n"
    )
  "*Template for new buffers.
Inserted by csound-sco-insert-new-buffer-strings if
csound-sco-mode-build-new-buffer is set to t")

(defvar csound-sco-timestamp-start ";****++++\n"
  "*Delimiter for timestamps.
Everything between csound-sco-timestamp-start and csound-sco-timestamp-end 
will be deleted and replaced with the output of the function 
csound-sco-insert-timestamp if csound-sco-mode-do-write-file-hooks is t")

(defvar csound-sco-timestamp-end ";****----\n"
  "*Delimiter for timestamps. 
Everything between csound-sco-timestamp-start and csound-sco-timestamp-end 
will be deleted and replaced with the output of the function 
csound-sco-insert-timestamp if csound-sco-mode-do-write-file-hooks is t")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Customisable code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cs-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Set if running on XEmacs")

;; csound-sco basic keymap 
(defvar csound-sco-mode-map (make-sparse-keymap) "Keymap for csound-sco-mode")

;; special mode keys
(define-key csound-sco-mode-map ";"	   'csound-sco-comment)
(define-key csound-sco-mode-map "\C-c;"    'comment-region)
(define-key csound-sco-mode-map "\C-c:"    'uncomment-region)
(define-key csound-sco-mode-map "\C-c\C-u" 'csound-csd-quick)
(define-key csound-sco-mode-map "\M-\C-t"
  'csound-sco-insert-timestamp-delimiter-at-point)
(define-key csound-sco-mode-map [(control c) (control ?2)] 'csound-e-power) 

;; indentation keys 
(define-key csound-sco-mode-map "\t" 'csound-sco-mode-indent-command)
(define-key csound-sco-mode-map "\C-j" 'newline-and-indent)

(defvar csound-sco-mode-syntax-table nil "Syntax table")

(if csound-sco-mode-syntax-table
    ()
  (setq csound-sco-mode-syntax-table
	(make-syntax-table text-mode-syntax-table))
;;
;;  (modify-syntax-entry ?'  "w   " csound-sco-mode-syntax-table)
)

(defvar csound-sco-mode-print-indent-info nil
  "If t, indent will print out information as a message.")

(defun csound-sco-mode-indent-command ()
  "Command for indenting text. 
Just calls csound-sco-mode-indent."
  (interactive)
  (csound-sco-mode-indent))

;; some of the ideas are borrowed from cc-mode.el.

(defun csound-sco-mode-indent ()
  "indentation workhorse function."
  (if csound-sco-mode-never-indent
      ()
    (let ((m (point-marker))
	  (ateol (eolp))
	  (pp nil))

      ;; unindent the line
      (end-of-line)
      (setq pp (point))
      (beginning-of-line)
      (narrow-to-region (point) pp)
      (delete-region (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)
	  ;; changed <Steve Kersten>
	  ;; using a single ';' on a line will tabify comments according to code
      (cond ((or (looking-at "[iftsarb]") (looking-at "[ \t]*;[^;???]"))
	     (forward-char 1)
	     (delete-horizontal-space)
	     (insert "%") (zap-to-char -1 ?%)
	     (re-search-forward "[ \t]+" (point-max) t)	;Skip i#
	     (insert "%") (zap-to-char -1 ?%)
	     (delete-horizontal-space)
	     (tab-to-tab-stop)
	     (insert "%") (zap-to-char -1 ?%)
						; Tab each field until end
	     (while (and (re-search-forward "[ \t]+" (point-max) t)
			 (not (looking-at "[ \t]*;;")))
	       (insert "%") (zap-to-char -1 ?%)
	       (delete-horizontal-space)
	       (tab-to-tab-stop)))
	    ((looking-at "[ \t]+;;;")
	     (delete-horizontal-space))
	    ((looking-at "[ \t]*;;"))	; Leave alone
	    ((looking-at "[ \t]+;")
	     (delete-horizontal-space)
	     (indent-to comment-column))
	    ((looking-at "[ \t]e")
	     (delete-horizontal-space)
	     (forward-char 1)
	     (cond ((looking-at "[ \t]+;")
		    (delete-horizontal-space)
		    (indent-to comment-column)))))
      ;; adjust point to where it was before, or at start of indentation
      (if ateol
	  (end-of-line)
	(goto-char (marker-position m)))
      (widen)
      (if (< (current-column) (current-indentation))
	  (back-to-indentation)))))

(defun csound-sco-mode-comment ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at ";")
	0
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column))))

(defun csound-sco-indent-line ()
  "Indents current Csound line based on its contents."
  (interactive)
  (csound-sco-mode-indent))

(defun csound-sco-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;; Recognize existing comments
  (cond ((looking-at ";")
	 (csound-sco-indent-line))
        ; otherwise goto end of line or sth else?
	;; No existing comment.
	;; If side-by-side comments are defined, insert one,
	;; unless line is now blank.
	((and comment-start (not (looking-at "^[ \t]*$")))
	 (end-of-line)
	 (delete-horizontal-space)
	 (indent-to (csound-sco-mode-comment))
	 (insert comment-start))
	;; Else insert separate-line comment, making a new line if nec.
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
;;	   (beginning-of-line)
;;	   (insert "\n")
;;	   (forward-char -1)
	   )
	 (insert ";")
	 (insert-char '\; (- (calculate-csound-sco-indent) (current-column))))))

(defun csound-sco-find-comment-start-skip ()
  "Move to past `comment-start-skip' found on current line.
Return t if `comment-start-skip' found, nil if not."
;;; In order to move point only if comment-start-skip is found,
;;; this one uses a lot of save-excursions.  Note that re-search-forward
;;; moves point even if comment-start-skip is inside a string-constant.
;;; Some code expects certain values for match-beginning and end
  (interactive)
  (if (save-excursion
	(re-search-forward comment-start-skip
			   (save-excursion (end-of-line) (point)) t))
      (let ((save-match-beginning (match-beginning 0))
	    (save-match-end (match-end 0)))
	(goto-char save-match-beginning)
	(re-search-forward comment-start-skip
			   (save-excursion (end-of-line) (point)) t)
	(goto-char (match-end 0))
	t)
    nil))

(defun csound-sco-current-line-indentation ()
  "Indentation of current line
This is the column position of the first non-whitespace character"
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "^;")
	   (goto-char (match-end 0))
	   (skip-chars-forward ";")))
    ;; Move past whitespace.
    (skip-chars-forward " \t")
    (current-column)))

(defun calculate-csound-sco-indent ()
  "Calculates the Csound indent column based on previous lines."
  (let (icol)
    (save-excursion
      (if (= (point) (point-min))
	  (setq icol 8)
	(save-excursion
	  (beginning-of-line)
	  (cond ((looking-at "^;")
		 (setq icol 0))
	  ;; Move past whitespace.
		(t (skip-chars-forward " \t")
		   (setq icol (current-column))))))
      (save-excursion
	(beginning-of-line)
	icol))))

;; timestamps

(defun csound-sco-update-timestamp ()
  "Basic function for updating timestamps. It finds the timestamp in
the buffer by looking for csound-sco-timestamp-start, deletes all text
up to csound-sco-timestamp-end, and runs csound-sco-mode-timestamp-hook
which will presumably insert an appropriate timestamp in the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (if (not (search-backward csound-sco-timestamp-start nil t))
	  (message "timestamp delimiter start was not found")
	(let ((ts-start (+ (point) (length csound-sco-timestamp-start)))
	      (ts-end (if (search-forward csound-sco-timestamp-end nil t)
			  (- (point) (length csound-sco-timestamp-end))
			nil)))
	  (if (not ts-end)
	      (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
	    (delete-region ts-start ts-end)
	    (goto-char ts-start)
	    (run-hooks 'csound-sco-mode-timestamp-hook)))))
    nil))

(defun csound-sco-default-insert-timestamp ()
  "Default timestamp insertion function"
  (insert ";**** Last modified: "
	  (current-time-string)
	  "\n"))

(defun csound-sco-insert-timestamp-delimiter-at-point ()
  "Simple function that inserts timestamp delimiters at point.
Useful for adding timestamps to existing buffers."
  (interactive)
  (insert csound-sco-timestamp-start)
  (insert csound-sco-timestamp-end))

;; csound-sco-insert-new-buffer-strings

(require 'tempo)
(tempo-define-template "csound-sco-skeleton" csound-sco-mode-new-buffer-template
		       nil
		       "Insert a skeleton for a CSOUND document")

(defun csound-sco-insert-new-buffer-strings ()
  "Insert csound-sco-mode-new-buffer-strings."
  (tempo-template-csound-sco-skeleton)
)

(defvar csound-sco-code-level-empty-comment-pattern nil
  "Pattern for entry line with comment")
(defvar csound-sco-flush-left-empty-comment-pattern nil
  "Pattern for left most comment")
(defvar csound-sco-inline-empty-comment-pattern nil
  "Patern for comment line inline")

;;
;; csound-sco-mode

(defun csound-sco-mode ()
  "Mode for editing Csound Scores

\\{csound-sco-mode-map}
Written by John Fitch (jpff@cs.bath.ac.uk)"
  (interactive)
  (kill-all-local-variables)

  (use-local-map csound-sco-mode-map)
  (set-syntax-table csound-sco-mode-syntax-table)

  (setq mode-name "Csound Score")
  (setq major-mode 'csound-sco-mode)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'indent-line-function)

  (setq comment-start "; "
	comment-end ""
	comment-column 40
	indent-line-function 'csound-sco-indent-line)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'csound-sco-mode-comment)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*")
  (setq csound-sco-inline-empty-comment-pattern "^.+;+ *$")
  (setq csound-sco-code-level-empty-comment-pattern "^[\t ]+;; *$")
  (setq csound-sco-flush-left-empty-comment-pattern "^;;; *$")
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  (if csound-sco-mode-do-write-file-hooks
      (add-hook 'local-write-file-hooks 'csound-sco-update-timestamp))

  (if (and csound-sco-mode-build-new-buffer (zerop (buffer-size)))
	(csound-sco-insert-new-buffer-strings))

;; Font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((csound-sco-font-lock-keywords
			      csound-sco-font-lock-keywords-1
			      csound-sco-font-lock-keywords-2
			      csound-sco-font-lock-keywords-3)
			     nil nil nil beginning-of-defun
			     (font-lock-comment-start-regexp . ";")
			     (font-lock-mark-block-function mark-defun)))
  (let ((colour-faces
	 (if cs-running-xemacs 
		 ;; be careful with background; doesn't work in all cases
             '(;; Format is:
               ;; face			"colour"	"background"
               (font-lock-cs-string	"green4"	nil)
               (font-lock-cs-comment	"red"	        nil)
               (font-lock-cs-table	"#6920ac"       nil)
               (font-lock-cs-tableno	"orange"	nil)
               (font-lock-cs-macro	"white"		nil)
               (font-lock-cs-eval	"firebrick"	nil)
               (font-lock-cs-funcs	"white"		nil)
               (font-lock-cs-global	"red"		nil)
               (font-lock-builtin-face  "blue"          nil) ;includes - font not defined in XEmacs (in Emacs ?!) 
               (font-lock-cs-i1		"white"	        nil)
               (font-lock-cs-i2		"white"	        nil)
               (font-lock-cs-i3		"SteelBlue"	nil)
               (font-lock-cs-i4		"Brown"		nil)
               (font-lock-cs-i5		"DarkOliveGreen" nil)
               (font-lock-cs-i6		"MediumSeaGreen" nil)
               (font-lock-cs-i7		"Maroon"	nil)
               (font-lock-cs-i8		"Plum"		nil)
               (font-lock-cs-i9		"OrangeRed"	nil)
               (font-lock-cs-i0		"Violet"	nil)
               (font-lock-cs-section	"orange"	nil)
               (font-lock-cs-tempo	"blue"	        nil))
           '(;; Format is:
             ;; face			"colour"	bold
             (font-lock-cs-string	"magenta"	nil)
             (font-lock-cs-comment	"grey50"	nil)
             (font-lock-cs-table	"RoyalBlue"	nil)
             (font-lock-cs-tableno	"red"		t)
             (font-lock-cs-i1		"Sienna"	nil)
             (font-lock-cs-i2		"DarkSlateBlue"	nil)
             (font-lock-cs-i3		"SteelBlue"	nil)
             (font-lock-cs-i4		"Brown"		nil)
             (font-lock-cs-i5		"DarkOliveGreen" nil)
             (font-lock-cs-i6		"MediumSeaGreen" nil)
             (font-lock-cs-i7		"Maroon"	nil)
             (font-lock-cs-i8		"Plum"		nil)
             (font-lock-cs-i9		"OrangeRed"	nil)
             (font-lock-cs-i0		"Violet"	nil)
             (font-lock-cs-section	"Sienna"	t)
             (font-lock-cs-tempo	"ForestGreen"	nil))))
	face colour colour-face colour-bdbg)
    (while colour-faces
      (setq face (nth 0 (car colour-faces))
	    colour (nth 1 (car colour-faces))
	    colour-face (if cs-running-xemacs face
                          (intern (concat (symbol-name face) "-colour")))
	    colour-bdbg (nth 2 (car colour-faces))
	    colour-faces (cdr colour-faces))
      (make-face colour-face)
      (if colour-bdbg (make-face-bold colour-face nil t))
      (set-face-foreground colour-face colour)
      (set face colour-face)))
  ;; menus added <Steve Kersten>
  (csound-install-sco-menu "Csound/Sco")
  (run-hooks 'csound-sco-mode-hook)
  (if csound-sco-mode-do-write-file-hooks
      (add-hook 'local-write-file-hooks 'csound-sco-update-timestamp))
  (if (and csound-sco-mode-build-new-buffer (zerop (buffer-size)))
	(csound-sco-insert-new-buffer-strings))
  (font-lock-mode 1))

;;

;;; Now an attempt at font-lock support

(defconst csound-sco-font-lock-keywords-1 nil
  "Subdued level highlighting for Csound mode.")

(defconst csound-sco-font-lock-keywords-2 nil
  "Medium level highlighting for Csound mode.")

(defconst csound-sco-font-lock-keywords-3 nil
  "Gaudy level highlighting for Csound mode.")

(setq csound-sco-font-lock-keywords-1
   (list
    ;; Comments
    '(";.*$" . font-lock-cs-comment)
    ;;
    ;; Fontify syntactically (assuming strings cannot be quoted or span lines).
    '("\"[^\"\n]*\"" . font-lock-cs-string)
    ;;
))

(setq csound-sco-font-lock-keywords-2
   (append csound-sco-font-lock-keywords-1
    (list
     ;;
     ;; Fontify all type specifiers (must be first; see below).
     '("\\b\\(e\\|s\\)\\b" 1 font-lock-cs-section)
     '("^[ \t]*\\(\\bf[ \t]*[0-9]+[ \t]+[0-9]+[ \t]+[0-9]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]+\\(.*\\)"
       (1 font-lock-cs-table)
       (2 font-lock-cs-tableno)
       (3 font-lock-cs-table keep t))
;;     '("\\bf[0-9]*\\b" 'font-lock-reference-face)
     '("^[ \t]*\\bi[0-9]*1\\b.*" . font-lock-cs-i1)
     '("^[ \t]*\\bi[0-9]*2\\b.*" . font-lock-cs-i2)
     '("^[ \t]*\\bi[0-9]*3\\b.*" . font-lock-cs-i3)
     '("^[ \t]*\\bi[0-9]*4\\b.*" . font-lock-cs-i4)
     '("^[ \t]*\\bi[0-9]*5\\b.*" . font-lock-cs-i5)
     '("^[ \t]*\\bi[0-9]*6\\b.*" . font-lock-cs-i6)
     '("^[ \t]*\\bi[0-9]*7\\b.*" . font-lock-cs-i7)
     '("^[ \t]*\\bi[0-9]*8\\b.*" . font-lock-cs-i8)
     '("^[ \t]*\\bi[0-9]*9\\b.*" . font-lock-cs-i9)
     '("^[ \t]*\\bi[0-9]*0\\b.*" . font-lock-cs-i0)
     '("^[ \t]*\\bs[0-9]*\\b.*" . font-lock-cs-krate)
     '("^[ \t]*\\b[btv][0-9]*\\b.*" . font-lock-cs-tempo))))

(setq csound-sco-font-lock-keywords-3
   (append
    ;;
    ;; The list `csound-sco-font-lock-keywords-2'.
    csound-sco-font-lock-keywords-2
    ;;
    (list 
     '("\\b<\\|\\b>\\b" . font-lock-cs-string)
     '("\\b+\\b" . font-lock-cs-global)
     '("\\b\\.\\b" . font-lock-cs-tempo))))


(defvar csound-sco-font-lock-keywords csound-sco-font-lock-keywords-2
  "Default expressions to highlight in Csound mode.")

;;(setq csound-sco-font-lock-keywords csound-sco-font-lock-keywords-2)

;;; Deal with comments
(defun csound-sco-line-matches (pattern &optional withcomment)
  (save-excursion
    (beginning-of-line)
    (looking-at pattern)))

(defun csound-sco-pop-comment-level ()
  ;; Delete an empty comment ending current line.  Then set up for a new one,
  ;; on the current line if it was all comment, otherwise above it
  (end-of-line)
  (delete-horizontal-space)
  (while (= (preceding-char) ?; )
    (delete-backward-char 1))
  (delete-horizontal-space)
  (if (bolp)
      nil
    (beginning-of-line)
    (open-line 1)))

(defun csound-sco-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

   1 -- comment to the right of the code (at the comment-column)
   2 -- comment on its own line, indented like code
   3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger csound-sco-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (cond

   ;; Blank line?  Then start comment at code indent level.
   ((csound-sco-line-matches "^[ \t]*$")
    (delete-horizontal-space)
    (tab-to-tab-stop)
    (insert comment-start))

   ;; Nonblank line with no comment chars in it?
   ;; Then start a comment at the current comment column
   ((csound-sco-line-matches "^[^;\n]+$")
    (indent-for-comment))

   ;; Flush-left comment present?  Just insert character.
   ((csound-sco-line-matches csound-sco-flush-left-empty-comment-pattern)
    (insert ";"))

   ;; Empty code-level comment already present?
   ;; Then start flush-left comment, on line above if this one is nonempty. 
   ((csound-sco-line-matches csound-sco-code-level-empty-comment-pattern)
    (csound-sco-pop-comment-level)
    (insert ";;;"))

   ;; Empty comment ends line?
   ;; Then make code-level comment, on line above if this one is nonempty. 
   ((csound-sco-line-matches csound-sco-inline-empty-comment-pattern)
    (csound-sco-pop-comment-level)
    (tab-to-tab-stop)
    (insert ";; "))

   ;; If all else fails, insert character
   (t
    (insert ";")))
  (end-of-line))

(defun uncomment-region ()
  "Call `\\[comment-region]' with an argument"
  (interactive)
  (comment-region (point) (mark) '(t)))


;; Menus

(defvar csound-sco-mode-menu nil "XEmacs menu variable")
(setq csound-sco-mode-menu
	  '(
		"Csound/Orc"
		["Comment Region" comment-region t]
		["Uncomment Region" uncomment-region t]
		["Power-of-2 Region" csound-e-power t]
		"-----"
                ["Retuern to csd mode" csound-csd-quick t]
		["Report Bug" csound-sco-submit-bug-report t]))

;; borrowed from hm--html-mode
(defun csound-install-sco-menu (menu-name)
  (if (and cs-running-xemacs (featurep 'menubar)
           current-menubar (not (assoc menu-name current-menubar)))
      (progn
        (set-buffer-menubar (copy-sequence current-menubar))
        (add-submenu nil
                     (cons menu-name (cdr csound-sco-mode-menu))
                     "Csound/Sco"))))

(if (not cs-running-xemacs)
    (progn
      (define-key csound-sco-mode-map [menu-bar csound-sco]
        (cons "Csound/Sco" (make-sparse-keymap "Csound/Sco")))
      (put 'uncomment-region 'menu-enable 'mark-active)
      (put 'csound-e-power 'menu-enable 'mark-active)
      (define-key csound-orc-mode-map [menu-bar csound-orc csd]
        '("Return to csd mode" . csound-csd-quick))
      (define-key csound-sco-mode-map [menu-bar csound-sco UnComment-Region]
        '("UnComment Region" . uncomment-region))
      (define-key csound-sco-mode-map [menu-bar csound-sco Comment-Region]
        '("Comment Region" . comment-region))
      (define-key csound-sco-mode-map [menu-bar csound-sco Power-of-2]
        '("Power-of-2 Region" . csound-e-power))
      (define-key csound-sco-mode-map [menu-bar csound-sco Submit-Bug-Report]
        '("Report Bug" . csound-sco-submit-bug-report))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDITING FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; computes power of 2 next to x, returns nil for x=0

(defun e-power-of-2 (x)
  (let (
        (min 2)           ;; Minimum of computation -- at least 2
        (max (expt 2 24)) ;; Maximum of computation -- 2^24 for csound-GEN sizes
        (round-log 0)
        (y 0)
        (z 0))
    
    (if (= x 0) nil
      (progn               
        (if (<= x min) 
            (setq x min)
          (progn 
            (if (>= x max) 
                (setq x max)
              (progn
                (setq round-log (truncate (log x 2)))
                (setq y (expt 2 round-log))
                (setq z (expt 2 (1+ round-log)))
                (if (>= (abs (/ (- x y) (- x z))) 1) 
                    z 
                  y)))))))))

(defun csound-e-power (start end)
  "Calculate smallest power of 2 greater or equal to region"
  (interactive "r")
  (let ((number 0))
    (when end
      (save-restriction
        (narrow-to-region start end)
        (setq number (string-to-number (buffer-string))
            number (e-power-of-2 number))
        (when number 
          (delete-region start end)
          (insert (format "%d" number)))))))

;; Debug support

(defun csound-sco-submit-bug-report ()
  "Submit via mail a bug report on csound-sco.el."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report "jpff@cs.bath.ac.uk" (concat "csound-sco" csound-sco-version)
     '(csound-sco-mode-do-write-file-hooks
      csound-sco-mode-build-new-buffer
      csound-sco-mode-never-indent
      csound-sco-mode-hook
      csound-sco-mode-load-hook
      csound-sco-mode-timestamp-hook
      csound-sco-mode-address-string
      csound-sco-mode-new-buffer-template
      csound-sco-timestamp-start
      csound-sco-timestamp-end)				; List of variables
     nil nil
     (concat "Hi John.,

I want to report a bug in csound-sco mode"))))

;; ffitch/Padget operations
(define-key csound-sco-mode-map "\C-cl" 'csound-sco-louder)
(define-key csound-sco-mode-map "\C-cs" 'csound-sco-softer)
(define-key csound-sco-mode-map "\C-cp" 'csound-sco-phrase)

(defun csound-sco-louder (numdb)
  "Change the amplitude of a region by number of dB"
  (interactive "NLouden by dB: ")
  (let (pp qq)
    (if (> (point) (mark)) (exchange-point-and-mark))
    (while (re-search-forward "^i[0-9]+\\b" (mark) t)
      (setq pp (point)) (end-of-line) (setq qq (point)) (goto-char pp)
      (re-search-forward "\\(\\+\\|[0-9]+\\|[0-9]+\\.[0-9]*\\)" qq) ; start time
      (re-search-forward "\\(\\.\\|[0-9]+\\|[0-9]+\\.[0-9]*\\)" qq) ; duration
      (skip-chars-forward " \t")
      (if (looking-at "\\b[0-9]\\b")
          (let ((n
                 (string-to-number (buffer-substring (match-beginning 0)
                                                     (match-end 0)))))
            (replace-match (int-to-string (+ n numdb))))))
    ))

(defun csound-sco-softer ()
  "Change the amplitude of a region by number of dB"
  (interactive "NLouden by dB: ")
  (let (pp qq)
    (if (> (point) (mark)) (exchange-point-and-mark))
    (while (re-search-forward "^i[0-9]+\\b" (mark) t)
      (setq pp (point)) (end-of-line) (setq qq (point)) (goto-char pp)
      (re-search-forward "\\(\\+\\|[0-9]+\\|[0-9]+\\.[0-9]*\\)" qq) ; start time
      (re-search-forward "\\(\\.\\|[0-9]+\\|[0-9]+\\.[0-9]*\\)" qq) ; duration
      (skip-chars-forward " \t")
      (if (looking-at "\\b[0-9]\\b")
          (let ((n
                 (string-to-number (buffer-substring (match-beginning 0)
                                                     (match-end 0)))))
            (replace-match (int-to-string (- n numdb))))))
    ))

(defun csound-sco-phrase ()
)


(provide 'csound-sco-mode)
(run-hooks 'csound-sco-mode-load-hook)

;;; csound-sco-mode.el ends here
