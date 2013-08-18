;;; -*- auto-recompile:t; turn-on-eel-mode:t -*-

;;; embedded-elisp-library.el --- evaluating Emacs Lisp within foreign code or text

;; Keywords: convenience, language, editing

;; This file is not part of GNU Emacs.
;; 
;; embedded-elisp-library.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; embedded-elisp-library.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.



;;; Installation:
;;---------------

;; put this file in your load-path and write this line in your .emacs
;;
;;         (require 'embedded-elisp-library)
;;
;; also, you may want to key-bind the main function here, as it has quite a dumb 
;; long name:
;;
;;         (global-set-key (kbd "C-c e") 'embedded-elisp-go-through-buffer)
;;
;; alternatively, this library can be used as a minor mode: M-x embedded-elisp-mode
;; this will install a EEL menu providing easy access to the more useful functions
;; of the library
;;
;; files with a buffer-local variable turn-on-eel-mode with value t should turn on
;; the minor mode automatically when read. 
;; such a file should have the following statement in its first line:
;;
;;          -*- turn-on-eel-mode:t -*-

;;; Commentary:
;;-------------

;; this library of elisp functions has been inspired by the elder.el package
;; (elder.el is from Deepak Goel <deego@glue.umd.edu>)
;;
;; the idea is to have Emacs Lisp code embedded in any kind of file, and evaluate it
;; at will, likely to update or modify the rest of the file content. 
;;
;; the main function here is embedded-elisp-go-through-buffer which scans the current 
;; buffer (or any buffer given as argument) for pairs of mark-ups defining elisp areas.
;; the found areas are evaluated one at a time, from the beginning to the end of the file.
;; for each area, the point is first located just after the elisp code.
;;
;; that's about it.
;;
;; the mark-ups are <ELISP> .. </ELISP>, with slight variations according to the file nature: 
;;
;;  - for HTML documents, they are     <!-- <ELISP> .. </ELISP> -->
;;  - for Emacs Lisp documents:        <ELISP> .. </ELISP> in a comment line
;;     (see the simple example below, around line 185)
;;  - for KeyKit code files:           #<ELISP> .. #</ELISP>
;;
;; etc... 
;; you can add your own settings by customizing embedded-elisp-doctypes
;;
;;
;; most of the code here is a library of functions useful in this specific context. 
;; for example, remove-this-elisp removes the elisp area when it is evaluated. 
;; the code being evaluated is taken as a whole, so that changes in the original buffer
;; can not effect the current elisp (they can modify subsequent elisp areas, though)
;;
;; as a consequence, if you want to keep the current buffer as a template and have 
;; the modifications made on a copy of the file, simply start the embedded elisp by 
;;  
;;     (write-file "/any/file/you/want")
;;
;; and end all your elisp areas by a 
;;
;;     (remove-this-elisp)
;;
;; this will produce an output file without any embedded lisp in it (very much like what
;; elder does); if you omit the (write-file ..) part, you will be working on the original 
;; file and have the embedded elisp destroyed in the process 
;; (note that embedded-elisp-go-through-buffer does not save the buffer, so it is always 
;; possible to revert it if something goes wrong)
;;
;;
;; another interesting function is <eval-this-elisp>, whose single argument should be a
;; quoted form. when embedded-elisp-go-through-buffer is run, <eval-this-elisp> simply
;; transform itself into a button. if you click it with the left mouse button, it will 
;; evaluate its argument. note that you can change the quoted form as you like, and still
;; use the button. this allows for many different pieces of Lisp to be scattered into your
;; document and evaluated one at a time, or not at all.
;;
;;
;; another way to have buttons embedded along with lisp areas is to set-up specific
;; keywords in embedded-buttons-alist and write them into brackets in the first line of
;; the elisp area (the line with the opening mark-up)
;; the buttons are then activated by evaluating (eel-wake-up-embedded-buttons), which
;; is done when the minor mode is turned on.
;;
;;
;;
;; using marks
;; -----------
;;
;; you can mark your file with keywords, in a syntax depending on the file nature:
;;  - for HTML documents, keyword "here" would be     <!--here-->
;;  - for Emacs Lisp documents:                       ;-here-;
;;  - for Keykit code files:                          #-here-#
;; (to include more types of documents, customize embedded-elisp-doctypes)
;;
;; this makes it easy to perform insertions and replacements. for example, the embedded
;; elisp code:
;;            (copy-between-marks "uh1" "oh1" )(paste-between-marks "uh2" "oh2")
;;
;; will copy everything that is between keywords "uh1" and "oh1" between keywords "uh2" and
;; "oh2", replacing what was there before. this can handle several pairs of "uh2"/"oh2"
;;
;; other functions insert text before or after keywords, delete keywords, etc... see the
;; documentations strings for details... but don't forget that any emacs lisp code can be
;; embedded between mark-ups. the only job done by embedded-elisp is to evaluate it within
;; a (progn ...) form. quite simple.
;;
;;
;; MMM-mode
;; --------
;;
;; for convenient editing of embedded lisp, check the great MMM-mode (Multiple Major Modes 
;; minor mode !!), at http://mmm-mode.sourceforge.net/
;;
;; install it as it is explained in MMM documentation, then add the following in your .emacs, 
;; just after the MMM stuff :
;;
;;     (mmm-add-classes
;;      '((embedded-elisp
;;         :submode emacs-lisp-mode
;;         :face mmm-code-submode-face
;;         :front "<ELISP>"
;;         :back "[;# \t]*</ELISP>")))
;;
;; (:back should match all possible comment + closing markup through the different types of
;; recognized documents)
;; now, for each file in which your intend to edit embed elisp, write the following somewhere  
;; in the first line:
;;
;;     -*- mmm-classes:embedded-elisp ; turn-on-eel-mode:t -*-
;;
;;
;;                                                      Stef <hepta@zogotounga.net>


;;; availability: 
;;---------------
;;
;;   http://www.zogotounga.net/comp/eindex.html


;; here's an example of embedded elisp code:
;;
;<ELISP> ;;; [collapse/unfold] - [;EVAL]
;   (if (re-search-forward "modified.*$" nil t)
;     (replace-match (concat "modified " (today-s-date)) nil nil))
;</ELISP>
;;
;; ... clicking [;EVAL] will automagically update the sentence below:

;; last modified March 4, 2008

;============================  core code  ===============================

(require 'easymenu)
(require 'cl)
(require 'mmm-mode nil t)

(if (featurep 'mmm-auto) ;; MMM-mode integration
   (mmm-add-classes
    '((embedded-elisp
       :submode emacs-lisp-mode
       :face mmm-code-submode-face
       :front "<ELISP>"
       :back "[;# \t]*</ELISP>"))))

(defun eel-update-submode ()
  "If MMM is on, make sure we have fresh major mode parameters at point"
  (when (and (featurep 'mmm-mode) mmm-mode)
    (mmm-update-submode-region)))

(defgroup embedded-elisp nil
  "Minor mode for evaluating bits of Emacs Lisp code embedded in a document"
  :group 'tools
  :prefix "embedded-elisp-")

(defcustom embedded-elisp-repository "~/site-lisp/"
  "Default directory for loading and saving bits of embedded elisp code"
  :type 'directory
  :group 'embedded-elisp)

(defcustom embedded-elisp-active-menu " ;;; [load] - [collapse/unfold] - [save] - [EVAL] - [;EVAL]"
  "Default menu string for active areas.
It must be an Emacs Lisp comment containing keywords from embedded-buttons-alist, between brackets. They will become buttons after evaluation of eel-wake-up-embedded-buttons (which is called when the minor mode is turned on, and also from the EEL menu)"
  :type 'string
  :group 'embedded-elisp)

(defcustom embedded-commented-elisp-auto-tag "<;auto>"
"When this string appears in the same line as a EEL begin area mark-up, the corresponding commented code is evaluated by 'eel-wake-up-embedded-buttons"
  :type 'string
  :group 'embedded-elisp)

(defcustom embedded-elisp-auto-tag "<auto>"
"When this string appears in the same line as a EEL begin area mark-up, the corresponding code is evaluated by 'eel-wake-up-embedded-buttons"
  :type 'string
  :group 'embedded-elisp)

(defcustom embedded-buttons-alist '(("load" . 'load-embedded-elisp-here)
				    ("collapse/unfold" . 'collapse-unfold-this-embedded-elisp)
				    ("save" . 'save-this-embedded-elisp)
				    (";EVAL" . 'eval-this-uncommented-elisp)
				    ("EVAL" . 'eval-this-embedded-elisp))
  "Set of available keywords for writing embedded buttons.
This list defines the keywords getting transformed into buttons if they appear between brackets in the same line as an opening mark-up. The so-called \"active areas\" (created by the function insert-active-embedded-elisp-area, available through the EEL menu) features a default string using a subset of embedded-buttons-alist and providing basic management of the corresponding embedded elisp area."
  :type '(repeat (cons (string :tag "keyword/button") (sexp :tag "quoted function")))
  :group 'embedded-elisp)

(defcustom embedded-elisp-doctypes
  '((:modes (keykit-mode) 
	    :filename "\\.k$"
	    :open "#<ELISP>"
	    :close "#</ELISP>"
	    :mopen "#-"
	    :mclose "-#")
    (:modes nil
	    :filename "\\.html*$"
	    :open "<!--<ELISP>"
	    :close "</ELISP>-->"
	    :mopen "<!--"
	    :mclose "-->")
    (:modes (emacs-lisp-mode csound-sco-mode csound-orc-mode)
	    :filename "\\.\\(el\\|sco\\|orc\\|udo\\)$"
	    :open ";<ELISP>"
	    :close ";</ELISP>"
	    :mopen ";-"
	    :mclose "-;")
    (:modes (texinfo-mode)
	    :filename "\\.texi$"
	    :open "@<ELISP>"
	    :close "@</ELISP>"
	    :mopen "@comment"
	    :mclose "")
    (:modes nil
	    :filename ""
	    :open "<ELISP>"
	    :close "</ELISP>"
	    :mopen "<M>"
	    :mclose "</M>"))
  "A list of mark-up syntaxes among which we choose for a given document. 
The choice is based on 
:modes      list of modes 
:filename   regexp matching the document file name
The syntax is described in a list featuring the following keywords:
:open       opening embedded elisp area mark-up
:close      ending embedded elisp area mark-up
:mopen      opening characters identifying a mark; must contain \"<ELISP>\"
:mclose     ending characters identifying a mark; must contain \"</ELISP>\"
Note that the corresponding strings are not regexps."
  :type '(repeat (sexp :tag "syntax"))
  :group 'embedded-elisp)

(defcustom embedded-elisp-mode-hook nil
  "*Hook called when embedded-elisp minor mode is activated or deactivated."
  :type 'hook
  :group 'embedded-elisp)

(defcustom embedded-elisp-clipboard-buffer-name " *embedded-elisp-clipboard*"
  "Name of the buffer used for cut/copy/paste operations"
  :type 'string
  :group 'embedded-elisp)


(defvar embedded-elisp-balise nil "Mark-up beginning an embedded-elisp area") 
(make-variable-buffer-local 'embedded-elisp-balise)
(defvar embedded-elisp-lizba nil "Mark-up ending an embedded-elisp area")
(make-variable-buffer-local 'embedded-elisp-lizba)
(defvar embedded-elisp-mark-balise nil
  "mark-up beginning an embedded-elisp keyword")
(make-variable-buffer-local 'embedded-elisp-mark-balise)
(defvar embedded-elisp-mark-lizba nil
  "mark-up ending an embedded-elisp keyword")
(make-variable-buffer-local 'embedded-elisp-mark-lizba)


(defun embedded-elisp-recognize-document-type (&optional document-buffer)  ;; renommer
  (eel-update-submode)
  (let* ((doc-name 
	  (or 
	   (buffer-file-name (or document-buffer (current-buffer)))
	   ""))
	 (type-alist
	  (or (loop for type in embedded-elisp-doctypes
		    if (memq major-mode (plist-get type :modes)) 
		    return type
		    finally return nil)
	      (loop for type in embedded-elisp-doctypes
		    until (string-match (plist-get type :filename) doc-name)
		    finally return type))))
    (setq embedded-elisp-balise (plist-get type-alist :open)
	  embedded-elisp-lizba (plist-get type-alist :close)
	  embedded-elisp-mark-balise (plist-get type-alist :mopen)
	  embedded-elisp-mark-lizba (plist-get type-alist :mclose))))


(defvar embedded-elisp-current-begpos (make-marker)
  "marker set at the beginning of current embedded elisp section")
(defvar embedded-elisp-current-endpos (make-marker)
  "marker set at the end of current embedded elisp section")
(defvar embedded-elisp-clipboard (get-buffer-create embedded-elisp-clipboard-buffer-name)
  "buffer used by the cut/copy/paste functions")

(defun eel-next-balise ()
  (when (search-forward "<ELISP>" nil t)
    (beginning-of-line)
    (embedded-elisp-recognize-document-type)
    (search-forward embedded-elisp-balise nil t)))

(defun embedded-elisp-go-through-buffer (document-buffer) 
  "Scan DOCUMENT-BUFFER \(or the current buffer) for pairs of mark-ups defining elisp areas.
Found areas are evaluated one at a time, from the beginning to the end of the file. 
See comments in embedded-elisp-library.el for details."
  (interactive "P")
  (let ((elisp-areas-found 0)
	beg end)
    (setq document-buffer (or document-buffer (current-buffer)))
    (set-buffer document-buffer)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (and (setq beg (eel-next-balise)) 
		    (search-forward embedded-elisp-lizba nil t)) 	      
	  (save-excursion
	    (setq end (search-backward embedded-elisp-lizba))) 
	  (embedded-elisp-eval-area beg end)
	  (setq elisp-areas-found (1+ elisp-areas-found)))))))

(defun embedded-elisp-eval-area (beg end)
    (set-marker embedded-elisp-current-begpos beg)
    (set-marker embedded-elisp-current-endpos end)
    (save-excursion
      (eval (read (format "(progn %s)" (buffer-substring-no-properties beg end))))))


;============================  usage as a minor mode  ===============================

(defvar embedded-elisp-mode nil
  "Non-nil if using embedded-elisp mode as a minor mode.
Use the command `embedded-elisp-mode' to toggle or set this variable.")

(make-variable-buffer-local 'embedded-elisp-mode)

(defvar eel-minor-mode-map nil
  "Keymap for embedded-elisp minor mode.")

(defvar eel-minor-mode-menu nil
  "Menu for embedded-elisp minor mode.")


;; definition of minor mode (this clean code is borrowed from hideshow.el)
;;-------------------------

(defun embedded-elisp-mode (&optional arg)
  "Toggle Embedded Elisp minor mode (this provides a EEL menu)
The value '(eel . t) is added to `buffer-invisibility-spec'.
The normal hook `embedded-elisp-mode-hook' is run using `run-hooks'.

Key bindings:
\\{embedded-elisp-mode-map}"

  (interactive "P")
  (setq embedded-elisp-mode (if (null arg)
				(not embedded-elisp-mode)
			      (> (prefix-numeric-value arg) 0)))
  (if embedded-elisp-mode
      (progn
        (easy-menu-add eel-minor-mode-menu)
	(eel-wake-up-embedded-buttons)
        (add-to-invisibility-spec '(eel . t)))
    (easy-menu-remove eel-minor-mode-menu)
    (remove-from-invisibility-spec '(eel . t)))
  (run-hooks 'embedded-elisp-mode-hook))


;; keymaps and menus (code is borrowed from hideshow.el, again)
;;------------------

(if eel-minor-mode-map
    nil
  (setq eel-minor-mode-map (make-sparse-keymap))
  (easy-menu-define
   eel-minor-mode-menu
   eel-minor-mode-map
   "Menu provided by embedded-elisp-mode"
   (cons "EEL"
	 (mapcar
	  ;; Interpret each table entry as follows: first, populate keymap
	  ;; with elements 2 and 1; then, for easymenu, use entry directly
	  ;; unless element 0 is nil, in which case the entry is "omitted".
	  (lambda (ent)
	    (define-key eel-minor-mode-map (aref ent 2) (aref ent 1))
	    (if (aref ent 0) ent "-----"))
	  ;; menu entry command key
	  '(["Insert plain EEL area"         insert-embedded-elisp-area  ""]
	    ["Insert active EEL area"        insert-active-embedded-elisp-area  ""]
	    ["Insert mark"                   insert-embedded-elisp-mark  ""]
	    ["Insert DO button" embedded-elisp-insert-do-button  ""]
	    ["Insert buffer-local liner"     embedded-elisp-set-local-variables  ""]
	    [nil nil ""]
	    ["Collapse all areas"            eel-collapse-all-areas ""]
	    ["Unfold all areas"              eel-unfold-all-areas ""]
	    [nil nil ""]
	    ["Comment all areas"             embedded-elisp-comment-all  ""]
	    ["Uncomment all areas"           embedded-elisp-uncomment-all   ""]
	    [nil nil ""]
	    ["Wake up embedded buttons"      eel-wake-up-embedded-buttons  ""]
	    ["Goto beginning of area"        eel-goto-beginning  ""]
	    ["Evaluate area at point"        eval-this-embedded-elisp  ""]
	    [nil nil ""]
	    ["Evaluate all areas"       embedded-elisp-go-through-buffer   "\C-ce"]
	    ["Evaluate all DO buttons"       embedded-elisp-eval-do-buttons   ""]
	    ["Evaluate everything"       embedded-elisp-eval-everything   ""]
        ;;; [nil             hs-mouse-toggle-hiding [(shift button2)]]
	    )))))


(or (assq 'embedded-elisp-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'embedded-elisp-mode eel-minor-mode-map)
		minor-mode-map-alist)))
(or (assq 'embedded-elisp-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
                                   (list '(embedded-elisp-mode " eel")))))



;; hook for auto-activation triggered by a buffer-local variable
;;--------------------------------------------------------------

(defvar turn-on-eel-mode nil
  "As a buffer-local variable, used to trigger embedded-elisp mode.
Main usage: include \"-*- turn-on-eel-mode:t -*-\" in the first line of file.")

(defun embedded-elisp-check-file ()
  "Turn embedded-elisp-mode on if buffer-local variable `turn-on-eel-mode' is t"
  (if turn-on-eel-mode
      (embedded-elisp-mode 1)))

(add-hook 'find-file-hooks 'embedded-elisp-check-file)


;; code for EEL menu items
;;------------------------


(defun insert-embedded-elisp-area ()   
  (interactive)
  (embedded-elisp-recognize-document-type)
  (insert embedded-elisp-balise "\n")
  (save-excursion
    (insert "\n" embedded-elisp-lizba "\n")))


(defun insert-embedded-elisp-mark (mstr)    
  (interactive "P")
  (embedded-elisp-recognize-document-type)
  (if (equal mstr nil)
      (setq mstr (read-from-minibuffer "keyword: ")))
  (insert embedded-elisp-mark-balise mstr embedded-elisp-mark-lizba))


(defun embedded-elisp-set-local-variables ()
  (interactive)
  (save-excursion
    (comment-region
     (goto-char (point-min))
     (insert "-*- mmm-classes:embedded-elisp; turn-on-eel-mode:t -*-\n"))))


(defun eel-wake-up-embedded-buttons (&optional noeval)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
       (wake-up-embedded-buttons-in-line noeval)
       (forward-line)))
  (embedded-elisp-wake-up-do-buttons))


(defun eel-collapse-all-areas ()
  (interactive)
  (embedded-elisp-collapse-all-areas 1))

(defun eel-unfold-all-areas ()
  (interactive)
  (embedded-elisp-collapse-all-areas 0))


(defun embedded-elisp-comment-all (&optional arg) 
  (interactive)
  (let(beg end)
    (save-excursion
      (goto-char (point-min))
      (while (and
	      (if (eel-next-balise)
		  (setq beg (save-excursion (forward-line 1) (beginning-of-line) (point)))
		nil)
	      (search-forward embedded-elisp-lizba nil t))
	(forward-line -1)
	(comment-region beg (point) arg)))))


(defun embedded-elisp-uncomment-all ()
  (interactive)
  (embedded-elisp-comment-all -1))


;======================================================================================
;============================  general purpose library  ===============================
;======================================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;; code for default embedded buttons 


(defun insert-active-embedded-elisp-area ()   
  (interactive)
  (embedded-elisp-recognize-document-type)
  (let (beg end)
    (save-excursion
      (insert embedded-elisp-balise embedded-elisp-active-menu))
    (wake-up-embedded-buttons-in-line)
    (forward-line 1)
    (beginning-of-line)
    (save-excursion
      (insert "\n" embedded-elisp-lizba "\n"))))

(defun eel-wake-up-buttons (spec-alist)
  (let* ((but-alist spec-alist)
	 (but-key (caar but-alist)))
    (while but-key
      (save-excursion
	(if (looking-at (concat ".*" but-key))
	  (let (beg end)
	    (search-forward but-key)
	    (setq end (point))
	    (search-backward but-key)
	    (setq beg (point))
	    (let ((map (make-sparse-keymap))
		  (tov (make-overlay beg end)))
	      (define-key map [mouse-1]
		(caddr (assoc but-key but-alist)))
	      (overlay-put tov 'priority (length but-key))
	      (overlay-put tov 'local-map map)
	      (overlay-put tov 'face 'custom-button-face)
	      (overlay-put tov 'mouse-face 'custom-button-pressed-face)
	      ;; (overlay-put tov 'mouse-face 'highlight)
	      ))))
      (setq but-alist (cdr but-alist))
      (setq but-key (caar but-alist)))))


(defun wake-up-embedded-buttons-in-line (&optional noeval)  
  (save-excursion
    (beginning-of-line)
    (embedded-elisp-recognize-document-type)
    (when (search-forward embedded-elisp-balise (save-excursion (end-of-line) (point)) t)
      (eel-wake-up-buttons embedded-buttons-alist)
      (unless noeval
        (if (looking-at (concat ".*" embedded-commented-elisp-auto-tag))
            (eval-this-uncommented-elisp))
        (if (looking-at (concat ".*" embedded-elisp-auto-tag))
            (eval-this-embedded-elisp))))))


(defun eel-goto-beginning () 
  ""
  (interactive)
  (save-excursion
    (search-backward "<ELISP>")
    (beginning-of-line)
    (embedded-elisp-recognize-document-type))
  (condition-case nil
      (forward-char (length embedded-elisp-balise))
    (error))
  (search-backward embedded-elisp-balise nil t)
  (search-forward embedded-elisp-balise nil t))

(defun eel-recognize-area-at-point ()        ;; bidon comme nom
  ""
  (save-excursion (eel-goto-beginning)))

(defun load-embedded-elisp-here ()
  (interactive)
  (save-excursion 
    (forward-line 1)
    (beginning-of-line)
    (insert-file-contents
     (embedded-elisp-query-filename "Load elisp from: "))))


(defun save-this-embedded-elisp ()       
  (interactive)
  (eel-recognize-area-at-point)
  (write-region (save-excursion
		  (forward-line 1)
		  (beginning-of-line)
		  (point))
		(- (embedded-elisp-find-next-lizba (point))
		   (length embedded-elisp-lizba))
		(embedded-elisp-query-filename "Save elisp as: ")))


(defun eval-this-embedded-elisp ()     
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (let ((beg (save-excursion (eel-goto-beginning)))
	    (end (- (embedded-elisp-find-next-lizba (point))
		    (length embedded-elisp-lizba))))
	(goto-char (embedded-elisp-find-next-lizba (point)))
	(embedded-elisp-eval-area beg end)))))


(defun eval-this-uncommented-elisp ()        
  (interactive)
  (eel-recognize-area-at-point)
  (save-excursion
    (let ((beg (save-excursion 
		 (end-of-line)
		 (1+ (point))))
	  (end (- (embedded-elisp-find-next-lizba (point))
		  (length embedded-elisp-lizba))))

      (save-excursion
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (re-search-forward "^;" nil t)
	  (replace-match "" nil nil)
	  (setq end (- end 1)))
	(widen))

      (goto-char (embedded-elisp-find-next-lizba (point)))
      (embedded-elisp-eval-area beg end)

      (save-excursion
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (< (save-excursion (end-of-line) (point)) (point-max))
	  (beginning-of-line)
	  (if (not (looking-at "[ \t]*;"))
	      (insert ";"))
	  (forward-line 1))
	(widen)))))


(defun collapse-unfold-this-embedded-elisp (&optional mode)  
"argument: 1 collapse, 0 unfold, nil toggle"
  (interactive)
  (eel-recognize-area-at-point)
  (let (beg)
    (put-text-property 
     (save-excursion 
       (beginning-of-line)
       (search-forward embedded-elisp-balise nil t)
       (if (looking-at "[ \t]*\\(;\\|$\\)")
	   (progn
	     (end-of-line)
	     (setq beg (1+ (point))))
	 (beginning-of-line)
	 (setq beg (search-forward embedded-elisp-balise nil t))))
     (embedded-elisp-find-next-lizba (point))
     'invisible
     (cond ((null mode) (if (not (null (get-text-property beg 'invisible)))
			    nil
			  'eel))
	   ((equal 1 mode) 'eel)
	   ((zerop mode) nil)))))


(defun embedded-elisp-collapse-all-areas (mode)
  (save-excursion
    (goto-char (point-min))
    (while (and
	    (search-forward embedded-elisp-balise nil t)
	    (search-forward embedded-elisp-lizba nil t))
      (save-excursion
	(search-backward embedded-elisp-balise nil t)
	(end-of-line)
	(collapse-unfold-this-embedded-elisp mode)))))



(defun embedded-elisp-find-next-lizba (pos)
  (save-excursion
    (condition-case nil
	(backward-char (length embedded-elisp-lizba))
      (error))
    (search-forward embedded-elisp-lizba nil t)))


(defun embedded-elisp-query-filename (qf-prompt)
  (interactive)
  (read-file-name qf-prompt
		  embedded-elisp-repository
		  nil t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;; <eval-this-elisp>

(defun <eval-this-elisp> (any-quoted-form)
 "Get transformed into a button: click on it to have its argument (a quoted form) evaluated. 
Only one invocation is allowed per <ELISP>..</ELISP> area."
  (let* ((beg (progn
		(goto-char embedded-elisp-current-begpos)
		(search-forward "(<")))
	 (end (- (search-forward ">") 1)))
    (put-text-property beg end
		       'mouse-face 'highlight)
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1] 
	'eval-next-<eval-this-elisp>-body)
      (put-text-property beg end 'local-map map))))


(defun eval-next-<eval-this-elisp>-body ()
  "this is the function called by a click on <eval-this-elisp> as a button"
  (interactive)
  (embedded-elisp-eval-area (search-forward "'") 
			    (progn
			      (forward-sexp)
			      (point))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;; indirect invocation

(defun eval-embedded-elisp-in-file (file &optional save-it remove-elisp no-backup)
  "visit FILE and eval all ELISP areas in it.
if SAVE-IT is t, save the modified FILE.
if REMOVE-ELISP is t, remove all ELISP areas \(before saving\).
if NO-BACKUP is t, do not create backup file when saving.
returns the buffer visiting FILE, which is not killed."
  (save-excursion
    (save-window-excursion
      (find-file file)
      (embedded-elisp-go-through-buffer (current-buffer))
      (when remove-elisp
	(eel-remove-all-elisp))
      (when save-it
	  (save-buffer (when no-backup 0)))
      (current-buffer))))

(defun eel-make-file-from-template (file template &optional force)
  "copy file TEMPLATE as FILE while evaluating all ELISP areas it contains.
if FORCE is t, do not raise an error when FILE already exists.
returns FILE.
see `eval-embedded-elisp-in-file'"
  (copy-file template file force)
  (kill-buffer (eval-embedded-elisp-in-file file t t t))
  file)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;; DO buttons

(defvar embedded-elisp-do-button-label "|DO|"
  "DO button label.")

(defvar embedded-elisp-do-button-prefix "|"
  "DO button prefix when `comment-start' is nil.")

(defun embedded-elisp-insert-do-button (code)
  (interactive "scode:")
  (beginning-of-line)
  (eel-update-submode)
  (insert (or comment-start embedded-elisp-do-button-prefix)
	  embedded-elisp-do-button-label " " code)
  (embedded-elisp-wake-up-do-buttons))

(defun embedded-elisp-goto-next-do-button ()
  (loop while (search-forward embedded-elisp-do-button-label nil t)
	do (eel-update-submode)
	thereis (save-excursion
		  (beginning-of-line)
		  (looking-at 
		   (concat (or comment-start embedded-elisp-do-button-prefix)
			   "[ \t]*" embedded-elisp-do-button-label)))))

(defun embedded-elisp-wake-up-do-buttons ()
  (save-excursion
    (goto-char (point-min))
    (while (embedded-elisp-goto-next-do-button)
      (let ((map (make-sparse-keymap))
	    (tov (make-overlay (match-beginning 0) (match-end 0))))
	(define-key map [mouse-1] 'embedded-elisp-eval-do-button)
	(overlay-put tov 'local-map map)
	(overlay-put tov 'rear-nonsticky t)
	(overlay-put tov 'priority 100)
	(overlay-put tov 'face 'custom-button-face)
	(overlay-put tov 'mouse-face 'custom-button-pressed-face)))))

(defvar embedded-elisp-eval-do-button-function
  (lambda ()
    (end-of-line)
    (eval-last-sexp nil))
  "Function actually evaluating the DO button in current line.
This is a buffer-local setting.")
    
(make-variable-buffer-local 'embedded-elisp-eval-do-button-function)

(defun embedded-elisp-eval-do-button ()
  (interactive)
  (funcall embedded-elisp-eval-do-button-function))

(defun embedded-elisp-eval-do-buttons ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (embedded-elisp-goto-next-do-button)
	(embedded-elisp-eval-do-button)))))

(defun embedded-elisp-eval-everything ()
  (interactive)
  (embedded-elisp-go-through-buffer (current-buffer))
  (embedded-elisp-eval-do-buttons))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;; marks 

(defun eel-mmm-overlays ()
  "Return all overlays used by MMM for regions with specific major modes."
  (delete-if-not (lambda (ovl) (overlay-get ovl 'mmm))
		 (overlays-in (point-min) (point-max))))

(defun eel-next-mark (emk &optional go-before) 
  "Return the position for the next mark labelled EMK, or nil if none.
Multiple major modes are taken into account if MMM is on."
  (save-excursion
    (let* ((mmm-overlays (eel-mmm-overlays))
	   (end nil))
      (when mmm-overlays 
	(let ((next-beg
	       (find-if (lambda (pos) (>= pos (point)))
			(sort (mapcar 'overlay-start mmm-overlays) '<)))
	      (next-end
	       (find-if (lambda (pos) (>= pos (point)))
			(sort (mapcar 'overlay-end mmm-overlays) '<))))
	  (setq end (if (and next-beg next-end)
			(min next-beg next-end)
		      (or next-beg next-end)))))
      (embedded-elisp-recognize-document-type)
      (let* ((full-emk (embedded-elisp-get-full-mark emk))
	     (mpos (search-forward full-emk end t)))
	(if (or mpos (null end))
	    (if (and go-before mpos) (search-backward full-emk) mpos)
	  (goto-char (+ 1 end))
	  (eel-next-mark emk go-before))))))

(defun eel-search-forward-mark (emk &optional go-before)
  "Go to the position for the next mark labelled EMK if ther is one.
If GO-BEFORE is not nil, set point just before the mark.
Multiple major modes are taken into account"
  (let ((next (eel-next-mark emk go-before)))
    (if next (goto-char next))))

(defun remove-marks (emk)
  "Delete all marks matching the argument (regexp)"
  (save-excursion
    (goto-char (point-min))
    (while (eel-search-forward-mark emk)
      (replace-match "" nil nil))))

(defun eel-remove-all-elisp ()
  (save-excursion
    (goto-char (point-min))
    (embedded-elisp-recognize-document-type)
    (while (search-forward embedded-elisp-balise nil t)
      (remove-this-elisp t) ; hack
      (embedded-elisp-recognize-document-type))))

(defun remove-this-elisp (&optional dont-move-point)
  "Delete the currently evaluated embedded elisp area. 
This can be called anywhere in the area: it will not prevent the rest of the code 
from being evaluated since it is copied into a temporary buffer for evaluation."
  (eel-recognize-area-at-point)
  (kill-region (save-excursion
		 (unless dont-move-point
		   (goto-char embedded-elisp-current-begpos))
		 (search-backward embedded-elisp-balise nil t))
	       (save-excursion
		 (unless dont-move-point
		   (goto-char embedded-elisp-current-endpos))
		 (search-forward embedded-elisp-lizba nil t))))


(defun comment-this-elisp (&optional arg)
  "Comment the current embedded elisp area.
This can be called anywhere in the area: it will not prevent the rest of the code 
from being evaluated since it is copied into a temporary buffer for evaluation."
  (eel-recognize-area-at-point)
  (comment-region (save-excursion
		    (goto-char embedded-elisp-current-begpos)
		    (beginning-of-line)
		    (point))
		  (save-excursion
		      (goto-char embedded-elisp-current-endpos)
		      (beginning-of-line)
		      (point))
		  arg))


(defun embedded-elisp-get-full-mark (emk)
  "Return a string matching the EMK keyword (a string) as it should appears in the document buffer"
  (concat embedded-elisp-mark-balise 
	  emk
	  embedded-elisp-mark-lizba))


(defun at-mark (emk)
  "Return the position just after the EMK keyword"
  (save-excursion
    (goto-mark emk)))

(defun goto-mark (emk)
  "Move point just after the EMK keyword"
  (goto-char (point-min))
  (eel-search-forward-mark emk))


(defun insert-at-marks* (emk str)
  "insert the second argument (a string) just after the keyword argument, then delete the keyword"
  (insert-at-marks emk str)
  (remove-marks emk))

(defun insert-at-marks (emk str)
  "insert the second argument (a string) just after the keyword argument"
  (save-excursion
    (goto-char (point-min))
    (while (eel-search-forward-mark emk)
      (insert str))))

(defun insert-before-marks* (emk str)
  "insert the second argument (a string) just before the keyword argument (regexp), then delete the keyword"
  (insert-before-marks emk str)
  (remove-marks emk))

(defun insert-before-marks (emk str)
  "insert the second argument (a string) just before the keyword argument (regexp)"
  (save-excursion
    (goto-char (point-min))
    (while (eel-search-forward-mark emk t)
      (insert str)
      (eel-search-forward-mark emk))))


(defun write-between-marks* (emk1 emk2 str)
  "write the third argument (string) between two keywords (regexps), overwriting what was there before, then delete the keywords"
  (write-between-marks emk1 emk2 str)
  (remove-marks emk1)
  (remove-marks emk2))

(defun write-between-marks (emk1 emk2 str)
  "write the third argument (string) between two keywords (regexps), overwriting what was there before"
  (let (pos1 pos2)
    (save-excursion
      (goto-char (point-min))
      (while (and (setq pos1 (eel-search-forward-mark emk1))
		  (setq pos2 (eel-search-forward-mark emk2 t)))
	(kill-region pos1 pos2)
	(insert str)))))


(defun paste-between-marks* (emk1 emk2)
  "yanks between two keywords (regexps), overwriting what was there before, then delete the keywords"
  (paste-between-marks emk1 emk2)
  (remove-marks emk1)
  (remove-marks emk2))

(defun paste-between-marks (emk1 emk2)
  "yanks between two keywords (regexps), overwriting what was there before"
  (let (pos1 pos2)
    (save-excursion
      (goto-char (point-min))
      (while (and (setq pos1 (eel-search-forward-mark emk1))
		  (setq pos2 (eel-search-forward-mark emk2 t)))
	(kill-region pos1 pos2)
	(insert-buffer-substring embedded-elisp-clipboard)))))


(defun copy-between-marks* (emk1 emk2)
  "copy-as-kill the region between two keywords (regexps), overwriting what was there before, then delete the keywords"
  (copy-between-marks emk1 emk2)
  (remove-marks emk1)
  (remove-marks emk2))

(defun copy-between-marks (emk1 emk2)
 "copy-as-kill the region between two keywords (regexps), overwriting what was there before"
  (let (pos1 pos2)
    (save-excursion
      (goto-char (point-min))
      (if (and (setq pos1 (eel-search-forward-mark emk1))
		  (setq pos2 (eel-search-forward-mark emk2 t)))
	  (copy-to-buffer embedded-elisp-clipboard pos1 pos2)))))

(defun eval-between-marks (emk1 emk2)
  "replace the region between two keywords by its value as Emacs Lisp code"
  (let (pos1 pos2)
    (save-excursion
      (goto-char (point-min))
      (while (and (setq pos1 (eel-search-forward-mark emk1))
		  (setq pos2 (eel-search-forward-mark emk2 t)))
	(let ((val (eval (read (buffer-substring pos1 pos2)))))
	  (kill-region pos1 pos2)
	  (insert (format "%s" val)))))))

(defun eval-between-marks* (emk1 emk2)
  "replace the region between two keywords by its value as Emacs Lisp code, then delete the keywords"
  (eval-between-marks emk1 emk2)
  (remove-marks emk1)
  (remove-marks emk2))

(defun clear-between-marks (emk1 emk2)
  "clear the region between two keywords and set the point there"
  (write-between-marks emk1 emk2 "\n")
  (goto-mark emk1)
  (insert "\n"))

(defun cut-between-marks* (emk1 emk2)
  "kill the region between two keywords (regexps), overwriting what was there before, then delete the keywords"
  (cut-between-marks emk1 emk2)
  (remove-marks emk1)
  (remove-marks emk2))

(defun cut-between-marks (emk1 emk2)
 "kill the region between two keywords (regexps), overwriting what was there before"
  (let (pos1 pos2)
    (save-excursion
      (goto-char (point-min))
      (if (and (setq pos1 (eel-search-forward-mark emk1))
		  (setq pos2 (eel-search-forward-mark emk2 t)))
	  (progn
	    (copy-to-buffer embedded-elisp-clipboard pos1 pos2)
	    (kill-region pos1 pos2))))))


(defun save-between-marks (emk1 emk2 file-name)
  "Write text between to marks in a file"
  (write-region (eel-search-forward-mark emk1)
		(eel-search-forward-mark emk2 t)
		file-name))


(defun save-clipboard-as (file-name)
  (save-excursion
    (set-buffer embedded-elisp-clipboard)
    (write-file file-name)
    (rename-buffer embedded-elisp-clipboard-buffer-name)))


(defun today-s-date ()
  "returns the current date in plain english" 
  (let ((today (cdr (cddr (decode-time (current-time))))))
    (concat 
     (car (rassoc
	   (cadr today)
	   '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
	     ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
	     ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12))))
     " "
     (number-to-string (car today))
     ", "
     (number-to-string (car (cddr today))))))

; (format-time-string "%-d %B %Y")
(defun aujourd-hui ()
  "returns the current date in plain french" 
  (let ((today (cdr (cddr (decode-time (current-time))))))
    (concat 
     (number-to-string (car today))
     " "
     (cdr (assoc
	   (cadr today)
	   '((1 . "janvier") (2 . "février") (3 . "mars") (4 . "avril")
	     (5 . "mai") (6 . "juin") (7 . "juillet") (8 . "août")
	     (9 . "septembre") (10 . "octobre") (11 . "novembre") (12 . "décembre"))))
     " "
     (number-to-string (car (cddr today))))))


;;;;;;;;;;;;; (the end)

(provide 'embedded-elisp-library)


