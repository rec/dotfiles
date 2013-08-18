;;; -*- auto-recompile: t -*-

;;; query-sheet.el --- 1) multiple queries across multiple files/buffers
;;;                    2) search buffer operating on other-window

;; Keywords: convenience, searching, editing

;; This file is not part of GNU Emacs.
;; 
;; query-sheet.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; query-sheet.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary: 
;;-------------
;;
;; usage:
;;
;;    1)  multiple queries across multiple files/buffers
;;    --------------------------------------------------
;;
;;    the main function is 'qsheet-query :
;;
;;    (qsheet-query SHEET MODE &optional STRING)
;;    writes in the buffer associated to SHEET the result of the query MODE on
;;    argument STRING
;;
;;    all the interesting job is done when defining SHEET and MODE
;;
;;    a SHEET is a querysheet structure returned by
;;    (make-querysheet 
;;      :buffer-name    ...a string, the name of the associated buffer
;;	:sources        ...a function of no argument returning a list of sources
;;                         OR a static list of sources
;;                         -> a source is either a file name or a buffer
;;                         -> instead of a list, a single source can be provided
;;	:modes          ...a list of modes (see below)
;;    )
;;
;;    a MODE is a querymode structure returned by       
;;    (make-querymode 
;;      :label        ...a string describing the query (used to label a button)
;;      :what-i-did   ...a string used to recall the query action (for information)
;;                       this will be prepended with ":" and the queried STRING
;;      :case         ...a boolean (t or nil) specifying case-fold-search 
;;      :interactive  ...a function of no argument used when no STRING
;;                       argument is given to 'qsheet-query and returning STRING
;;      :get      ...a function of two arguments STRING and SOURCE
;;                   returning the RESULT of the next query for STRING in file or
;;                   buffer SOURCE and returning nil if there is no match.
;;                   (it will be invoked as many times as necessary to get all matches
;;                   for STRING in SOURCE; so it should take the point as is, and 
;;                   it should move the point forward across the match) 
;;                   --> REMEMBER: SOURCE may be a file name or a buffer.
;;                                 the code should account for both cases, except if
;;                                 you know that all SHEET :sources will always be only
;;                                 files or only buffers
;;                   --> REMEMBER: this code is evaluated while in the source buffer
;;                                 (in case of a file that was not visited, this is a 
;;                                  temporary buffer)
;;      :display  ...a function of one argument RESULT (as returned by :get)
;;                   which insert some information in the SHEET buffer
;;                   (there is no need to set the buffer or the point, this is
;;                   done automatically) 
;;                   --> REMEMBER: this code is evaluated while in the SHEET buffer
;;                       if you want to access the source buffer, use the local variable
;;                       qsheet-source-buffer whose scope includes this code
;;      :wrapper-macro  ...an optional quoted macro wrapped around the whole 
;;                         query operation ,@body which can be used to define
;;                         local variables used by :get and :display (allowing for
;;                         example the computing of statistics on the query result).
;;                         the macro may access (for reading only !) the following
;;                         variables which are defined in its scope: 
;;                           'sheet' is the SHEET associated to the current query.
;;                           'sources' is the current set of source files/buffers
;;                                     to be scanned.
;;                           'string' is the current query argument
;;                           'mode' is the MODE this macro belongs to (you can use
;;                                  this to write a code common to several modes)
;;                         --> IMPORTANT: the code around ,@body is evaluated in 
;;                         the SHEET buffer (where the query results are displayed)
;;    )
;;
;;
;;    ok, I know this is a terse and rather obscure description. 
;;    ...so please check the detailed example provided just below: (qsheet-example)
;;  
;;
;;    2) search buffer operating on other-window
;;    ------------------------------------------
;;
;;    usage: invoke M-x qsheet-display-search-buffer
;;    or bind it to a key: (global-set-key [f4] 'qsheet-display-search-buffer)
;;
;;
;;                   ... not yet documented !..
;;
;;
;;
;;                                                      Stef <hepta@zogotounga.net>
;;
;;
;; get latest version at:    http://www.zogotounga.net/comp/eindex.html
;;
;;
;; last modified March 28, 2006
;; (requires Emacs 21.1)

(require 'cl)

;;; -----------------------------------------------------------------------------
;;;
;;;  core code
;;;
;;; -----------------------------------------------------------------------------

(defgroup query-sheet nil
  "Handling search and replace queries across multiple files or buffers"
  :group 'matching
  :prefix "qsheet-")

(defstruct querysheet buffer-name sources modes)
(defstruct querymode label what-i-did case interactive get display wrapper-macro)

(defvar qsheet-query-separator ">>> "
  "String used to mark the beginning of a new query results display.
this should not be possibly confused with anything written elsewhere in the sheet.")

(defmacro qsheet-query-wrap-macro (mode &rest body)
  `(if (querymode-wrapper-macro ,mode)
       (eval (list (querymode-wrapper-macro ,mode) '(progn ,@body)))
     ,@body))

(defun qsheet-query (sheet mode &optional string)
  "Go through all the files in SHEET sources and returns a clickable list of
all occurence of STRING found according to MODE in SHEET buffer."
  (interactive)
  (setq string (or string 
		   (funcall (querymode-interactive mode)))) 
  (let ((sources (if (functionp (querysheet-sources sheet))
		     (funcall (querysheet-sources sheet))
		   (querysheet-sources sheet)))
	qsheet-source-buffer)
    (unless (listp sources)
      (setq sources (list sources))) 
    (qsheet-insert-search-headers string mode sheet)
    (setq buffer-read-only nil)
    (save-excursion
      (qsheet-query-wrap-macro mode
       (dolist (source (remove-duplicates sources :test 'equal))
	 (if (and (stringp source)
		  (file-readable-p source))
	     ;; case 1: source is a file name
	     (with-temp-buffer
	       (setq qsheet-source-buffer (current-buffer)
		     case-fold-search (querymode-case mode))
	       (insert-file-contents source)
	       (goto-char (point-min))
	       (let (results)
		 (while (setq results (funcall (querymode-get mode) string source))
		   (save-excursion
		     (set-buffer (qsheet-get-buffer-create-sheet sheet))
		     (funcall (querymode-display mode) results)))))
	   ;; case 2: source is a buffer
	   (when (bufferp source)
	     (set-buffer source)
	     (setq qsheet-source-buffer (current-buffer)
		   case-fold-search (querymode-case mode))
	     (save-excursion
	       (goto-char (point-min))
	       (let (results)
		 (while (setq results (funcall (querymode-get mode) string source)) 
		   (save-excursion
		     (set-buffer (qsheet-get-buffer-create-sheet sheet))
		     (funcall (querymode-display mode) results)))))
	     (set-buffer (qsheet-get-buffer-create-sheet sheet))))))
      (goto-char (point-max))
      (insert-char ?\n 3)
      (setq buffer-read-only t))
    (pop-to-buffer (qsheet-get-buffer-create-sheet sheet))))


(defun qsheet-get-buffer-create-sheet (sheet)
  "`get-buffer-create' SHEET :buffer-name, inserting the header if necessary"
  (let* ((buff-name (querysheet-buffer-name sheet))
	 (buff (get-buffer buff-name)))
    (unless buff
	(save-excursion
	  (setq buff (get-buffer-create buff-name))
	  (set-buffer buff)
          (qsheet-insert-button "(bury)" 'bury-buffer " ")
          (qsheet-insert-button "(kill)" 
	    '(lambda () (interactive)
	       (kill-buffer ())
	       (delete-window)) " ")
          (qsheet-insert-button "(reset)"
	    `(lambda () 
	       (interactive)
	       (kill-buffer ()) 
	       (switch-to-buffer
		(qsheet-get-buffer-create-sheet ,sheet)))
	    "\n")
	  (dolist (mode (querysheet-modes sheet))
	    (qsheet-insert-button (querymode-label (eval mode))
	      `(lambda ()
		 (interactive)
		 (qsheet-query ,sheet ,(eval mode)))
	      "\n"))  
	  (insert "\n\n")))
    buff))

(defun qsheet-insert-search-headers (keyword mode sheet)
  "Insert a couple of buttons at the beginning of the query results"
  (set-buffer (qsheet-get-buffer-create-sheet sheet))
  (setq buffer-read-only nil)
  (goto-char (point-max))
  (insert (propertize (concat qsheet-query-separator (querymode-what-i-did mode) ": ")
		      'face 'bold
		      'mode mode)
	  keyword ?\n
	  (propertize "Results are:" 'face 'bold) 
	  ?\n)
  (qsheet-insert-button "S" 
    `(lambda () (interactive)
       (switch-to-buffer
	(qsheet-get-buffer-create-search-buffer
	 ,keyword nil nil) t)
       (qsheet-search-buffer-size-window)) " ")
  (qsheet-insert-button "R"
    `(lambda () (interactive)
       (switch-to-buffer
	(qsheet-get-buffer-create-search-buffer
	 nil ,keyword nil) t)
       (qsheet-search-buffer-size-window)) " ")
  (qsheet-insert-button "Erase" 'qsheet-erase-search " ")
  (qsheet-insert-button "Refresh"
    `(lambda () 
       (interactive) 
       (qsheet-erase-search)
       (qsheet-query ,sheet ,mode ,keyword))
    " ")
  (qsheet-insert-button "Other"
    `(lambda () (interactive) (qsheet-query ,sheet ,mode)) " ")
  (qsheet-insert-button "Case" 
    `(lambda ()
       (interactive)
       (if (y-or-n-p (concat 
		      "Case is currently "
		      (if (querymode-case ,mode) "ignored" "respected")
		      ". Toggle ? "))
	   (setf (querymode-case ,mode) (null (querymode-case ,mode))))))
  (insert "(was "
	  (if (querymode-case mode) "ignored" "respected")
	  ")")
  (insert-char ?\n 2)
  (setq buffer-read-only t))

(defun qsheet-erase-search ()
  "Used by the 'Erase' and 'Refresh' buttons"
  (interactive)
  (previous-line 2)
  (beginning-of-line)
  (setq buffer-read-only nil)
  (delete-region (point) (save-excursion (next-line 4)
					 (if (null (re-search-forward 
						    (concat "^" qsheet-query-separator) 
						    nil t))
					     (goto-char (point-max))
					   (match-beginning 0))))
  (setq buffer-read-only t))


;;; -----------------------------------------------------------------------------
;;;
;;;  misc. utilities
;;;
;;; -----------------------------------------------------------------------------

(defun qsheet-beginning-of-results () 
  "Return the position of the closest occurence of `qsheet-query-separator' before point
or nil if there is none"
  (save-excursion
    (search-backward qsheet-query-separator nil t)))

(defun qsheet-insert-button (text action &optional delimiter map)
  "Insert a button labelled TEXT which triggers ACTION when clicked
then insert DELIMITER if present"
  (declare (indent 1))
  (qsheet-primitive-insert-button text action nil delimiter map))

(defun qsheet-insert-hard-button (text action &optional delimiter map)
  "Insert a button labelled TEXT which triggers ACTION when clicked
then insert DELIMITER if present, as read-only text"
  (declare (indent 1))
  (qsheet-primitive-insert-button text action t delimiter map))

(defun qsheet-primitive-insert-button (text action hard &optional delimiter map)
  "Insert a button labelled TEXT which triggers ACTION when clicked
then insert DELIMITER if present
the boolean HARD decides weither inserted text is read-only"
  (let ((beg (point)))
    (if (null (string= text ""))
	(insert (propertize (concat " " text " ")
			    'face 'custom-button-face
			    'mouse-face 'custom-button-pressed-face
			    'local-map (qsheet-action-keymap action map)
			    'action action 
			    'is-a-button t)))
    (when delimiter
	(insert delimiter))
    (put-text-property (1- (point)) (point) 'rear-nonsticky t)
    (put-text-property beg (point) 'read-only hard)))

(defun qsheet-action-keymap (action &optional supermap)
  "Return a local map associating ACTION to mouse clicks and <SPACE>"
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    (define-key map [mouse-2] action)
    (define-key map " " action)
    (when supermap
      (set-keymap-parent map supermap))
    map))

(defcustom qsheet-separator-string
  (propertize
   "<=============================================================================>\n"
   'face 'font-lock-warning-face)
  "String inserted in the query sheet to \(optionally) separate sources"
  :type 'string
  :group 'query-sheet)

(defun qsheet-insert-source-separator ()
  (let* ((sep-string qsheet-separator-string)
	 (lim-regexp (concat "\\(^\n\n" qsheet-query-separator "\\)\\|\\(" sep-string "\\)")))
    (insert sep-string)
    (qsheet-insert-button
     "toggle display" 
     `(lambda ()
       (interactive)
       (setq buffer-read-only nil)
       (put-text-property (save-excursion (forward-line 1) (point))
			  (save-excursion (if (re-search-forward ,lim-regexp nil t)
					      (re-search-backward ,lim-regexp)
					    (point-max)))
			  'invisible
			  (null (get-text-property 
				 (save-excursion (forward-line 1) 
						 (point))
				 'invisible)))
       (setq buffer-read-only t)))))

(defun qsheet-erase-to-previous-source-separator ()
  (kill-region (point)
	       (search-backward qsheet-separator-string)))

(defun qsheet-go-and-press-next-button (text)
  (loop
   (unless (search-forward text nil t)
     (return))
   (backward-char 1)
   (when (get-text-property (point) 'is-a-button)
     (funcall (get-text-property (point) 'action))
     (return))))

(defun qsheet-insert-global-toggle-display-button ()
  (qsheet-insert-button
   "toggle all displays" 
   `(lambda ()
      (interactive)
      (let ((end (or (save-excursion (re-search-forward (concat "^" qsheet-query-separator) 
							nil t))
		     (point-max))))
	(save-excursion
	  (while (save-excursion (and (search-forward "toggle display" nil t)
				      (<= (point) end)))
	    (qsheet-go-and-press-next-button "toggle display")))))))


;;; -----------------------------------------------------------------------------
;;;
;;; defining basic functionality, for further copy/customize:
;;;
;;; -----------------------------------------------------------------------------

(defvar qsheet-blank-sheet
  (make-querysheet :buffer-name "*Query Sheet*"
		   :sources 'ignore
		   :modes '(qsheet-search-query
			    qsheet-search-word-query
			    qsheet-search-exact-word-query
			    qsheet-search-regexp-query))
  "a query sheet templates featuring the four basic query modes.
it is not associated to any source")


(defun new-querysheet (&optional sources name modes)
  "Return a copy of `qsheet-blank-sheet', customizable through the optional arguments
the provided modes are a plain search and a regexp search
no sources are defined by default"
  (let ((qsheet (copy-querysheet qsheet-blank-sheet)))
    (when sources
      (setf (querysheet-sources qsheet) sources)) 
    (when name
      (setf (querysheet-buffer-name qsheet) name)) 
    (when modes
      (setf (querysheet-modes qsheet) modes))
    qsheet))


(defun qsheet-make-query-functions (sheet prefix)
  "Define a set of functions that can be used interactively to invoque the query modes
associated to SHEET.
the function names are derived from the mode names, with PREFIX replacing \"qsheet-\",
or PREFIX directly prepended if the mode name do not start with \"qsheet-\""
  (dolist (mode (querysheet-modes sheet))
    (let ((function-name (if (and (> (length (symbol-name mode)) 7)
			     (string= "qsheet-" (substring (symbol-name mode) 0 7)))
			(concat prefix (substring (symbol-name mode) 7 ))
		      (concat prefix (symbol-name mode)))))
      (unless (eval `(functionp (quote ,(read function-name))))
	(eval `(defun ,(read function-name)
		 (&optional string)
		 (interactive)
		 (qsheet-query ,sheet ,mode string)))))))

(defun qsheet-goto-source (source line)
  (if (bufferp source)
      (switch-to-buffer-other-window source)
    (find-file-other-window source))
  (goto-line line)
  (other-window -1))

(defun qsheet-name-source (source)
  (if (bufferp source)
      (with-output-to-string (prin1 source))
    source))
;    (file-relative-name source "~")))

(defvar qsheet-search-query
  (make-querymode 
   :label "Look for"
   :what-i-did "Searched for"
   :case t
   :interactive (lambda () (read-from-minibuffer "Search: ")); (word-at-point)))
   :wrapper-macro 
   '(macro lambda (&rest body)
	   `(let ((counter 0))
	      ,@body
	      (insert "\n")
	      (qsheet-insert-button "replace all of above matches"
		(lambda ()
		  (interactive)
		  (let ((replacement
			 (read-from-minibuffer "Replace with:" nil nil nil query-replace-history)))
		    (save-excursion
		      (let ((beg (qsheet-beginning-of-results)))
			(while (re-search-backward "^ rep\\. " beg t)
			  (funcall (plist-get (text-properties-at (point)) 'action) replacement)))))))
	      (insert (format "\n\ntotal of %s matches found\n (%s sources scanned)" counter (length sources)))))
   :get
   (lambda (string source) 
     (when (search-forward string nil t)
       (incf counter)
       (append (list :source source)
	       (list :found (propertize (match-string 0)  
					'face 'font-lock-warning-face))
	       (list :in-line (propertize (thing-at-point 'line)
					  'face 'font-lock-string-face))
	       (list :line-number (count-lines (point-min) (point))))))
   :display
   (lambda (result)
     (insert (plist-get result :in-line))
     (qsheet-insert-button "rep."
       `(lambda (&optional replacement)
	  (interactive)
	  (save-window-excursion
	    (find-file ,(plist-get result :source))
	    (goto-line ,(plist-get result :line-number))
	    (if (looking-at (format "^.*\\(%s\\)" ,(plist-get result :found)))
		(replace-match
		 (or replacement
		     (read-from-minibuffer "Replace with:" nil nil nil query-replace-history))
		 nil nil nil 1)
	      (error "no match in this line"))))
       " ")
     (qsheet-insert-button "-"
       `(lambda ()
	  (interactive)
	  (let ((buffer-read-only nil)
		(kill-whole-line t)) 
	    (forward-line -1)
	    (kill-line 2))))
     (insert " " (plist-get result :found)
	     " --> "
	     (propertize (qsheet-name-source (plist-get result :source))
			 'mouse-face 'highlight
			 'local-map 
			 (qsheet-action-keymap 
			  `(lambda () (interactive)
			     (qsheet-goto-source ,(plist-get result :source)
						 ,(plist-get result :line-number)))))
	     (format " (Line %s)\n" (plist-get result :line-number)))))
  "A very general purpose query mode simply searching for a string in the sources
it also provides replacement facilities.
this is a good template for further specializations such as `qsheet-search-word-query'")


(defvar qsheet-search-word-query
  (let ((mode (copy-querymode qsheet-search-query)))
    (setf (querymode-label mode) "Look for words containing...")
    (setf (querymode-what-i-did mode) "Looked for words containing")
    (setf (querymode-interactive mode) 
;	  (lambda () (read-from-minibuffer "Word fragment: " (word-at-point))))
	  (lambda () (read-from-minibuffer "Word fragment: ")))
    (setf (querymode-get mode)    
	  (lambda (string source)
	    (when (search-forward string nil t)
	      (let ((word-found (save-excursion (backward-char 1) (word-at-point))))
		(when word-found
		  (incf counter)
		  (append (list :source source)
			  (list :found (propertize word-found	  
						   'face (if (string= word-found string) 
							     'font-lock-warning-face
							   'font-lock-keyword-face)))
			  (list :in-line (propertize (thing-at-point 'line)
				     'face 'font-lock-string-face))
			  (list :line-number
				(count-lines (point-min) (point)))))))))
    mode)
  "A query mode derived from `qsheet-search-query' and searching for words embedding a string")


(defvar qsheet-search-exact-word-query
  (let ((mode (copy-querymode qsheet-search-query)))
    (setf (querymode-label mode) "Look for a word")
    (setf (querymode-what-i-did mode) "Looked for word")
    (setf (querymode-interactive mode) 
;	  (lambda () (read-from-minibuffer "Word: " (word-at-point))))
	  (lambda () (read-from-minibuffer "Word: ")))
    (setf (querymode-get mode)    
	  (lambda (string source)
	    (when (search-forward string nil t)
	      (let ((word-found (save-excursion (backward-char 1) (word-at-point))))
		(when (and word-found (= (length word-found) (length string)))
		  (incf counter)
		  (append (list :source source)
			  (list :found (propertize word-found
						   'face 'font-lock-warning-face ))
			  (list :in-line (propertize (thing-at-point 'line)
						     'face 'font-lock-string-face))
			  (list :line-number (count-lines (point-min) (point)))))))))
    mode)
  "A query mode derived from `qsheet-search-query' and searching for precise words")


(defvar qsheet-search-regexp-query
  (make-querymode 
   :label "Look for regexp"
   :what-i-did "Searched for regexp"
   :case t
   :interactive (lambda () (read-from-minibuffer "Search for regexp: "))
   :wrapper-macro 
   '(macro lambda (&rest body)
	   `(let ((counter 0))
	      ,@body
	      (insert "\n")
	      (qsheet-insert-button "replace all of above matches"
		(lambda ()
		  (interactive)
		  (let* ((subex (read (read-from-minibuffer "Replace subexpression:" "0")))
			 (replacement (read-from-minibuffer (format "Replace subexpression %s with:" subex)
							    nil nil nil query-replace-history)))
		    (save-excursion
		      (let ((beg (qsheet-beginning-of-results)))
			(while (re-search-backward "^ rep\\. " beg t)
			  (funcall (plist-get (text-properties-at (point)) 'action) 
				   replacement subex)))))))
	      (insert (format "\n\ntotal of %s matches found\n (%s sources scanned)" counter (length sources)))))
   :get
   (lambda (reg source) 
     (when (re-search-forward reg nil t)
       (incf counter)
       (append (list :source source)
	       (list :found (match-string 0))
	       (list :in-line (thing-at-point 'line))
	       (list :line-number (count-lines (point-min) (point))))))
   :display
   (lambda (result)
     (insert (propertize (plist-get result :in-line)
			 'face 'font-lock-string-face))
     (qsheet-insert-button "rep."
       `(lambda (&optional replacement subex)
	  (interactive)
	  (let* ((subex (or subex (read (read-from-minibuffer "Replace subexpression:" "0"))))
		 (replacement (or replacement
				  (read-from-minibuffer (format "Replace subexpression %s with:" subex)
							nil nil nil query-replace-history))))
	    (save-window-excursion
	      (find-file ,(plist-get result :source))
	      (goto-line ,(plist-get result :line-number))
	      (if (re-search-forward ,string (save-excursion (end-of-line) (point)) t)
		  (replace-match replacement nil nil nil subex)
		(error "no match in this line")))))
       " ")
     (qsheet-insert-button "-"
       `(lambda ()
	  (interactive)
	  (let ((buffer-read-only nil)
		(kill-whole-line t)) 
	    (forward-line -1)
	    (kill-line 2))))
     (insert " " (propertize (plist-get result :found) 
			     'face 'bold)
	     " --> "
	     (propertize (qsheet-name-source (plist-get result :source))
			 'mouse-face 'highlight
			 'local-map 
			 (qsheet-action-keymap
			  `(lambda () (interactive)
			     (qsheet-goto-source ,(plist-get result :source)
						 ,(plist-get result :line-number)))))
	     " (Line " (number-to-string (plist-get result :line-number)) ")\n")))
  "A very general purpose query mode simply searching for a regexp in the sources
also provides replacement facilities
this is a good template for further specializations")


;;; =============================================================================
;;;
;;;  search buffer: a utility for convenient searching/replacing in a buffer
;;;
;;;  usage: invoke M-x qsheet-display-search-buffer
;;;  or bind it to a key: (global-set-key [f4] 'qsheet-display-search-buffer)
;;;
;;; =============================================================================

(defcustom qsheet-search-buffer-name "*search other-window*"
  "Name of the query buffer"
  :type 'string
  :group 'query-sheet)
(defvar qsheet-case-fold-search case-fold-search)
(defvar qsheet-search-buffer-match-data nil)

(defvar qsheet-search-ring '(""))
(defvar qsheet-regexp-search-ring '(""))

(defvar qsheet-local-map nil)
(defvar qsheet-menu nil)     
(defvar qsheet-search-buffer-menu-items nil
  "Property list storing the structure of the \"Search\" menu
use `qsheet-menu-add-sources' to add new entries to the menu")   

(defcustom qsheet-buffer-keybindings nil        
  "Key bindings active in the query buffer"
  :type '(alist :key-type (sexp :tag "key") :value-type (function :tag "command"))
  :group 'query-sheet)

(defun qsheet-display-search-buffer ()
  "open a search buffer in other-window"
  (interactive)
  (switch-to-buffer-other-window 
   (qsheet-get-buffer-create-search-buffer (and (thing-at-point-looking-at "[^ \t\r\n(),;.'\"]+")
						(match-string-no-properties 0)))
   t)
  (qsheet-search-buffer-size-window))

(defun qsheet-search-buffer-size-window ()
  (interactive)
  (enlarge-window (- (+ (count-lines (point-min) (point-max)) 2)
		     (window-height))))

(defun qsheet-get-buffer-create-search-buffer (&optional string regexp with)
  "`get-buffer-create' the search buffer"
  (let ((buff (get-buffer qsheet-search-buffer-name)))
    (when (and buff (or string regexp with)) 
      (kill-buffer buff)
      (setq buff nil))
    (unless buff
	(save-excursion  
	  (set-buffer (setq buff (get-buffer-create qsheet-search-buffer-name)))
	  (use-local-map (setq qsheet-local-map (qsheet-define-local-map)))
	  (qsheet-insert-search-buttons (propertize " string:" 'face 'bold 'local-map (qsheet-context-local-map))
					(qsheet-context-local-map))
	  (if string (insert string))
	  (insert "\n")
	  (qsheet-insert-search-buttons (propertize " regexp:" 'face 'bold 'local-map (qsheet-context-local-map t))
					(qsheet-context-local-map t))
	  (if regexp (insert regexp))
	  (insert "\n")
          (qsheet-insert-hard-button "replace match" 'qsheet-replace-in-other-window " ")
	  (insert "0")
          (qsheet-insert-hard-button "" 'ignore (propertize " with:" 'face 'bold 'local-map (qsheet-context-local-map)))
	  (if with (insert with))
	  (insert "\n")
          (qsheet-insert-hard-button "RESIZE" 'qsheet-search-buffer-size-window " ")
          (qsheet-insert-hard-button "KILL" '(lambda () (interactive)
					       (kill-buffer ())
					       (delete-window)) " ")
          (qsheet-insert-hard-button "UNDO" 'qsheet-search-buffer-undo " ")
          (qsheet-insert-hard-button "CASE-FOLD" 'qsheet-toggle-case-fold " ")
          (insert (propertize (with-output-to-string (prin1 qsheet-case-fold-search))
			      'face 'bold)
		  " ")
	  (goto-char (point-min))
	  (search-forward ":")))
    buff))

(defun qsheet-insert-search-buttons (xtra map)
  (qsheet-insert-hard-button "all" 'qsheet-search-all-other-window  " " map)
  (qsheet-insert-hard-button "up" 'qsheet-search-other-window-upward " " map)
  (qsheet-insert-hard-button "down" 'qsheet-search-other-window " " map)
  (qsheet-insert-hard-button "rep" 'qsheet-search-other-window-and-replace xtra map))

(defun qsheet-search-all-other-window ()
  (interactive)
  (let ((buff (window-buffer (previous-window))))
    (qsheet-buffer-query (new-querysheet buff (concat "*Querying " (prin1-to-string buff) "*")))))

(defun qsheet-define-local-map ()
  "Return the local map associated to the search buffer"
  (let ((map (make-sparse-keymap))
	(menu '("Search")))
    (dolist (keybinding qsheet-buffer-keybindings)
      (define-key map (car keybinding) (cdr keybinding))) 
    (easy-menu-define qsheet-menu map
      "Menu provided by query-sheet search buffer"
      (dolist (sub qsheet-search-buffer-menu-items menu)
	(unless (and sub (listp sub))
	  (let ((sublist (if sub (list (symbol-name sub)) menu)))
	    (dolist (item (plist-get qsheet-search-buffer-menu-items sub))
	      (setq sublist (append sublist
				    `([,(car item) (qsheet-buffer-query ,(cadr item)) t]))))
	    (if sub
		(setq menu (append menu (list sublist)))
	      (setq menu sublist))))))
    map))

(defun qsheet-context-local-map (&optional regf)
  "Return the local keymap used when point is on \"string:\" or \"regexp:\" "
  (let ((map (make-sparse-keymap))
	(ring (if regf 'qsheet-regexp-search-ring 'qsheet-search-ring)))
    (define-key map "-"
      `(lambda () (interactive)
	(save-excursion
	  (let* ((txt (qsheet-get-search-argument))
		 (rep (or (cadr (member txt ,ring))
			  (car ,ring)
			  txt)))
	    (replace-match rep t t nil 1)))))
    (define-key map "+" 
      `(lambda () (interactive)
	(save-excursion
	  (let* ((txt (qsheet-get-search-argument))
		 pos
		 (rep (if (setq pos (position txt ,ring :test 'equal))
			  (if (zerop pos)
			      (car (last ,ring))
			    (nth (1- pos) ,ring))
			txt)))
	    (replace-match rep t t nil 1)))))
    (define-key map "d" 'qsheet-search-other-window)        
    (define-key map "r" 'qsheet-search-other-window-and-replace)     
    (define-key map "u" 'qsheet-search-other-window-upward)
    (define-key map " " 'qsheet-erase-search-argument)
    (set-keymap-parent map qsheet-local-map)
    map))

(defun qsheet-get-search-argument ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward ":\\(.*\\)$")
    (match-string-no-properties 1)))

(defun qsheet-regexp-argument-p ()
  (= 2 (count-lines (point-min) (point))))

(defun qsheet-erase-search-argument ()
  (interactive)
  (qsheet-get-search-argument)
  (replace-match "" t t nil 1))

(defun qsheet-get-search-numeric-argument ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\([0-9]+\\) with" nil t) 
	(string-to-number (match-string 1))
      0)))

(defun qsheet-toggle-case-fold ()
  (interactive)
  (setq qsheet-case-fold-search (null qsheet-case-fold-search))
  (save-excursion
    (let ((new-case (propertize (with-output-to-string (prin1 qsheet-case-fold-search))
				'face 'bold)))
      (re-search-forward "\\(t\\)\\|\\(nil\\)")
      (replace-match new-case))))

(defmacro with-qsheet-search-argument (&rest body)
  `(let ((arg (qsheet-get-search-argument))
	 (num (qsheet-get-search-numeric-argument))
	 (regf (qsheet-regexp-argument-p))
	 (tmp case-fold-search))
     (when (> (length arg) 0)
       (if regf
	   (add-to-list 'qsheet-regexp-search-ring arg)
	 (add-to-list 'qsheet-search-ring arg))
       (other-window 1)
       (setq case-fold-search qsheet-case-fold-search)
       (condition-case error-spec
	   (progn
	     ,@body)
	 (error (message (prin1-to-string error-spec)))) 
       (setq case-fold-search tmp)
       (other-window -1))))

(defun qsheet-grab-match-data ()
  (setq qsheet-search-buffer-match-data (match-data))
  (let ((data (qsheet-all-matches)))
    (other-window -1)
    (save-excursion
      (kill-region (point-max) (progn
				 (goto-char (point-min))
				 (if (null (re-search-forward "^full " nil t))
				     (point-max)
				   (previous-line 1)
				   (end-of-line)
				   (point))))
      (goto-char (point-max))
      (insert data))
    (qsheet-search-buffer-size-window)
    (other-window 1)))

(defun qsheet-all-matches ()
  (let ((n 1) (str ""))
    (when (match-string 0)
	(setq str (concat (propertize "\nfull match:" 'face 'bold) (match-string 0))))
    (while (match-string n)
      (setq str (concat str "\n" 
			(propertize (concat "match " (number-to-string n) ":")
				    'face 'bold)    
			(match-string n)))   
      (incf n))
    str))

(defun qsheet-replace-in-other-window ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^ replace")
    (with-qsheet-search-argument
     (set-match-data qsheet-search-buffer-match-data)
     (replace-match arg t nil nil num))))

(defun qsheet-search-other-window ()
  (interactive)
  (let (result)
    (with-qsheet-search-argument
     (if (setq result (if regf (re-search-forward arg nil t)
			(search-forward arg nil t)))
	 (qsheet-grab-match-data)))
    result))

(defun qsheet-search-other-window-and-replace ()
  (interactive)
  (if (qsheet-search-other-window)
      (qsheet-replace-in-other-window)))

(defun qsheet-search-other-window-upward ()
  (interactive)
  (let (result)
    (with-qsheet-search-argument
     (if (setq result (if regf (re-search-backward arg nil t)
			  (search-backward arg nil t)))
	 (qsheet-grab-match-data)))
    result))

(defun qsheet-search-buffer-undo ()
  (interactive)
  (other-window 1)
  (undo)
  (other-window -1))

;;; -----------------------------------------------------------------------------
;;;
;;;  from search-buffer to query sheets
;;;
;;; -----------------------------------------------------------------------------

(defun qsheet-buffer-query (sheet)
  "Launch a query in SHEET with the search buffer string and mode \(a simple search or regexp)"
  (interactive)
  (with-qsheet-search-argument
     (qsheet-query sheet
		   (if regf qsheet-search-regexp-query
		     qsheet-search-query)
		   arg)))

(defun qsheet-menu-add-sources (name sources &optional submenu)
  "Add SOURCES to the \"Search\" menu at item labelled NAME
if SUBMENU is not nil, it should be a symbol: the item will appear under SUBMENU
the menu will be updated when the query buffer gets recreated"
  (declare (indent 1))
  (setq qsheet-search-buffer-menu-items
	(plist-put qsheet-search-buffer-menu-items
		   submenu
		   (append (plist-get qsheet-search-buffer-menu-items submenu)
			   (list (list name (new-querysheet sources (format "*%s query*" name)) submenu))))))


;;; -----------------------------------------------------------------------------
;;;
;;;  sources building utilities
;;;
;;; -----------------------------------------------------------------------------

(defun load-path-sources (&optional top-dir dir-match source-match)
  "Return a list of all emacs-lisp files in the load-path
if TOP-DIR is non-nil, only return the files in the tree below TOP-DIR
if DIR-MATCH is not-nil, only return the files whose path matches DIR-MATCH
if SOURCE-MATCH is not-nil, only return the files whose full name matches SOURCE-MATCH
if SOURCE-MATCH is nil, it defaults to \"\\\\.el$\"
these three filtering tests can be combined"
  (qsheet-files-in-directories load-path top-dir dir-match "\\.el$"))

(defun qsheet-files-in-directories (directories &optional top-dir dir-match source-match only-dirs)
  "Return a list of all files in DIRECTORIES \(a list of directories)
if TOP-DIR is non-nil, only return the files in the tree below directory TOP-DIR
if DIR-MATCH is not-nil, only return the files whose path name matches DIR-MATCH
if SOURCE-MATCH is not-nil, only return the files whose full name matches SOURCE-MATCH
if ONLY-DIRS is not nil, only return the files that are actually directories
these four filtering tests can be combined"
  (let ((files))
    (dolist (dir directories files)
      (when (and (or (null top-dir) (file-below-dir-p dir top-dir))
		 (or (null dir-match) (string-match dir-match (expand-file-name dir))))
	(setq files 
	      (append files
		      (if only-dirs
			  (delete-if-not 'file-directory-p (directory-files dir t (or source-match ".*")))
			(directory-files dir t (or source-match ".*")))))))
    (delete-if (lambda (f) (string-match "\\.$" f)) files)))

(defun qsheet-directories-under (directories)
  "list all directories in the tree structures whose tops are DIRECTORIES,
which are included"
  (append directories (qsheet-files-in-deep-directories directories nil nil nil t)))

(defun qsheet-files-in-deep-directories (directories &optional top-dir dir-match source-match only-dirs)
  "Return a list of all files in DIRECTORIES \(a list of directories) and their sub-directories
if TOP-DIR is non-nil, only return the files in the tree below directory TOP-DIR
if DIR-MATCH is not-nil, only return the files whose path name matches DIR-MATCH
if SOURCE-MATCH is not-nil, only return the files whose full name matches SOURCE-MATCH
if ONLY-DIRS is not nil, only return the files that are actually directories
these four filtering tests can be combined"
  (let ((files))
    (dolist (dir directories files)
      (when (and (or (null top-dir) (file-below-dir-p dir top-dir))
		 (or (null dir-match) (string-match dir-match (expand-file-name dir))))
	(setq files 
	      (append files 
		      (if only-dirs
			(delete-if-not 'file-directory-p (deep-directory-files dir (or source-match ".*")))
		      (deep-directory-files dir (or source-match ".*")))))))
    (delete-if (lambda (f) (string-match "\\.$" f)) files)))

(defun deep-directory-files (directory &optional match)
  "Return a list of names of files in DIRECTORY and all it sub-directories.
If MATCH is non-nil, mention only file names that match the regexp MATCH."
  (let ((files (directory-files directory t match)))  
    (dolist (dir (delete-if-not 'file-directory-p (directory-files directory t)) files)
      (when (file-below-dir-p dir directory t)
	(setq files (append files (deep-directory-files dir match)))))))

(defun file-below-dir-p (file dir &optional strict)
  "Tell weither DIR is a parent directory of FILE
if STRICT is non-nil then return false when FILE is DIR"
  (let ((relative-name  (if (or (eq system-type 'ms-dos)
				(eq system-type 'windows-nt))
			    (file-relative-name (downcase file) (downcase (expand-file-name dir)))
			  (file-relative-name file (expand-file-name dir)))))
    (not (or
	  (string-match "^\\.\\." relative-name)
	  (and strict (string= relative-name "."))))))

;; TEST (file-below-dir-p "c:/bof" "c:/bof") => t
;; TEST (file-below-dir-p "c:/bof" "c:/bof" t) => nil
;; TEST (file-below-dir-p "c:/bof" "C:/Bof" t) => nil


;; provided default sources, useful for elisp developement:

(qsheet-menu-add-sources "load-path" 'load-path-sources 'elisp)
(qsheet-menu-add-sources "site-lisp" (lambda nil (load-path-sources nil "site-lisp")) 'elisp)


;;; -----------------------------------------------------------------------------
;;;
;;;  basic example:
;;;
;;; -----------------------------------------------------------------------------
;;;
;;; make sure this file is located in your load-path, then evaluate
;;;  (qsheet-example)
;;; and have a look at the code:

(defun qsheet-example ()
  "An example of query-sheet usage. 
we define a sheet exploring this very file, query-sheet.el, assuming it is located in your load-path.
we also define specific modes by copying and sligthly customizing the provided basic modes.
much more interesting changes are possible by setf-ing the :wrapper-macro, :get 
and :display slots of the modes
...see Info node `(cl)Structures' for more details about working with defstruct and structures"

  ;; don't forget this !
  (require 'query-sheet)

  ;; here we define a wrapper for the basic functionality:
  (defun qsheet-example-search (&optional string)
    "an example of 'qsheet-query usage"
    (interactive)
    (qsheet-query qsheet-example-sheet qsheet-example-query string))

  ;; this defines the sheet configuration:
  (defvar qsheet-example-sheet
    (make-querysheet :buffer-name "*Example Query Sheet*"
		     :sources (list (locate-library "query-sheet.el"))
		     :modes '(qsheet-example-query
			      qsheet-example-regexp-query)))

  ;; this defines the first mode (search for a string):
  ;; (see the definition of qsheet-search-query at the end of this file)
  (defvar qsheet-example-query 
    (copy-querymode qsheet-search-query))
  (setf (querymode-label qsheet-example-query)
	"Look in query-sheet.el for a string")

  ;; this defines the second mode (search for a regexp):
  ;; (see the definition of qsheet-search-regexp-query at the end of this file)
  (defvar qsheet-example-regexp-query 
    (copy-querymode qsheet-search-regexp-query))
  (setf (querymode-label qsheet-example-regexp-query)
	"Search query-sheet.el for a regexp")
  (setf (querymode-case qsheet-example-regexp-query) nil)

  ;; now let's go:

  (qsheet-example-search "label")

  ;;... you should get a list of  all occurences of the word "label" in this file
  ;;    choose one, then click in the --> corresponding location (Line ??) to jump there
)

;;; -----------------------------------------------------------------------------
;;;  (end of example)
;;; -----------------------------------------------------------------------------



;; backward-compatibility stuff (useful to anyone ??)
(defalias 'qsheet-local-map 'qsheet-action-keymap)

;; to do: defcustoms

;;; -----------------------------------------------------------------------------
;;; this is it.

(provide 'query-sheet)



