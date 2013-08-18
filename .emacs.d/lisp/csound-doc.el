;;; csound-doc.el --- utilities for Csound documentation

;; Keywords: csound, convenience, documentation

;; This file is not part of GNU Emacs.
;; 
;; csound-doc.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-doc.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; Commentary:  Csound on-line help within GNU Emacs 
;;              (this package requires Emacs 21.1)


;; installation
;;-------------

;; put this file in your load-path and add the following to your .emacs: 
;;   (require 'csound-doc)

;; if you want the doc window to pop up in a specific frame, also add:
;;   (add-to-list 'special-display-buffer-names "*Csound Documentation*")

;; this package is able to use both the pretty-printed text version of the 
;; documentation and the usual HTML pages

;; go to the 'csound-doc' customization group (a sub-group of the 'convenience'
;; group) and define your settings as follow:

;; 1) HTML documentation: (enabled by default)
;;
;;    set 'csdoc-html-directory' and 'csdoc-html-entry-point' as needed to fit 
;;    your installation
;;
;;    if you want to use the emacs w3 browser instead of the default external
;;    one, toggle 'csdoc-use-w3' on 
;;    (note that w3 is an independant emacs package; it is not installed in
;;    the default configuration)

;; 2) Text documentation: (disabled by default)
;;
;;    toggle 'csdoc-use-text' on for enabling pretty-printed docs usage
;;    'csdoc-directory' must point to the folder where live the pretty-printed 
;;    docs (which are a set of *.txt files)



;; usage
;;------

;; when using John ffitch's csound-orc-mode, a couple of items should appear
;; in the Csound/Orc menu. 

;; (there is no need to hack csound-orc.el, everything is done from the code 
;; in this file)

;; the following interactive commands are equivalent to the menu items:
;;
;; 1) when using HTML documentation
;;
;;    M-x csdoc-html-document-opcode           document the opcode at point
;;    M-x csdoc-insert-opcode-html-template    insert a template
;;    M-x csdoc-browse-html                    browse the HTML documentation
;;
;; 2) when using text documentation:
;;
;;    M-x csdoc-document-opcode                document the opcode at point
;;    M-x csdoc-insert-opcode-template         does what its name says
;;
;;
;; If you just updated the documentation, you may also find the following useful:
;;
;;    M-x csdoc-refresh-info            refresh the hash table for text docs 
;;    M-x csdoc-refresh-html            refresh the hash table for html docs


;; feel free to comment or request/provide features, feedback is always welcome
;;
;;                         Stef        
;;                              hepta@zogotounga.net  

;; --------
;; last modified January 8, 2002

;; code:

;;;-----------------------------------------------------------------------------------
;;; customization group


(defgroup csound-doc nil
  "Utilities for on-line Csound documentation"
  :group 'convenience
  :prefix "csdoc-")

(defcustom csdoc-use-html nil
  "Install items in the Csound/Orc menu for browsing HTML documentation."
  :type 'boolean
  :group 'csound-doc)

(defcustom csdoc-use-w3 nil
  "Weither w3 should be invoked to browse the HTML docs.\nIf nil, the default external browser is used."
  :type 'boolean
  :group 'csound-doc)

(defcustom csdoc-html-directory "c:/Csound/html"
  "Directory containing the HTML version of Csound docs"
  :type 'directory
  :group 'csound-doc)

(defcustom csdoc-html-entry-point "c:/Csound/html/manual.htm"
  "The starting page of your choice for browsing the HTML docs"
  :type 'file
  :group 'csound-doc)

(defcustom csdoc-use-text t
  "Install items in the Csound/Orc menu for browsing text documentation"
  :type 'boolean
  :group 'csound-doc)

(defcustom csdoc-directory "/usr/local/doc/csound-txt"
  "Directory containing the pretty printed version of Csound docs\n(this should be a bunch of .txt files)"
  :type 'directory
  :group 'csound-doc)



;;;-----------------------------------------------------------------------------------
;;; adding to John ffitch's csound-orc major mode menu


(add-hook 'csound-orc-mode-hook 'csound-doc-install)

(defun csound-doc-install ()
  (define-key csound-orc-mode-map [menu-bar csound-orc docseparator]
    '("--"))
  (if csdoc-use-text
      (progn
	(define-key csound-orc-mode-map [menu-bar csound-orc Insert-Opcode-Template]
	  '("Insert Opcode Template" . csdoc-insert-opcode-template))
	(define-key csound-orc-mode-map [menu-bar csound-orc Document-Opcode]
	  '("Document Opcode" . csdoc-document-opcode))))
  (if csdoc-use-html
      (progn
	(define-key csound-orc-mode-map [menu-bar csound-orc Insert-Opcode-Html-Template]
	  '("Insert Opcode Template" . csdoc-insert-opcode-html-template))
	(define-key csound-orc-mode-map [menu-bar csound-orc HTML-Document-Opcode]
	  '("Document Opcode" . csdoc-html-document-opcode))
	(define-key csound-orc-mode-map [menu-bar csound-orc Fetch-HTML-Doc]
	  '("Browse HTML Documentation" . csdoc-browse-html)))))




;;;-----------------------------------------------------------------------------------
;;; for docs in text format

(defun csdoc-scan-txt-files ()
  (let ((table (makehash 'equal)))
    (dolist (txt-file (directory-files csdoc-directory t "\\.txt"))
      (with-temp-buffer
	(insert-file txt-file)
	(beginning-of-buffer)
	(if (re-search-forward "^Name" () t)
	    (while (null (bobp))
	      (forward-line -1)
	      (dolist (opcode (split-string (thing-at-point 'line) "[ ,_\t\n]"))
		(if (null (equal opcode ""))
		    (puthash opcode txt-file table)))))))
    table))

(defvar csdoc-opcodes (csdoc-scan-txt-files)
  "Hash table associating each documented opcode to its file")

(defun csdoc-refresh-info ()
  (interactive)
  (setq csdoc-opcodes (csdoc-scan-txt-files)))

(defun csdoc-document-opcode (opcode)
  "Pop-up the documentation related to opcode at point"
  (interactive "P")
  (if (equal opcode nil)
      (setq opcode (read-from-minibuffer
		    "Opcode: "
		    (thing-at-point 'word))))
  (let ((source (gethash opcode csdoc-opcodes)))
    (if (and source
	     (file-readable-p source))
	(progn
	  (pop-to-buffer (get-buffer-create "*Csound Documentation*"))
	  (setq buffer-read-only ())
	  (erase-buffer)
	  (insert-file source)
	  (setq buffer-read-only t)))))
    
(defun csdoc-insert-opcode-template (opcode)
  "Insert the documentation template related to opcode at point\nwarning:this replaces the line at point !"
  (interactive "P")
  (if (equal opcode nil)
      (setq opcode (read-from-minibuffer
		    "Opcode: "
		    (thing-at-point 'word))))
  (let ((source (gethash opcode csdoc-opcodes)))
    (if (and source
	     (file-readable-p source))
	(let ((opcode-template ""))
	  (with-temp-buffer
	    (insert-file source)
	    (beginning-of-buffer)
	    (if (and (re-search-forward "^Syntax" () t)
		     (search-forward (concat " " opcode " ") () t))
		(setq opcode-template (thing-at-point 'line))))
	  (beginning-of-line)
	  (kill-line)
	  (insert opcode-template)))))




;;;-----------------------------------------------------------------------------------
;;; for HTML docs:

(defun csdoc-browse-html ()
  "Browse the HTML documentation\n(starting with the file pointed at by csdoc-html-entry-point)"
  (interactive )
  (if csdoc-use-w3
      (w3-open-local csdoc-html-entry-point)
    (browse-url-of-file csdoc-html-entry-point)))

(defun csdoc-scan-html-files ()
  (let ((table (makehash 'equal)))
    (dolist (sub-dir (directory-files csdoc-html-directory t))
      (if (and (file-directory-p sub-dir)
	       (null (or (equal (substring sub-dir -2) "/.")
			 (equal (substring sub-dir -3) "/.."))))
	  (dolist (html-file (directory-files sub-dir t "\\.htm"))
	    (with-temp-buffer
	      (insert-file html-file)
	      (beginning-of-buffer)
	      (if (re-search-forward "^<h2>" () t)
		  (dolist (opcode (split-string (thing-at-point 'line) "[ ,\t\n/<>]"))
		    (if (null (or (equal opcode "")
				  (equal opcode "h2")))
			(puthash opcode html-file table))))))))
    table))

(defvar csdoc-html-opcodes (csdoc-scan-html-files)
  "Hash table associating each documented opcode to its file")

(defun csdoc-refresh-html ()
  (interactive)
  (setq csdoc-html-opcodes (csdoc-scan-html-files)))

(defun csdoc-html-document-opcode (opcode)
  "Pop-up the HTML documentation related to opcode at point"
  (interactive "P")
  (if (equal opcode nil)
      (setq opcode (read-from-minibuffer
		    "Opcode: "
		    (thing-at-point 'word))))
  (let ((source (gethash opcode csdoc-html-opcodes)))
    (if (and source
	     (file-readable-p source))
	(if csdoc-use-w3
	    (w3-open-local source)
	  (browse-url-of-file source)))))
    
(defun csdoc-insert-opcode-html-template (opcode)
  "Insert the documentation template related to opcode at point\nwarning:this replaces the line at point !"
  (interactive "P")
  (if (equal opcode nil)
      (setq opcode (read-from-minibuffer
		    "Opcode: "
		    (thing-at-point 'word))))
  (let ((source (gethash opcode csdoc-html-opcodes)))
    (if (and source
	     (file-readable-p source))
	(let ((opcode-template ""))
	  (with-temp-buffer
	    (insert-file source)
	    (beginning-of-buffer)
	    (if (search-forward (concat "<strong>" opcode "</strong>") () t)
		(setq opcode-template 
		      (replace-regexp-in-string "</*strong>" ""
						(thing-at-point 'line)))))
	  (beginning-of-line)
	  (kill-line)
	  (insert opcode-template)))))



;;;-----------------------------------------------------------------------------------
;;; this is it.

(provide 'csound-doc)

;; csound-doc.el ends here
