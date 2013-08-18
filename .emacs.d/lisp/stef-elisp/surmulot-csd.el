;;; -*- auto-recompile: t -*-

;; This file is not part of GNU Emacs.
;; 
;; surmulot-csd.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; surmulot-csd.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;-------------
;;             Surmulot integration for CSD buffers 

;; last modified November 19, 2010
;;; --------------------------------------------------------------------------


(defalias 'muo-do 'squeak-eval) 

; (muo-get "'a,e,f' kmusic asBox")
(defun muo-get (object)
  (squeak-do (format "SurmulotManager emacsSend: (%s)" object)))

(defun muo-add-morph (object)
  (squeak-do (format "(%s) openInWorld" object)))

(defun muo-edit-string (encoded-string rid mode label)
  "Decode ENCODED-STRING and create a buffer to edit it
RID is a numeric handle allowing muO to know what to do with the edited string
MODE is the major mode the edit buffer should be in
LABEL is the name for that buffer

This function is required by EmacsInteraction in muO"
  (save-excursion
    (switch-to-buffer-other-frame
     (generate-new-buffer label))
    (erase-buffer)
    (insert (squeak-decode encoded-string))
    (goto-char (point-min))
    (condition-case nil
	(funcall mode)
      (error nil))
    (set (make-local-variable 'receiver-id) rid)
    (setq header-line-format
	  (list
	   (squeak-console-button-do 
	    " Accept " "accept the changes"
	    (squeak-do
	     (format "EmacsInteraction updateString: '%s' receiverId: %s " 
		     (squeak-encode-buffer)
		     receiver-id))
	    (set-buffer-modified-p nil))
	   " "
	   (squeak-console-button-do 
	    " Explore receiver " "open an ObjectExplorer in Squeak"
	    (squeak-do 
	     (format "(EmacsInteraction msDict at: %s) explore" receiver-id)))
	   " "
	   (squeak-console-button-do 
	    " Accept and exit " "accept the changes and kill this buffer"
	    (squeak-do 
             (format "EmacsInteraction updateString: '%s' receiverId: %s cleanUp: true" 
                     (squeak-encode-buffer)
		     receiver-id))
	    (let ((f (selected-frame)))
	      (when (and (kill-buffer (current-buffer))
			 (eq f (selected-frame)))
		(delete-frame))))
	   " "
	   (squeak-console-button-do 
	    " Exit " "kill this buffer and its frame"
	    (squeak-do 
	     (format "EmacsInteraction cleanUpReceiverId: %s" receiver-id))
	    (let ((f (selected-frame)))
	      (when (and (kill-buffer (current-buffer))
			 (eq f (selected-frame)))
		(delete-frame))))))
    (set-buffer-modified-p nil)
    ;;; making the buffer persistent across sessions
    (setq desktop-save-buffer 
	  `(lambda (sdir)
	    (list (squeak-encode 
		   (buffer-substring-no-properties (point-min) (point-max)))
		  ,rid ',mode ,label)))))

;==
(defun surmulot-restore-buffer (desktop-buffer-file-name
				   desktop-buffer-name
				   desktop-buffer-misc)
  "restore a buffer editing a string in behalf of muO"
  (if desktop-buffer-misc
      (apply 'muo-edit-string desktop-buffer-misc)
    (desktop-restore-file-buffer desktop-buffer-file-name
				 desktop-buffer-name
				 desktop-buffer-misc)))
				 
(mapc (lambda (mode)
	(add-to-list 'desktop-buffer-mode-handlers
		     (cons mode 'surmulot-restore-buffer)))
      '(text-mode csound-csd-mode csound-orc-mode csound-sco-mode))
;==

(defun muo-get-sco ()
  "send to muO the appropriate MGraphElement for representing the current score"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (and filename (string-match "\\.sco$" filename))
        (muo-get
	 (format "MGraphMusicFile newFromFile: '%s'"
		 (squeak-filename filename)))
      (muo-get 
       (format "MGraphCsoundScore newFromB64: '%s'" 
	       (if (cscsd-buffer-is-a-sco-p)
		   (squeak-encode-region (cscsd-sco-beginning) (cscsd-sco-end))
		 (squeak-encode-buffer)))))))

(defun muo-get-orc ()
  (interactive)
  "send to muO the appropriate MGraphElement for representing the current orchestra"
  (let ((filename (buffer-file-name)))
    (if (and filename (string-match "\\.orc$" filename))
        (muo-get (format "MGraphCsoundComposer newFromFile: '%s'" (squeak-filename filename)))
      (muo-get (format "MGraphCsoundComposer newFromB64: '%s'" 
                          (if (cscsd-buffer-is-an-orc-p)
                              (squeak-encode-region (cscsd-orc-beginning) (cscsd-orc-end))
                            (squeak-encode-buffer)))))))

(defun muo-get-patch (bank program name)
  (muo-get 
   (format "MGraphCode new code: '%s'; label: '%s'" 
	   (format "\r
    value _ MusicalPhrase bank: %s program: %s\r
                      channel: (box1 ifNil: [1] ifNotNil: [box1 output ifNil: [1]])"
		   bank program)
	   name)))

(defun muo-get-csd ()
  (interactive)
  "Send to muO the appropriate MGraphElement for representing the current csd"
  (let ((filename (buffer-file-name (buffer-base-buffer))))
    (if (and (not (buffer-modified-p))
	     filename
	     (string-match "\\.csd$" filename))
        (muo-get (format "(CSDFile named: '%s') asBox" 
			 (squeak-filename filename)))
      (muo-get (format "MGraphCsoundComposition newFromB64: '%s'" 
			  (squeak-encode-buffer))))))


(defun surmulot-eval-in-csd (quoted-form)
  "Evaluate QUOTED-FORM in the current buffer if that buffer is a CSD, 
else return 'not-a-csd"
  (surmulot-widget-default-eval-in-csd-function nil quoted-form))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ experimental


(defvar cscsd-instrument-format-template 
  "/*| 
 ((pitchField: nil)
  (pitchFormat: nil)
  (pitchInflexionField: nil)
  (ampField: nil)
  (ampMin:max: (0 1))
  (panField: nil)
  (panMin:max: (-1 1))
  (iTemplate: nil))
|*/")

(defun cscsd-get-format (&optional csd-file)
  "Reads all instrument formats in CSD-FILE or in current CSD buffer."
  (if csd-file
      (with-temp-buffer
	(insert-file-contents csd-file)
	(cscsd-get-format))
    (with-base-buffer
      (append `((header: ,(cscsd-get-score-header)))
	      ;; intruments formats & effects
	      (loop for i in (cscsd-instruments)
		    do (goto-char (cscsd-beginning-of-instrument i))
		    if (re-search-forward "/\\*|[ \t]*effect\\(.*\\)|\\*/" 
					  (cscsd-end-of-instrument i) t)
		    append `((readEffect: ,(match-string-no-properties 1)))
		    into spec
		    if (re-search-forward "/\\*|\\([^;]+\\)|\\*/" 
					  (cscsd-end-of-instrument i) t)
		    ;; support for multiple instrument numbers
		    append (let ((fmt (read (match-string 1))))
			     (mapcar (lambda (ea) (list (read ea) fmt))
				     (split-string i ",")))
;		    append (list (list (read i) (read (match-string 1))))
		    into spec
		    finally return spec)))))

(defun muo-insert-itemplates ()
  ""
  (interactive)
  (save-excursion
    (loop for i in (cscsd-instruments)
	  do (progn
	       (goto-char (cscsd-beginning-of-instrument i))
	       (end-of-line))
	  (insert ?\n cscsd-instrument-format-template ?\n))))

(defun cscsd-get-score-header ()
  ""
  (save-excursion
    (cscsd-goto-sco)
    (condition-case nil
	(buffer-substring-no-properties
	 (re-search-forward "/\\*|[ \t]*header[ \t]*|\\*/" (cscsd-sco-end))
	 (progn
	   (goto-char (cscsd-sco-end))
	   (re-search-backward "/\\*|[ \t]*header[ \t]*end[ \t]*|\\*/")))
      (error ""))))

(defun muo-edit-csd ()
  "Edit the current CSD buffer contents as a CsoundComposition in MuO"
  (interactive)
  (squeak-do 
    (format
     "(CSDFile named: '%s') asCsoundComposition edit"
     (cscsd-make-temp-buffer-for-processing))))

(defun muo-edit-csd-as-widget ()  
  "Edit the current CSD buffer contents in a CsoundCompositionEditor widget.
This is a graphical tool for score composition."
  (interactive)
  (surmulot-attach-widget
   (surmulot-widgets-add
    "Composition Editor"
    (format
     "(CSDFile named: '%s') asCsoundComposition editorMorph"
     (cscsd-make-temp-buffer-for-processing)))))

(defun muo-get-csd-as-composition ()  
  "Send the current CSD buffer contents to muO as a CsoundComposition"
  (interactive)
  (muo-get
   (format "(CSDFile named: '%s') asCsoundComposition asBox"
	   (cscsd-make-temp-buffer-for-processing))))

(defun muo-register-csd-as-synthesizer (&optional address)  
  "Register the current CSD at ADDRESS in the CsoundSynthesizer library.
ADDRESS is a string defining the access path of the new synth, such as
\"my-synths/specials/example soundfont-based synth\"."
  (interactive "sAddress in library:")
  (muo-do
   (format "CsoundSynthesizer libraryAt: #(%s) put: (CSDFile named: '%s')"
	   (mapconcat (lambda (s) (concat "#'" s "'"))
		      (split-string address "/") " ")
	   (cscsd-make-temp-buffer-for-processing))))

(defun surmulot-csd-menu ()
  '(("Surmulot"
     ["Edit in widget"  muo-edit-csd-as-widget t]
     ["Edit in muO"  muo-edit-csd t]
     "--"
     ["Send to muO (as a CSDFile)"  muo-get-csd t]
     ["Send to muO (as a CsoundComposition)"  muo-get-csd-as-composition t]
     "--"
     ["Insert instrument format templates" muo-insert-itemplates t]
     ["Register CSD as synthesizer" muo-register-csd-as-synthesizer t])
    "--"))


(provide 'surmulot-csd)

;;; surmulot-csd.el ends here







