;;; -*- auto-recompile: t -*-

;; This file is not part of GNU Emacs.
;; 
;; squeak-interaction.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; squeak-interaction.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;-------------
;;             interacting with Squeak via TCP/IP

;; last modified April 15, 2005
;; for Emacs 21.1
;;; ---------------------------------------------------------------------------

(defun squeak-connected-somehow-p ()
  (or (squeak-tcpip-process)
      (squeak-connected-via-surman-p)))

(defun squeak-connected-via-surman-p ()
  (and (featurep 'surman)
       (surman-squeak-connected-p)))

(defun squeak-eval (str)
  "Have Squeak evaluate STR as smalltalk code and return the result"
  (if (squeak-connected-via-surman-p)
    (surman-ask-squeak str)
    (squeak-eval-with-return str)))

(defun squeak-do (str)
  "Have Squeak evaluate STR as smalltalk code"
  (if (squeak-connected-via-surman-p)
    (surman-ask-squeak str)
    (squeak-eval-no-return str)))

(defun squeak-filename (filename)     ;; est-ce correct ?
  (if (memq system-type '(windows-nt cygwin))
    (mapconcat 'identity (split-string filename "/") "\\")
    filename))

(defun squeak-decode (string)
  (decode-coding-string (base64-decode-string string) 'utf-8))

(defun squeak-encode (string)
  (base64-encode-string 
   (encode-coding-string string 'undecided-mac)
   t))

(defun squeak-encode-region (&optional beg end)
  (interactive "r")
  (squeak-encode
   (buffer-substring-no-properties beg end)))

(defun squeak-encode-buffer ()
  (squeak-encode-region (point-min) (point-max)))



;;; -------------- Smalltalk as Lisp (not quite done yet)

(defvar smalltalk-selectors '())

(defun smalltalk-register-symbols (symbols)
  "TO DO: provide a way to have Squeak be aware of which classes
and selectors are used in Emacs."
  (add-to-list 'smalltalk-selectors symbols t))

(defun $ (smalltalk-code)
  "Convert list SMALLTALK-CODE into a string, 
register all selectors in variable 'smalltalk-selectors"
  (loop for token in smalltalk-code
	with symbols = '()
	collect (case (type-of token)
		  ('cons (format "(%s)" ($ token)))
		  ('vector (format "[%s]" ($ (append token nil))))
		  ('string (format "'%s'" token))
		  ('symbol (add-to-list 'symbols token 't)
			   (symbol-name token))
		  (otherwise (prin1-to-string token)))     
	into form
	finally do (smalltalk-register-symbols symbols)
	finally return (mapconcat 'identity form " ")))


(defun squeak-do$ (smalltalk-code)
  (declare (indent 0))
  (let* ((sfe-code ($ `(,smalltalk-code printStringForEmacs)))
	 (val (substring (squeak-eval sfe-code) 1 -1)))
    (condition-case nil
	(read val)
      (error val))))

;(squeak-do$ '(nil ufNil: [paf]))

(defun muo-get$ (object)
  (squeak-do$ `(SurmulotManager emacsSend: ,object)))

(defun muo-add-morph$ (object)
  (squeak-do$ `(,object openInWorld)))

'(examples

;TEST (squeak-do$ '(Morph new position: 5@6 \; center)) => "#<30@26>"
;TEST (squeak-do$ '({1 \. 2 \. 3} second)) => 2
;TEST (squeak-do$ '(\#(1 2 3))) => [1 2 3]
;TEST (squeak-do$ '({6})) => [6]
;TEST (squeak-do$ '({6 \. 7})) => [6 7]
;TEST (squeak-do$ '(\#(1 3) with: \#(-1 -3) collect: [:i :j | i + j])) => [0 0]
;TEST ($ '("bouh")) => "'bouh'"
;TEST (squeak-do$ '(\#beuh)) => 'beuh
;TEST (squeak-do$ '(\#beuh asString)) => "beuh"
;TEST (squeak-do$ `(1 = 1)) => t
;TEST (squeak-do$ `(1 = 2)) => nil
;TEST (squeak-do$ `(true)) => t
;TEST (squeak-do$ `(false)) => nil
;TEST (squeak-do$ '([:t | t + 1] value: 7)) => 8
;TEST (funcall (lambda (x) (squeak-do$ `(,x > 0 ifTrue: [,x negated]))) 5) => -5
;TEST (squeak-do$ '("beuh" , "gah")) => "beuhgah"

;; troublesome behaviors:

;TEST ($ `("beuh" \, "gah")) => "'beuh'"
;TEST (squeak-do$ '("beuh" ifNuk: [yo])) => 'essageNotUnderstood:
;TEST (squeak-do$ '(\#MessageNotUnderstood:)) => 'MessageNotUnderstood:

;TEST (squeak-do$ '("123\"45" size)) => 6
;TEST (squeak-do$ '(("123\"45" at: 4) asString)) => "\""


(muo-get$ '("c,e,f" kmusic asBox))

(let ((ph "c,e,f"))
  (muo-get$ `(,ph kmusic asBox)))

(muo-add-morph$ '(MusicalPhraseEditor new))

;; >>>>>>>>>>>>>>>>>>>>>> test, to update before trying..

(defun surmulot-widget-restore (&optional label)
  (interactive "sRestore widget: ")
  (let* (wid
	 (serialized-widget
	 (save-excursion
	   (save-restriction
	     (widen)
	     (goto-char (point-min))
	     (when (search-forward (concat "<WIDGET>" label) nil t)
	       (re-search-forward "^.*$")
	       (setq wid (match-string 0))
	       (buffer-substring-no-properties 
		(1+ (1+ (point)))                              ;;; !!!
		(- (search-forward "</WIDGET>") 10)))))))  ;;;  !!!!
    (when serialized-widget
      (surmulot-widgets-add$ 
       wid
       serialized-widget reconstituteObject))))

(defun surmulot-widgets-add$ (name command)
  (let ((widget (surmulot-make-widget$ command)))
    (add-to-list 'surmulot-widgets-list widget)
    (setf (surmulot-widget-name widget) name)
    (setf (surmulot-widget-buffer widget) (current-buffer)) 
    widget))

(defmacro surmulot-make-widget$ (command &optional width height)
  "Have Squeak evaluate COMMAND, which should return a Morph,
and return a surmulot-widget.
WIDTH and HEIGHT optionally set the Morph geometry \(in pixels)."
  `(let* ((port (surmulot-new-squeak-widget-port))
	  (processes (process-list))
	  (scommand
	   (format 
	    "%s %s free doIt \"%s\" &"
	    (shell-quote-argument (surmulot-squeak-vm))
	    (shell-quote-argument (surmulot-squeak-widget-image))
	    (base64-encode-string
	     ($ `(MuODeployer 
		  makeWidget: ,command 
		  extent: ,(or width surmulot-widget-width) 
 @ ,(or height surmulot-widget-height)
		  port: ,port)) t)))) 
     (save-window-excursion
       (with-temp-buffer ;; needed because of the cd below
	 (cd (file-name-directory (surmulot-squeak-vm)))
	 (shell-command scommand
			(generate-new-buffer-name " *surmulot widget*"))))
     ;; giving a dummy sentinel 
     ;; else we are annoyed with longish exit messages
     (condition-case nil
	 (set-process-sentinel 
	  (first (set-difference (process-list) processes :test 'eq))
	  (lambda (process event)))
       (error nil))
    (let* ((proc (squeak-tcpip-connect port))
	   (widget (make-surmulot-widget :port port)))
      (setf (surmulot-widget-muo-version widget)
            (squeak-eval-with-return "MuO version" port))
      (setf (surmulot-widget-process widget) proc)
      (surmulot-widget-delete-output widget)
      widget)))


;; <<<<<<<<<<<<<<<<<<< end test



)


;;; --------------------------- End  -----------------------------

(provide 'squeak-interaction)

;;; squeak-interaction.el ends here







