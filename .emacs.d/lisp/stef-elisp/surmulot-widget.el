;;; -*- auto-recompile: t -*-

;; This file is not part of GNU Emacs.
;; 
;; surmulot-widget.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; surmulot-widget.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;-------------
;;             Surmulot widgets (µO morphs) 

;; last modified November 25, 2010
;; for Emacs 23
;;; --------------------------------------------------------------------------

(require 'cl)
(require 'timidity-cfg)

;;; ==========================================================================
;;;              widget creation
;;; ==========================================================================

(defstruct surmulot-widget
  "A surmulot widget, that is a Squeak Morph connected via TCP/IP to Emacs."
  name
  port
  process
  buffer
  muo-version 
  (insert-function 'surmulot-widget-default-insert-function)
  (eval-function 'surmulot-widget-default-eval-function)
  (eval-in-csd-function 'surmulot-widget-default-eval-in-csd-function)) 

;; (defcustom surmulot-squeak-vm (squeak-tcpip-launch-command)
;;   "Squeak VM for Surmulot"
;;   :type 'string
;;   :group 'spfa-paths
;;   :group 'surmulot-widget)

(defun surmulot-squeak-vm ()
  (expand-file-name (substitute-in-file-name (squeak-tcpip-vm))))

(defgroup surmulot-widget nil
  "Squeak widgets for Emacs"
  :group 'surmulot)

(defcustom surmulot-squeak-widget-image "$SURMULOTDIR/squeak/muO.image"
  "Squeak image to start when requesting a widget.
Do not use this variable: use function `surmulot-squeak-widget-image' instead."
  :type 'string
  :group 'spfa-paths
  :group 'surmulot-widget)

(defcustom surmulot-squeak-widget-use-latest-image-p nil
  "When not nil, use the latest version of `surmulot-squeak-widget-image'"
  :type 'boolean
  :group 'surmulot-widget)

(defcustom surmulot-squeak-widget-prefers-connected-image-p nil
  "When not nil, always use the currently connected Squeak image
\(mostly useful for widget development)"
  :type 'boolean
  :group 'surmulot-widget)

(defun surmulot-squeak-widget-image ()
  "Return the Squeak image to start when requesting a widget."
  (let* ((connected-image
	  (when (and surmulot-squeak-widget-prefers-connected-image-p
		     (squeak-connected-somehow-p))
	    (squeak-do$ '(SmalltalkImage current imageName))))
	 (image-name (or connected-image
			 surmulot-squeak-widget-image))
	 (candidate (if surmulot-squeak-widget-use-latest-image-p
			(squeak-latest-image image-name)
		      (expand-file-name 
		       (substitute-in-file-name image-name)))))
    (if (and (not (string= candidate ""))
	     (file-exists-p candidate))
	candidate
      (let ((image (concat (expand-file-name 
			    (substitute-in-file-name 
			     (file-name-sans-extension 
			      squeak-tcpip-default-image)))
			   ".image")))
	(if surmulot-squeak-widget-use-latest-image-p
	    (squeak-latest-image image) image)))))

;(surmulot-squeak-widget-image)

(defcustom surmulot-squeak-reserved-ports-start 6000
  "TCP/IP local port numbers for widgets start at this value upward"
  :type 'integer
  :group 'surmulot-widget)

(defcustom surmulot-widget-width 650
  "Default width in pixels for a widget."
  :type 'integer
  :group 'surmulot-widget)

(defcustom surmulot-widget-height 450
  "Default height in pixels for a widget."
  :type 'integer
  :group 'surmulot-widget)

(defun surmulot-make-widget (command &optional width height)
  "Have Squeak evaluate COMMAND, which should return a Morph,
and return a surmulot-widget.
WIDTH and HEIGHT optionally set the Morph geometry \(in pixels)."
  (let* ((port (surmulot-new-squeak-widget-port))
	 (processes (process-list))
	 (scommand
	  (format 
	   "%s %s free doIt \"%s\" &"
	   (shell-quote-argument (surmulot-squeak-vm))
	   (shell-quote-argument (surmulot-squeak-widget-image))
	   (base64-encode-string
	    (format "MuODeployer makeWidget: (%s) extent: (%s@%s) port: %s" 
		    command
		    (or width surmulot-widget-width) 
		    (or height surmulot-widget-height)
		    port) t)))) 
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
    (surmulot-connect-to-widget-at-port port)))

(defun surmulot-connect-to-widget-at-port (port)
    (let ((widget (make-surmulot-widget 
		   :process (squeak-tcpip-connect port)
		   :port port)))
      (setf (surmulot-widget-muo-version widget)
            (squeak-eval-with-return "MuO version" port))
      (surmulot-widget-delete-output widget)
      widget))

(defun surmulot-reconnect-to-lone-widget-at-port (&optional port)
  (interactive "nPort number: ")
  (let ((widget (surmulot-connect-to-widget-at-port port)))
    (add-to-list 'surmulot-widgets-list widget)
    (setf (surmulot-widget-name widget)
	  (squeak-eval-with-return "Widget" port))
    (setf (surmulot-widget-buffer widget) (current-buffer))))

(defun surmulot-new-squeak-widget-port ()
  (let ((used-ports (surmulot-squeak-active-ports)))
    (loop for port = surmulot-squeak-reserved-ports-start then (1+ port)
	  if (not (member port used-ports)) return port)))

(defun surmulot-squeak-active-ports ()
  (save-excursion
    (loop for b in (buffer-list)
          do (set-buffer b)
          if (processp squeak-tcpip-process)
          collect (squeak-port-for-process squeak-tcpip-process))))

;; (surmulot-make-widget "MuO EnvelopeEditor")

(defcustom muo-docbooks nil
  "A list of available documentation from muO"
  :type '(repeat string)
  :group 'surmulot-widget)

(defun muo-docbooks ()
  (setq muo-docbooks
	(delete-if
	 (lambda (s) 
	   (or (string-match "test" s) (string-match "example" s)))
	 (read (squeak-do$ '(Surmulot docBooks))))))

(defun muo-doc (&optional reference)
  (interactive 
   (list (completing-read "Open documentation: " (or muo-docbooks
						     (muo-docbooks)))))
  (surmulot-make-widget 
   (format "MuO openDocumentation: #'%s'" reference)))

; (muo-doc "Musical notes and phrases")


;;; ===========================================================================
;;;              OSC controls 
;;; ===========================================================================

(defcustom surmulot-OSC-controls-source 
  "$SURMULOTDIR/csound/include/surmulot-OSC.orc"
  "Csound orchestra code file defining the available OSC controls as macros"
  :type 'string
  :group 'spfa-paths
  :group 'surmulot-widget)

(defun surmulot-OSC-controls ()
  "Return a list of all defined OSC controls macros.
Do this by scanning the file pointed by `surmulot-OSC-controls-source'"
  (interactive)
  (condition-case nil
      (setq surmulot-OSC-controls
	    (with-temp-buffer
	      (insert-file-contents 
	       (substitute-in-file-name surmulot-OSC-controls-source))
	      (loop while (re-search-forward "#define +\\(.*\\)(" nil t)
		    collect (match-string-no-properties 1))))
    (error nil)))

(defvar surmulot-OSC-controls (surmulot-OSC-controls)
  "list of all defined OSC controls macros")

(defun surmulot-widget-OSC-controls()
  "Create a widget with all OSC controls defined in the current buffer
\(assumed to be a CSD file)"
  (interactive)
  (surmulot-widget-make-OSC-controls (cscsd-get-OSC-controls)))

;;(add-hook 'csound-csd-before-process-hook 'surmulot-widget-OSC-controls) 

(defun surmulot-widget-make-OSC-controls (controls)
  (surmulot-attach-widget
   (surmulot-widgets-add 
    "Controls"
    (concat "OSCControl csoundControlPanel: #("
	    (mapconcat (lambda (c)
			 (concat "("
				 (mapconcat (lambda (p)
					      (concat "'" p "'"))
					    c " ")
				 ")"))
		       controls " ")
	    ") forBuffer: '"
	    (buffer-name) "'"))))

(defun cscsd-get-OSC-controls ()
  "Return a list of all OSC controls defined in the current buffer
\(assumed to be a CSD file)
A control is a macro invocation of form $NAME(p1'p2'p3) where NAME
must be a member of the list returned by `surmulot-OSC-controls'
The returned list form is \((NAME: p1 p2 p3) ...)"
  (when (cscsd-buffer-is-a-csd-p)
    (let (controls (all-controls surmulot-OSC-controls))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*$\\([A-Z]*\\)[ \t]*(\\(.*\\))" nil t)
	  (let ((control (match-string-no-properties 1))
		(parameters (split-string (match-string-no-properties 2) "'")))
	    (when (member control all-controls )
	      (add-to-list 'controls 
			   `(,(concat control ":") ,@parameters))))))
      controls)))

;;; ===========================================================================
;;;              high-level functions
;;; ===========================================================================

(defvar surmulot-widgets-list '()
  "Maintains the list of widgets accessible from the \"Widgets\" menu.")   

(defun buffer-surmulot-widgets (&optional buffer)
  "Returns a list of all Surmulot widgets attached to BUFFER.
If BUFFER is nil, consider the current buffer."
  (remove-if
   (lambda (w) (not (eq (surmulot-widget-buffer w)
			(or buffer (current-buffer)))))
   surmulot-widgets-list))

(defun surmulot-widget-kill-buffer-query-function ()
  (or (null (buffer-surmulot-widgets (current-buffer)))
      (yes-or-no-p "Buffer has attached widgets. Kill anyway? ")))

(add-hook 'kill-buffer-query-functions
	  'surmulot-widget-kill-buffer-query-function)



;;; ===========================================================================
;;;              Squeak side protocol
;;; ===========================================================================

(defmacro surmulot-widget-define (symb)
  "Define a function 'surmulot-widget-SYMB used by Squeak 
to invoque the function in a widget SYMB slot."
  `(defun ,(intern (concat "surmulot-widget-" (prin1-to-string symb)))
     (&rest args)
     ,(format 
       "This is a private function intended to be called from Squeak. 
No public usage.
It is evaluated in the comint buffer associated to a given WIDGET: 
it calls the function returned by \(surmulot-widget-%s-function WIDGET) 
with arguments ARGS.
That function is defined by setting the '%s slot in the surmulot-widget 
structure."
       symb symb)
     (let ((widget (find-if (lambda (w) 
			  ;; w: widget associated to the current comint buffer
			  (= (surmulot-widget-port w)
			     (squeak-port-for-process squeak-tcpip-process)))
			surmulot-widgets-list)))
       (apply
	(funcall ',(intern (concat "surmulot-widget-" 
				   (prin1-to-string symb) "-function")) 
		 widget)
	widget args))))

(surmulot-widget-define insert)
(surmulot-widget-define eval)
(surmulot-widget-define eval-in-csd)
; ... more here later

(defmacro surmulot-widget-with-proper-buffer-do (&rest body)
  "Evaluate BODY with a \"proper\" current buffer. 
More precisely: do not use the actual current buffer if it is not displayed
in any window or if it is not visiting a file."
  `(with-current-buffer 
       (car (remove-if-not (lambda (b)
			     (and (buffer-file-name b)
				  (get-buffer-window b t))) (buffer-list)))
     ,@body))

(defun surmulot-widget-default-insert-function (w str)
  "Insert STR in a proper buffer in behalf of widget W"
  (surmulot-widget-with-proper-buffer-do
   (insert str)))

(defun surmulot-widget-exclusive-insert-function (w str)
  "Insert STR in the buffer associated to widget W"
  (with-current-buffer
      (surmulot-widget-buffer w)
   (insert str)))

(defun surmulot-widget-default-eval-function (w quoted-form)
  "Evaluate QUOTED-FORM in a proper buffer in behalf of widget W"
  (surmulot-widget-with-proper-buffer-do
   (surmulot-widget-eval-quoted-form quoted-form)))

(defun surmulot-widget-exclusive-eval-function (w quoted-form)
  "Evaluate QUOTED-FORM in the buffer associated to widget W"
  (with-current-buffer
      (surmulot-widget-buffer w)
    (surmulot-widget-eval-quoted-form quoted-form)))

(defun surmulot-widget-in-csd-p (w)
  "Tells weither the current proper buffer for widget W is a CSD"
  (surmulot-widget-with-proper-buffer-do
    (cscsd-buffer-is-a-csd-p)))

(defun surmulot-widget-default-eval-in-csd-function (w quoted-form)
  "Evaluate QUOTED-FORM in a proper buffer in behalf of widget W,
if that buffer is a CSD, else return 'not-a-csd"
  (surmulot-widget-with-proper-buffer-do
   ;; if attached buffer is a CSD, use it
   (when (cscsd-buffer-is-a-csd-p (surmulot-widget-buffer w))
     (set-buffer (surmulot-widget-buffer w)))
   (if (not (cscsd-buffer-is-a-csd-p))
       'not-in-csd
     (unwind-protect
     	 (surmulot-widget-eval-quoted-form quoted-form)
       ;; (csound-csd-mode) ; was overkill...
       (when cscsd-use-MMM (mmm-parse-buffer))))))


(defun surmulot-widget-eval-quoted-form (quoted-form)
  (let ((val (eval quoted-form)))
    (if (stringp val)
	(encode-coding-string val 'undecided-mac)
      val)))
  

(defun surmulot-attach-widget (widget &optional buffer)
  "Redefine the functions associated to WIDGET so that they operate
in the context of the buffer from which WIDGET was first invoked.
If BUFFER is not nil, also redefine that buffer"
  (setf (surmulot-widget-insert-function widget)
	'surmulot-widget-exclusive-insert-function
	(surmulot-widget-eval-function widget)
	'surmulot-widget-exclusive-eval-function)
  (if buffer
      (setf (surmulot-widget-buffer widget) 
	    (or (buffer-base-buffer buffer) buffer))))


;;; ===========================================================================
;;;              high-level functions & Widgets menu
;;; ===========================================================================

(defcustom surmulot-widget-specs 
  '(("Envelope Editor" "EnvelopeEditor new")
;    ("Score Editor" "CsoundCompositionEditor new" )
    ("Phrase Editor" "MusicalPhraseEditor new")
    ("Time-line Workshop" "TimeLineWorkshop openNewMusicalPhraseEditor")
    ("Time-line & drums Workshop" "TimeLineWithDrumsWorkshop openNew")
    ("Csound Blocks Book" "MuO CsoundBlocksWidget")
    ("Function Editor" "NFunctionEditor new")
;    ("Musical phrase scratchpad" "MusicalScratchPad newMode: Mode major")
) 
  "List of registered widgets \(\(label constructor) ... ) 
where label is a string identifying the widget type and constructor is the Smalltalk code instanciating the widget Morph"
  :type '(repeat (list (string :tag "label") (string :tag "constructor")))
  :group 'surmulot-widget)

(defun surmulot-widgets-add (name command)
  (let ((widget (surmulot-make-widget "Morph new color: Color transparent")))
    (add-to-list 'surmulot-widgets-list widget)
    (setf (surmulot-widget-name widget) name)
    (setf (surmulot-widget-buffer widget) (current-buffer)) 
    (surmulot-widget-do widget (format "Surmulot widget: (%s)" command))
    (surmulot-widget-delete-output widget)
    widget))


(defun surmulot-define-widgets-menu ()
  (easy-menu-define surmulot-widgets-menu global-map "Squeak widgets"
    (surmulot-make-widgets-menu)))

(defalias 'surmulot-widgets-update-menu 'surmulot-define-widgets-menu)

(defun surmulot-make-widgets-menu ()
  (let ((stored-widgets (surmulot-buffer-widgets))
	(active-widgets surmulot-widgets-list) 
	(controls (cscsd-get-OSC-controls)))
    `("Widgets"
      ("New widget"
       ,@(mapcar 
          (lambda (spec) `[,(car spec) (surmulot-widgets-add ,(car spec) ,(cadr spec)) t])
          surmulot-widget-specs))
      ,@(when controls 
	  '(["Show OSC controls" surmulot-widget-OSC-controls t]))
      ,@(when stored-widgets
	  (list 
	   (append
	    '("Stored widgets")
	    '(["Restore a widget" surmulot-widget-restore t]
	      "--")
	    (mapcar 
	     (lambda (label) `[,label (surmulot-widget-restore ,label) t])
	     stored-widgets))))
      ,@(when active-widgets '("--"))
      ,@(mapcar 
	 (lambda (widget)
           `(,(format "%s (port %d)" (surmulot-widget-name widget) (surmulot-widget-port widget))
	     ["store" (surmulot-widget-store ,widget) (cscsd-buffer-is-a-csd-p)]
	     ["interact" (surmulot-widget-interact ,widget) t]
	     ["clone" (surmulot-widget-clone ,widget) t]
	     "--"
	     [,(format "attached to \"%s\""
		      (buffer-name (surmulot-widget-buffer widget))) 
	      (switch-to-buffer-other-frame (surmulot-widget-buffer ,widget))
	      (not (eq (current-buffer) (surmulot-widget-buffer ,widget)))]
	     ["attach to current buffer"
	      (surmulot-attach-widget ,widget (current-buffer))
	      (not (eq (current-buffer) (surmulot-widget-buffer ,widget)))]
	     "--"
             ["send to muO" (surmulot-widget-send-to-muo ,widget) t]
	     "--"
             ["close" (surmulot-widget-close ,widget) t]))
         active-widgets)
      "--"
      ["Build from active image" 
       (setq surmulot-squeak-widget-prefers-connected-image-p 
	     (not surmulot-squeak-widget-prefers-connected-image-p))
	     :style toggle
	     :selected surmulot-squeak-widget-prefers-connected-image-p]
      ["Build from image latest version" 
       (setq surmulot-squeak-widget-use-latest-image-p
	     (not surmulot-squeak-widget-use-latest-image-p))
	     :style toggle
	     :selected surmulot-squeak-widget-use-latest-image-p]
      "--"
      ["Re-connect to lone widget" 
       surmulot-reconnect-to-lone-widget-at-port t])))

(defun surmulot-widget-close (&optional widget-or-port)
  (let* ((port (if (surmulot-widget-p widget-or-port)
		  (surmulot-widget-port widget-or-port)
		widget-or-port))
	 (widget (if (surmulot-widget-p widget-or-port)
		     widget-or-port
		   (find-if (lambda (w)
			      (= port (surmulot-widget-port w)))
			    surmulot-widgets-list))))
    (condition-case nil
	(squeak-eval-no-return "MuO closeWidget" port)
      (error nil))
    (setq surmulot-widgets-list
	  (remove widget surmulot-widgets-list))
    (condition-case nil
	(kill-process (surmulot-widget-process widget))
      (error nil))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer (squeak-comint-buffer port)))))

(defun surmulot-widget-do (widget command)
  (squeak-eval-with-return command (surmulot-widget-port widget)))

(defun surmulot-widget-interact (widget)
  (switch-to-buffer-other-window
   (squeak-comint-buffer (surmulot-widget-port widget))))

(defun surmulot-widget-clone (widget)
  (let ((name (surmulot-widget-name widget))
	(str (surmulot-widget-do
	      widget
	      "Widget biggerSurmulotWidget asEncodedString asOneLine")))
    (surmulot-widgets-add name (format "%s reconstituteObject" str))
    (surmulot-widget-delete-output widget)))

(defun surmulot-widget-delete-output (widget)
  "Delete all output from widget since last input.
Does not delete the prompt."
  (interactive)
  (save-excursion
    (set-buffer (squeak-comint-buffer (surmulot-widget-port widget)))
    (let ((proc (get-buffer-process (current-buffer)))
	  (inhibit-read-only t))
      (let ((pmark (progn (goto-char (process-mark proc))
			  (forward-line 0)
			  (point-marker))))
	(delete-region comint-last-input-end pmark)))
	(goto-char (point-max))))

(defun surmulot-widget-send-to-muo (widget)
  (let 
      ((str 
	(surmulot-widget-do
	 widget
	 "Widget veryDeepCopy biggerSurmulotWidget asEncodedString asOneLine")))
    (surmulot-widget-delete-output widget)
    (muo-add-morph
     (format "%s reconstituteObject quitBehavingAsSurmulotWidget" str))))


(defun surmulot-widget-on-port (port)
  (find-if (lambda (w)
	     (= port (surmulot-widget-port w)))
	   surmulot-widgets-list))

(defun surmulot-widget-rename (port name)
  "ReNAME the widget associatd to PORT"
  (setf (surmulot-widget-name (surmulot-widget-on-port port)) name))

(defun surmulot-widget-store (widget)
  "Store the widget NAME as an XML area in the source buffer"
  (let ((label (read-from-minibuffer "Store widget under label: ")) 
	(name (surmulot-widget-name widget))
	(str (surmulot-widget-do
	      widget 
	      "Widget biggerSurmulotWidget asEncodedString asOneLine")))
    (save-restriction
      (widen)
      (set-buffer (or (buffer-base-buffer (current-buffer)) (current-buffer)))  
      (when (or (cscsd-buffer-is-a-csd-p)
		(y-or-n-p "Buffer does not seem to be a valid CSD. Continue ?"))
	(save-excursion
	  (cscsd-go-after-cssynth)
	  (insert "\n\n<WIDGET>" label ?\n) 
	  (insert name ?\n str)
	  (insert "</WIDGET>"))
	(cscsd-hide-storage-areas)))))

(defun surmulot-buffer-widgets ()
  (when (cscsd-buffer-is-a-csd-p)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let ((wlist))
	  (while (re-search-forward "<WIDGET>\\(.*\\)$" nil t)
	    (add-to-list 'wlist (match-string-no-properties 1) t))
	  wlist)))))

(defun surmulot-widget-restore (&optional label)
  (interactive 
   (list (completing-read "Restore widget: " (surmulot-buffer-widgets))))
  (let* (wid
	 (serialized-widget
	 (save-excursion
	   (save-restriction
	     (widen)
	     (goto-char (point-min))
	     (when (re-search-forward (concat "<WIDGET>" label "$") nil t)
	       (re-search-forward "^.*$")
	       (setq wid (match-string 0))
	       (buffer-substring-no-properties 
		(1+ (point))
		(- (search-forward "</WIDGET>") 9)))))))
    (when serialized-widget
      (surmulot-widgets-add 
       wid
       (format "%s reconstituteObject" serialized-widget)))))

(surmulot-define-widgets-menu)
(add-hook 'menu-bar-update-hook 'surmulot-widgets-update-menu)


;;; used by Csound Blocks:
(defun cscsd-replace-instrument (code)
  ""
  (condition-case nil
      (save-excursion
	(let ((instr (progn (string-match "instr[ \t]+\\([^ \t;]+\\)" code)
			    (match-string 1 code))))
	  (goto-char (cscsd-beginning-of-instrument instr))
	  (delete-region (cscsd-beginning-of-instrument instr)
			 (cscsd-end-of-instrument instr))
	  (insert code)))
    (error nil)))


;;; --------------------------- End  -----------------------------

(provide 'surmulot-widget)

;;; surmulot-widget.el ends here







