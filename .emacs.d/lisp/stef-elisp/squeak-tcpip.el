;;; -*- auto-recompile: t -*-

;; This file is not part of GNU Emacs.
;; 
;; squeak-tcpip.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; squeak-tcpip.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;-------------
;;             interacting with Squeak via TCP/IP

;; last modified November 11, 2010
;; for Emacs 23
;;; ---------------------------------------------------------------------------

(require 'cl)

;;; Code:

(defcustom squeak-tcpip-vm "$SURMULOTDIR/squeak/vm/squeak"
  "Shell command used to launch Squeak if necessary"
  :type 'string
  :group 'spfa-paths
  :group 'squeak)

(defun squeak-tcpip-vm ()
  (expand-file-name (substitute-in-file-name squeak-tcpip-vm)))

(defcustom squeak-tcpip-default-image "$SURMULOTDIR/squeak/muO"
  "Default Squeak image to start when none is specified.
It is interpreted as a root name so that latest corresponding versions
can be considered.
See function `squeak-tcpip-default-image' for details"
  :type 'string
  :group 'spfa-paths
  :group 'squeak)

(defcustom squeak-tcpip-prompt "squeak> "
  "Squeak console prompt.
Must have the same value as Squeak class variable promptTCPIP in 
EmacsInteraction"
  :type 'string
  :group 'squeak)

(defcustom squeak-tcpip-init-command "'(Squeak connected)'"
  "Command send to Squeak when the TCPIP console starts.
Its returned value will be the first thing to appear in the console"
  :type 'string
  :group 'squeak)

(defcustom squeak-console-hide-commands t
  "If not nil, the Elisp commands from Squeak are not fully displayed at
the console; only a summary is displayed. If nil, nothing is hidden "
  :type 'boolean
  :group 'squeak)

(defcustom squeak-tcpip-ELisp-error "<evaluation error>"
  "Error message returned by ELisp() when evaluation failed."
  :type 'string
  :group 'squeak)

(defcustom squeak-tcpip-port 5552
  "Default Squeak TCP/IP port in local host. 
Must have the same value as the default value for EmacsInteraction class
variable LocalPort in Squeak"
  :type 'integer
  :group 'squeak)

(defcustom squeak-tcpip-console-is-special t
  "When non-nil, the console buffer is added to special-display-buffer-names"
  :type 'boolean
  :group 'squeak)

(defvar squeak-tcpip-process nil
  "buffer-local: the Squeak interaction TCP/IP process")

(defvar squeak-tcpip-last-evalue nil
  "buffer-local: used by `squeak-eval-with-return' to store the return value
of a Squeak expression")

(defvar squeak-tcpip-accumulated-preoutput ""
  "buffer-local: accumulated incoming strings from Squeak.
This is parsed in squeak-tcpip-preoutput-filter")

(defvar squeak-tcpip-last-output ""
  "buffer-local: last output from the TCP/IP process \(that is, from Squeak")


;;; ---------------------------------------------------------------------------
;;;
;;; connection
;;;
;;; ---------------------------------------------------------------------------

(defcustom squeak-tcpip-proposed-images 2
  "The number of latest images to be interactively proposed when starting
Squeak from Emacs.
If 0, the latest image is automatically selected.
If negative, the root image \(from variable `squeak-tcpip-default-image') is
automatically selected."
  :type 'integer
  :group 'squeak)

(defun squeak-tcpip-default-image ()
  "Return the default image to be opened when starting Squeak from Emacs.
Valid image file names are built by replacing \".image\" with \".N.image\" in
variable `squeak-tcpip-default-image', where N stands for any integer.
The variable `squeak-tcpip-proposed-images' controls which images are considered
candidates; if none found, return \"\" which make the Squeak VM interactively
query the user."
  (squeak-latest-image squeak-tcpip-default-image squeak-tcpip-proposed-images))

(defun squeak-latest-image (image-name &optional n-last)
  ""
   (let* ((image-name
	   (expand-file-name
	    (substitute-in-file-name (file-name-sans-extension image-name))))
	  (versions 
	   (directory-files 
	    (file-name-directory image-name) 
	    t (format "%s\\.[0-9]+\\.image"
		      (regexp-quote (file-name-nondirectory image-name)))))
	  (sorted-versions 
	   (sort versions 
		 (lambda (v1 v2) 
		   (< (read 
		       (car (last (split-string 
				   (file-name-sans-extension v1) "\\."))))
		      (read 
		       (car (last (split-string
				   (file-name-sans-extension v2) "\\.")))))))))
     (if (and versions n-last (> n-last 0))
	 (x-popup-dialog
	  (selected-frame) 
	  (append '("choose a Squeak image")
		  (mapcar
		   (lambda (s) (cons s s))
		   (append
		    (when (file-exists-p (concat image-name ".image"))  
		      (list (concat image-name ".image")))
		    (last sorted-versions (min n-last (length versions)))))))
       (if (and versions (or (null n-last) 
			     (zerop n-last)))
	   (car (last sorted-versions))
	 (if (file-exists-p (concat image-name ".image"))  
	     (concat image-name ".image")
	   "")))))

;(squeak-latest-image (surmulot-squeak-widget-image))
;(squeak-latest-image "$SURMULOTDIR/squeak/muO.image")

(defun squeak-start-and-connect 
  (&optional confirmation-message image-file script-file port)
  "Start Squeak image IMAGE-FILE.
The VM executable is specified in variable `squeak-tcpip-vm'
IMAGE-FILE defaults to the value of function `squeak-tcpip-default-image'
SCRIPT-FILE is a Smalltalk script which will be evaluated at start up.
If CONFIRMATION-MESSAGE is not nil, it is supposed to be a string; the user
will be prompted to answer yes or no to that message in order to confirm the
starting up of squeak.
When no PORT is specified, the image is supposed to open immediately a socket
listening to `squeak-tcpip-port'.
\(see Smalltalk EmacsInteraction>>startOnPort:)
If the image do not set up the socket, this can be coded in SCRIPT-FILE. 
\(see `squeak-start-image-on-port' for an example of use)
Return the connection process."
  (when (or (null confirmation-message) 
	    (y-or-n-p confirmation-message))
   (save-window-excursion
     (with-temp-buffer
       (cd (file-name-directory (squeak-tcpip-vm)))
       (shell-command 
	(format "%s %s %s &"
		(safe-shell-quote-argument (squeak-tcpip-vm))
		(safe-shell-quote-argument 
		 (or image-file (squeak-tcpip-default-image))) 
		(safe-shell-quote-argument (or script-file "")))
	 (generate-new-buffer-name " *Squeak VM*"))))
   (squeak-tcpip-connect port)))

(defun safe-shell-quote-argument (arg)
  (if (string= arg "") ""
    (shell-quote-argument arg)))

(defmacro with-squeak-on-port (port &rest body)
  "Evaluate BODY in the comint buffer associated to PORT.
When PORT is nil, this is the Squeak console buffer"
  (declare (indent 1))
  `(save-excursion
    (set-buffer (squeak-buffer-name ,port))
    ,@body))

(defun squeak-tcpip-connect (&optional port once)
  "Set up a comint buffer on a TCP/IP process on port PORT.
When nil, PORT defaults to `squeak-tcpip-port'.
If ONCE try and connect only once; else try harder.
Return the newly started process."
  (let (proc)
    (flet ((connect () (make-comint
			(squeak-buffer-root-name port) 
			(cons "127.0.0.1" (or port squeak-tcpip-port)))))
      (loop repeat 300
	    unless (or (condition-case nil (connect) (error nil))
		       (when once (error "connection failed")))
	    do (sit-for 0.2))
      (connect) ;; so that an error is thrown if connection eventually failed
      (with-squeak-on-port port
	(set (make-local-variable 'squeak-tcpip-last-evalue) "<nope>")
	(set (make-local-variable 'squeak-tcpip-last-output) "")
	(set (make-local-variable 'squeak-tcpip-accumulated-preoutput) "")
	(set (make-local-variable 'comint-input-sender) 
	     (function squeak-tcpip-simple-send))
	(set (make-local-variable 'comint-preoutput-filter-functions)
	     (list 'squeak-tcpip-preoutput-filter))
	(add-to-list (make-local-variable 'comint-output-filter-functions)
		     'comint-truncate-buffer)
	(set (make-local-variable 'squeak-tcpip-process)
	     (get-buffer-process (current-buffer)))
	(setq proc squeak-tcpip-process)))
    proc))

(defun squeak-port-for-process (process)
  "Return the local port number for PROCESS \(a TCP/IP process)"
  (second (process-contact process)))

(defun squeak-buffer-root-name (&optional port)
  (if (and port (not (= port squeak-tcpip-port)))
      (format "Squeak on port %s" port)
    "Squeak console"))

(defun squeak-buffer-name (&optional port)
  (format "*%s*" (squeak-buffer-root-name port)))

(defun squeak-tcpip-process (&optional port)
  (get-buffer-process (squeak-buffer-name port)))

(defun squeak-tcpip-simple-send (proc string)
  (comint-send-string proc (squeak-tcpip-format-for-sending string)))

(defun squeak-comint-buffer (port)
  (get-buffer (squeak-buffer-name port)))

;;; ---------------------------------------------------------------------------
;;;
;;; console output filter
;;;
;;; ---------------------------------------------------------------------------

(defun squeak-output-is-command (str)
  "Return nil if str is not an elisp command from Squeak
else return the command as a string to be read"
  (when (and (>= (length str) (+ 7 8))
	     (equal (substring str 0 7) "<ELisp>")
	     (equal (substring str -8) "</ELisp>"))
    (substring (substring str 0 (- (length str) 8))
	       7)))

;TEST (squeak-output-is-command "(+ 1 2)") => nil
;TEST (squeak-output-is-command "<ELisp>(+ 1 2)</ELisp>") => "(+ 1 2)"
;TEST (eval (read (squeak-output-is-command "<ELisp>(+ 1 2)</ELisp>"))) => 3

(defun squeak-tcpip-preoutput-filter (strinput)
  (let ((str "")
	(output "")
	(command "")
	(decoded-str ""))

    (setq squeak-tcpip-accumulated-preoutput
          (concat squeak-tcpip-accumulated-preoutput strinput))

    ;; remove all ?\n at the beginning
    (while (and (> (length squeak-tcpip-accumulated-preoutput) 0)
                (equal (elt squeak-tcpip-accumulated-preoutput 0) ?\n))
      (setq squeak-tcpip-accumulated-preoutput 
            (substring squeak-tcpip-accumulated-preoutput 1)))

    ;; if one ?\n is left, at least one expression is complete
    (while (and (string-match "\n" squeak-tcpip-accumulated-preoutput)
                (setq str (car (split-string squeak-tcpip-accumulated-preoutput
					     "\n" t))))

      (setq decoded-str (decode-coding-string (base64-decode-string str)
					      'compound-text-mac))

      ;; reduce the stack by removing first expression str
      (setq squeak-tcpip-accumulated-preoutput 
            (substring squeak-tcpip-accumulated-preoutput 
		       (1+ (max (length str) 1))))

      ;; again remove all ?\n in the beginning
      ;;  ... then start over as long as ?\n are found
      (while (and (> (length squeak-tcpip-accumulated-preoutput) 0)
                  (equal (substring squeak-tcpip-accumulated-preoutput 0 1)
			 "\n"))
        (setq squeak-tcpip-accumulated-preoutput
              (substring squeak-tcpip-accumulated-preoutput 1)))

      ;; Lisp expression to be evaluated (see EmacsInteraction>>elisp:)
      (when (setq command (squeak-output-is-command decoded-str))
	(squeak-tcpip-send-string 
	 (condition-case nil
	     (prin1-to-string (eval (read command)))
	   (error squeak-tcpip-ELisp-error))))

      (unless (and command squeak-console-hide-commands)
	(setq output (concat output decoded-str))))

    ;; storing current value
    (setq str (replace-regexp-in-string squeak-tcpip-prompt "" decoded-str)
	  squeak-tcpip-last-output str
	  squeak-tcpip-last-evalue str)

    ;; output is what is eventually printed in console
    (setq output (replace-regexp-in-string 
		  squeak-tcpip-prompt 
		  "" 
		  output))
    (if (string= output "")
      ""
     (format "%s\n%s" output squeak-tcpip-prompt))))


;;; ---------------------------------------------------------------------------
;;;
;;; Squeak console
;;;
;;; ---------------------------------------------------------------------------

(defmacro squeak-console-button-do (label info &rest body)
  `(propertize 
    ,label
    'face 'custom-button-face
    'mouse-face 'custom-button-pressed-face
    'help-echo ,info
    'local-map
    '(keymap (header-line keymap
			  (mouse-1 . (lambda () (interactive) ,@body))
			  (mouse-3 . (lambda () (interactive) ,@body))
			  (mouse-2 . (lambda () (interactive) ,@body))))))

(defun display-squeak-tcpip-console ()
  "Open a buffer establishing an interactive TCP/IP connection with Squeak.
Which image is started depends on function `squeak-tcpip-default-image'.
That image is supposed to immediately open a socket on port `squeak-tcpip-port';
when that port is already open, no special action is taken apart from 
displaying the console buffer"
  (interactive)
  (if squeak-tcpip-console-is-special
      (add-to-list 'special-display-buffer-names "*Squeak console*"))
  (condition-case nil
      (squeak-tcpip-connect nil t)
    (error (squeak-start-and-connect 
	    "Connection was refused. Shall I launch Squeak ?")))
  (when (get-buffer-process "*Squeak console*")
    (save-excursion
      (set-buffer "*Squeak console*")
      (setq header-line-format
	    (list
	     "- Squeak TCP/IP console - "
	     (squeak-console-button-do 
	      " Restart " "(display-squeak-tcpip-console)"
	      (display-squeak-tcpip-console)
	      (end-of-buffer)))))
    (display-buffer "*Squeak console*")
    (squeak-eval-with-return squeak-tcpip-init-command)))


;;; ---------------------------------------------------------------------------
;;;
;;; high-level communication
;;;
;;; ---------------------------------------------------------------------------
          
(defun squeak-tcpip-send-region (beg end &optional port)
  (interactive "r")
  (if (squeak-tcpip-process port)
      (squeak-tcpip-send-string
       (subst-char-in-string 
	?\n ? 
	(buffer-substring-no-properties beg end)))))

(defun squeak-eval-with-return (&optional str port)
  "Has its argument (a string) evaluated as Squeak code.
Always returns a string"
  (interactive "P")
  (when (null str)
    (setq str (read-from-minibuffer squeak-tcpip-prompt)))
  (unless (and (squeak-tcpip-process port)
	       (eq (process-status (squeak-tcpip-process port)) 'open))
    (if port
        (error "port not available")
      (squeak-start-and-connect)))
  (with-squeak-on-port port
    (setq squeak-tcpip-last-evalue "<nope>")
    (squeak-tcpip-send-string str)
    (while (or (string= squeak-tcpip-last-evalue "<nope>") 
	       (string= squeak-tcpip-last-evalue "")) ;; ?? 
      (accept-process-output nil 0.01))
    (let ((result squeak-tcpip-last-evalue))
      (setq squeak-tcpip-last-evalue "<nope>")
      result)))

(defun squeak-eval-no-return (&optional str port)
  "Has its argument (a string) evaluated as Squeak code"
  (interactive "P")
  (when (null str)
    (setq str (read-from-minibuffer squeak-tcpip-prompt)))
  (unless (and (squeak-tcpip-process port)
	       (eq (process-status (squeak-tcpip-process port)) 'open))
    (if port
        (error "port not available")
      (squeak-start-and-connect)))
  (with-squeak-on-port port
    (squeak-tcpip-send-string str)))

(defun squeak-eval-buffer-string (beg end &optional port)
  (squeak-eval-with-return (buffer-substring beg end) port))

(defun squeak-tcpip-send-string (str)
  (process-send-string 
   squeak-tcpip-process (squeak-tcpip-format-for-sending str)))

(defun squeak-tcpip-format-for-sending (str)
  ""
  (format "%s \"end\"" 
	  (with-temp-buffer
	    (insert str)
	    (goto-char (point-min))
	    (while (search-forward "!#" nil t)
	      (let* ((beg (point))
		     (val (progn
			    (forward-sexp)
			    (eval (read (buffer-substring beg (point)))))))
		(delete-region (- beg 2) (point))
		(insert (prin1-to-string val))))
	    (buffer-string))))

;;; --------------------------- End  -----------------------------

(provide 'squeak-tcpip)

;;; squeak-tcpip.el ends here







