;;; -*- auto-recompile: t -*-

;; This file is not part of GNU Emacs.
;; 
;; surman.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; surman.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;-------------
;;             interacting with Surmulot manager via TCP/IP

;; last modified November 11, 2010
;; for Emacs 23
;;; ---------------------------------------------------------------------------

(require 'cl)

;;; Code:

(defgroup surmulot-manager nil
  "Surmulot local TCP/IP network"
  :group 'surmulot
  :prefix "surman-")

(defcustom surman-init-file "$SURMULOTDIR/localports.ini"
  "INI file defining the local TCP/IP ports used by SURMULOT"
  :type 'string
  :group 'surmulot-manager)

(defcustom surman-start-script-file 
  (if (eq system-type 'windows-nt) 
      "$SURMULOTDIR/../start manager.bat"
    "$SURMULOTDIR/../start manager")
  "Location of the Surmulot manager launch script."
  :type 'string
  :group 'surmulot-manager)

(defvar surman-local-ports
  (let ((init-file (substitute-in-file-name surman-init-file)))
    (when (file-exists-p init-file)
      (with-temp-buffer
	(insert-file-contents init-file)
	(goto-char (point-min))
	(loop while (re-search-forward "^\\(.*\\)=\\(.*\\)$" nil t)
	      collect (list (match-string 1)
			    (read (match-string 2))) into ports
			    finally return ports)))))

(defvar surman-port (cadr (assoc "emacs_manager" surman-local-ports))
  "Default TCP/IP port in local host")

(defcustom surman-repl-sit-for 10
  "How long should we display evaluation results in the minibuffer
within a REPL session. See commands `keykit-repl' and `squeak-repl'"
  :type 'integer
  :group 'surmulot-manager)

(defcustom surman-is-daemon nil
  "When t we reconnect upon deconnection"
  :type 'boolean
  :group 'surmulot-manager)

(defvar surman-process-timer nil
  "The timer enabling auto-reconnection")

(defvar surman-process nil
  "The manager interaction TCP/IP process")

(defvar surman-separator "\v"
  "One-character string used to separate items in a TCP request")

(defvar surman-end-separator "\v\v"
  "String used to separate TCP requests")

(defvar surman-answers (make-hash-table)
  "Store answers from manager to emacs queries")

(defvar surman-pending-input ""
  "Accumulated incoming strings from manager.
This is parsed in surman-process-filter")

;;; ---------------------------------------------------------------------------
;;;
;;; connection to manager
;;;
;;; ---------------------------------------------------------------------------

(defun surman-start-manager ()
  (interactive)
  (let ((script (substitute-in-file-name surman-start-script-file)))
    (save-window-excursion
      (with-temp-buffer
	(cd (file-name-directory script)) 
	(shell-command (format "%s &"  (shell-quote-argument script)) 
		       (generate-new-buffer-name " *surman start*"))))
    (surman-start-daemon)))

; (surman-connect)
(defun surman-connect (&optional ntry wait-for)
  "Set up the connection to the manager TCP/IP server"
  (interactive)
  (unless (surman-connected-p)
    (setq ntry (or ntry 1)
          wait-for (or wait-for 3))
    (while (not (zerop ntry))
      (condition-case nil
          (when (setq surman-process
		      (open-network-stream "surman" nil 
					   "127.0.0.1" surman-port))
            (surman-cancel-daemon)
            (setq ntry 0)
            (set-process-sentinel surman-process 'surman-process-sentinel)
            (set-process-filter surman-process 'surman-process-filter))
        (error (unless (zerop (setq ntry (1- ntry)))      
                 (sit-for wait-for)))))))

(defun surman-process-sentinel (process event)
;  (message (format "Surmulot manager: %s" event))
  (when surman-is-daemon 
    (surman-start-daemon)))

(defun surman-start-daemon ()
  (interactive)
  (surman-cancel-daemon)
  (setq surman-process-timer (run-with-timer t 3 'surman-connect)))

(defun surman-cancel-daemon ()
  (interactive)
  (when (timerp surman-process-timer)
    (cancel-timer surman-process-timer)
    (setq surman-process-timer nil)))

(defun surman-daemon-waiting-p ()
  (timerp surman-process-timer))

(defun surman-process-filter (process strinput)
  (setq surman-pending-input 
	(concat surman-pending-input (remove 4 strinput)))
  (when (string-match surman-end-separator surman-pending-input)
    (while (string-match (concat "\\(.+?" 
				 surman-end-separator "\\)\\(\\(.\\|\n\\)*\\)")
			 surman-pending-input)
      (let* ((rest (or (match-string 2 surman-pending-input) ""))
	     (command (split-string (match-string 1 surman-pending-input)
				    surman-separator))
	     (data (first command))
	     (manager (second command))
	     (type (third command))
	     (tid (fourth command)))
        (setq surman-pending-input rest)
	(when (string= type "Q")
          (process-send-string 
	   surman-process
	   (concat (condition-case nil
		       (prin1-to-string (eval (read data)))
		     (error "evaluation error")) surman-separator
		     manager surman-separator 
		     "A" surman-separator
		     tid surman-end-separator)))
	(when (string= type "A")
	  (puthash (read tid) data surman-answers))))))

(defun surman-disconnect ()
  (interactive)
  (delete-process surman-process))

(defun surman-connected-p ()
  (interactive)
  "return nil if Emacs is not connected to its manager"
  (eq (process-status "surman") 'open))


;;; ---------------------------------------------------------------------------
;;;
;;; interaction with the managers
;;;
;;; ---------------------------------------------------------------------------

(defun surman-manager-connected-p (manager)
  (and (surman-connected-p)
       (string= (surman-ask "super_listener" manager) "true")))


(defun surman-squeak-connected-p ()
  (interactive)
  (surman-manager-connected-p "squeak_manager"))

(defun surman-keykit-connected-p ()
  (interactive)
  (surman-manager-connected-p "keykit_manager"))

;TEST (surman-ask "super_listener" "(+ 10 30 2)") => "42"
;TEST (surman-ask "keykit_manager" "fractal('a,e,f')") => "'ad32,e,f,e,bo2,co3,f,c,c+'"
;TEST (surman-ask-squeak "'a,e,f' kmusic fractal") => "'ad32,e,f,e,bo2,co3,f,c,c+'"
;TEST (surman-ask-squeak "'a String'") => "'a String'"

;TEST (surman-ask "squeak_manager" "5 sin") => "-0.958924274663138"
;TEST (length (surman-ask "keykit_manager" "fractal(fractal(fractal('ad20000,e,f')))")) => 25410
;TEST (length (surman-ask-squeak "'ad20000,e,f' kmusic fractal fractal fractal")) => 19634

;TEST  (surman-ask-squeak "'two\nlines'") => "'two\nlines'"
;; wrong:
;TEST  (surman-ask-squeak "'two\rlines'") => "'two\nlines'"

(defvar surman-counter -1)

(defun surman-ask (manager question)
  ""
  (let (my-counter)
    (when (surman-connected-p)
      (if (not (or (string= manager "super_listener")
		   (surman-manager-connected-p manager)))
	  (error (format "%s is not connected" manager))
	(setq my-counter (incf surman-counter))
	(process-send-string surman-process (concat question surman-separator
						    manager surman-separator
						    "Q" surman-separator
						    (format "%s" my-counter)
						    surman-end-separator))
	(while (not (gethash my-counter surman-answers))
	  (accept-process-output nil 0.01))
;	(decf surman-counter)
	(gethash my-counter surman-answers)))))

;(surman-ask "super_listener" "squeak_manager")
;(gethash 3 surman-answers)

(defun surman-ask-squeak (question)
  (interactive "sSqueak> ")
  (let ((ans (decode-coding-string
	      (surman-ask "squeak_manager" 
			 (encode-coding-string question 'compound-text-mac))
	      'compound-text-mac)))
    (if (interactive-p) (message ans) ans)))

(defun surman-ask-keykit (question)
  (interactive "sKeykit> ")
  (let ((ans (surman-ask "keykit_manager" question)))
    (if (interactive-p) (message ans) ans)))

(defun surman-repl (function)
  (while t
    (call-interactively function)
    (sit-for surman-repl-sit-for)))

(defun squeak-repl ()
  (interactive)
  (surman-repl 'surman-ask-squeak))

(defun keykit-repl ()
  (interactive)
  (surman-repl 'surman-ask-keykit))

;;; --------------------------- End  -----------------------------

(provide 'surman)

;;; surman ends here

