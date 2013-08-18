;;; -*- auto-recompile: t -*-

;; This file is not part of GNU Emacs.
;; 
;; keykit-mode.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; keykit-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;-------------
;;              this file is required by keykit-mode.el

;; last modified July 30, 2001
;;; --------------------------------------------------------------------------------

;;; Code:

(require 'thingatpt)
(require 'widget)



;;; -------- I guess this already exists somewhere, but I could't find where:


(defun kk-replace-in-string (string rep-list)
  "Makes replacements in string according to a list of couples regexp/replacement"
  (let (regexp 
        repexp
        deb fin eye)
    (while rep-list
      (setq regexp (caar rep-list))
      (setq repexp (cadar rep-list))
      (setq eye 0)
      (while (setq deb (string-match regexp string eye))
        (setq fin (match-end 0))
        (setq string (concat
                      (substring string 0 deb)
                      repexp
                      (substring string fin)))
        (setq eye (+ deb (length repexp))))
      (setq rep-list (cdr rep-list)))
    string))


;examples:
;(kk-replace-in-string "bouhagueege" '(("u" "U") ("g" "G")))
;(kk-replace-in-string "a<b && b>a" '(("<" "<coucou!<") (">" ">coucou!>")))



;;; ----------------------- TCP/IP session --------------------------

(defvar kk-tcpip-process nil
  "process communicating with KeyKit through TCP/IP")

(defcustom kk-tcpip-prompt "key> "
  "KeyKit console prompt.
Must have the same value as KeyKit variable TCP_IP_prompt"
  :type 'string
  :group 'keykit-util)

(defcustom kk-tcpip-ELisp-error "<evaluation error>"
  "Error message returned by ELisp() when evaluation failed.
Must have the same value as KeyKit variable TCP_IP_elisp_error"
  :type 'string
  :group 'keykit-util)

(defcustom kk-tcpip-port 5861
  "KeyKit TCP/IP port in local host. 
Must have the same value as KeyKit variable TCP_IP_localport (defined in lib/tcpip.k)"
  :type 'integer
  :group 'keykit-util)

(defcustom kk-tcpip-console-is-special t
  "When non-nil, the console buffer is added to special-display-buffer-names"
  :type 'boolean
  :group 'keykit-util)



(defun kk-tcpip ()
  (interactive)
  (if kk-tcpip-console-is-special
      (add-to-list 'special-display-buffer-names "*KeyKit console*"))
  (make-comint "KeyKit console" `("127.0.0.1" . ,kk-tcpip-port))

  (setq kk-tcpip-last-evalue "<nope>")
  (setq kk-tcpip-accumulated-preoutput "")

  (save-excursion
    (set-buffer "*KeyKit console*")
    (setq comint-input-sender
	  (function kk-tcpip-simple-send))
    (setq comint-preoutput-filter-functions 
	  (list 'kk-tcpip-preoutput-filter)))
  (setq kk-tcpip-process (get-buffer-process "*KeyKit console*"))
;  (comint-send-string kk-tcpip-process ":\"<connected>\"\n")
  (display-buffer "*KeyKit console*"))


(defun kk-tcpip-simple-send (proc string)
  (comint-send-string proc (kk-replace-in-string string '(("\n" "\r"))))
  (comint-send-string proc "\n"))

(defun kk-tcpip-send-region (beg end)
  (interactive "r")
  (if kk-tcpip-process
      (process-send-string kk-tcpip-process
			   (concat
			    (kk-replace-in-string
			     (buffer-substring-no-properties beg end)
			     '(("\n" "\r")))
			    "\n"))))


(defvar kk-tcpip-last-evalue "<nope>"
  "Used by kk-eval to get the return value of a KeyKit expression")

(defvar kk-tcpip-accumulated-preoutput ""
  "Buffer of incoming strings from KeyKit.
This is parsed in kk-tcpip-preoutput-filter")

(defun kk-tcpip-preoutput-filter (strinput)
  (let (str)

    (setq kk-tcpip-accumulated-preoutput
	  (concat kk-tcpip-accumulated-preoutput strinput))

    ;; on enlève tout les sauts de ligne du début
    (while (and (> (length kk-tcpip-accumulated-preoutput) 0)
		(equal (substring kk-tcpip-accumulated-preoutput 0 1) "\n"))
      (setq kk-tcpip-accumulated-preoutput 
	    (substring kk-tcpip-accumulated-preoutput 1)))

    ;; s'il reste des sauts de ligne, au moins une expression est complète:
    (while (and (string-match "\n" kk-tcpip-accumulated-preoutput)
		(setq str (car (split-string kk-tcpip-accumulated-preoutput "\n"))))

      ;; on réduit la pile (supprimant la premiere expression str):
      (setq kk-tcpip-accumulated-preoutput 
	    (substring kk-tcpip-accumulated-preoutput (1+ (max (length str) 1))))

      ;; on enlève tout les sauts de ligne du début (re)
      ;;  ... et le tout recommencera tant qu'il reste des "\n"
      (while (and (> (length kk-tcpip-accumulated-preoutput) 0)
		  (equal (substring kk-tcpip-accumulated-preoutput 0 1) "\n"))
	(setq kk-tcpip-accumulated-preoutput
	      (substring kk-tcpip-accumulated-preoutput 1)))


      ;; expression lisp a évaluer (fonction ELisp):
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (if (and (>= (length str) (+ 7 8))
	       (equal (substring str 0 7) "<ELisp>")
	       (equal (substring str -8) "</ELisp>"))
	    (let ((return-str (concat "<ELisp>"
				      (condition-case nil
					  (with-output-to-string
					    (princ (eval
						    (read
						     (substring 
						      (substring str 
								 0
								 (- (length str) 7))
						      7)))))
					(error kk-tcpip-ELisp-error))
					"\n")))
	      ;; ici le détour par return-str est nécessaire car kk-tcpip-process peut être
	      ;;  modifié durant son évaluation (cf. EmacsResetConnection in tcpip.k)
	      (process-send-string kk-tcpip-process return-str)))

      ;; retour d'une expression passée par kk-eval:
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (if (and (>= (length str) (+ 3 3))
	       (equal (substring str 0 3) "<->")
	       (equal (substring str -3) "</>"))
	  (setq kk-tcpip-last-evalue
		(substring
		 (substring str 0 (- (length str) 3))
		 3)))
      ))
  strinput)

			

(defun kk-eval (str)
  "has its argument (a string) evaluated as KeyKit code.
always returns a string"
  (interactive "P")
  (if (null str)
      (setq str (read-from-minibuffer kk-tcpip-prompt)))
  (if kk-tcpip-process
      (progn
	(process-send-string kk-tcpip-process (concat "<->" str "\n"))
	(while (equal kk-tcpip-last-evalue "<nope>")                   ;;; dangereux!
	  (accept-process-output kk-tcpip-process))
	(let ((result kk-tcpip-last-evalue))
	  (setq	kk-tcpip-last-evalue "<nope>")
	  result))
    ""))


;;; --------------------------- Lowkey invocation  -----------------------------

;; à terminer et à documenter...

(defun kk-run-lowkey (&optional kk-command beg end)
  (interactive "P")
  (if (null kk-command)
      (setq kk-command (read-from-minibuffer "with command: "))) 
  (let* ((file-name (if (null beg)
			(expand-file-name (buffer-file-name (current-buffer)))
		      kk-temp-code-filename))
	 (command (concat
                    kk-lowkey-executable
                    " "
                    file-name
                    (if (or (null kk-command) 
                            (equal kk-command ""))
                        ""
                      (concat " -c \"" kk-command "\"")))))
    (if (null beg)
	(save-buffer)
      (write-region beg end kk-temp-code-filename))
;    (print command)
    (shell-command command
		   (get-buffer-create "*Lowkey Output*"))
    (if kk-buttonize-lowkey-output
	(progn
	  (sleep-for 1)
	  (kk-buttonize-midi-files-in-buffer "*Lowkey Output*")))))
               

(defcustom kk-buttonize-lowkey-output t
  "If non-nil, the output of a lowkey invocation is grossly analysed,
and buttons appear where it seems that they could be useful.
For example, [wav] and [play] buttons appear after a midi file name,
allowing the corresponding actions to be performed on that file."
  :type 'boolean
  :group 'keykit-util)


(defcustom kk-temp-midifiles-rootname "c:/audio/KeyKit/MIDI/emacs"
  "Temporary midi files are named by appending a number to this string.
It should include the path to the temporary midi files directory"
  :type 'file
  :group 'keykit-util)


(defcustom kk-temp-code-filename "c:/audio/KeyKit/MIDI/bof.k"
  "Name for temporary file used to evaluate code with lowkey.
It should include the path to the temporary files directory"
  :type 'file
  :group 'keykit-util)


(defun kk-soft-synth (midi-file wav-file &optional listen edit)
  "Customize this function so that it fits your system.
Given a midi-file name, it must render it as a *.wav called wav-file.
If listen is non-nil, it must play wav-file (in an asynchronous process).
If edit is non-nil, it must open a sound editor on wav-file."
  (interactive)
  (shell-command 
   (concat 
    (if (equal system-type 'gnu/linux)
	"timidity -EFchorus=0 -EFreverb=0 -A80 -s44100 -p128 -Ow -o "
      "c:/timidity/timpp2100q.exe -EFchorus=0 -EFreverb=0 -A80 -s44100 -p128 -Ow -o ")
    wav-file
    " "
    midi-file))  
  (if listen
      (shell-command 
       (if (equal system-type 'gnu/linux)
	   (concat "play " wav-file " &")
	 (concat "\"c:/Program Files/Windows Media Player/mplayer2.exe\" " wav-file " &"))))
  (if edit
      (shell-command 
       (if (equal system-type 'gnu/linux)
	   (concat "snd " wav-file)
       (concat "c:/cool/COOL96.EXE " wav-file)))))


(defun kk-play-midi (midi-file)
  "Customize this function so that it fits your system.
Given a midi-file name, it must play it, preferably in an asynchronous process."
  (interactive)
  (shell-command 
   (concat 
    (if (equal system-type 'gnu/linux)
	"timidity -EFchorus=0 -EFreverb=0 -A80 -s44100 -p128 "
      "c:/timidity/timpp2100q.exe -EFchorus=0 -EFreverb=0 -A80 -s44100 -p128 ")
    midi-file
    " &")))


(defun kk-buttonize-midi-files-in-buffer (buff)
  (save-excursion
    (set-buffer buff)
    (beginning-of-buffer)
    (while (search-forward ".mid" nil t)
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (save-excursion
                                 (search-backward ".mid")
                                 (kk-soft-synth (thing-at-point 'filename)
                                               (kk-replace-in-string
                                                (thing-at-point 'filename)
                                                '(("mid$" "wav")))
                                               nil t)))
                     "wav")
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (save-excursion
                                 (search-backward ".mid")
                                 (kk-play-midi (thing-at-point 'filename))))
                     "play")
      (use-local-map widget-keymap)
      (widget-setup)
      (setq inhibit-read-only t))))
        


;;; --------------------------- Code mutations -----------------------------


;;; this code is in a very early stage... quite useless at this point

(defun kk-mutate-this-line ()
  (beginning-of-line)
  ;;; ... à faire
)


(defun kk-mark-region-as-mutable (beg end)
  (interactive "r")
  (beginning-of-line)
  (let ((endl (save-excursion (end-of-line) (point))))
    (if (null (search-forward "#m> " endl t))
	(save-excursion 
	  (end-of-line)
	  (insert " #m> ")))
    (search-backward "#m> ")
    (setq end (if (> end (point))
		  (point)
		end))
    (if (> end beg)
	(progn

;;test

	  (search-forward "#m> ")
	  (insert (int-to-string beg) ":" (int-to-string end))

;;

	  ))))
	 


;;; --------------------------- Debugging stuff  -----------------------------



(defun kk-test1 ()
  (interactive)
  (if (null (re-search-forward "^Searched" nil t))
      (end-of-buffer)
    (match-beginning 0)))

(defun kk-test2 ()
  (interactive)
  (prin1 (thing-at-point 'filename)))

(defun kk-test3 ()
  (interactive)
  (prin1 (looking-at "#\(name\)\|\(desc\)")))





;;; --------------------------- End  -----------------------------

(provide 'kk_utils)

;;; kk_utils.el ends here







