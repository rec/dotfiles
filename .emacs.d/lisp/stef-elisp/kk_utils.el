;;; -*- auto-recompile: t -*-

;; This file is not part of GNU Emacs.
;; 
;; kk_utils.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; kk_utils.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;-------------
;;              this file is required by keykit-mode.el

;; last modified February 17, 2009
;;; --------------------------------------------------------------------------------

;;; Code:

;;; ----------------------- Unfolding arrays  --------------------------

(defun kk-unfold-next-array-in-line (&optional depth-limit sub)
"Unfold the next array after point in line. 
If an argument depth-limit is present and non-nil, only unfold the corresponding first levels in the tree structure."
  (interactive)
  (if (or (null depth-limit)
          (null sub)
          (<= sub depth-limit))
      (if (null sub)         
          (if (search-forward "[" (save-excursion (end-of-line) (point)) t)  
              (kk-unfold-next-array-in-line depth-limit (setq sub 1)))
        (message (concat "calculating... depth " (number-to-string sub)))
        (let ((ze-end ()))
          (while (and (null ze-end)
                      (null (looking-at "[ \t;()]*$")))
            (if (looking-at "[ \t]*=[ \t]*\\[")
                (save-excursion
                  (search-forward "[")
                  (kk-unfold-next-array-in-line depth-limit (1+ sub))))
            (if (looking-at ",")
                (progn
                  (forward-char 1)
                  (if (looking-at "[ \t]*$")
                      (progn
                        (forward-line 1)
                        (beginning-of-line))
                    (insert ?\n))
                  (kk-indent-line-in-array)))
            (condition-case nil
                (forward-sexp 1)
              (error
               (setq ze-end t))))))))


(defun kk-indent-line-in-array ()
  (let ((indent (save-excursion
                  (let ((indent-point (point))
                        containing-sexp)
                    (beginning-of-defun)
                    (while (< (point) indent-point)
                      (setq containing-sexp 
                            (cadr (parse-partial-sexp (point) indent-point 0))))
                    (goto-char (1+ containing-sexp))
                    (current-column))))
        beg shift-amt)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (progn
      (delete-region beg (point))
      (indent-to indent))
    shift-amt))


;;; ----------------------- TCP/IP session --------------------------

(defvar kk-tcpip-process nil
  "Process communicating with KeyKit through TCP/IP")

(defcustom kk-tcpip-key-command "key.exe"
  "Shell command used to launch key.exe if necessary"
  :type 'string
  :group 'spfa-paths
  :group 'keykit-util)

(defun kk-tcpip-key-command ()
  (expand-file-name (substitute-in-file-name kk-tcpip-key-command)))

(defcustom kk-tcpip-key-start-directory "c:/keykit"
  "The directory from where to start key.exe with `kk-tcpip-key-command'"
  :type 'string
  :group 'spfa-paths
  :group 'keykit-util)

(defun kk-tcpip-key-start-directory ()
  (expand-file-name (substitute-in-file-name kk-tcpip-key-start-directory)))

(defcustom kk-tcpip-prompt "key> "
  "KeyKit console prompt.
Must have the same value as KeyKit variable TCP_IP_prompt"
  :type 'string
  :group 'keykit-util)

(defcustom kk-tcpip-ELisp-error "<evaluation error>"
  "Error message returned by ELisp\() when evaluation failed.
Must have the same value as KeyKit variable TCP_IP_elisp_error"
  :type 'string
  :group 'keykit-util)

(defcustom kk-tcpip-port 5861
  "KeyKit TCP/IP port in local host. 
Must have the same value as KeyKit variable TCP_IP_localport \(defined in lib/tcpip.k)"
  :type 'integer
  :group 'keykit-util)

(defcustom kk-tcpip-console-is-special t
  "When non-nil, the console buffer is added to `special-display-buffer-names'"
  :type 'boolean
  :group 'keykit-util)

(defmacro kk-console-button-do (label info &rest body)
  `(propertize ,label
	       'face 'custom-button-face
	       'mouse-face 'custom-button-pressed-face
	       'help-echo ,info
	       'local-map
	       '(keymap (header-line keymap 
				     (mouse-1 . (lambda () (interactive) ,@body))
				     (mouse-3 . (lambda () (interactive) ,@body))
				     (mouse-2 . (lambda () (interactive) ,@body))))))

(defun kk-tcpip-launch-key ()
  (interactive)
  (save-window-excursion
    (with-temp-buffer ;; because of the cd below
      (when (file-exists-p (kk-tcpip-key-start-directory))
	(cd (kk-tcpip-key-start-directory)))
      (call-process (kk-tcpip-key-command) nil 0)))
  (dotimes (n 30)
    (sit-for 1)
    (condition-case nil
	(kk-tcpip-connect)
      (error nil)))
  (kk-tcpip-connect))

(defun kk-tcpip-connect ()
  (make-comint "KeyKit console" `("127.0.0.1" . ,kk-tcpip-port)))

(defun kk-tcpip ()
  (interactive)
  (if kk-tcpip-console-is-special
      (add-to-list 'special-display-buffer-names "*KeyKit console*"))
  (condition-case nil
      (kk-tcpip-connect)
    (error (when (y-or-n-p "Connection was refused. Shall I launch KeyKit ?")
	     (kk-tcpip-launch-key))))
  (when (get-buffer-process "*KeyKit console*")
    (setq kk-tcpip-last-evalue "<nope>")
    (setq kk-tcpip-accumulated-preoutput "")
    (save-excursion
      (set-buffer "*KeyKit console*")
      (setq header-line-format
	    (list
	     "- KeyKit TCP/IP console - "
	     (kk-console-button-do 
	      "W" "toggle sharing window with notepad"
	      (if (one-window-p)
		  (progn
		    (if (null (get-buffer "*KeyKit interaction*")) (kk-open-back-door-console))
		    (if (get-buffer-window "*KeyKit interaction*" t)
		        (delete-frame (window-frame 
				       (get-buffer-window "*KeyKit interaction*" t))))
		    (select-window (split-window))
		    (switch-to-buffer "*KeyKit interaction*"))
		(delete-other-windows (get-buffer-window "*KeyKit console*" t))))
	     " "
	     (kk-console-button-do 
	      ">> notepad " "jump to back-door notepad" 
	      (kk-open-back-door-console)
	      (set-buffer "*KeyKit interaction*"))
	     " "
	     (kk-console-button-do
	      " Restore " "eval(#include tcpip.k) restores display if necessary"
	      (kk-eval "eval(\"#include tcpip.k\")"))
	     " "
	     (kk-console-button-do 
	      " Hist " "toggle recording of commands in Hist"
	      (kk-eval "print(RecordEmacsCommandsInHist = 1-RecordEmacsCommandsInHist)"))
	     " "
	     (kk-console-button-do 
	      " Restart " "(kk-tcpip)"
	      (kk-tcpip)
	      (goto-char (point-max)))))
      (set (make-local-variable 'comint-input-sender)
	   (function kk-tcpip-simple-send))
      (set (make-local-variable 'comint-preoutput-filter-functions)
	   (list 'kk-tcpip-preoutput-filter))
      (add-to-list (make-local-variable'comint-output-filter-functions) 
		   'comint-truncate-buffer))
    (setq kk-tcpip-process (get-buffer-process "*KeyKit console*"))
    (display-buffer "*KeyKit console*")
    (kk-eval "print(\"Emacs sayz: hello !\")")))

(defun kk-tcpip-simple-send (proc string)
;; à faire: #include .. -> eval("#include ..")
;; à faire: #define .. -> eval("#define ..")         ??
  (comint-send-string proc (subst-char-in-string ?\n ?\r string))
  (comint-send-string proc "\n"))

(defun kk-tcpip-send-region (beg end)
  (interactive "r")
  (kk-eval-with-return (buffer-substring-no-properties beg end)))

(defun kk-tcpip-send-string (str)
  (process-send-string
   kk-tcpip-process
   (format "%s\n" (subst-char-in-string  ?\n ?\r str))))
    
(defvar kk-tcpip-last-evalue "<nope>"
  "Used by kk-eval to get the return value of a KeyKit expression")

(defvar kk-tcpip-accumulated-preoutput ""
  "Accumulated incoming strings from KeyKit.
This is parsed in kk-tcpip-preoutput-filter")

(defvar kk-tcpip-last-output ""
  "Last output from the TCP/IP process (that is, from KeyKit")

(defun kk-tcpip-preoutput-filter (strinput)
  (let ((str ""))

    (setq kk-tcpip-accumulated-preoutput
          (concat kk-tcpip-accumulated-preoutput strinput))

    ;; on enlève tout les sauts de ligne du début
    (while (and (> (length kk-tcpip-accumulated-preoutput) 0)
                (equal (substring kk-tcpip-accumulated-preoutput 0 1) "\n"))
      (setq kk-tcpip-accumulated-preoutput 
            (substring kk-tcpip-accumulated-preoutput 1)))

    ;; s'il reste des sauts de ligne, au moins une expression est complète:
    (while (and (string-match "\n" kk-tcpip-accumulated-preoutput)
                (setq str (car (split-string kk-tcpip-accumulated-preoutput "\n" t))))

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
            (let ((return-str 
                   (concat "<ELisp>"
                           (condition-case nil
                               (with-output-to-string
                                 (prin1 (eval (read (substring 
                                                     (substring str 0 (- (length str) 7))
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
                 3))))

    ;; on garde de côté la valeur courante :
    (while (string-match kk-tcpip-prompt str)
      (setq str (replace-match "" t t str)))
    (setq kk-tcpip-last-output str))
  
  strinput)

                    
(defun kk-eval-with-return (&optional str)
  "Has its argument \(a string) evaluated as KeyKit code.
always returns a string"
  (interactive "P")
  (if (null str)
      (setq str (read-from-minibuffer kk-tcpip-prompt)))
  (unless (and kk-tcpip-process
	       (eq (process-status kk-tcpip-process) 'open))
    (kk-tcpip))
  (progn
    (kk-tcpip-send-string (format "<->%s" str))
    (while (equal kk-tcpip-last-evalue "<nope>")                   
      (accept-process-output kk-tcpip-process))
    (let ((result kk-tcpip-last-evalue))
      (setq kk-tcpip-last-evalue "<nope>")
      result)))

(defalias 'kk-eval 'kk-tcpip-send-string)


;;; ------------------------  TCP/IP:  back door  -----------------------------


(defcustom kk-notepad-is-special t
  "When non-nil, the back-door notepad buffer name is added to `special-display-buffer-names'"
  :type 'boolean
  :group 'keykit-util)


(defun kk-open-back-door-console ()
  (interactive)
  (if kk-notepad-is-special (add-to-list 'special-display-buffer-names "*KeyKit interaction*"))
  (pop-to-buffer (get-buffer-create "*KeyKit interaction*"))
  (keykit-mode)
  (setq header-line-format 
	'(:eval
	  (list
	   (kk-console-button-do 
	    " Send " "have KeyKit evaluate this buffer"
	    (set-buffer "*KeyKit interaction*")
	    (kk-send-buffer-through-back-door))
	   " - KeyKit TCP/IP notepad - "
	   (kk-console-button-do
	    "W" "toggle sharing window with console"
	    (if (one-window-p)
		(progn
		  (if (null (get-buffer "*KeyKit console*")) (kk-tcpip))
		  (if (get-buffer-window "*KeyKit console*" t)
		      (delete-frame (window-frame
				     (get-buffer-window "*KeyKit console*" t))))
		  (select-window (split-window))
		  (switch-to-buffer "*KeyKit console*"))
	      (delete-other-windows (get-buffer-window "*KeyKit interaction*" t))))
         " "
         (kk-console-button-do 
	  ">> console " "jump to TCP/IP console" 
          (if (null (get-buffer "*KeyKit console*")) (kk-tcpip))
          (pop-to-buffer "*KeyKit console*"))
         " "
         (kk-console-button-do 
	  " Clear " "erase buffer"
	  (set-buffer "*KeyKit interaction*")
	  (kill-region 1 (save-excursion (goto-char (point-max)) (point))))
	 " "
         (kk-console-button-do 
	  " Last " "insert last TCP/IP output from KeyKit"
	  (set-buffer "*KeyKit interaction*")
	  (insert kk-tcpip-last-output))))))

(defmacro kk-do-this-to-back-door-file (this)
  `(let*((file
          (expand-file-name (concat (make-temp-name "tcpip")
                                    ".k")
                            (kk-default-directory))))
     ,this
     (kk-eval (concat "eval(\"#include " file "\")"))
     (delete-file file)))

(defun kk-send-string-through-back-door (str)
  (interactive "MKeyKit code: ")
  (kk-do-this-to-back-door-file (write-region str () file)))

(defun kk-send-region-through-back-door (beg end)
  (interactive "r")
  (kk-do-this-to-back-door-file (write-region beg end file)))

(defun kk-send-buffer-through-back-door ()
  (interactive)
  (kk-do-this-to-back-door-file 
   (write-region 1 (save-excursion (goto-char (point-max)) (point)) file)))


(defcustom kk-max-region-for-tcpip 500
  "A region longer than that will be sent through the back door."
  :type 'integer
  :group 'keykit-util)


;; à faire: meilleure optimisation (selon nombre de lignes, présence de commentaires..) 
(defun kk-optimized-send-region (beg end)
  (interactive "r")
  (if kk-tcpip-process
      (if (< (abs (- end beg)) kk-max-region-for-tcpip)
          (kk-tcpip-send-region beg end)
        (kk-tcpip-simple-send kk-tcpip-process "ListenToEmacs()")
        (sleep-for .1)
        (kk-send-region-through-back-door beg end))))



;;; --------------------------- Lowkey invocation  -----------------------------

;; à terminer et à documenter...

(defun lowkey-one-phrase-code-p (str)
  "Test weither STR, a string of keykit code, is simply the representation of a phrase"
  (string-match "^'[^']*'$" str))

;TEST (lowkey-one-phrase-code-p "repeat('a',127)") => nil
;TEST (lowkey-one-phrase-code-p "'a'") => 0
;TEST (lowkey-one-phrase-code-p "'a'+'b'") => nil

(defun lowkey-eval (&rest str)
  (with-temp-buffer
    (apply 'insert str)
    (kk-run-lowkey "" (point-min) (point-max)))
  (save-excursion
    (set-buffer "*Lowkey Output*")
    (buffer-string)))

(defun kk-run-lowkey (&optional kk-command beg end)
  (interactive "P")
  (if (null kk-command)
      (setq kk-command (read-from-minibuffer "with command: "))) 
  (let* ((file-name (if (null beg)
                        (expand-file-name (buffer-file-name (current-buffer)))
                      (kk-temp-code-filename)))
         (command (concat
                    (shell-quote-argument (kk-lowkey-executable))
                    " "
                    (shell-quote-argument file-name)
                    (if (or (null kk-command) 
                            (equal kk-command ""))
                        ""
                      (concat " -c \"" kk-command "\"")))))
    (if (null beg)
        (save-buffer)
      (write-region beg end (kk-temp-code-filename)))
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

; is that used ??
(defcustom kk-temp-midifiles-rootname "c:/audio/KeyKit/MIDI/emacs"
  "Temporary midi files are named by appending a number to this string.
It should include the path to the temporary midi files directory"
  :type 'file
  :group 'spfa-paths
  :group 'keykit-util)

(defun kk-temp-midifiles-rootname ()
  (substitute-in-file-name kk-temp-midifiles-rootname))

(defcustom kk-temp-code-filename "c:/audio/KeyKit/MIDI/bof.k"
  "Name for temporary file used to evaluate code with lowkey.
It should include the path to the temporary files directory"
  :type 'file
  :group 'spfa-paths
  :group 'keykit-util)

(defun kk-temp-code-filename ()
  (substitute-in-file-name kk-temp-code-filename))

(defun kk-soft-synth (midi-file wav-file &optional listen edit)
  "Customize this function so that it fits your system.
Given a midi-file name, it must render it as a *.wav called wav-file.
If listen is non-nil, it must play wav-file \(in an asynchronous process).
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
    (goto-char (point-min))
    (while (search-forward ".mid" nil t)
      (widget-create 'push-button
                     :notify (lambda (&rest ignore)
                               (save-excursion
                                 (search-backward ".mid")
                                 (kk-soft-synth (thing-at-point 'filename)
						(replace-regexp-in-string "mid$" "wav"
									  (thing-at-point 'filename))
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

;;; --------------------------- Debugging stuff  -----------------------------

(defun kk-test1 ()
  (interactive)
  (if (null (re-search-forward "^Searched" nil t))
      (goto-char (point-max))
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







