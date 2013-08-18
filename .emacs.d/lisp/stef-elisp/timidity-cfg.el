;;; -*- auto-recompile: t -*-

;;; timidity-cfg.el --- editing TiMidity++ configuration files

;; Keywords: timidity

;; This file is not part of GNU Emacs.
;; 
;; timidity-cfg.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; timidity-cfg.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

;;; Commentary:
;;-------------

;;    this mode allows fast and interactive editing of Timidity++
;;    configuration files
;;
;;    first of all, customize it by going to the group 'timidity-cfg
;;
;;    then ensure that a midi file is present to be used at a test:
;;    note that you can create one such file from emacs by selecting
;;    the item "set the test melody" in the Timidity menu (when in 
;;    timidity-cfg mode), and entering a Keykit phrase expression 
;;    (this will only work if the lowkey settings are correct in
;;    the 'keykit-util customization group)
;;
;;    when the buffer is in read-only mode (which you can toggle from
;;    the Timidity menu), the following keys have special actions:
;;
;;    <RETURN> plays the test melody with the patch at point
;;             (or the drum pattern if it is a drum patch at point)
;;    t        lets you define a new test melody (a KeyKit phrase)
;;    d        lets you define a new drum pattern (a KeyKit phrase)
;;    b, n     jump to next bank/drumset
;;    p        jump to previous bank/drumset
;;    a        set amplification
;;    +        rise amplification by 50%
;;    -        lower amplification by 33%
;;    *        rise amplification by 50% for the whole bank/drumset
;;    /        lower amplification by 33% for the whole bank/drumset
;;    o        comment out the current line (delete a mix line)
;;    i        uncomment the current line
;;    s        stores the patch at point in a KeyKit variable
;;    e        stores the patch and the test melody as a KeyKit variable
;;    m        import the patch and the test melody as two Compositor boxes
;;    r        import the patch by replacing an existing Compositor box
;;    f        edit the current line soundfont with 'timcfg-sf2-editor
;;

;;; Installation:
;;---------------                   
;;
;; install the rest of the keykit-mode package,
;; then add the following to your .emacs:
;;
;;   (require 'timidity-cfg)
;;
;; if you want a given *.cfg file to be edited in timidity-cfg mode
;; by default, add the following in its first line:
;;
;;     #-*-timidity-cfg-*-


;; last modified February 11, 2008

(require 'cl)
(require 'keykit-mode)


(defgroup timidity-cfg nil
  "Major mode for editing TiMidity++ configuration files"
  :group 'keykit
  :prefix "timcfg-")

(defcustom timcfg-timidity-executable-linux "$SURMULOTDIR/timidity/timidity"
  "TiMidity++ binary for linux"
  :type 'string
  :group 'spfa-paths
  :group 'timidity-cfg)

(defun timcfg-timidity-executable-linux ()
  (substitute-in-file-name timcfg-timidity-executable-linux))

(defcustom timcfg-timidity-executable-win "$SURMULOTDIR/timidity/timidity.bat"
  "TiMidity++ executable for windows"
  :type 'string
  :group 'spfa-paths
  :group 'timidity-cfg)

(defun timcfg-timidity-executable-win ()
  (substitute-in-file-name timcfg-timidity-executable-win))

(defun timcfg-timidity-executable ()
  (if (equal system-type 'gnu/linux)
      (timcfg-timidity-executable-linux)
    (timcfg-timidity-executable-win)))

(defcustom timcfg-timidity-options " -EFchorus=0 -EFreverb=0 -A80 -s44100 -p128 -Od "
  "Timidity command-line options"
  :type 'string
  :group 'timidity-cfg)

(defcustom timcfg-timidity-program-options 
  (if (eq system-type 'gnu/linux) "-Ei" "-I") 
  ;; safe default: linux has more recent versions of timidity than windows
  "Timidity command-line option for setting a default program
\(either -I or -Ei, depending on the version)"
  :type 'string
  :group 'timidity-cfg)

(defcustom timcfg-midi-test-file "$SURMULOTDIR/timidity/test.mid"
  "MIDI file to be played when testing a patch"
  :type 'string
  :group 'spfa-paths
  :group 'timidity-cfg)

(defun timcfg-midi-test-file ()
  (substitute-in-file-name timcfg-midi-test-file))

(defvar timcfg-midi-test-file-duration 20) ;;; à revoir

(defcustom timcfg-midi-test-length 0
  "Number of notes to keep from a midi file when defining the testing melody
if 0, take all of it"
  :type 'number
  :group 'timidity-cfg)

(defcustom timcfg-midi-drumtest-file "$SURMULOTDIR/timidity/drumtest.mid"
  "MIDI file to be played when testing a drum"
  :type 'string
  :group 'timidity-cfg)

(defun timcfg-midi-drumtest-file ()
  (substitute-in-file-name timcfg-midi-drumtest-file))

(defcustom timcfg-midi-mix-file "$SURMULOTDIR/timidity/mix.mid"
  "Temporary MIDI file used to play a mix"
  :type 'string
  :group 'spfa-paths
  :group 'timidity-cfg)

(defun timcfg-midi-mix-file ()
  (substitute-in-file-name timcfg-midi-mix-file))

(defcustom timcfg-drum-template "'ad40,a,a,a'"
  "Musical phrase used as rhythm template when testing a drum, in Keykit format"
  :type 'string
  :group 'timidity-cfg)

(defvar timcfg-previous-drum-template "")
(defvar timcfg-previous-note "")
(defvar timcfg-melody "''")

(defcustom timcfg-bad-word "BEURK"
  "Label used when commenting out a patch.
Warning: do not use characters having special meanings in a regexp"
  :type 'string
  :group 'timidity-cfg)

(defcustom timcfg-sf2-editor "$SURMULOTDIR/soundfonts/viena/viena.exe"
  "Binary for a soundfont editor program"
  :type 'string
  :group 'spfa-paths
  :group 'timidity-cfg)

(defun timcfg-sf2-editor ()
  (substitute-in-file-name timcfg-sf2-editor))

(defun timcfg-play-midi-file (midi-file &rest options)
  (interactive "f")
  (save-window-excursion
   (with-temp-buffer
     (cd (file-name-directory (timcfg-timidity-executable)))
     (shell-command (format "\"%s\" %s %s %s &" (timcfg-timidity-executable)
			    timcfg-timidity-options
			    (mapconcat 'identity options " ")
			    (if (eq system-type 'windows-nt)
				(w32-short-file-name
				 (expand-file-name midi-file))
			      (expand-file-name midi-file)))
		    (generate-new-buffer " *async timidity*")))))

(defun timcfg-csound-play-soundfont (soundfont bank preset midi-file)
  (require 'csound-x)
  (with-temp-buffer 
    (insert (format 
"<CsoundSynthesizer>
<CsOptions>
-dodac -TF \"%s\"
</CsOptions>
<CsInstruments>
sr = 44100
ksmps = 100
nchnls = 2
giengine  fluidEngine
isfnum    fluidLoad \"%s\", giengine, 1
          fluidProgramSelect giengine, 1, isfnum, %s, %s
instr 1
          mididefault     60, p3
          midinoteonkey   p4, p5
  ikey    init            p4
  ivel    init            p5
          fluidNote       giengine, 1, ikey, ivel
endin
instr 99
  imvol   init            70000
  asigl, asigr fluidOut   giengine
          outs            asigl * imvol, asigr * imvol
endin
</CsInstruments>
<CsScore>
i 1 0 100000
i 99 0 100000
e
</CsScore>
</CsoundSynthesizer>

"
midi-file soundfont bank preset))
    (csound-csd-mode)
    (cscsd-process nil nil nil "$csound -dodac %s &")))

(defun timcfg-soundfont-presets (soundfont)
  (require 'csound-x)
  (let ((tmpfile (make-temp-file "tmp" nil ".csd")))
    (with-temp-file tmpfile
      (insert (format 
"<CsoundSynthesizer>
<CsInstruments>
sr = 44100
ksmps = 100
giengine  fluidEngine
isfnum    fluidLoad \"%s\", giengine, 1
instr dummy
endin
</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>" soundfont)))
    (unwind-protect
	(with-temp-buffer
	  (insert (cscsd-call-csound-to-string "$csound -d %s" tmpfile))
	  (goto-char (point-min))
	  (loop 
	   while (re-search-forward 
		  "^SoundFont:.*?Bank:\\(.*?\\)Preset: +\\([^ ]+\\) +\\(.*?\\) *$" 
		  nil t)
	   collect (list (match-string 3) 
			 (read (match-string 1)) 
			 (read (match-string 2))) into presets
	   finally return presets))
      (delete-file tmpfile))))

;TEST (timcfg-soundfont-presets "Doumbek-Faisal.sf2") => '(("Doumbek1" 0 0) ("Doumbek2" 128 0))

(defun timcfg-insert-presets ()
  (let ((invalid-preset "#!! invalid preset !")
	soundfont last-soundfont presets)
    (save-excursion
      (goto-char (point-min))      
      (while (re-search-forward "^[^#\n\r]+\\%font[ \t].*sf2" nil t)
	(unless (or (save-excursion
		      (search-forward "#" (point-at-eol) t)) ;; already done
		    (save-excursion
		      (search-forward " 128 " (point-at-eol) t))) ; drumset
	  (setq soundfont (timcfg-soundfont-at-point))
	  (unless (equal soundfont last-soundfont)
	    (setq last-soundfont soundfont
		  presets (timcfg-soundfont-presets soundfont)))
	  (let* ((bank (when (re-search-forward "[0-9]+" nil t)
			 (read (match-string 0))))
		 (preset (when (re-search-forward "[0-9]+" nil t)
			   (read (match-string 0)))))
	    (goto-char (point-at-eol))
	    (insert " #" 
		    (or
		     (car (find-if (lambda (p) 
				     (and (= bank (cadr p))
					  (= preset (caddr p))))
				   presets))
		     invalid-preset))))))
    (occur invalid-preset)))
  
;;; --------------------------- keymap  -----------------------------

(defvar timidity-cfg-mode-map (make-sparse-keymap)
  "Keymap used in `timidity-cfg-mode'")

(define-key timidity-cfg-mode-map "8" 'timcfg-electric-8)
(define-key timidity-cfg-mode-map "2" 'timcfg-electric-2)
(define-key timidity-cfg-mode-map "9" 'timcfg-electric-9)
(define-key timidity-cfg-mode-map "3" 'timcfg-electric-3)
(define-key timidity-cfg-mode-map "x" 'timcfg-electric-x)
(define-key timidity-cfg-mode-map "e" 'timcfg-electric-e)
(define-key timidity-cfg-mode-map "m" 'timcfg-electric-m)
(define-key timidity-cfg-mode-map "r" 'timcfg-electric-r)
(define-key timidity-cfg-mode-map "s" 'timcfg-electric-s)
(define-key timidity-cfg-mode-map "a" 'timcfg-electric-a)
(define-key timidity-cfg-mode-map "+" 'timcfg-amp-plus)
(define-key timidity-cfg-mode-map "-" 'timcfg-amp-minus)
(define-key timidity-cfg-mode-map "*" 'timcfg-amp-plus-b)
(define-key timidity-cfg-mode-map "/" 'timcfg-amp-minus-b)
(define-key timidity-cfg-mode-map "b" 'timcfg-electric-b)
(define-key timidity-cfg-mode-map "p" 'timcfg-electric-p)
(define-key timidity-cfg-mode-map "n" 'timcfg-electric-n)
(define-key timidity-cfg-mode-map "\r" 'timcfg-electric-return)
(define-key timidity-cfg-mode-map "o" 'timcfg-electric-o)
(define-key timidity-cfg-mode-map "i" 'timcfg-electric-i)
(define-key timidity-cfg-mode-map "t" 'timcfg-electric-t)
(define-key timidity-cfg-mode-map "d" 'timcfg-electric-d)
(define-key timidity-cfg-mode-map "f" 'timcfg-electric-f)


;;; --------------------------- major mode definition ---------------------------

(defun timidity-cfg-mode ()
  "Major mode for editing TiMidity++ configuration files. 
Requires the prior installation of keykit-mode and its correct setting.

When the buffer is in read-only mode \(which you can toggle from the Timidity menu, or from the mode line), the following keys have special actions:

    <RETURN> plays the test melody with the patch at point
             (or the drum pattern if it is a drum patch at point)
    t        lets you define a new test melody (a KeyKit phrase)
    d        lets you define a new drum pattern (a KeyKit phrase)
    b, n     jump to next bank/drumset
    p        jump to previous bank/drumset
    a        set amplification
    +        rise amplification by 50%
    -        lower amplification by 33%
    *        rise amplification by 50% for the whole bank/drumset
    /        lower amplification by 33% for the whole bank/drumset
    o        comment out the current line (delete a mix line)
    i        uncomment the current line
    s        stores the patch at point in a KeyKit variable
    e        stores the patch and the test melody as a KeyKit variable
    m        import the patch and the test melody as two Compositor boxes
    r        import the patch by replacing an existing Compositor box
    f        edit the current line soundfont

The corresponding bindings are:

\\[timidity-cfg-mode-map]

Turning on timidity-cgf mode calls the value of the variable `timidity-cgf-mode-hook'
with no args, if that value is non-nil.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map timidity-cfg-mode-map)
  (setq major-mode 'timidity-cfg-mode)
  (setq mode-name "CFG")
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (toggle-read-only t)
  (timcfg-fontify)
  (timidity-cfg-define-menu)
  (timcfg-make-buttons)
  (run-hooks 'timidity-cfg-mode-hook))


(defun timidity-cfg-define-menu ()
  (easy-menu-define timidity-cfg-mode-menu timidity-cfg-mode-map
    "Menu provided by `timidity-cfg-mode'"
    `("Timidity"
      ["Read only" toggle-read-only 
       :style toggle :selected buffer-read-only]
      ["Refresh buttons" timcfg-make-buttons t]
      "--"
      ["Set amplification" timcfg-write-amp t]
      "--"
      ["Define the drum template" timcfg-change-drum-template t]
      ["Play drum at point" timcfg-play-drum-at-point  t]
      ["Define the test melody" timcfg-change-melody  t]
      ,@(when (require 'csound-mid nil t)
	   (csmid-midifiles-submenu 'test-melody
				    "... from MIDI file"
				    'timidity-cfg-define-menu
				    'timcfg-change-melody-to-file
				    (file-name-directory (timcfg-midi-test-file))))
      ["Play patch at point (Timidity)" 
       timcfg-timidity-play-patch-at-point  t]
      ["Play soundfont at point (Csound)" 
       timcfg-csound-play-soundfont-at-point  t]
      "--"
      ["Edit file at point" timcfg-edit-sound-file-at-point  t]
      ["List required files" (message "Files: %s" (timcfg-files))  t]
      ["List missing files" (message "Missing files: %s" (timcfg-missing-files))  t]
      ["Total files size" (message "Files total size: %s" (timcfg-files-size))  t]
      "--"
      ["Describe mode" describe-mode t]
      "--"
      ["Send patch at point to Squeak" timcfg-send-patch-to-squeak 
       (and (featurep 'squeak) (squeak-connected-somehow-p))])))


;;; --------------------------- navigation -----------------------------

(defun timcfg-patch-line-p ()
  (and (timcfg-current-patch)
       (timcfg-current-bank)))

(defun timcfg-mix-line-p ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^#\\^\\(.+\\)\\^[ \t]")
	(match-string 1)
      nil)))

(defun timcfg-current-bank ()
  (save-excursion
    (if (timcfg-previous-bank)
	(progn
	  (re-search-forward "[0-9]+")
	  (match-string 0))
    nil)))

(defun timcfg-current-bank-type ()
  (save-excursion
    (if (timcfg-previous-bank)
	(progn
	  (re-search-forward "^[a-z]+")
	  (intern (match-string 0)))
      nil)))

(defun timcfg-next-bank ()
  (interactive)
  (re-search-forward "^\\(bank\\|drumset\\)[ \t]*\\([0-9]+\\)" nil t))

(defun timcfg-previous-bank ()
  (interactive)
  (re-search-backward "^\\(bank\\|drumset\\)[ \t]*\\([0-9]+\\)" nil t))

(defun timcfg-current-patch ()
  (save-excursion
    (beginning-of-line) 
    (if (looking-at "[0-9]+") 
	(match-string 0)
      nil)))

(defun timcgf-current-patch-name ()
  (save-excursion
    (beginning-of-line) 
    (if (looking-at ".*#[ ]*\\(.*\\)$") 
	(match-string 1)
	(forward-line -1)
	(beginning-of-line) 
	(if (looking-at ".*#n[ ]*\\(.*\\)$") 
	  (match-string 1)
	  nil))))

(defun timcfg-end-of-bank ()
  (save-excursion
    (end-of-line)
    (goto-char (if (timcfg-next-bank) (point) (point-max)))
    (forward-line -1)
    (end-of-line)
    (point)))

;;; --------------------------- keymap commands -----------------------------

(defun timcfg-electric-b ()
  "Go to next bank"
  (interactive)
  (if buffer-read-only (timcfg-next-bank)
    (insert "b")))

(defun timcfg-electric-n ()
  "Go to next bank"
  (interactive)
  (if buffer-read-only (timcfg-next-bank)
    (insert "n")))

(defun timcfg-electric-p ()
  "Go to previous bank"
  (interactive)
  (if buffer-read-only (timcfg-previous-bank)
    (insert "p")))

(defun timcfg-electric-m ()
  "Import boxes into the Compositor tool"
  (interactive)
  (if buffer-read-only (timcfg-import-boxes)
    (insert "m")))

(defun timcfg-electric-r ()
  "Import patch at point into the Compositor tool as a box replacement"
  (interactive)
  (if buffer-read-only (timcfg-replace-box)
    (insert "r")))

(defun timcfg-electric-s ()
  "Store patch at point in a keyKit variable"
  (interactive)
  (if buffer-read-only (timcfg-snarf-patch nil)
    (insert "s")))

(defun timcfg-electric-e ()
  "Store patch at point and test melody as a keyKit variable"
  (interactive)
  (if buffer-read-only (timcfg-snarf-patch t)
    (insert "e")))

(defun timcfg-electric-f ()
  "Edit soundfont at point"
  (interactive)
  (if buffer-read-only (timcfg-edit-sound-file-at-point)
    (insert "f")))

(defun timcfg-electric-x ()
  "Add a new component to a mix"
  (interactive)
  (if buffer-read-only (timcfg-add-to-mix)
    (insert "x")))

(defun timcfg-electric-8 ()
  "Add a new component to a mix"
  (interactive)
  (if buffer-read-only (timcfg-mix-move-up 1)
    (insert "8")))

(defun timcfg-electric-9 ()
  "Add a new component to a mix"
  (interactive)
  (if buffer-read-only (timcfg-mix-move-up 10)
    (insert "9")))

(defun timcfg-electric-2 ()
  "Add a new component to a mix"
  (interactive)
  (if buffer-read-only (timcfg-mix-move-down 1)
    (insert "2")))

(defun timcfg-electric-3 ()
  "Add a new component to a mix"
  (interactive)
  (if buffer-read-only (timcfg-mix-move-down 10)
    (insert "3")))

(defun timcfg-electric-return ()
  "Play the current patch if any, else jump to next patch"
  (interactive)
  (if buffer-read-only
      (if (timcfg-patch-line-p)
	  (if (eq (timcfg-current-bank-type) 'drumset)
	      (timcfg-play-drum-at-point)
	    (if (eq (timcfg-current-bank-type) 'bank)
		(timcfg-play-patch-at-point)))
	(if (timcfg-mix-line-p)
	    (timcfg-listen-mix-at-point)
	  (re-search-forward "^[0-9]+" nil t)))
    (insert "\n")))

(defun timcfg-electric-t()
  "Invoke KeyKit for defining a new test melody."
  (interactive)
  (if buffer-read-only (timcfg-change-melody)
    (insert "t")))

(defun timcfg-electric-d()
  "Query for a new drum template."
  (interactive)
  (if buffer-read-only (timcfg-change-drum-template)
    (insert "d")))

(defun timcfg-electric-o ()
  "Comment out the current patch if any, delete a mix line"
  (interactive)
  (if buffer-read-only
      (if (timcfg-patch-line-p)
	  (with-read-only-status
	    (beginning-of-line)
	    (insert "#" timcfg-bad-word "  "))
	(when (timcfg-mix-line-p)
	  (timcfg-kill-line)))
    (insert "o")))

(defun timcfg-electric-i ()
  "Uncomment the current patch"
  (interactive)
  (if buffer-read-only
      (with-read-only-status
       (beginning-of-line)
       (if (looking-at (concat "\\(#" timcfg-bad-word "[ \t]*\\)[0-9]+"))
	   (replace-match "" t t nil 1)))
    (insert "i")))

(defun timcfg-amp-plus-b ()
  "Rise amplification by 50% in whole bank"
  (interactive)
  (if buffer-read-only
      (with-whole-bank
       (timcfg-write-amp (round (* 1.5 (timcfg-read-amp)))))
    (insert "*")))

(defun timcfg-amp-minus-b ()
  "Lower amplification by 33% in whole bank"
  (interactive)
  (if buffer-read-only
      (with-whole-bank
	(timcfg-write-amp (round (* 2 (/ (timcfg-read-amp) 3)))))
    (insert "/")))

(defun timcfg-amp-plus ()
  "Rise amplification by 50%"
  (interactive)
  (if (and buffer-read-only
	   (timcfg-patch-line-p))
	(timcfg-write-amp (round (* 1.5 (timcfg-read-amp))))
    (insert "+")))

(defun timcfg-amp-minus ()
  "Lower amplification by 33%"
  (interactive)
  (if (and buffer-read-only
	   (timcfg-patch-line-p))
	(timcfg-write-amp (round (* 2 (/ (timcfg-read-amp) 3))))
    (insert "-")))

(defun timcfg-read-amp ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "amp=\\([0-9]+\\)" (save-excursion (end-of-line) (point)) t)
	(string-to-number (match-string 1))
      (if (timcfg-patch-line-p)
	  100
	nil))))

(defun timcfg-write-amp (&optional val)
  (interactive)
  (let ((sval (if val (number-to-string val)
		(read-from-minibuffer "amp="))))
    (if (timcfg-patch-line-p)
      (with-read-only-status
       (beginning-of-line)
       (let ((beg (point)))
	 (if (re-search-forward "amp=\\([0-9]+\\)" 
				(save-excursion (end-of-line) (point)) t)    
	     (replace-match sval t t nil 1)
	   (end-of-line)
	   (search-backward "#" beg t)
	   (skip-chars-backward " \t")
	   (insert " amp=" sval)))))))

(defun timcfg-electric-a ()
  "Set the current amplification"
  (interactive)
  (if buffer-read-only
      (timcfg-write-amp)
    (insert "a")))

(defmacro with-read-only-status (&rest body)
  `(let ((read-only-status buffer-read-only))
     (if read-only-status
	 (toggle-read-only))
     ,@body
     (if read-only-status
	 (toggle-read-only))))

    
(defmacro with-whole-bank (&rest body)
 `(save-excursion
    (timcfg-previous-bank)
    (while (< (point) (timcfg-end-of-bank))
      (forward-line 1)
      (if (timcfg-patch-line-p)
	  ,@body))))

;;; --------------------------- menu items -----------------------------


(defun timcfg-play-drum-at-point ()
  "Call Timidity and play the drum at point.
This only works in drumsets: for a regular patch, use `timcfg-timidity-play-patch-at-point'"
  (interactive)
  (let ((midi-test (timcfg-midi-drumtest-file))    ;(file-exists-p timcfg-midi-drumtest-file)
	(note (save-excursion
		 (beginning-of-line) 
		 (word-at-point)))
 	(bank (save-excursion
		   (when (re-search-backward "^bank[ \t]*\\([0-9]+\\)" nil t)
		       (match-string 1))))
 	(drumset (save-excursion
		   (when (re-search-backward "^drumset[ \t]*\\([0-9]+\\)" nil t)
		       (match-string 1)))))
    (if (and bank drumset)
      (with-temp-buffer
	(when (file-exists-p midi-test)
	  (delete-file midi-test))
	(insert "ph = " timcfg-drum-template "\n"
		"ph.chan = 10 \n"
		"ph.pitch = " note "\n"
		"ph = progchange(" drumset " + 1,10) + ph \n"
		"ph = controller(10,0," bank ") + delay(ph,2) \n"
		"writemf(ph, \"" midi-test "\")")
	(kk-run-lowkey "1" 1 (point-max))
	(timcfg-play-midi-file midi-test))
      (unless drumset
	(message "no drumset specified !"))
      (unless bank
	(message "no bank specified !")))))
  
(defun timcfg-change-drum-template (&optional melody)
  "Query for a new drum template."
  (interactive)
  (setq timcfg-drum-template (or melody
				 (read-from-minibuffer "KeyKit phrase> "))))

(defun timcfg-timidity-play-patch-at-point (&optional midi-test)
  "Call Timidity and play a few notes with the patch at point.
This only works in banks: for a drumset, use `timcfg-play-drum-at-point'"
  (interactive)
  (let ((midi-test (or midi-test (timcfg-midi-test-file)))
	(patch (save-excursion
		 (beginning-of-line) 
		 (word-at-point)))
	(bank (save-excursion
		(if (re-search-backward "^bank[ \t]*\\([0-9]+\\)" nil t)
		    (match-string 1)
		  nil))))
    (if bank
	(timcfg-play-midi-file midi-test (concat "-EB" bank) 
			       (concat timcfg-timidity-program-options patch))
      (message "no bank specified !"))))

(defun timcfg-soundfont-at-point ()
  (save-excursion
    (goto-char (point-at-bol))
    (when (re-search-forward "font[ \t]+\\(.*\\.sf2\\)" (point-at-eol) t)
			  (match-string-no-properties 1))))

(defun timcfg-csound-play-soundfont-at-point (&optional midi-test)
  "Call Csound and play a few notes with the soundfont at point"
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (let* ((midi-test (or midi-test (timcfg-midi-test-file)))
	   (soundfont (when (re-search-forward "font[ \t]+\\(.*\\.sf2\\)" nil t)
			  (match-string 1)))
	   (bank (when (re-search-forward "[0-9]+" nil t)
		     (match-string 0)))
	   (preset (when (re-search-forward "[0-9]+" nil t)
		       (match-string 0))))
      (when (and soundfont bank preset)
	(timcfg-csound-play-soundfont soundfont bank preset midi-test)))))

(defalias 'timcfg-play-patch-at-point 'timcfg-timidity-play-patch-at-point)

(defun timcfg-make-buttons ()
  (interactive)
  (when (require 'embedded-elisp-library nil t)     
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(^\\([0-9]+\\|source\\)\\)[ \t]" nil t)
	(save-excursion
	  (beginning-of-line)
	  (eel-wake-up-buttons 
	   `((,(match-string 1) . 'timcfg-do-the-button-thing))))))))

(defun timcfg-do-the-button-thing ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "\\(^[0-9]+\\)[ \t]")
	(if (eq (timcfg-current-bank-type) 'drumset)
	    (timcfg-play-drum-at-point)
	  (timcfg-play-patch-at-point))
      (if (looking-at "^source[ \t]+\\([^ \t\n\r]*\\)")
	  (find-file (expand-file-name (match-string 1)))))))
	  

(defun timcfg-change-melody (&optional melody)
  "Invoke KeyKit for defining a new test melody."
  (interactive)
  (setq timcfg-melody (or melody (read-from-minibuffer "KeyKit phrase> ")))
  (with-key-or-lowkey "writemf(" timcfg-melody ", \"" (timcfg-midi-test-file) "\")")
  (if (timcfg-patch-line-p) (timcfg-play-patch-at-point)))

(defun timcfg-change-melody-to-file (midifile)
  "Define the test melody as the first `timcfg-midi-test-length' notes of MIDIFILE"
  (interactive)
  (when (file-exists-p midifile)
    (if (zerop timcfg-midi-test-length)
        (copy-file midifile (timcfg-midi-test-file) t)
      (with-key-or-lowkey 
	  "ph=readmf(\"" midifile "\");writemf((ph-cut(ph,CUT_TYPE,PROGRAM)){??.number<=" (format "%s" timcfg-midi-test-length) "}, \"" (timcfg-midi-test-file) "\")"))))


;;; --------------------------- mixes ---------------------------

(defun timcfg-add-to-mix (&optional mix channel ph)
  (interactive)
  (let* ((bank (timcfg-current-bank))
	 (patch (timcfg-current-patch))
	 (mix (or mix (read-from-minibuffer "mix name> ")))
	 (ch (or channel (read-from-minibuffer "channel> " "1")))
	 (ph (read-from-minibuffer "phrase> " timcfg-melody)))
    (when (and bank patch)
      (end-of-line)
      (with-read-only-status
       (insert "\n#^" mix "^ " ch " " ph)))))


(defun timcfg-listen-mix-at-point ()
  "Call Timidity and play the mix at point"
  (interactive)
  (let ((mix (timcfg-mix-line-p)))
    (when mix
      (with-key-or-lowkey
       (kk-eval 
	(concat "writemf(" (timcfg-get-mix-phrase mix) ",\"" (timcfg-midi-mix-file) "\")")))
      (shell-command
       (concat (if (equal system-type 'gnu/linux)
		   (timcfg-timidity-executable-linux)
		 (timcfg-timidity-executable-win))
	       " " timcfg-timidity-options " " (timcfg-midi-mix-file) " &")
       (generate-new-buffer " *async timidity*")))))

(defun timcfg-get-mix-phrase (mix)
 (save-excursion
   (let ((mixph "''"))
     (goto-char (point-min))
     (while (re-search-forward (concat "#\\^" mix "\\^[ \t]\\([0-9]+\\)[ \t]\\(.*\\)$") nil t)
       (let* ((ch (match-string 1))
	      (ph (match-string 2))
	      (patch (save-excursion
		       (while (null (timcfg-patch-line-p))
			 (forward-line -1))
		       (concat "PourAC(" (timcfg-current-bank) "," 
			       (timcfg-current-patch) "," ch ")"))))
	 (setq mixph (concat mixph "|(" patch "+ SetChan(" ph "," ch "))"))))
     mixph)))

(defun timcfg-kill-line ()
  (with-read-only-status
   (beginning-of-line)
   (kill-line)
   (backward-char 1) 
   (delete-char 1)))

(defun timcfg-mix-move-up (n)
  (when (timcfg-mix-line-p)
    (timcfg-kill-line)
    (while (and (> n 0)
		(> (count-lines (point-min) (point)) 0))
      (if (timcfg-patch-line-p)
	  (setq n (- n 1)))
      (forward-line -1))
    (end-of-line)
    (with-read-only-status
     (insert "\n")
     (yank))))

(defun timcfg-mix-move-down (n)  
  (when (timcfg-mix-line-p)
    (timcfg-kill-line)
    (while (and (> n 0)
		(> (count-lines (point) (point-max)) 0))
      (if (timcfg-patch-line-p)
	  (setq n (- n 1)))
      (forward-line 1))
    (end-of-line)
    (with-read-only-status
     (insert "\n")
     (yank))))


;;; --------------------------- GeoMaestro interface -----------------------------


(defun with-key-or-lowkey (&rest command)
  "Evaluate KeyKit COMMAND, preferably with KeyKit through TCP/IP,
else with lowkey if no connection is available"
  (if (and kk-tcpip-process (zerop (process-exit-status kk-tcpip-process)))
      (kk-eval (mapconcat 'identity command ""))
    (with-temp-buffer
      (insert (mapconcat 'identity command ""))
      (kk-run-lowkey "1" 1 (point-max)))))

(defmacro with-keykit-tcpip (&rest body)
  "Ensure that a TCP-IP connection with KeyKit is enabled before evaluating BODY"
  `(when (or (and kk-tcpip-process (zerop (process-exit-status kk-tcpip-process)))
	       (if (yes-or-no-p  "We are not connected right now ! Start a connection ?")
		   (progn (kk-tcpip) t) nil))
	 ,@body))

(defmacro with-open-compositor (&rest body)
  "Ensure that the LastGMCOMPO tool is open in current KeyKit page"
  `(if (string= "0" (kk-eval-with-return "BringCompositorUp()"))
       (message "Compositor was not found !")
     ,@body))

(defmacro with-open-gui (&rest body)
  "Ensure that the LastGMGUI tool is open in current KeyKit page"
  `(if (string= "0" (kk-eval-with-return "BringGUIUp()"))
       (message "Main GUI was not found !")
     ,@body))


(defun timcfg-import-boxes (&optional channel) ;;; à améliorer...
  ""
  (interactive)
  (let* ((bank (timcfg-current-bank))
	 (patch (timcfg-current-patch))
	 (pos "xy(300,200)")
	 (dx "40")
	 (ch (or channel (read-from-minibuffer "channel: " "1")))
	 (ph (if (string= ch "1")
		 timcfg-melody
	       (concat "SetChan(" timcfg-melody "," ch ")"))))
    (if (and bank patch)
	(with-keykit-tcpip
	 (with-open-compositor
	  (kk-eval
	   (concat "LastGMCOMPOf.importation(string(" ph "), "
		   "[\"rx\"=" dx ",\"ry\"=0,\"nb\"="
		   (kk-eval-with-return 
		    (concat
		     "LastGMCOMPOf.importation(\"PourAC(" bank "," patch ","
		     ch ")\", " pos ", \"" bank "-" patch ":" ch "\")[\"i\"]"))
		   "], string(" ph "))")))))))


(defun timcfg-replace-box (&optional nb channel) 
  ""
  (interactive)
  (let* ((bank (timcfg-current-bank))
	 (patch (timcfg-current-patch))
	 (nb (or nb (read-from-minibuffer "box number: ")))
	 (ch (or channel (read-from-minibuffer "channel: " "1"))))
    (if (and bank patch)
	(with-keykit-tcpip
	 (with-open-compositor
	  (kk-eval
	   (concat
	    "LastGMCOMPOf.importation(\"PourAC(" bank "," patch "," ch ")\","
	    nb ", \"" bank "-" patch ":" ch "\")")))))))


(defun timcfg-snarf-patch (melodyp &optional channel var)
  ""
  (interactive)
  (let* ((bank (timcfg-current-bank))
	 (patch (timcfg-current-patch))
	 (ch (or channel (read-from-minibuffer "channel: " "1")))
	 (ph (if (string= ch "1")
		 timcfg-melody
	       (concat "SetChan(" timcfg-melody "," ch ")")))
	 (var (or var (read-from-minibuffer "variable: " "Snarf"))))
    (if (and bank patch)
	(with-keykit-tcpip
	 (kk-eval (concat var " = PourAC(" bank "," patch "," ch ")"
			  (if melodyp (concat "+" ph) "")))))))

(defun timcfg-snarf-mix ()
  (interactive)
  (let ((mix (timcfg-mix-line-p))
	(var (read-from-minibuffer "variable: " "Snarf")))
    (when mix
      (kk-eval (concat var "=" (timcfg-get-mix-phrase mix))))))


;;; --------------------------- Squeak interface -----------------------------

(defun timcfg-send-patch-to-squeak ()
  ""
  (interactive)
  (when (and (featurep 'squeak) (squeak-connected-somehow-p))
    (let* ((bank (timcfg-current-bank))
	   (patch (timcfg-current-patch)))
      (when (and bank patch)
	(muo-get-patch bank patch
		       (format "%s %s:%s" 
			       (or (timcgf-current-patch-name)
				   (read-from-minibuffer "patch name: "))
			       bank patch))))))


;;; --------------------------- sound files support -----------------------------

(defun timcfg-files ()
  "return a list of all sound files required by the current configuration"
  (interactive)
  (remove-duplicates
   (mapcar (lambda (spec) (or (plist-get spec :pat) (plist-get spec  :sf2))) 
	   (timcfg-parse-buffer))
   :test 'string=))
  
(defun timcfg-missing-files ()
  (interactive)
  "return a list of all sound files required by the current configuration that can not be found"
  (remove-if 'file-exists-p (timcfg-files)))

(defun timcfg-files-size ()
  "return the size in kilobytes of all the sound files required by the current configuration"
  (interactive)
  (/ 
   (apply '+ (mapcar 'float
		     (delete nil (mapcar (lambda (f) (nth 7 (file-attributes f)))
					 (timcfg-files)))))
   1024))

(defun timcfg-parse-buffer ()
  (timcfg-parse-file (buffer-file-name)))

(defun timcfg-parse-file (filename &optional dir)
  "return a list of specification for each program definition in the config file FILENAME
if they are recursively sourced configuration files, there are taken into account"
  (let* ((filedir (file-name-directory filename))
	 (dir (or dir filedir))
	 (bank 0)
	 (drumset 0)
	 (all ()))
    (with-temp-buffer
      (insert-file-contents filename)
      (flush-lines "^[ \t]*#")
      (flush-lines "^[ \t]*$")
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((spec (timcfg-parse-line (thing-at-point 'line) dir bank drumset))
	       (action (first spec)))
	  ;; regular program definition
	  (when (or (eq action :bank) (eq action :drumset))
	    (setq all (append all (list spec))))
	  ;; sourcing another cfg
	  (when (eq action :source)
	    (setq all (append all (timcfg-parse-file 
				   (expand-file-name (second spec) filedir) 
				   dir))))
	  ;; changing dir
	  (when (eq action :dir)
	    (setq dir (expand-file-name (second spec) dir)))
	  ;; changing bank
	  (when (eq action :newbank)
	    (setq bank (string-to-number (second spec))
		  drumset nil))
	  ;; changing drumset
	  (when (eq action :newdrumset)
	    (setq drumset (string-to-number (second spec))
		  bank nil)))
	(forward-line)))
    all))

(defun timcfg-parse-line (string dir bank drumset)
  (let* ((items (split-string string))
	 (command (first items))
	 (second (second items))
	 (beg (if (null bank) (list :drumset drumset) (list :bank bank))))
    (if (string= command "bank")
      (list :newbank second)
      (if (string= command "drumset")
	(list :newdrumset second)
	(if (string= command "dir")
	  (list :dir second)
	  (if (string= command "source")
	    (list :source second)
	    (if (string-match "[0-9]+" command)
	      (append beg
		      (if (string= "%font" second)
			(list :prog (string-to-number command) 
			      :sf2 (expand-file-name (third items) dir)
			      :sf2bank (fourth items)
			      :sf2patch (fifth items))
			(list :prog (string-to-number command) 
			      :pat (concat (file-name-sans-extension
					    (expand-file-name second dir)) ".pat")))
		      (when (string-match "# *\\(.*\\)$" string)
			(list :name (match-string 1 string))))
	      (list :dnu))))))))
	  
(defun timcfg-sound-file-at-point ()
  (let* ((dir (or (save-excursion
		    (if (re-search-backward "^[ \t]*dir[ \t]+\\([^# \t\r\n]+\\)$" nil t)
		      (match-string 1)
		      nil))
		  (file-name-directory (buffer-file-name))))                
	 (spec (timcfg-parse-line (thing-at-point 'line) dir 0 0)))
    (or (plist-get spec :pat) (plist-get spec :sf2))))

(defun timcfg-edit-sound-file (filename)
    (call-process (timcfg-sf2-editor) nil 0 nil filename))

(defun timcfg-edit-sound-file-at-point ()
  (interactive)
  (let ((f (timcfg-sound-file-at-point)))
    (when f (timcfg-edit-sound-file f))))

;;; --------------------------- Font Lock Mode  -----------------------------

(defconst timcfg-font-lock-keywords
  (list
   '("^\\(bank\\|dir\\|source\\|drumset\\).*$"
     0 font-lock-comment-face t)
   '("#.*$"
     0 font-lock-type-face t)
   '("^#\\^\\(.+\\)^" 
     1 font-lock-warning-face t)
   '("^#\\^.+^[ \t]\\([0-9]+\\)" 
     1 font-lock-keyword-face t)
   '("^#\\^.+^[ \t][0-9]+[ \t]\\(.*\\)$" 
     1 font-lock-comment-face t)
  "timidity-cfg-mode fontification"))

(defun timcfg-fontify ()
  "Load the `timcfg-font-lock-keywords' into a local version of `font-lock-keywords'."
  (set (make-local-variable 'font-lock-defaults)
       '(timcfg-font-lock-keywords
         t t nil nil))
  (font-lock-mode 1))

;;; --------------------------- EDB export  -----------------------------

(defun timcfg-write-database (filename)
  (with-temp-file (concat (file-name-sans-extension filename) ".fmt")
    (insert "     File: \\file
     Bank: \\bank
     Program: \\prog
     
     Local"
	    " Variables:
     eval: (database-set-fieldnames-to-list
             database '(file bank prog))
     End:"))
  (let ((all (timcfg-parse-buffer)))
    (with-temp-file (concat (file-name-sans-extension filename) ".dat")
      (dolist (spec all)
	(insert (format "%s\t%s\t%s\n"
			(or (plist-get spec :pat) (plist-get spec :sf2))
			(or (plist-get spec :bank) (plist-get spec :drumset))
			(or (plist-get spec :prog))))))))

; (timcfg-write-database "c:/testdb")

;; this is it
(provide 'timidity-cfg)